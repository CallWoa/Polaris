/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import difftest._

trait HasRegFileParameter {
  val NRReg = 32
  val NSReg = 4
}

class RegFile extends HasRegFileParameter with HasNutCoreParameter {
  val rf = Mem(NRReg, UInt(XLEN.W))
  def read(addr: UInt, fp: Boolean = false): UInt = if(fp) rf(addr) else Mux(addr === 0.U, 0.U, rf(addr))
  def write(addr: UInt, data: UInt) = { rf(addr) := data(XLEN-1,0) }
} 

class ScoreBoard extends HasRegFileParameter {
  val busy = RegInit(0.U(NRReg.W))
  def isBusy(idx: UInt): Bool = busy(idx)
  def mask(idx: UInt) = (1.U(NRReg.W) << idx)(NRReg-1, 0)
  def update(setMask: UInt, clearMask: UInt) = {
    // When clearMask(i) and setMask(i) are both set, setMask(i) wins.
    // This can correctly record the busy bit when reg(i) is written
    // and issued at the same cycle.
    // Note that rf(0) is always free.
    busy := Cat(((busy & ~clearMask) | setMask)(NRReg-1, 1), 0.U(1.W))
  }
}

class InstQueue extends NutCoreModule with HasRegFileParameter{
  val io = IO(new Bundle{
        val setnum     = Input(UInt(log2Up(Queue_num).W))
        val clearnum   = Input(UInt(log2Up(Queue_num).W))
        val HeadPtr    = Output(UInt(log2Up(Queue_num).W))
        val TailPtr    = Output(UInt(log2Up(Queue_num).W))
        val Flag       = Output(UInt(1.W))
        val flush      = Input(Bool())
    })
  val QueueValid = Reg(Vec(32,UInt(1.W)))
  //val QueueFlag  = Reg(Vec(32,UInt(1.W)))
  val HeadPtr = RegInit(0.U(log2Up(Queue_num).W))
  val TailPtr = RegInit(0.U(log2Up(Queue_num).W))
  val FlagNow = RegInit(0.U(1.W))
  def update(setnum: UInt,clearnum:UInt) = {
    val newHeadPtr = setnum + HeadPtr
    val newTailPtr = clearnum + TailPtr
    val startNewQueue = newHeadPtr < HeadPtr

    TailPtr := newTailPtr
    HeadPtr := newHeadPtr
    when(startNewQueue){
      FlagNow := !FlagNow
    }

    for(i <- 0 to Queue_num-1){
      when(i.U >= TailPtr && i.U<newTailPtr){
        QueueValid(i) := false.B
      }
    }

    for(i <- 0 to Queue_num-1){
      when(i.U >= HeadPtr && i.U<newHeadPtr){
        QueueValid(i) := true.B
        //QueueFlag(i) := Mux(i.U < HeadPtr,!FlagNow,FlagNow)
      }
    }
  }
  def flushqueue() = {
    for(i <- 0 to Queue_num-1){
      QueueValid(i) := 0.U
      //QueueFlag(i) := 0.U
      HeadPtr := 0.U
      TailPtr := 0.U
      FlagNow := 0.U
    }
  }
  when(io.flush || reset.asBool){
    flushqueue()
  }otherwise{
    update(io.setnum,io.clearnum)
  }
  io.HeadPtr:=HeadPtr
  io.TailPtr:=TailPtr
  io.Flag   :=FlagNow
  Debug("[Inst_Q] Headptr %x TailPtr %x FlagNow %x set_num %x flush %x\n", HeadPtr,TailPtr, FlagNow,io.setnum,io.flush)
}

class InstBoard(float: Boolean = false) extends NutCoreModule with HasRegFileParameter{
  val io = IO(new Bundle{
      val Wen        = Vec(NRReg, Input(Bool()))
      val clear      = Vec(NRReg, Input(Bool()))
      val WInstNo    = Vec(NRReg, Input(UInt(log2Up(Queue_num).W)))
      val valid      = Vec(NRReg, Output(Bool()))
      val RInstNo    = Vec(NRReg, Output(UInt(log2Up(Queue_num).W)))
      val flush      = Input(Bool())
  })
  val validBoard = Reg(Vec(NRReg,Bool()))
  val InstBoard  = Reg(Vec(NRReg,UInt(log2Up(Queue_num).W)))
  def flushboard() = {
    for(i <- 0 to NRReg-1){
      validBoard(i) := false.B
      InstBoard(i)  := 0.U
    }
  }
  def update() = {
    for(i <- 0 to NRReg-1){
      when(io.Wen(i)){
        InstBoard(i) := io.WInstNo(i)
        validBoard(i):= true.B
      }.elsewhen(io.clear(i)){
        validBoard(i) := false.B
      }
    }
    if(!float){
      validBoard(0):=false.B
    }
  }

  when(io.flush || reset.asBool){
    flushboard()
  }otherwise{
    update()
  }

  io.valid  := validBoard
  io.RInstNo:= InstBoard
}

class SRegFile extends HasRegFileParameter with HasNutCoreParameter {
  val srf = Mem(NSReg, UInt(XLEN.W))
  def read(addr: UInt) : UInt = srf(addr)
  def write(addr: UInt, data: UInt) = { srf(addr) := data(XLEN-1,0) }
} 