package nutcore

import chisel3._
import chisel3.util._

import utils._

class NeuronIO(len: Int = 16) extends NutCoreBundle{
    val in = Input(Vec(XLEN/len, UInt(len.W)))
    val weight = Input(Vec(XLEN/len, UInt(len.W)))
    val vr = Input(UInt(XLEN.W))
    val hasTs = Input(Bool())
    val en = Input(Bool())
    val tau = Input(UInt(XLEN.W))
    val res = Output(Vec(XLEN/len, UInt(len.W)))
}

class LNU_IO extends NutCoreBundle{
    val in = Flipped(Decoupled(new Bundle{
        val dcIn = new DecodeIO
        val SCtrl = new SCtrlIO
    }))

    val out = Decoupled(new Bundle{
        val res = Output(UInt(XLEN.W))
        val dcOut = new DecodeIO
    })
}

class Neuron(len: Int = 16) extends NutCoreModule{
    val io = IO(new NeuronIO(len))

    val nu_res = WireInit(VecInit(Seq.fill(XLEN/len)(0.U(len.W))))
    val nu = WireInit(VecInit(Seq.fill(XLEN/len)(0.U(len.W))))
    val tmp_nu = WireInit(VecInit(Seq.fill(XLEN/len)(0.U((len).W))))

    when(io.en){
        for(i <- 0 until (XLEN/len)){
            when(io.hasTs){
                nu(i) := io.in(i)(7, 0)
                
                tmp_nu(i) := nu(i) + ((io.vr + io.weight(i)) >> io.tau) - (nu(i) >> io.tau)
                nu_res(i) := tmp_nu(i) | (io.in(i)(15, 8) << (len/2).U)
                
                io.res(i) := nu_res(i)
                Debug("[SNN_LNU]ts(%d) = %x, nu(%d) = %x weight = %x res(%d) = %x out(%d) = %x\n",i.U, io.in(i)(15, 8), i.U, nu(i), io.weight(i), i.U, nu_res(i), i.U, io.res(i))
            }.otherwise{
                nu(i) := io.in(i)
                // tmp_nu(i) := (io.vr - nu(i) + io.weight(i)) * io.tau
                nu_res(i) := nu(i) + ((io.vr + io.weight(i)) >> io.tau) - (nu(i) >> io.tau)
                io.res(i) := nu_res(i)
                Debug("[SNN_LNU]nu(%d) = %x weight = %x res(%d) = %x out(%d) = %x\n",i.U, nu(i), io.weight(i), i.U, nu_res(i), i.U, io.res(i))
            }
        }
    }.otherwise{
        for(i <- 0 until (XLEN/len)){
            io.res(i) := 0.U
        }
    }   
}

class LNU extends NutCoreModule{
    val io = IO(new LNU_IO)

    val nu0 = Module(new Neuron) 

    nu0.io.en := io.in.bits.SCtrl.isNup
    nu0.io.in := io.in.bits.SCtrl.DIn1
    nu0.io.weight := io.in.bits.SCtrl.DIn2
    nu0.io.hasTs := io.in.bits.SCtrl.hasTs
    nu0.io.vr := io.in.bits.SCtrl.SRF4(SRFAddr.VR)
    nu0.io.tau := io.in.bits.SCtrl.SRF4(SRFAddr.TAU)

    io.out.bits.res := Mux(io.in.bits.SCtrl.isNup, nu0.io.res.reverse.reduce(Cat(_,_)), 0.U)

    io.in.ready := !io.in.valid
    io.out.valid := io.in.valid 

    io.out.bits.dcOut := io.in.bits.dcIn
    Debug(io.in.bits.SCtrl.isNup, "[SNN_LNU] hasTs: %b, n0: %x n1: %x n2: %x n3: %x\n", io.in.bits.SCtrl.hasTs, io.in.bits.SCtrl.DIn1(0), io.in.bits.SCtrl.DIn1(1), io.in.bits.SCtrl.DIn1(2), io.in.bits.SCtrl.DIn1(3))
}
