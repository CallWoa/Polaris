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
import bus.simplebus._
import bus.axi4._
import chisel3.experimental.IO
import utils._
import top.Settings

trait HasTLBIO extends HasNutCoreParameter with HasTlbConst with HasCSRConst {
  class TLBIO extends Bundle {
    val in = Flipped(new SimpleBusUC(userBits = userBits, addrBits = VAddrBits))
    val out = new SimpleBusUC(userBits = userBits)

    val mem = new SimpleBusUC()
    val flush = Input(Bool())
    val csrMMU = new MMUIO
    val cacheEmpty = Input(Bool())
    val ipf = Output(Bool())
  }
  val io = IO(new TLBIO)
}

class EmbeddedTLBMD(implicit val tlbConfig: TLBConfig) extends TlbModule {
  val io = IO(new Bundle {
    val tlbmd = Output(Vec(Ways, UInt(tlbLen.W)))
    val write = Flipped(new TLBMDWriteBundle(IndexBits = IndexBits, Ways = Ways, tlbLen = tlbLen))
    val rindex = Input(UInt(IndexBits.W))
    val ready = Output(Bool())
  })

  //val tlbmd = Reg(Vec(Ways, UInt(tlbLen.W)))
  val tlbmd = Mem(Sets, Vec(Ways, UInt(tlbLen.W)))
  io.tlbmd := tlbmd(io.rindex)

  //val reset = WireInit(false.B)
  val resetState = RegInit(true.B)//RegEnable(true.B, init = true.B, reset)
  val (resetSet, resetFinish) = Counter(resetState, Sets)
  when (resetFinish) { resetState := false.B }

  val writeWen = io.write.wen//WireInit(false.B)
  val writeSetIdx = io.write.windex
  val writeWayMask = io.write.waymask
  val writeData = io.write.wdata

  val wen = Mux(resetState, true.B, writeWen)
  val setIdx = Mux(resetState, resetSet, writeSetIdx)
  val waymask = Mux(resetState, Fill(Ways, "b1".U), writeWayMask)
  val dataword = Mux(resetState, 0.U, writeData)
  val wdata = VecInit(Seq.fill(Ways)(dataword))

  when (wen) { tlbmd.write(setIdx, wdata, waymask.asBools) }

  io.ready := !resetState
  def rready() = !resetState
  def wready() = !resetState
}

class EmbeddedTLB(implicit val tlbConfig: TLBConfig) extends TlbModule with HasTLBIO {

  val satp = WireInit(0.U(XLEN.W))
  BoringUtils.addSink(satp, "CSRSATP")

  // tlb exec
  val tlbExec = Module(new EmbeddedTLBExec)
  val tlbEmpty = Module(new EmbeddedTLBEmpty)
  val mdTLB = Module(new EmbeddedTLBMD)
  val mdUpdate = Wire(Bool())
  
  tlbExec.io.flush := io.flush
  tlbExec.io.satp := satp
  tlbExec.io.mem <> io.mem
  tlbExec.io.pf <> io.csrMMU
  tlbExec.io.md <> RegEnable(mdTLB.io.tlbmd, mdUpdate)
  tlbExec.io.mdReady := mdTLB.io.ready
  mdTLB.io.rindex := getIndex(io.in.req.bits.addr)
  mdTLB.io.write <> tlbExec.io.mdWrite
  
  io.ipf := false.B
  
  // meta reset
  val flushTLB = WireInit(false.B)
  BoringUtils.addSink(flushTLB, "MOUFlushTLB")
  mdTLB.reset := reset.asBool || flushTLB

  // VM enable && io
  val vmEnable = satp.asTypeOf(satpBundle).mode === 8.U && (io.csrMMU.priviledgeMode < ModeM)

  def PipelineConnectTLB[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T], update: Bool, rightOutFire: Bool, isFlush: Bool, vmEnable: Bool) = {
    val valid = RegInit(false.B)
    when (rightOutFire) { valid := false.B }
    when (left.valid && right.ready && vmEnable) { valid := true.B }
    when (isFlush) { valid := false.B }

    left.ready := right.ready
    right.bits <> RegEnable(left.bits, left.valid && right.ready)
    right.valid := valid //&& !isFlush

    update := left.valid && right.ready
  }

  tlbEmpty.io.in <> DontCare
  tlbEmpty.io.out.ready := DontCare
  PipelineConnectTLB(io.in.req, tlbExec.io.in, mdUpdate, tlbExec.io.isFinish, io.flush, vmEnable)
  if(tlbname == "dtlb") {
    PipelineConnect(tlbExec.io.out, tlbEmpty.io.in, tlbEmpty.io.out.fire(), io.flush)
  }
  when(!vmEnable) {
    tlbExec.io.out.ready := true.B // let existed request go out
    if( tlbname == "dtlb") { tlbEmpty.io.out.ready := true.B }
    io.out.req.valid := io.in.req.valid
    io.in.req.ready := io.out.req.ready
    io.out.req.bits.addr := io.in.req.bits.addr(PAddrBits-1, 0)
    io.out.req.bits.size := io.in.req.bits.size
    io.out.req.bits.cmd := io.in.req.bits.cmd
    io.out.req.bits.wmask := io.in.req.bits.wmask
    io.out.req.bits.wdata := io.in.req.bits.wdata
    io.out.req.bits.user.map(_ := io.in.req.bits.user.getOrElse(0.U))
  }.otherwise {
    if (tlbname == "dtlb") { io.out.req <> tlbEmpty.io.out}
    else { io.out.req <> tlbExec.io.out }
  }
  io.out.resp <> io.in.resp

  // lsu need dtlb signals
  if(tlbname == "dtlb") {
    val alreadyOutFinish = RegEnable(true.B, init=false.B, tlbExec.io.out.valid && !tlbExec.io.out.ready)
    when(alreadyOutFinish && tlbExec.io.out.fire()) { alreadyOutFinish := false.B}
    val tlbFinish = (tlbExec.io.out.valid && !alreadyOutFinish) || tlbExec.io.pf.isPF()
    BoringUtils.addSource(tlbFinish, "DTLBFINISH")
    BoringUtils.addSource(io.csrMMU.isPF(), "DTLBPF")
    BoringUtils.addSource(vmEnable, "DTLBENABLE")
  }

  // instruction page fault
  if (tlbname == "itlb") {
    when (tlbExec.io.ipf && vmEnable) {
      tlbExec.io.out.ready := io.cacheEmpty && io.in.resp.ready
      io.out.req.valid := false.B
    }

    when (tlbExec.io.ipf && vmEnable && io.cacheEmpty) {
      io.in.resp.valid := true.B
      io.in.resp.bits.rdata := 0.U
      io.in.resp.bits.cmd := SimpleBusCmd.readLast
      io.in.resp.bits.user.map(_ := tlbExec.io.in.bits.user.getOrElse(0.U))
      io.ipf := tlbExec.io.ipf
    }
  }

  Debug("InReq(%d, %d) InResp(%d, %d) OutReq(%d, %d) OutResp(%d, %d) vmEnable:%d mode:%d\n", io.in.req.valid, io.in.req.ready, io.in.resp.valid, io.in.resp.ready, io.out.req.valid, io.out.req.ready, io.out.resp.valid, io.out.resp.ready, vmEnable, io.csrMMU.priviledgeMode)
  Debug("InReq: addr:%x cmd:%d wdata:%x OutReq: addr:%x cmd:%x wdata:%x\n", io.in.req.bits.addr, io.in.req.bits.cmd, io.in.req.bits.wdata, io.out.req.bits.addr, io.out.req.bits.cmd, io.out.req.bits.wdata)
  Debug("OutResp: rdata:%x cmd:%x Inresp: rdata:%x cmd:%x\n", io.out.resp.bits.rdata, io.out.resp.bits.cmd, io.in.resp.bits.rdata, io.in.resp.bits.cmd)
  Debug("satp:%x flush:%d cacheEmpty:%d instrPF:%d loadPF:%d storePF:%d \n", satp, io.flush, io.cacheEmpty, io.ipf, io.csrMMU.loadPF, io.csrMMU.storePF)
}

class EmbeddedTLBExec(implicit val tlbConfig: TLBConfig) extends TlbModule{
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, addrBits = VAddrBits)))
    val out = Decoupled(new SimpleBusReqBundle(userBits = userBits))

    val md = Input(Vec(Ways, UInt(tlbLen.W)))
    val mdWrite = new TLBMDWriteBundle(IndexBits = IndexBits, Ways = Ways, tlbLen = tlbLen)
    val mdReady = Input(Bool())

    val mem = new SimpleBusUC()
    val flush = Input(Bool()) 
    val satp = Input(UInt(XLEN.W))
    val pf = new MMUIO
    val ipf = Output(Bool())
    val isFinish = Output(Bool())
  })

  val md = io.md//RegEnable(mdTLB.io.tlbmd, io.in.ready)
  
  // lazy renaming
  val req = io.in.bits
  val vpn = req.addr.asTypeOf(vaBundle2).vpn.asTypeOf(vpnBundle)
  val pf = io.pf
  val satp = io.satp.asTypeOf(satpBundle)
  val ifecth = if(tlbname == "itlb") true.B else false.B

  // pf init
  pf.loadPF := false.B
  pf.storePF := false.B
  pf.addr := req.addr

  // check hit or miss
  val hitVec = VecInit(md.map(m => m.asTypeOf(tlbBundle).flag.asTypeOf(flagBundle).v && (m.asTypeOf(tlbBundle).asid === satp.asid) && MaskEQ(m.asTypeOf(tlbBundle).mask, m.asTypeOf(tlbBundle).vpn, vpn.asUInt))).asUInt
  val hit = io.in.valid && hitVec.orR
  val miss = io.in.valid && !hitVec.orR

  val victimWaymask = if (Ways > 1) (1.U << LFSR64()(log2Up(Ways)-1,0)) else "b1".U
  val waymask = Mux(hit, hitVec, victimWaymask)

  val loadPF = WireInit(false.B)
  val storePF = WireInit(false.B)

  // hit
  val hitMeta = Mux1H(waymask, md).asTypeOf(tlbBundle2).meta.asTypeOf(metaBundle)
  val hitData = Mux1H(waymask, md).asTypeOf(tlbBundle2).data.asTypeOf(dataBundle)
  val hitFlag = hitMeta.flag.asTypeOf(flagBundle)
  val hitMask = hitMeta.mask
  // hit write back pte.flag
  val hitinstrPF = WireInit(false.B)
  val hitWB = hit && (!hitFlag.a || !hitFlag.d && req.isWrite()) && !hitinstrPF && !(loadPF || storePF || io.pf.isPF())
  val hitRefillFlag = Cat(req.isWrite().asUInt, 1.U(1.W), 0.U(6.W)) | hitFlag.asUInt
  val hitWBStore = RegEnable(Cat(0.U(10.W), hitData.ppn, 0.U(2.W), hitRefillFlag), hitWB)

  // hit permission check
  val hitCheck = hit /*&& hitFlag.v */&& !(pf.priviledgeMode === ModeU && !hitFlag.u) && !(pf.priviledgeMode === ModeS && hitFlag.u && (!pf.status_sum || ifecth))
  val hitExec = hitCheck && hitFlag.x
  val hitLoad = hitCheck && (hitFlag.r || pf.status_mxr && hitFlag.x)
  val hitStore = hitCheck && hitFlag.w
  
  val isAMO = WireInit(false.B)
  if (tlbname == "dtlb") {
    BoringUtils.addSink(isAMO, "ISAMO")
  }

  io.pf.loadPF := RegNext(loadPF, init =false.B)
  io.pf.storePF := RegNext(storePF, init = false.B)

  if (tlbname == "itlb") { hitinstrPF := !hitExec  && hit}
  if (tlbname == "dtlb") { 
    loadPF := !hitLoad && req.isRead() && hit && !isAMO
    storePF := (!hitStore && req.isWrite() && hit) || (!hitLoad && req.isRead() && hit && isAMO)
  }

  // miss
  val s_idle :: s_memReadReq :: s_memReadResp :: s_write_pte :: s_wait_resp :: s_miss_slpf :: Nil = Enum(6)
  val state = RegInit(s_idle)
  val level = RegInit(Level.U(log2Up(Level).W))
  
  val memRespStore = Reg(UInt(XLEN.W))
  val missMask = WireInit("h3ffff".U(maskLen.W))
  val missMaskStore = Reg(UInt(maskLen.W))
  val missMetaRefill = WireInit(false.B)
  val missRefillFlag = WireInit(0.U(8.W))
  val memRdata = io.mem.resp.bits.rdata.asTypeOf(pteBundle)
  val raddr = Reg(UInt(PAddrBits.W))
  val alreadyOutFire = RegEnable(true.B, init = false.B, io.out.fire)

  //handle flush
  val needFlush = RegInit(false.B)
  val ioFlush = io.flush
  val isFlush = needFlush || ioFlush
  when (ioFlush && (state =/= s_idle)) { needFlush := true.B}
  when (io.out.fire() && needFlush) { needFlush := false.B}

  val missIPF = RegInit(false.B)

  // state machine to handle miss(ptw) and pte-writing-back
  switch (state) {
    is (s_idle) {
      when (!ioFlush && hitWB) {
        state := s_write_pte
        needFlush := false.B
        alreadyOutFire := false.B
      }.elsewhen (miss && !ioFlush) {
        state := s_memReadReq
        raddr := paddrApply(satp.ppn, vpn.vpn2) //
        level := Level.U
        needFlush := false.B
        alreadyOutFire := false.B
      }
    }

    is (s_memReadReq) { 
      when (isFlush) {
        state := s_idle
        needFlush := false.B
      }.elsewhen (io.mem.req.fire()) { state := s_memReadResp}
    }

    is (s_memReadResp) { 
      val missflag = memRdata.flag.asTypeOf(flagBundle)
      when (io.mem.resp.fire()) {
        when (isFlush) {
          state := s_idle
          needFlush := false.B
        }.elsewhen (!(missflag.r || missflag.x) && (level===3.U || level===2.U)) {
          when(!missflag.v || (!missflag.r && missflag.w)) { //TODO: fix needflush
            if(tlbname == "itlb") { state := s_wait_resp } else { state := s_miss_slpf }
            if(tlbname == "itlb") { missIPF := true.B }
            if(tlbname == "dtlb") { 
              loadPF := req.isRead() && !isAMO 
              storePF := req.isWrite() || isAMO 
            }  
            Debug("tlbException!!! ")
            Debug(false, p" req:${req}  Memreq:${io.mem.req}  MemResp:${io.mem.resp}")
            Debug(false, " level:%d",level)
            Debug(false, "\n")
          }.otherwise {
            state := s_memReadReq
            raddr := paddrApply(memRdata.ppn, Mux(level === 3.U, vpn.vpn1, vpn.vpn0))
          }
        }.elsewhen (level =/= 0.U) { //TODO: fix needFlush
          val permCheck = missflag.v && !(pf.priviledgeMode === ModeU && !missflag.u) && !(pf.priviledgeMode === ModeS && missflag.u && (!pf.status_sum || ifecth))
          val permExec = permCheck && missflag.x
          val permLoad = permCheck && (missflag.r || pf.status_mxr && missflag.x)
          val permStore = permCheck && missflag.w
          val updateAD = if (Settings.get("FPGAPlatform")) !missflag.a || (!missflag.d && req.isWrite()) else false.B
          val updateData = Cat( 0.U(56.W), req.isWrite(), 1.U(1.W), 0.U(6.W) )
          missRefillFlag := Cat(req.isWrite(), 1.U(1.W), 0.U(6.W)) | missflag.asUInt
          memRespStore := io.mem.resp.bits.rdata | updateData 
          if(tlbname == "itlb") {
            when (!permExec) { missIPF := true.B ; state := s_wait_resp}
            .otherwise { 
              state := Mux(updateAD, s_write_pte, s_wait_resp)
              missMetaRefill := true.B
            }
          }
          if(tlbname == "dtlb") {
            when((!permLoad && req.isRead()) || (!permStore && req.isWrite())) { 
              state := s_miss_slpf
              loadPF := req.isRead() && !isAMO
              storePF := req.isWrite() || isAMO
            }.otherwise {
              state := Mux(updateAD, s_write_pte, s_wait_resp)
              missMetaRefill := true.B
            }
          }
          missMask := Mux(level===3.U, 0.U(maskLen.W), Mux(level===2.U, "h3fe00".U(maskLen.W), "h3ffff".U(maskLen.W)))
          missMaskStore := missMask
        }
        level := level - 1.U
      }
    }

    is (s_write_pte) {
      when (isFlush) {
        state := s_idle
        needFlush := false.B
      }.elsewhen (io.mem.req.fire()) { state := s_wait_resp }
    }

    is (s_wait_resp) { when (io.out.fire() || ioFlush || alreadyOutFire){
      state := s_idle
      missIPF := false.B
      alreadyOutFire := false.B
    }}

    is (s_miss_slpf) {
      state := s_idle
    }
  }

  // mem
  val cmd = Mux(state === s_write_pte, SimpleBusCmd.write, SimpleBusCmd.read)
  io.mem.req.bits.apply(addr = Mux(hitWB, hitData.pteaddr, raddr), cmd = cmd, size = (if (XLEN == 64) "b11".U else "b10".U), wdata =  Mux( hitWB, hitWBStore, memRespStore), wmask = 0xff.U)
  io.mem.req.valid := ((state === s_memReadReq || state === s_write_pte) && !isFlush)
  io.mem.resp.ready := true.B

  // tlb refill
  io.mdWrite.apply(wen = RegNext((missMetaRefill && !isFlush) || (hitWB && state === s_idle && !isFlush), init = false.B), 
    windex = RegNext(getIndex(req.addr)), waymask = RegNext(waymask), vpn = RegNext(vpn.asUInt), 
    asid = RegNext(Mux(hitWB, hitMeta.asid, satp.asid)), mask = RegNext(Mux(hitWB, hitMask, missMask)), 
    flag = RegNext(Mux(hitWB, hitRefillFlag, missRefillFlag)), ppn = RegNext(Mux(hitWB, hitData.ppn, memRdata.ppn)), 
    pteaddr = RegNext((Mux(hitWB, hitData.pteaddr, raddr))))

  // io
  io.out.bits := req
  io.out.bits.addr := Mux(hit, maskPaddr(hitData.ppn, req.addr(PAddrBits-1, 0), hitMask), maskPaddr(memRespStore.asTypeOf(pteBundle).ppn, req.addr(PAddrBits-1, 0), missMaskStore))
  io.out.valid := io.in.valid && Mux(hit && !hitWB, !(io.pf.isPF() || loadPF || storePF), state === s_wait_resp)// && !alreadyOutFire
  
  io.in.ready := io.out.ready && (state === s_idle) && !miss && !hitWB && io.mdReady && (!io.pf.isPF() && !loadPF && !storePF)//maybe be optimized

  io.ipf := Mux(hit, hitinstrPF, missIPF)
  io.isFinish := io.out.fire() || io.pf.isPF()

  Debug("In(%d, %d) Out(%d, %d) InAddr:%x OutAddr:%x cmd:%d \n", io.in.valid, io.in.ready, io.out.valid, io.out.ready, req.addr, io.out.bits.addr, req.cmd)
  Debug("isAMO:%d io.Flush:%d needFlush:%d alreadyOutFire:%d isFinish:%d\n",isAMO, io.flush, needFlush, alreadyOutFire, io.isFinish)
  Debug("hit:%d hitWB:%d hitVPN:%x hitFlag:%x hitPPN:%x hitRefillFlag:%x hitWBStore:%x hitCheck:%d hitExec:%d hitLoad:%d hitStore:%d\n", hit, hitWB, hitMeta.vpn, hitFlag.asUInt, hitData.ppn, hitRefillFlag, hitWBStore, hitCheck, hitExec, hitLoad, hitStore)
  Debug("miss:%d state:%d level:%d raddr:%x memRdata:%x missMask:%x missRefillFlag:%x missMetaRefill:%d\n", miss, state, level, raddr, memRdata.asUInt, missMask, missRefillFlag, missMetaRefill)
  Debug("meta/data: (0)%x|%b|%x (1)%x|%b|%x (2)%x|%b|%x (3)%x|%b|%x rread:%d\n", md(0).asTypeOf(tlbBundle).vpn, md(0).asTypeOf(tlbBundle).flag, md(0).asTypeOf(tlbBundle).ppn, md(1).asTypeOf(tlbBundle).vpn, md(1).asTypeOf(tlbBundle).flag, md(1).asTypeOf(tlbBundle).ppn, md(2).asTypeOf(tlbBundle).vpn, md(2).asTypeOf(tlbBundle).flag, md(2).asTypeOf(tlbBundle).ppn, md(3).asTypeOf(tlbBundle).vpn, md(3).asTypeOf(tlbBundle).flag, md(3).asTypeOf(tlbBundle).ppn, io.mdReady)
  Debug("md: wen:%d windex:%x waymask:%x vpn:%x asid:%x mask:%x flag:%x asid:%x ppn:%x pteaddr:%x\n", io.mdWrite.wen, io.mdWrite.windex, io.mdWrite.waymask, io.mdWrite.wdata.asTypeOf(tlbBundle).vpn, io.mdWrite.wdata.asTypeOf(tlbBundle).asid, io.mdWrite.wdata.asTypeOf(tlbBundle).mask, io.mdWrite.wdata.asTypeOf(tlbBundle).flag, io.mdWrite.wdata.asTypeOf(tlbBundle).asid, io.mdWrite.wdata.asTypeOf(tlbBundle).ppn, io.mdWrite.wdata.asTypeOf(tlbBundle).pteaddr)
  Debug("MemReq(%d, %d) MemResp(%d, %d) addr:%x cmd:%d rdata:%x cmd:%d\n", io.mem.req.valid, io.mem.req.ready, io.mem.resp.valid, io.mem.resp.ready, io.mem.req.bits.addr, io.mem.req.bits.cmd, io.mem.resp.bits.rdata, io.mem.resp.bits.cmd)
  Debug("io.ipf:%d hitinstrPF:%d missIPF:%d pf.loadPF:%d pf.storePF:%d loadPF:%d storePF:%d\n", io.ipf, hitinstrPF, missIPF, io.pf.loadPF, io.pf.storePF, loadPF, storePF)
}

class EmbeddedTLBEmpty(implicit val tlbConfig: TLBConfig) extends TlbModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits)))
    val out = Decoupled(new SimpleBusReqBundle(userBits = userBits))
  })

  io.out <> io.in
  io.in.ready := !io.in.valid || io.out.fire()
}

class EmbeddedTLB_fake(implicit val tlbConfig: TLBConfig) extends TlbModule with HasTLBIO {
  io.mem <> DontCare
  io.out <> io.in
  io.csrMMU.loadPF := false.B
  io.csrMMU.storePF := false.B
  io.csrMMU.addr := io.in.req.bits.addr
  io.ipf := false.B
  val ismmio = io.out.req.fire() && AddressSpace.isMMIO(io.in.req.bits.addr)	
  BoringUtils.addSource(ismmio,"lsuMMIO")
}


object EmbeddedTLB {
  def apply(in: SimpleBusUC, mem: SimpleBusUC, flush: Bool, csrMMU: MMUIO, enable: Boolean = true)(implicit tlbConfig: TLBConfig) = {
    val tlb = if (enable) {
      Module(new SIMD_TLB)
    } else {
      Module(new EmbeddedTLB_fake)
    }
    tlb.io.in <> in
    tlb.io.mem <> mem
    tlb.io.flush := flush
    tlb.io.csrMMU <> csrMMU
    tlb
  }
}

class SIMD_TLB(implicit val tlbConfig: TLBConfig) extends TlbModule with HasTLBIO {

  val satp = WireInit(0.U(XLEN.W))
  BoringUtils.addSink(satp, "CSRSATP")

  // tlb exec
  val tlbExec = Module(new SIMD_TLBEXEC)
  val tlbEmpty = Module(new EmbeddedTLBEmpty)
  val mdTLB = Module(new EmbeddedTLBMD)
  val mdUpdate = Wire(Bool())
  
  tlbExec.io.flush := io.flush
  tlbExec.io.satp := satp
  tlbExec.io.mem <> io.mem
  tlbExec.io.pf.priviledgeMode := io.csrMMU.priviledgeMode
  tlbExec.io.pf.status_sum := io.csrMMU.status_sum
  tlbExec.io.pf.status_mxr := io.csrMMU.status_mxr
  io.csrMMU.loadPF := RegNext(tlbExec.io.pf.loadPF)
  io.csrMMU.storePF := RegNext(tlbExec.io.pf.storePF)
  io.csrMMU.addr := RegNext(tlbExec.io.pf.addr)
  tlbExec.io.md <> RegEnable(mdTLB.io.tlbmd, mdUpdate)
  tlbExec.io.mdReady := mdTLB.io.ready
  mdTLB.io.rindex := getIndex(io.in.req.bits.addr)
  mdTLB.io.write <> tlbExec.io.mdWrite
  
  io.ipf := false.B
  
  // meta reset
  val flushTLB = WireInit(false.B)
  BoringUtils.addSink(flushTLB, "MOUFlushTLB")
  mdTLB.reset := reset.asBool || flushTLB

  // VM enable && io
  val vmEnable = satp.asTypeOf(satpBundle).mode === 8.U && (io.csrMMU.priviledgeMode < ModeM)

  def PipelineConnectTLB[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T], update: Bool, rightOutFire: Bool, isFlush: Bool, vmEnable: Bool) = {
    val valid = RegInit(false.B)
    when (rightOutFire) { valid := false.B }
    when (left.valid && right.ready && vmEnable) { valid := true.B }
    when (isFlush) { valid := false.B }

    left.ready := right.ready
    right.bits <> RegEnable(left.bits, left.valid && right.ready)
    right.valid := valid //&& !isFlush

    update := left.valid && right.ready
  }

  tlbEmpty.io.in <> DontCare
  tlbEmpty.io.out.ready := DontCare
  //if(tlbname == "dtlb") {
    //PipelineConnect(tlbExec.io.out, tlbEmpty.io.in, tlbEmpty.io.out.fire(), io.flush)
  //}
  val tlbexec_inbundle = Wire(Decoupled(new SIMD_TLBEXEC_INBUNDLE))
  tlbexec_inbundle.bits := 0.U.asTypeOf(new SIMD_TLBEXEC_INBUNDLE)
  val req_cancel = WireInit(false.B)
  PipelineConnectTLB(tlbexec_inbundle, tlbExec.io.in, mdUpdate, tlbExec.io.isFinish, io.flush || req_cancel, vmEnable)

  val out_req = Wire(Decoupled(new SimpleBusReqBundle(userBits = userBits, addrBits = VAddrBits)))
  out_req.bits := tlbExec.io.out.bits
  out_req.valid := false.B
  out_req.ready := false.B

  if (tlbname == "dtlb") { 
      PipelineConnect(out_req, tlbEmpty.io.in, tlbEmpty.io.out.fire(), io.flush)
      io.out.req <> tlbEmpty.io.out
  }

  val s_idle :: s_exec :: Nil = Enum(2)
  val state = RegInit(s_idle)
  val ismmio = WireInit(false.B)
  when(!vmEnable) {
    tlbexec_inbundle.valid := false.B
    tlbExec.io.out.ready := true.B // let existed request go out
    //tlbEmpty.io.in.valid := false.B
    if( tlbname == "dtlb") { tlbEmpty.io.out.ready := true.B }
    io.out.req.valid := io.in.req.valid
    io.in.req.ready := io.out.req.ready
    io.out.req.bits.addr := io.in.req.bits.addr(PAddrBits-1, 0)
    io.out.req.bits.size := io.in.req.bits.size
    io.out.req.bits.cmd := io.in.req.bits.cmd
    io.out.req.bits.wmask := io.in.req.bits.wmask
    io.out.req.bits.wdata := io.in.req.bits.wdata
    io.out.req.bits.user.map(_ := io.in.req.bits.user.getOrElse(0.U))
    io.out.req.bits.vector := io.in.req.bits.vector
    when(io.flush){state := s_idle}
    out_req.valid := false.B
    out_req.ready := false.B
    ismmio := io.out.req.fire() && AddressSpace.isMMIO(io.out.req.bits.addr)
  }.otherwise {
    //io.out.req <> tlbExec.io.out
    val req = io.in.req.bits
    val vpn = req.addr.asTypeOf(vaBundle2).vpn.asTypeOf(vpnBundle)
    val ifecth = if(tlbname == "itlb") true.B else false.B

    val md = mdTLB.io.tlbmd
    val hitVec = VecInit(md.map(m => m.asTypeOf(tlbBundle).flag.asTypeOf(flagBundle).v && (m.asTypeOf(tlbBundle).asid === satp.asTypeOf(satpBundle).asid) && MaskEQ(m.asTypeOf(tlbBundle).mask, m.asTypeOf(tlbBundle).vpn, vpn.asUInt))).asUInt
    val hit = io.in.req.valid && hitVec.orR
    val miss = io.in.req.valid && !hitVec.orR

    val victimWaymask = if (Ways > 1) (1.U << LFSR64()(log2Up(Ways)-1,0)) else "b1".U
    val waymask = Mux(hit, hitVec, victimWaymask)

    //io.csrMMU.loadPF := false.B
    //io.csrMMU.storePF := false.B
    //io.csrMMU.addr := req.addr

    // hit
    val hitMeta = Mux1H(waymask, md).asTypeOf(tlbBundle2).meta.asTypeOf(metaBundle)
    val hitData = Mux1H(waymask, md).asTypeOf(tlbBundle2).data.asTypeOf(dataBundle)
    val hitFlag = hitMeta.flag.asTypeOf(flagBundle)
    val hitMask = hitMeta.mask
    val loadPF = WireInit(false.B)
    val storePF = WireInit(false.B)

    // hit write back pte.flag
    val hitinstrPF = WireInit(false.B)
    val hitWB = hit && (!hitFlag.a || !hitFlag.d && req.isWrite()) && !hitinstrPF && !(loadPF || storePF || io.csrMMU.isPF())
    val hitRefillFlag = Cat(req.isWrite().asUInt, 1.U(1.W), 0.U(6.W)) | hitFlag.asUInt
    val hitWBStore = RegEnable(Cat(0.U(10.W), hitData.ppn, 0.U(2.W), hitRefillFlag), hitWB)

    // hit permission check
    val hitCheck = hit /*&& hitFlag.v */&& !(io.csrMMU.priviledgeMode === ModeU && !hitFlag.u) && !(io.csrMMU.priviledgeMode === ModeS && hitFlag.u && (!io.csrMMU.status_sum || ifecth))
    val hitADCheck = if (Settings.get("FPGAPlatform")) false.B else !hitFlag.a || !hitFlag.d && req.isWrite()
    val hitExec = hitCheck && hitFlag.x && !hitADCheck
    val hitLoad = hitCheck && (hitFlag.r || io.csrMMU.status_mxr && hitFlag.x) && !hitADCheck
    val hitStore = hitCheck && hitFlag.w && !hitADCheck

    val isAMO = WireInit(false.B)
    if (tlbname == "dtlb") {
      BoringUtils.addSink(isAMO, "ISAMO")
    }

    //io.csrMMU.loadPF := RegNext(loadPF, init =false.B)
    //io.csrMMU.storePF := RegNext(storePF, init = false.B)

    if (tlbname == "itlb") { hitinstrPF := !hitExec  && hit}
    if (tlbname == "dtlb") { 
      loadPF := !hitLoad && req.isRead() && hit && !isAMO
      storePF := (!hitStore && req.isWrite() && hit) || (!hitLoad && req.isRead() && hit && isAMO)
    }

    out_req.bits := io.in.req.bits
    out_req.valid := Mux(state === s_idle,hit && !hitWB && !(io.csrMMU.isPF() || loadPF || storePF),tlbExec.io.out.valid)
    out_req.bits.addr := Mux(state === s_idle,maskPaddr(hitData.ppn, req.addr(PAddrBits-1, 0), hitMask), tlbExec.io.out.bits.addr)
    tlbExec.io.out.ready := out_req.fire()
    io.in.req.ready := out_req.fire()

    if (tlbname == "itlb") {
       io.out.req <> out_req
    }

    switch (state){
      is(s_idle){
        when((miss || hitWB || loadPF || storePF || hitinstrPF) && !io.flush && tlbexec_inbundle.fire()){
          state := s_exec
        }
      }
      is(s_exec){
        when(out_req.fire() || io.flush){
          //tlbExec.io.out.ready := true.B
          state := s_idle
        }
        when(!io.in.req.valid){
          state := s_idle
          tlbExec.io.flush := true.B
          req_cancel := true.B
        }
      }
    }
    val cntstate = RegInit(0.U(64.W))
    val a = RegInit(false.B)
    when(state.asBool() && !vmEnable){a:=true.B}
    when(a){cntstate := cntstate + 1.U}

    tlbexec_inbundle.bits.req := io.in.req.bits
    tlbexec_inbundle.valid := state === s_idle && io.in.req.valid && (miss || hitWB || loadPF || storePF || hitinstrPF)
    tlbexec_inbundle.bits.hitVec := hitVec
    tlbexec_inbundle.bits.miss := miss
    tlbexec_inbundle.bits.hitWB := hitWB
    tlbexec_inbundle.bits.loadPF := loadPF
    tlbexec_inbundle.bits.storePF := storePF
    tlbexec_inbundle.bits.hitinstrPF := hitinstrPF
    Debug("state:%x hit %x miss %x currentlp %x currentsp %x currentipf %x\n",state,hit,miss,loadPF,storePF,hitinstrPF)
    Debug("hit:%d hitWB:%d hitVPN:%x hitFlag:%x hitPPN:%x hitRefillFlag:%x hitWBStore:%x hitCheck:%d hitExec:%d hitLoad:%d hitStore:%d\n", hit, hitWB, hitMeta.vpn, hitFlag.asUInt, hitData.ppn, hitRefillFlag, hitWBStore, hitCheck, hitExec, hitLoad, hitStore)
    ismmio := out_req.fire() && AddressSpace.isMMIO(out_req.bits.addr)
  }
  io.in.resp <> io.out.resp
  //io.in.resp.bits.vector := 0.U.asTypeOf(io.in.resp.bits.vector)
  //Debug("state:%x \n",state)

  // lsu need dtlb signals
  if(tlbname == "dtlb") {
    val alreadyOutFinish = RegEnable(true.B, init=false.B, tlbExec.io.out.valid && !tlbExec.io.out.ready)
    when(alreadyOutFinish && tlbExec.io.out.fire()) { alreadyOutFinish := false.B}
    val tlbFinish = (out_req.fire()) || tlbExec.io.pf.isPF()
    if(HasDTLB){
    BoringUtils.addSource(tlbFinish, "DTLBFINISH")
    BoringUtils.addSource(io.csrMMU.isPF(), "DTLBPF")
    BoringUtils.addSource(vmEnable, "DTLBENABLE")
    BoringUtils.addSource(ismmio,"lsuMMIO")
    }
    Debug("alreadyOutFinish %x \n",alreadyOutFinish)
  }

  // instruction page fault
  if (tlbname == "itlb") {
    when (tlbExec.io.ipf && vmEnable) {
      tlbExec.io.out.ready := io.cacheEmpty && io.in.resp.ready
      out_req.valid := false.B
    }

    when (tlbExec.io.ipf && vmEnable && io.cacheEmpty) {
      io.in.resp.valid := true.B
      io.in.resp.bits.rdata := 0.U
      io.in.resp.bits.cmd := SimpleBusCmd.readLast
      io.in.resp.bits.user.map(_ := tlbExec.io.in.bits.req.user.getOrElse(0.U))
      io.ipf := tlbExec.io.ipf
    }
  }

  Debug("InReq(%d, %d) InResp(%d, %d) OutReq(%d, %d) OutResp(%d, %d) vmEnable:%d mode:%d \n", io.in.req.valid, io.in.req.ready, io.in.resp.valid, io.in.resp.ready, io.out.req.valid, io.out.req.ready, io.out.resp.valid, io.out.resp.ready, vmEnable, io.csrMMU.priviledgeMode)
  Debug("OutReq-vector:vstep %x vwdata %x velen %x vxlen %x vecEnable %x\n",io.out.req.bits.vector.vstep,io.out.req.bits.vector.vwdata,io.out.req.bits.vector.velen,io.out.req.bits.vector.vxlen,io.out.req.bits.vector.vecEnable)
  Debug("InReq: addr:%x cmd:%d wdata:%x OutReq: addr:%x cmd:%x wdata:%x\n", io.in.req.bits.addr, io.in.req.bits.cmd, io.in.req.bits.wdata, io.out.req.bits.addr, io.out.req.bits.cmd, io.out.req.bits.wdata)
  Debug("OutResp: rdata:%x cmd:%x Inresp: rdata:%x cmd:%x\n", io.out.resp.bits.rdata, io.out.resp.bits.cmd, io.in.resp.bits.rdata, io.in.resp.bits.cmd)
  Debug("satp:%x flush:%d cacheEmpty:%d instrPF:%d loadPF:%d storePF:%d \n", satp, io.flush, io.cacheEmpty, io.ipf, io.csrMMU.loadPF, io.csrMMU.storePF)
  Debug("tlbempty invalid %x inready %x outvalid %x outready %x tlbexecoutvalid %x\n",tlbEmpty.io.in.valid,tlbEmpty.io.in.ready,tlbEmpty.io.out.valid,tlbEmpty.io.out.ready,tlbExec.io.out.valid)
}

class SIMD_TLBEXEC(implicit val tlbConfig: TLBConfig) extends TlbModule{
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SIMD_TLBEXEC_INBUNDLE))
    val out = Decoupled(new SimpleBusReqBundle(userBits = userBits))

    val md = Input(Vec(Ways, UInt(tlbLen.W)))
    val mdWrite = new TLBMDWriteBundle(IndexBits = IndexBits, Ways = Ways, tlbLen = tlbLen)
    val mdReady = Input(Bool())

    val mem = new SimpleBusUC()
    val flush = Input(Bool()) 
    val satp = Input(UInt(XLEN.W))
    val pf = new MMUIO
    val ipf = Output(Bool())
    val isFinish = Output(Bool())
  })

  val missIPF = RegInit(false.B)
  val missLPF = RegInit(false.B)
  val missSPF = RegInit(false.B)

  val md = io.md//RegEnable(mdTLB.io.tlbmd, io.in.ready)
  
  // lazy renaming
  val req = io.in.bits.req
  val vpn = req.addr.asTypeOf(vaBundle2).vpn.asTypeOf(vpnBundle)
  val pf = io.pf
  val satp = io.satp.asTypeOf(satpBundle)
  val ifecth = if(tlbname == "itlb") true.B else false.B

  // check hit or miss
  val hitVec = io.in.bits.hitVec
  val hit = io.in.valid && hitVec.orR
  val miss = io.in.valid && io.in.bits.miss

  val victimWaymask = if (Ways > 1) (1.U << LFSR64()(log2Up(Ways)-1,0)) else "b1".U
  val waymask = Mux(hit, hitVec, victimWaymask)

  val loadPF = WireInit(io.in.bits.loadPF && io.in.valid)
  val storePF = WireInit(io.in.bits.storePF && io.in.valid)
  val hitinstrPF = io.in.bits.hitinstrPF && io.in.valid
  val hitWB = io.in.bits.hitWB && io.in.valid

  // pf
  io.pf.loadPF := (loadPF || missLPF) && io.in.valid
  io.pf.storePF := (storePF || missSPF) && io.in.valid
  io.pf.addr := req.addr

  // hit
  val hitMeta = Mux1H(waymask, md).asTypeOf(tlbBundle2).meta.asTypeOf(metaBundle)
  val hitData = Mux1H(waymask, md).asTypeOf(tlbBundle2).data.asTypeOf(dataBundle)
  val hitFlag = hitMeta.flag.asTypeOf(flagBundle)
  val hitMask = hitMeta.mask
  // hit write back pte.flag
  val hitRefillFlag = Cat(req.isWrite().asUInt, 1.U(1.W), 0.U(6.W)) | hitFlag.asUInt
  val hitWBStore = RegEnable(Cat(0.U(10.W), hitData.ppn, 0.U(2.W), hitRefillFlag), hitWB)
  
  val isAMO = WireInit(false.B)
  if (tlbname == "dtlb") {
    BoringUtils.addSink(isAMO, "ISAMO")
  }

  // miss
  val s_idle :: s_memReadReq :: s_memReadResp :: s_write_pte :: s_wait_resp :: s_miss_slpf :: s_memwriteResp :: Nil = Enum(7)
  val state = RegInit(s_idle)
  val level = RegInit(Level.U(log2Up(Level).W))
  
  val memRespStore = Reg(UInt(XLEN.W))
  val missMask = WireInit("h3ffff".U(maskLen.W))
  val missMaskStore = Reg(UInt(maskLen.W))
  val missMetaRefill = WireInit(false.B)
  val missRefillFlag = WireInit(0.U(8.W))
  val memRdata = io.mem.resp.bits.rdata.asTypeOf(pteBundle)
  val raddr = Reg(UInt(PAddrBits.W))
  val alreadyOutFire = RegEnable(true.B, init = false.B, io.out.fire)

  //handle flush
  val needFlush = RegInit(false.B)
  val ioFlush = io.flush
  val isFlush = needFlush || ioFlush
  when (ioFlush && (state =/= s_idle)) { needFlush := true.B}
  when (io.out.fire() && needFlush) { needFlush := false.B}

  // state machine to handle miss(ptw) and pte-writing-back
  switch (state) {
    is (s_idle) {
      when (!isFlush && hitWB) {
        state := s_write_pte
        needFlush := false.B
      }.elsewhen (miss && !isFlush) {
        state := s_memReadReq
        raddr := paddrApply(satp.ppn, vpn.vpn2) //
        level := Level.U
        needFlush := false.B
      }
    }

    is (s_memReadReq) { 
      when (ioFlush) {
        state := s_idle
        needFlush := false.B
        missIPF := false.B
        missSPF := false.B
        missLPF := false.B
      }.elsewhen (io.mem.req.fire()) { state := s_memReadResp}
    }

    is (s_memReadResp) { 
      val missflag = memRdata.flag.asTypeOf(flagBundle)
      when (io.mem.resp.fire()) {
        when (isFlush) {
          state := s_idle
          missIPF := false.B
          missSPF := false.B
          missLPF := false.B
          needFlush := false.B
        }.elsewhen (!(missflag.r || missflag.x) && (level===3.U || level===2.U)) {
          when(!missflag.v || (!missflag.r && missflag.w)) { //TODO: fix needflush
            if(tlbname == "itlb") { state := s_wait_resp } else { state := s_miss_slpf }
            if(tlbname == "itlb") { missIPF := true.B }
            if(tlbname == "dtlb") { 
              loadPF := req.isRead() && !isAMO 
              missLPF := loadPF
              storePF := req.isWrite() || isAMO 
              missSPF := storePF
            }  
            Debug("tlbException!!! ")
            Debug(false, p" req:${req}  Memreq:${io.mem.req}  MemResp:${io.mem.resp}")
            Debug(false, " level:%d",level)
            Debug(false, "\n")
          }.otherwise {
            state := s_memReadReq
            raddr := paddrApply(memRdata.ppn, Mux(level === 3.U, vpn.vpn1, vpn.vpn0))
          }
        }.elsewhen (level =/= 0.U) { //TODO: fix needFlush
          val permCheck = missflag.v && !(pf.priviledgeMode === ModeU && !missflag.u) && !(pf.priviledgeMode === ModeS && missflag.u && (!pf.status_sum || ifecth))
          val permAD = if (Settings.get("FPGAPlatform")) false.B else !missflag.a || (!missflag.d && req.isWrite())
          val permExec = permCheck && missflag.x && !permAD 
          val permLoad = permCheck && (missflag.r || pf.status_mxr && missflag.x) && !permAD
          val permStore = permCheck && missflag.w && !permAD
          val updateAD = if (Settings.get("FPGAPlatform")) !missflag.a || (!missflag.d && req.isWrite()) else false.B
          val updateData = Cat( 0.U(56.W), req.isWrite(), 1.U(1.W), 0.U(6.W) )
          missRefillFlag := Cat(req.isWrite(), 1.U(1.W), 0.U(6.W)) | missflag.asUInt
          memRespStore := io.mem.resp.bits.rdata | updateData 
          if(tlbname == "itlb") {
            when (!permExec) { 
              missIPF := true.B ; state := s_wait_resp
            }.otherwise { 
              state := Mux(updateAD, s_write_pte, s_wait_resp)
              missMetaRefill := true.B
            }
          }
          if(tlbname == "dtlb") {
            when((!permLoad && req.isRead()) || (!permStore && req.isWrite())) { 
              state := s_miss_slpf
              loadPF := req.isRead() && !isAMO
              missLPF := loadPF
              storePF := req.isWrite() || isAMO
              missSPF := storePF
            }.otherwise {
              state := Mux(updateAD, s_write_pte, s_wait_resp)
              missMetaRefill := true.B
            }
          }
          missMask := Mux(level===3.U, 0.U(maskLen.W), Mux(level===2.U, "h3fe00".U(maskLen.W), "h3ffff".U(maskLen.W)))
          missMaskStore := missMask
        }
        level := level - 1.U
      }
    }

    is (s_write_pte) {
      when (ioFlush) {
        state := s_idle
        needFlush := false.B
        missIPF := false.B
        missSPF := false.B
        missLPF := false.B
      }.elsewhen (io.mem.req.fire()) { state := s_memwriteResp }
    }
    
    is(s_memwriteResp){
      when (io.mem.resp.fire()){
        when(isFlush){
          state := s_idle
          needFlush := false.B
          missIPF := false.B
          missSPF := false.B
          missLPF := false.B
        }.otherwise{
          state := s_wait_resp
        }
      }
    }

    is (s_wait_resp) { when (io.out.fire() || ioFlush ){
      state := s_idle
      missIPF := false.B
      missSPF := false.B
      missLPF := false.B
      needFlush := false.B
    }}

    is (s_miss_slpf) {
      when (io.out.fire() || ioFlush){
          state := s_idle
          missSPF := false.B
          missLPF := false.B
          missIPF := false.B
          needFlush := false.B
      }
    }
  }

  // mem
  val cmd = Mux(state === s_write_pte, SimpleBusCmd.write, SimpleBusCmd.read)
  io.mem.req.bits.apply(addr = Mux(hitWB, hitData.pteaddr, raddr), cmd = cmd, size = (if (XLEN == 64) "b11".U else "b10".U), wdata =  Mux( hitWB, hitWBStore, memRespStore), wmask = 0xff.U)
  io.mem.req.bits.vector := DontCare
  io.mem.req.valid := ((state === s_memReadReq || state === s_write_pte) && !ioFlush)
  io.mem.resp.ready := true.B

  // tlb refill
  io.mdWrite.apply(wen = io.in.valid && (missMetaRefill && !isFlush || hitWB && state === s_idle && !isFlush), 
    windex = getIndex(req.addr), waymask = waymask, vpn = vpn.asUInt, 
    asid = Mux(hitWB, hitMeta.asid, satp.asid), mask = Mux(hitWB, hitMask, missMask), 
    flag = Mux(hitWB, hitRefillFlag, missRefillFlag), ppn = Mux(hitWB, hitData.ppn, memRdata.ppn), 
    pteaddr = (Mux(hitWB, hitData.pteaddr, raddr)))

  // io
  io.out.bits := req
  io.out.bits.addr := Mux(hit, maskPaddr(hitData.ppn, req.addr(PAddrBits-1, 0), hitMask), maskPaddr(memRespStore.asTypeOf(pteBundle).ppn, req.addr(PAddrBits-1, 0), missMaskStore))
  io.out.valid := !ioFlush && io.in.valid && Mux(false.B, !(io.pf.isPF() || loadPF || storePF || missLPF || missSPF), state === s_wait_resp && !(io.pf.isPF() || loadPF || storePF || missLPF || missSPF || io.ipf))// && !alreadyOutFire
  
  io.in.ready := (!io.in.valid && (state === s_idle) || io.out.fire()) && io.mdReady

  io.ipf := Mux(hit, hitinstrPF, missIPF)
  io.isFinish := io.out.fire() //|| io.pf.isPF()

  Debug("In(%d, %d) Out(%d, %d) InAddr:%x OutAddr:%x cmd:%d \n", io.in.valid, io.in.ready, io.out.valid, io.out.ready, req.addr, io.out.bits.addr, req.cmd)
  Debug("isAMO:%d io.Flush:%d needFlush:%d alreadyOutFire:%d isFinish:%d\n",isAMO, io.flush, needFlush, alreadyOutFire, io.isFinish)
  Debug("miss:%d state:%d level:%d raddr:%x memRdata:%x missMask:%x missRefillFlag:%x missMetaRefill:%d\n", miss, state, level, raddr, memRdata.asUInt, missMask, missRefillFlag, missMetaRefill)
  Debug("meta/data: (0)%x|%b|%x (1)%x|%b|%x (2)%x|%b|%x (3)%x|%b|%x rread:%d\n", md(0).asTypeOf(tlbBundle).vpn, md(0).asTypeOf(tlbBundle).flag, md(0).asTypeOf(tlbBundle).ppn, md(1).asTypeOf(tlbBundle).vpn, md(1).asTypeOf(tlbBundle).flag, md(1).asTypeOf(tlbBundle).ppn, md(2).asTypeOf(tlbBundle).vpn, md(2).asTypeOf(tlbBundle).flag, md(2).asTypeOf(tlbBundle).ppn, md(3).asTypeOf(tlbBundle).vpn, md(3).asTypeOf(tlbBundle).flag, md(3).asTypeOf(tlbBundle).ppn, io.mdReady)
  Debug("md: wen:%d windex:%x waymask:%x vpn:%x asid:%x mask:%x flag:%x asid:%x ppn:%x pteaddr:%x\n", io.mdWrite.wen, io.mdWrite.windex, io.mdWrite.waymask, io.mdWrite.wdata.asTypeOf(tlbBundle).vpn, io.mdWrite.wdata.asTypeOf(tlbBundle).asid, io.mdWrite.wdata.asTypeOf(tlbBundle).mask, io.mdWrite.wdata.asTypeOf(tlbBundle).flag, io.mdWrite.wdata.asTypeOf(tlbBundle).asid, io.mdWrite.wdata.asTypeOf(tlbBundle).ppn, io.mdWrite.wdata.asTypeOf(tlbBundle).pteaddr)
  Debug("MemReq(%d, %d) MemResp(%d, %d) addr:%x cmd:%d rdata:%x cmd:%d\n", io.mem.req.valid, io.mem.req.ready, io.mem.resp.valid, io.mem.resp.ready, io.mem.req.bits.addr, io.mem.req.bits.cmd, io.mem.resp.bits.rdata, io.mem.resp.bits.cmd)
  Debug("io.ipf:%d hitinstrPF:%d missIPF:%d pf.loadPF:%d pf.storePF:%d loadPF:%d storePF:%d\n", io.ipf, hitinstrPF, missIPF, io.pf.loadPF, io.pf.storePF, loadPF, storePF)
}

class SIMD_TLBEXEC_INBUNDLE(implicit val tlbConfig: TLBConfig)  extends TlbBundle{
  val req = Flipped(new SimpleBusReqBundle(userBits = userBits, addrBits = VAddrBits))
  val hitVec = Output(UInt(Ways.W))
  val miss = Output(Bool())
  val hitWB = Output(Bool())
  val loadPF = Output(Bool())
  val storePF = Output(Bool())
  val hitinstrPF = Output(Bool())
}