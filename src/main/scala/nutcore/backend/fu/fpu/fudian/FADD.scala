package nutcore.backend.fu.fpu.fudian

import chisel3._
import chisel3.util._
import nutcore.backend.fu.fpu.fudian.utils._
import sun.awt.geom.AreaOp.SubOp

//sigWidth includes hidden bit
class FarPath(val expWidth: Int, val sigWidth: Int, val outSigWidth: Int)
  extends Module {
  val io = IO(new Bundle() {
    val in = Input(new Bundle() {
      val a, b = new RawFloat(expWidth, sigWidth)
      val inv = Bool()
      val addSig = Bool()
      val tinyAdd = Bool()
      val shiftNum = UInt(log2Ceil(sigWidth).W)
      val roundingMode = UInt(3.W)
    })
    val out = Output(new Bundle() {
      val result = UInt((expWidth + outSigWidth).W)
      val far_path_of = Bool()
      val far_path_ix = Bool()
      val far_path_uf = Bool()
    })
  })
  val shiftNum = io.in.shiftNum
  val tinyAdd = io.in.tinyAdd
  val aIsZero = !io.in.a.exp.orR
  val bIsZero = !io.in.b.exp.orR
  val bothZero = aIsZero & bIsZero
  val result_sign = io.in.a.sign
  val (alignedSigB, sigB_sticky) = ShiftRightJam(Cat(io.in.b.sig, 0.U(2.W)), shiftNum)
  //  val alignedSigB = Cat(io.in.b.sig, 0.U(2.W)) >> shiftNum
  //  val sigB_sticky = (Cat(io.in.b.sig, 0.U(2.W)).tail(sigWidth + 2 - shiftNum) & ((1 << shiftNum) - 1).U).orR
  val pos_sigB = Cat(0.U(1.W), alignedSigB, sigB_sticky)
  val neg_sigB = Cat(1.U(1.W), ~Cat(alignedSigB, sigB_sticky) + 1.U)
  val adder_in_sigB = Mux(io.in.addSig, pos_sigB, neg_sigB)
  val adder_in_sigA = Cat(0.U(1.W), io.in.a.sig, 0.U(3.W))
  val resultSigInNormalCase = adder_in_sigA + adder_in_sigB
  val resultSigInZeroCase = adder_in_sigA
  val result_sig_raw = Mux(bIsZero, resultSigInZeroCase, resultSigInNormalCase)

  val cout = result_sig_raw.head(1).asBool
  val keep = result_sig_raw.tail(1).head(1).asBool
  val borrow = result_sig_raw.head(2) === 0.U
  val resultSigNoRound = PriorityMux(
    Seq(cout, keep||tinyAdd, borrow),
    Seq(
      result_sig_raw.head(outSigWidth + 2) ## result_sig_raw.tail(outSigWidth + 2).orR,
      result_sig_raw.tail(1).head(outSigWidth + 2) ## result_sig_raw.tail(outSigWidth + 3).orR,
      result_sig_raw.tail(2).head(outSigWidth + 2) ## result_sig_raw.tail(outSigWidth + 4).orR
    )
  )

  val resultExpNoRound = PriorityMux(
    Seq(
      cout -> (io.in.a.exp + 1.U),
      (keep || bothZero) -> io.in.a.exp,
      (borrow || tinyAdd) -> (io.in.a.exp - 1.U)
    )
  )
  //    Mux1H(
  //    Seq(cout, keep||tinyAdd, borrow && (!tinyAdd)),
  //    Seq(
  //      io.in.a.exp + 1.U,
  //      io.in.a.exp,
  //      io.in.a.exp - 1.U
  //    )
  //  )
  val farPathResult = Wire(new RawFloat(expWidth, outSigWidth + 3))
  farPathResult.exp  := resultExpNoRound
  farPathResult.sign := io.in.inv ^ result_sign
  farPathResult.sig  := resultSigNoRound

  val far_path_tininess_rounder = Module(new TininessRounder(expWidth, outSigWidth))
  far_path_tininess_rounder.io.in := farPathResult
  far_path_tininess_rounder.io.rm := io.in.roundingMode
  val far_path_tininess = tinyAdd && far_path_tininess_rounder.io.tininess

  val far_path_rounder = RoundingUnit(
    in = resultSigNoRound.tail(1),
    rm = io.in.roundingMode,
    sign = result_sign,
    width = outSigWidth - 1
  )

  val far_path_exp_rounded = far_path_rounder.io.cout + resultExpNoRound
  val far_path_sig_rounded = far_path_rounder.io.out

  val far_path_may_of = io.in.b.exp.andR && io.in.addSig
  //  val far_path_may_uf = far_path_out.tininess && !far_path_mul_of
  val far_path_may_uf = far_path_tininess && !far_path_may_of

  val far_path_of_before_round =
    resultExpNoRound === ((BigInt(1) << expWidth) - 1).U
  val far_path_of_after_round = far_path_rounder.io.cout &&
    resultExpNoRound === ((BigInt(1) << expWidth) - 2).U

  io.out.far_path_of :=
    far_path_of_before_round || far_path_of_after_round || far_path_may_of
  io.out.far_path_ix := far_path_rounder.io.inexact | io.out.far_path_of
  io.out.far_path_uf := far_path_may_uf & io.out.far_path_ix

  io.out.result :=
    Cat(farPathResult.sign, far_path_exp_rounded, far_path_sig_rounded)
}

class ClosePath(val expWidth: Int, val sigWidth: Int, val outSigWidth: Int)
  extends Module {
  val io = IO(new Bundle() {
    val in = Input(new Bundle() {
      val a, b = new RawFloat(expWidth, sigWidth)
      val inv = Bool()
      val needShift = Bool()
      val roundingMode = Input(UInt(3.W))
    })
    val out = Output(new Bundle() {
      val result = UInt((expWidth + outSigWidth).W)
      val near_path_of = Bool()
      val near_path_ix = Bool()
      val near_path_uf = Bool()
    })
  })
  //result calculate
  val a_sig = Cat(io.in.a.sig, 0.U(1.W))
  val b_sig = Cat(io.in.b.sig, 0.U(1.W)) >> io.in.needShift
  val bIsZero = !io.in.b.exp.orR
  val resultSigComplementForm = Mux(bIsZero,
    Cat(0.U(1.W), a_sig),
    Cat(0.U(1.W), a_sig) + Cat(1.U(1.W), ~b_sig + 1.U))
  val a_LessThan_b = resultSigComplementForm.head(1).asBool
  val resultSigNoRound = Mux(a_LessThan_b,
    ~resultSigComplementForm.tail(1) + 1.U,
    resultSigComplementForm.tail(1))
  val result_sign = Mux(a_LessThan_b, ~io.in.a.sign, io.in.a.sign)
  //leading zero counting
  val lza = Module(new LZA(sigWidth + 1))
  lza.io.a := a_sig
  lza.io.b := b_sig
  val lzc = lza.io.lzc
  val lzaError = lza.io.error
  val closePathSigIsZero = lza.io.zero

  // val near_path_res = io.in.near_path_out
  val closePathResult = Wire(new RawFloat(expWidth, outSigWidth + 3))
  closePathResult.sign := result_sign

  // val near_path_sig = near_path_res.sig
  val resultExpNoRound = Mux(closePathSigIsZero, 0.U(expWidth.W), io.in.a.exp)
  val resultExpIsZero = resultExpNoRound === 0.U

  val shiftLimit = resultExpNoRound <= (lzc + lzaError.asUInt)
  val exp_s1 = resultExpNoRound - lzc
  val exp_s2 = exp_s1 - lzaError
  closePathResult.exp := Mux(shiftLimit, 0.U, exp_s2)

  val sigShiftNum = PriorityMux(
    Seq(
      resultExpIsZero -> 0.U,
      shiftLimit -> (resultExpNoRound - 1.U),
      true.B -> lzc
    )
  )
  val sig_s1 = (resultSigNoRound << sigShiftNum)(sigWidth, 0)
  val sig_s2 = Mux(lzaError, Cat(sig_s1.tail(1), 0.U(1.W)), sig_s1)
  val sig_s3 = if (outSigWidth + 3 > sigWidth + 1) {
    Cat(
      Mux(shiftLimit, sig_s1, sig_s2),
      0.U((outSigWidth + 3 - sigWidth - 1).W)
    )
  } else {
    Mux(shiftLimit, sig_s1, sig_s2)
  }
  val close_path_sig = sig_s3.head(outSigWidth + 2) ## sig_s3.tail(outSigWidth + 2).orR
  closePathResult.sig := close_path_sig

  val near_path_tininess_rounder = Module(new TininessRounder(expWidth, outSigWidth))
  near_path_tininess_rounder.io.in := closePathResult
  near_path_tininess_rounder.io.rm := io.in.roundingMode
  val near_path_tininess = near_path_tininess_rounder.io.tininess

  val near_path_rounder = RoundingUnit(
    in = close_path_sig.tail(1),
    rm = io.in.roundingMode,
    sign = closePathResult.sign,
    width = outSigWidth - 1
  )

  val near_path_exp_rounded = near_path_rounder.io.cout + closePathResult.exp
  val near_path_sig_rounded = near_path_rounder.io.out
  val near_path_zero_sign = io.in.roundingMode === RDN
  io.out.result := Cat(
    ((io.in.inv ^ closePathResult.sign) && !closePathSigIsZero) || (near_path_zero_sign && closePathSigIsZero),
    near_path_exp_rounded,
    near_path_sig_rounded
  )

  io.out.near_path_of := near_path_exp_rounded === (~0.U(expWidth.W)).asUInt
  io.out.near_path_ix := near_path_rounder.io.inexact || io.out.near_path_of
  io.out.near_path_uf := near_path_tininess && io.out.near_path_ix

}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
class FADDIn (val expWidth: Int, val sigWidth: Int) extends Bundle {
  val subOp = Output(Bool())
  val inv = Output(Bool())
  val a = Output(UInt((expWidth + sigWidth).W))
  val b = Output(UInt((expWidth + sigWidth).W))
  val roundingMode = Output(UInt(3.W))
  val a_inter_valid = Output(Bool())
  val b_inter_valid = Output(Bool())
  val a_inter_flags = Output(new FMULToFADD_fflags)
  val b_inter_flags = Output(new FMULToFADD_fflags)
}

class FADDOut (val expWidth: Int, val sigWidth: Int) extends Bundle {
  val fflags = Output(UInt(5.W))
  val result = Output(UInt((expWidth + sigWidth).W))
}

class FADD(inExpWidth: Int, inSigWidth: Int, outSigWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new FADDIn(inExpWidth, inSigWidth)))
    val out = Decoupled(new FADDOut(inExpWidth, outSigWidth))
  })
  val valid = io.in.valid
  val ready = io.out.ready
  val fp_a = FloatPoint.fromUInt(io.in.bits.a, inExpWidth, inSigWidth)
  val fp_b = FloatPoint.fromUInt(io.in.bits.b, inExpWidth, inSigWidth)
  val decode_a = fp_a.decode
  val decode_b = fp_b.decode
  val raw_a = RawFloat.fromFP(fp_a, Some(decode_a.expNotZero), Some(decode_a.isSubnormal))
  val raw_b = RawFloat.fromFP(fp_b, Some(decode_b.expNotZero), Some(decode_b.isSubnormal))
  //Path Select & Exponent Difference
  val effSignB = fp_b.sign ^ io.in.bits.subOp
  val addSig = fp_a.sign === effSignB
  val needSwap = raw_a.exp < raw_b.exp
  val diffExp = Mux(needSwap, raw_b.exp - raw_a.exp, raw_a.exp - raw_b.exp)
  //Exponent Align Limit
  val alignLimitWidth = log2Ceil(inSigWidth)
  val initShiftNum = diffExp(alignLimitWidth - 1, 0)
  val isMaxAlign = diffExp > (inSigWidth + 2).U  //the difference between a.exp and b.exp is out of sig shift limit
  val shiftNum = Mux(isMaxAlign, (inSigWidth + 2).U(alignLimitWidth.W), initShiftNum)
  val closePathSel = !addSig && !isMaxAlign && (initShiftNum <= 1.U)
  /*------------------------------------------------------------------------
  far Path
  *------------------------------------------------------------------------*/
  val farPath = Module(new FarPath(inExpWidth, inSigWidth, outSigWidth))
  farPath.io.in.a := Mux(needSwap, raw_b, raw_a)
  farPath.io.in.b := Mux(needSwap, raw_a, raw_b)
  farPath.io.in.inv := io.in.bits.inv || (io.in.bits.subOp && needSwap)
  farPath.io.in.addSig := addSig
  farPath.io.in.tinyAdd := decode_a.expIsZero & decode_b.expIsZero
  farPath.io.in.shiftNum := shiftNum
  farPath.io.in.roundingMode := io.in.bits.roundingMode
  val farPathResult = farPath.io.out.result
  val far_path_of = farPath.io.out.far_path_of
  val far_path_ix = farPath.io.out.far_path_ix
  val far_path_uf = farPath.io.out.far_path_uf
  /*------------------------------------------------------------------------
  close Path
   ------------------------------------------------------------------------*/
  val closePath = Module(new ClosePath(inExpWidth, inSigWidth, outSigWidth))
  closePath.io.in.a := Mux(needSwap, raw_b, raw_a)
  closePath.io.in.b := Mux(needSwap, raw_a, raw_b)
  closePath.io.in.inv := io.in.bits.inv
  closePath.io.in.needShift := initShiftNum === 1.U
  closePath.io.in.roundingMode := io.in.bits.roundingMode
  val closePathResult = closePath.io.out.result
  val near_path_of = closePath.io.out.near_path_of
  val near_path_ix = closePath.io.out.near_path_ix
  val near_path_uf = closePath.io.out.near_path_uf
  /*------------------------------------------------------------------------
  special case
  *------------------------------------------------------------------------*/
  val a_is_inter = io.in.bits.a_inter_valid
  val a_flags = io.in.bits.a_inter_flags
  val a_isNaN = Mux(a_is_inter, a_flags.isNaN, decode_a.isNaN)
  val a_isSNaN = Mux(a_is_inter, a_flags.isInv, decode_a.isSNaN)
  val a_isInf = Mux(a_is_inter, a_flags.isInf, decode_a.isInf)

  val b_is_inter = io.in.bits.b_inter_valid
  val b_flags = io.in.bits.b_inter_flags
  val b_isNaN = Mux(b_is_inter, b_flags.isNaN, decode_b.isNaN)
  val b_isSNaN = Mux(b_is_inter, b_flags.isInv, decode_b.isSNaN)
  val b_isInf = Mux(b_is_inter, b_flags.isInf, decode_b.isInf)

  val special_path_hasNaN = a_isNaN || b_isNaN
  val special_path_hasSNaN = a_isSNaN || b_isSNaN
  val special_path_hasInf = a_isInf || b_isInf
  val special_path_inf_iv = a_isInf && b_isInf && !addSig
  val special_case_happen = special_path_hasNaN || special_path_hasInf
  val special_path_iv = special_path_hasSNaN || special_path_inf_iv

  val special_path_result = Mux(
    special_path_hasNaN || special_path_inf_iv,
    Cat(0.U(1.W), Fill(inExpWidth + 1, 1.U(1.W)), 0.U((inSigWidth - 2).W)), //qNaN
    Cat(                                                                //Inf
      Mux(a_isInf, fp_a.sign, fp_b.sign),
      ~0.U(inExpWidth.W),
      0.U((inSigWidth - 1).W)
    )
  )
  val special_path_fflags = Cat(special_path_iv, 0.U(4.W))
  /*------------------------------------------------------------------------
  result
  *------------------------------------------------------------------------*/
  val common_overflow_sign =
    Mux(closePathSel, closePathResult.head(1).asBool, farPathResult.head(1).asBool)
  val rmin = RoundingUnit.is_rmin(io.in.bits.roundingMode, farPathResult.head(1).asBool)
  val common_overflow_exp = Mux(
    rmin,
    ((BigInt(1) << inExpWidth) - 2).U(inExpWidth.W),
    ((BigInt(1) << inExpWidth) - 1).U(inExpWidth.W)
  )
  val common_overflow_sig =
    Mux(rmin, Fill(inSigWidth - 1, 1.U(1.W)), 0.U((inSigWidth - 1).W))
  val common_overflow =
    !closePathSel && far_path_of || closePathSel && near_path_of
  val common_underflow =
    !closePathSel && far_path_uf || closePathSel && near_path_uf
  val common_inexact =
    !closePathSel && far_path_ix || closePathSel && near_path_ix
  val common_fflags = Cat(
    false.B,
    false.B,
    common_overflow,
    common_underflow,
    common_inexact
  )

  io.in.ready := ready
  io.out.valid := valid
  io.out.bits.result := Mux(
    special_case_happen,
    special_path_result,
    Mux(
      common_overflow,
      Cat(common_overflow_sign, common_overflow_exp, common_overflow_sig),
      Mux(closePathSel, closePathResult, farPathResult)
    )
  )
  io.out.bits.fflags := Mux(special_case_happen, special_path_fflags, common_fflags)
}

object FADD {
  def apply(a: UInt, b: UInt, rm: UInt, subOp: Bool, inv: Bool,
            expWidth: Int, inSigWidth: Int, outSigWidth: Int,
            pre_valid: Bool, post_ready: Bool,
            a_inter_valid: Bool, b_inter_valid: Bool,
            a_inter_flags: Option[FMULToFADD_fflags] = None,
            b_inter_flags: Option[FMULToFADD_fflags] = None):(UInt, UInt, Bool, Bool) = {
    require((a.getWidth <= expWidth + inSigWidth) && (b.getWidth <= expWidth + inSigWidth))

    val faddModule = Module(new FADD(expWidth, inSigWidth, outSigWidth))

    def padd_tail(x: UInt, w: Int): UInt = Cat(x, 0.U((w - x.getWidth).W))
    val a_pad = if(a.getWidth < expWidth + inSigWidth) padd_tail(a, expWidth + inSigWidth) else a
    val b_pad = if(b.getWidth < expWidth + inSigWidth) padd_tail(b, expWidth + inSigWidth) else b
    val a_flag = if(a_inter_flags.isDefined) a_inter_flags.get else DontCare
    val b_flag = if(b_inter_flags.isDefined) b_inter_flags.get else DontCare

    faddModule.io.in.valid := pre_valid
    faddModule.io.in.bits.subOp := subOp
    faddModule.io.in.bits.inv := inv
    faddModule.io.in.bits.a := a_pad
    faddModule.io.in.bits.b := b_pad
    faddModule.io.in.bits.roundingMode := rm
    faddModule.io.in.bits.a_inter_valid := a_inter_valid
    faddModule.io.in.bits.b_inter_valid := b_inter_valid
    faddModule.io.in.bits.a_inter_flags := a_flag
    faddModule.io.in.bits.b_inter_flags := b_flag
    faddModule.io.out.ready := post_ready

    (faddModule.io.out.bits.result, faddModule.io.out.bits.fflags, faddModule.io.out.valid, faddModule.io.in.ready)
  }
}

object FADD32 extends App {
  emitVerilog(new FADD(8, 48, 24), Array("--target-dir", "generated"))
}