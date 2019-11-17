package mips

import spinal.core._

class ProgramCounter extends Component {
  val io = new Bundle {
    val en  = in Bool
    val value = out UInt(32 bits)
  }

  val programCounterReg = Reg(UInt(32 bits)) init (U"hbfc00000")
  when(io.en) {
    programCounterReg := programCounterReg + 0x4
  }
  io.value := programCounterReg
}
