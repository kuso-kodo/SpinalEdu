package mips

import spinal.core._

object ALUOp extends SpinalEnum {
  val ALU_NOP, ALU_ADD = newElement()
}

class ALU extends Component {
  val io = new Bundle {
    val aluOp   = in(Bits(ALUOp.ALU_NOP.asBits.getWidth bits))
    val srcA    = in UInt(32 bits)
    val srcB    = in UInt(32 bits)
    val result  = out UInt(32 bits)
  }

  switch(io.aluOp) {
    is(ALUOp.ALU_ADD.asBits) {
      io.result := io.srcA + io.srcB
    }
    default {
      io.result := U"h00000000"
    }
  }
}
