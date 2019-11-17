package mips

import spinal.core._

case class InstructionInfo() extends Bundle {
  val opcode  = Bits(6 bits)
  val rs      = Bits(5 bits)
  val rt      = Bits(5 bits)
  val rd      = Bits(5 bits)
  val shamt   = Bits(5 bits)
  val funct   = Bits(6 bits)
  val imm     = Bits(16 bits)
  val address = Bits(26 bits)
}

class Decoder extends Component {
  val io = new Bundle {
    val instruction = in Bits(32 bits)
    val instructionInfo = out(InstructionInfo())
  }

  io.instructionInfo.opcode := io.instruction(31 downto 26)
  io.instructionInfo.rs     := io.instruction(25 downto 21)
  io.instructionInfo.rt     := io.instruction(20 downto 16)
  io.instructionInfo.rd     := io.instruction(15 downto 11)
  io.instructionInfo.shamt  := io.instruction(10 downto 6)
  io.instructionInfo.funct  := io.instruction(5 downto 0)
  io.instructionInfo.imm    := io.instruction(15 downto 0)
  io.instructionInfo.address:= io.instruction(25 downto 0)
}
