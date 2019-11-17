package mips

import spinal.core._
import spinal.lib

object AluSrcInfo extends SpinalEnum {
  val SRC_REG, SRC_IMM_UNSIGNED, SRC_IMM_SIGNED = newElement()
}

object OpcodeInfo extends SpinalEnum {
  val SPECIAL, ADDI, ADDIU, DUMMY = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    SPECIAL -> Integer.parseInt("000000", 2),
    ADDI    -> Integer.parseInt("001000", 2),
    ADDIU   -> Integer.parseInt("001001", 2),
    DUMMY   -> Integer.parseInt("111111", 2)
  )
}

object FunctInfo extends SpinalEnum {
  val ADD, ADDU = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    ADD     -> Integer.parseInt("100000", 2),
    ADDU    -> Integer.parseInt("100001", 2)
  )
}

case class InternalInstructionInfo() extends Bundle {
  val programCounter  = UInt(32 bits)
  val rsAddress       = Bits(5 bits)
  val rtAddress       = Bits(5 bits)
  val destAddress     = Bits(5 bits)
  val writeToDest     = Bool()
  val aluOp           = Bits(ALUOp.ALU_NOP.asBits.getWidth bits)
}

class Interpreter extends Component{
  val io = new Bundle {
    val programCounter  = in UInt(32 bits)
    val instructionInfo = in(InstructionInfo())
    val internalInstructionInfo = out(InternalInstructionInfo())
    val aluSrcInfo = out(Bits(AluSrcInfo.SRC_REG.asBits.getWidth bits))
  }

  io.internalInstructionInfo.programCounter := io.programCounter
  io.internalInstructionInfo.rsAddress := io.instructionInfo.rs
  io.internalInstructionInfo.rtAddress := io.instructionInfo.rt

  var opcodeMap = scala.collection.mutable.MutableList[(Any, Seq[Bits])](
    OpcodeInfo.ADDI.asBits -> List(AluSrcInfo.SRC_IMM_SIGNED.asBits, ALUOp.ALU_ADD.asBits, io.instructionInfo.rd, True.asBits),
    OpcodeInfo.ADDIU.asBits -> List(AluSrcInfo.SRC_IMM_SIGNED.asBits, ALUOp.ALU_ADD.asBits, io.instructionInfo.rd, True.asBits),
    default -> List(AluSrcInfo.SRC_REG.asBits, ALUOp.ALU_NOP.asBits, io.instructionInfo.rd, False.asBits)
  )

  var functMap = scala.collection.mutable.MutableList[(Any, Seq[Bits])](
    FunctInfo.ADD.asBits -> List(AluSrcInfo.SRC_REG.asBits, ALUOp.ALU_ADD.asBits, io.instructionInfo.rd, True.asBits),
    FunctInfo.ADDU.asBits -> List(AluSrcInfo.SRC_REG.asBits, ALUOp.ALU_ADD.asBits, io.instructionInfo.rd, True.asBits),
    default -> List(AluSrcInfo.SRC_REG.asBits, ALUOp.ALU_NOP.asBits, io.instructionInfo.rd, False.asBits)
  )

  val decodedOpcodeMap = util.DecodeUtil.decodeMap(opcodeMap)
  val decodedFunctMap = util.DecodeUtil.decodeMap(functMap)

  def getResult(x: Int) = {
    io.instructionInfo.opcode.mux(
      OpcodeInfo.SPECIAL.asBits -> io.instructionInfo.opcode.muxList(
        decodedFunctMap(x)
      ),
      default -> io.instructionInfo.opcode.muxList(
        decodedOpcodeMap(x)
      )
    )
  }

  io.aluSrcInfo := getResult(0)
  io.internalInstructionInfo.aluOp := getResult(1)
  io.internalInstructionInfo.destAddress := getResult(2)
  io.internalInstructionInfo.writeToDest := getResult(3).as(Bool)
}
