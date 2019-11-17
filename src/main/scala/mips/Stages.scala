package mips

import spinal.core._
import spinal.lib._

case class IFBundle() extends Bundle {
  val programCounter  = UInt(32 bits)
  val instruction     = Bits(32 bits)
}

class InstructionFetchStage extends Component {
  val io = new Bundle {
    val en = in(Bool)
    val result = out(IFBundle())
  }
  val programCounter = new ProgramCounter
  programCounter.io.en <> io.en
  programCounter.io.value <> io.result.programCounter
  io.result.instruction := B"32'b0010010000100001000000000001"
}

case class IDBundle() extends Bundle {
  val internalInstructionInfo = InternalInstructionInfo()
  val srcA = Bits(32 bits)
  val srcB = Bits(32 bits)
}

class InstructionDecode extends Component {
  val registerConfig = RegisterConfig(
    addressWidth  = 5,
    dataWidth     = 32
  )
  val io = new Bundle {
    val input = in(IFBundle())
    val result = out(IDBundle())
    val readPortA = master(RegisterReadInterface(registerConfig))
    val readPortB = master(RegisterReadInterface(registerConfig))
    val exData = in(BypassPara())
    val mmData = in(BypassPara())
    val wbData = in(BypassPara())
  }

  val decoder = new Decoder
  val interpreter = new Interpreter
  val rsBypass = new RegBypass
  val rtBypass = new RegBypass

  decoder.io.instruction <> io.input.instruction
  decoder.io.instructionInfo <> interpreter.io.instructionInfo
  interpreter.io.programCounter <> io.input.programCounter
  interpreter.io.internalInstructionInfo <> io.result.internalInstructionInfo

  io.readPortA.en := True
  io.readPortB.en := True
  io.readPortA.address := decoder.io.instructionInfo.rs.asUInt
  io.readPortB.address := decoder.io.instructionInfo.rt.asUInt

  rsBypass.io.idData.address := decoder.io.instructionInfo.rs
  rsBypass.io.idData.data := io.readPortA.data.asBits
  rsBypass.io.exData <> io.exData
  rsBypass.io.mmData <> io.mmData
  rsBypass.io.wbData <> io.wbData

  rtBypass.io.idData.address := decoder.io.instructionInfo.rt
  rtBypass.io.idData.data := io.readPortB.data.asBits
  rtBypass.io.exData <> io.exData
  rtBypass.io.mmData <> io.mmData
  rtBypass.io.wbData <> io.wbData

  val rsBypassResult = rsBypass.io.result
  val rtBypassResult = rtBypass.io.result

  io.result.srcA := rsBypassResult
  io.result.srcB := interpreter.io.aluSrcInfo.mux(
    default -> rtBypassResult,
    AluSrcInfo.SRC_IMM_SIGNED.asBits -> decoder.io.instructionInfo.imm.asSInt.resize(32 bit).asBits,
    AluSrcInfo.SRC_IMM_UNSIGNED.asBits -> decoder.io.instructionInfo.imm.asUInt.resize(32 bit).asBits
  )
}

case class EXBundle() extends Bundle {
  val internalInstructionInfo = InternalInstructionInfo()
  val alu_result = Bits(32 bits)
}

class Execution extends Component {
  val io = new Bundle {
    val input = in(IDBundle())
    val output = out(EXBundle())
  }

  val alu = new ALU

  io.output.internalInstructionInfo <> io.input.internalInstructionInfo

  alu.io.aluOp <> io.input.internalInstructionInfo.aluOp
  alu.io.srcA <> io.input.srcA.asUInt
  alu.io.srcB <> io.input.srcB.asUInt
  alu.io.result.asBits <> io.output.alu_result
}

case class MMBundle() extends Bundle {
  val internalInstructionInfo = InternalInstructionInfo()
  val mm_result = Bits(32 bits)
}

// Dummy stage
class MemoryStage extends Component {
  val io = new Bundle {
    val input = in(EXBundle())
    val output = out(MMBundle())
  }

  io.input.internalInstructionInfo <> io.output.internalInstructionInfo
  io.input.alu_result <> io.output.mm_result
}

class WriteBackStage extends Component {
  val registerConfig = RegisterConfig(
    addressWidth  = 5,
    dataWidth     = 32
  )

  val io = new Bundle {
    val input = in(MMBundle())
    val output = master(RegisterWriteInterface(registerConfig))
  }

  io.output.en := io.input.internalInstructionInfo.writeToDest
  io.output.address := io.input.internalInstructionInfo.destAddress.asUInt
  io.output.data := io.input.mm_result.asUInt
}