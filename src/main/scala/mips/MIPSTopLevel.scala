package mips

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._

import scala.util.Random

class MIPSTopLevel extends Component {
  val io = new Bundle {
    val en = in(Bool())
  }
  val instructionFetchStage = new InstructionFetchStage
  val instructionDecode = new InstructionDecode
  val execution = new Execution
  val memoryStage = new MemoryStage
  val writeBackStage = new WriteBackStage
  val registerFile = new RegisterFile

  val ifReg = RegNext(instructionFetchStage.io.result)
  val idReg = RegNext(instructionDecode.io.result)
  val exReg = RegNext(execution.io.output)
  val mmReg = RegNext(memoryStage.io.output)

  ifReg <> instructionDecode.io.input
  idReg <> execution.io.input
  exReg <> memoryStage.io.input
  mmReg <> writeBackStage.io.input

  registerFile.io.readPortA <> instructionDecode.io.readPortA
  registerFile.io.readPortB <> instructionDecode.io.readPortB
  registerFile.io.writePort <> writeBackStage.io.output

  instructionDecode.io.exData.en <> idReg.internalInstructionInfo.writeToDest
  instructionDecode.io.exData.address <> idReg.internalInstructionInfo.destAddress
  instructionDecode.io.exData.data <> execution.io.output.alu_result

  instructionDecode.io.mmData.en <> exReg.internalInstructionInfo.writeToDest
  instructionDecode.io.mmData.address <> exReg.internalInstructionInfo.destAddress
  instructionDecode.io.mmData.data <> memoryStage.io.output.mm_result

  instructionDecode.io.wbData.en <> mmReg.internalInstructionInfo.writeToDest
  instructionDecode.io.wbData.address <> mmReg.internalInstructionInfo.destAddress
  instructionDecode.io.wbData.data <> writeBackStage.io.output.data.asBits
}

//Generate the MyTopLevel's Verilog
object MyTopLevelVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new MIPSTopLevel)
  }
}

object MyTopLevelVerilogSim {
  def main(args: Array[String]) {
    SimConfig.withWave.compile(new MIPSTopLevel).doSim {
      dut => var x = 0
    }
  }
}