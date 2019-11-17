package mips

import spinal.core._
import spinal.core.Mem
import spinal.lib.IMasterSlave
import spinal.lib.slave

case class RegisterConfig(
                         addressWidth : Int,
                         dataWidth : Int
                         )

case class RegisterReadInterface(config: RegisterConfig) extends
Bundle with IMasterSlave {
  val address     = UInt(config.addressWidth bit)
  val data        = UInt(config.dataWidth bit)
  val en          = Bool

  override def asMaster(): Unit = {
    in(data)
    out(address, en)
  }
}

case class RegisterWriteInterface(config: RegisterConfig) extends
Bundle with IMasterSlave {
  val address     = UInt(config.addressWidth bit)
  val data        = UInt(config.dataWidth bit)
  val en          = Bool

  override def asMaster(): Unit = {
    out(data, address, en)
  }
}

class RegisterFile extends Component {
  val registerConfig = RegisterConfig(
    addressWidth  = 5,
    dataWidth     = 32
  )

  val io = new Bundle{
    val readPortA = slave(RegisterReadInterface(registerConfig))
    val readPortB = slave(RegisterReadInterface(registerConfig))
    val writePort = slave(RegisterWriteInterface(registerConfig))
  }

  val regFileSize = 32
  val regFile = Mem(UInt(32 bits), regFileSize) init (List.fill(regFileSize)(U(0, 32 bits)))
  io.readPortA.data := regFile.readAsync(io.readPortA.address)
  io.readPortB.data := regFile.readAsync(io.readPortB.address)

  when(io.writePort.en) {
    regFile.write(io.writePort.address, io.writePort.data)
  }
}
