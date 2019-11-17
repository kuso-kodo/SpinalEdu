package mips

import spinal.core._

case class BypassPara() extends Bundle {
  val address = Bits(5 bits)
  val data =  Bits(32 bits)
  val en = Bool()
}

class RegBypass extends Component{
  val io = new Bundle {
    val idData = in(BypassPara())
    val exData = in(BypassPara())
    val mmData = in(BypassPara())
    val wbData = in(BypassPara())
    val result = out(Bits(32 bits))
  }

  when(io.idData.address === B"5'x00") {
    io.result := B"32'x0"
  }.elsewhen(io.exData.en && io.idData.address === io.exData.address) {
    io.result := io.exData.data
  }.elsewhen(io.mmData.en && io.idData.address === io.mmData.address) {
    io.result := io.mmData.data
  }.elsewhen(io.wbData.en && io.wbData.address === io.idData.address) {
    io.result := io.wbData.data
  }.otherwise {
    io.result := io.idData.data
  }

}
