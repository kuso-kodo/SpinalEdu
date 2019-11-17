package util

import spinal.core._
import spinal.lib._

import scala.collection.mutable

object DecodeUtil {
  def decodeMap[T <: Data](mappings: Seq[(Any, Seq[T])]): Seq[Seq[(Any, T)]] = {
    if(mappings.isEmpty)
      throw new IllegalArgumentException("Empty list is not permitted")
    var result = scala.collection.mutable.MutableList[mutable.MutableList[(Any, T)]]()

    mappings.head._2.foreach(
      _ => result += mutable.MutableList[(Any, T)]()
    )

    mappings.foreach{
      single_map : (Any, Seq[T]) => {
        single_map._2.zipWithIndex.foreach{
          case (item, index) => result(index) += (single_map._1 -> item)
        }
      }
    }
    result
  }
}
