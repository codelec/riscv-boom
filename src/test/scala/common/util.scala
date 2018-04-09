package boom.unittest.common

import chisel3._
import chisel3.tester._

import org.scalatest._
import boom._

import scala.collection.immutable.ListMap

trait UnittestHelperFunc extends FlatSpec {

	def initAll(lm: ListMap[String,Data]): Unit = {
		lm.foreach { case (str, dat) => 
		   dat match {
		      case b : Bool => b.poke(false.B) 
		      case a : UInt => a.poke(0.U)  
		      case c : Vec[_] => c.zipWithIndex.foreach { case (each, i) => initAll(ListMap((str+"("+i+")") -> each)) }
		      case d : Bundle => initAll(d.elements)
		   }
		}
   }
   def step(implicit mod: Module) = mod.clock.step(1)
}