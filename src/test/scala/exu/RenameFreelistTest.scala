package boom.unittest.exu

import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import boom._
import boom.unittest.common._
import freechips.rocketchip.config._
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}

trait RenameTestHelper extends PeekPokeTester[RenameFreeList] {
   implicit val p: Parameters
   var idx = 0
   def checkFreeStatus = { implicit c: RenameFreeList => expect(c.io.can_allocate(idx), 1) }
   def uopGen(stale_pdst: Int, ldst_val: Int, pdst: Int) = { implicit c: RenameFreeList => 
      poke(c.io.com_uops(idx).dst_rtype,RT_FIX.litValue)
      poke(c.io.com_uops(idx).valid,true)
   }
   /*def init = { implicit c: RenameFreeList => 
      poke(c.io.brinfo, 0.U.asTypeOf(new BrResolutionInfo()).asUInt.litValue)
      poke(c.io.kill, 0)
   }*/
   var basic = ListMap[String,chisel3.core.Data]()
   var nonbasic = ListMap[String,chisel3.core.Data]()
   def onlyBasic[T <: Bundle](bun: T) = {
      val a = bun.elements.filter (t => 
         t._2 match {
            case c : Bits => true
            case _ => {
               basic += (t._1 -> t._2) 
               false
            }
         })
      a.map {c => c._1}
   }

   def getBundleElement(map: mutable.LinkedHashMap[String, Bits], indexPrefix: ArrayBuffer[String], signalName: String, signalData: Data, prevVec: Boolean): Unit = {
      indexPrefix += signalName
/*      if (prevVec) indexPrefix.remove(indexPrefix.size - 1)
         val append = indexPrefix.take(indexPrefix.size - 1)
         indexPrefix.remove(indexPrefix.size - 1)
         indexPrefix += append + "(0)" */
      signalData match {
         case bundle: Bundle =>
            for ((name, value) <- bundle.elements) {
               getBundleElement(map, indexPrefix, name, value, false)
            }
         case bits: Bits =>
            val index = indexPrefix.mkString(".")
            map(index) = bits
         case vectype: Vec[Any] => 
            vectype(0) match {
               case a : Bits => 
                  val index = indexPrefix.mkString(".")
                  map(index) = vectype(0).asInstanceOf[Bits]
               case b : Bundle => getBundleElement(map, indexPrefix, "(0)", vectype(0).asInstanceOf[Bundle], true)
            }
      }
      indexPrefix.remove(indexPrefix.size - 1)
   }
}


class RenameFreeListTester(c: RenameFreeList)(implicit val p: Parameters) extends PeekPokeTester(c) 
   with RenameTestHelper {

   implicit val d = c
   val bitsMap = mutable.LinkedHashMap[String, Bits]()
   val index = ArrayBuffer[String]()
//   getBundleElement(bitsMap,index,"c.io",c.io, false)
//   println(" " + bitsMap.keys)
   step(1)
   idx = 1
   checkFreeStatus 
}

// Invoke test with:
//    $ sbt 'testOnly boom.unittest.exu.RenameFreeListSpec'
class RenameFreeListSpec extends FlatSpec with Matchers {
   behavior of "RenameFreeListSpec"

   val manager = new TesterOptionsManager {
      testerOptions = testerOptions.copy(backendName = "firrtl", isVerbose = true) // firrtl or verilator
   }

   implicit val p = new TesterConfig

   it should "compute rename excellently" in {
      chisel3.iotesters.Driver.execute(() => new RenameFreeList(1,RT_FIX.litValue,32)(p),manager) { c =>
         new RenameFreeListTester(c)(p)
      } should be(true)
   }
}
