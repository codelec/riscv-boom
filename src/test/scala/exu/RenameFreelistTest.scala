package boom.unittest.exu

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester, TesterOptionsManager, SteppedHWIOTester}
import org.scalatest.{Matchers, FlatSpec}
import boom._
import boom.unittest.common._

class RenameFreeListTester(c: RenameFreeList) extends PeekPokeTester(c) {

   private val rfl = c

}

// Invoke test with:
//    $ sbt 'testOnly boom.unittest.exu.RenameFreeListSpec'
//
class RenameFreeListSpec extends FlatSpec with Matchers {
   behavior of "RenameFreeListSpec"

   val manager = new TesterOptionsManager {
      testerOptions = testerOptions.copy(backendName = "firrtl") // firrtl or verilator
   }

   it should "compute rename excellently" in {
      chisel3.iotesters.Driver.execute(() => new RenameFreeList(1,RT_FIX.litValue,32)(new TesterConfig),manager) { c =>
         new RenameFreeListTester(c)
      } should be(true)
   }
}
