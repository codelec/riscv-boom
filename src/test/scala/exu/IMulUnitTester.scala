package boom.unittest.exu

import org.scalatest._

import chisel3._
import chisel3.tester._

import boom.exu._
import boom.unittest.common._

import firrtl_interpreter._
import firrtl.{ExecutionOptionsManager, HasFirrtlOptions}

class IMulUnitTester2 extends FlatSpec with ChiselScalatestTester with UnittestHelperFunc {
  behavior of "Testers2"

  it should "test static circuits" in {
    test(new IMul(imul_stages = 3)) { imul =>
      initAll(imul.io.elements)

      imul.clock.step()
      imul.io.in0.poke(1.U)
      imul.io.in1.poke(2.U)
      imul.io.valid.poke(true.B)
      imul.clock.step()
      imul.clock.step()
      imul.io.out.expect(0.U)
      imul.clock.step()
      imul.io.out.expect(2.U)
    }
  }
}
