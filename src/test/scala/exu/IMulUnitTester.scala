package boom.unittest.exu

import org.scalatest._

import chisel3._
import chisel3.tester._

import boom._

import firrtl_interpreter._
import firrtl.{ExecutionOptionsManager, HasFirrtlOptions}

class IMulUnitTester2 extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2"

  val manager = new ExecutionOptionsManager("vcd") with HasChiselExecutionOptions with HasFirrtlOptions with HasInterpreterSuite {
    interpreterOptions = interpreterOptions.copy(writeVCD = true)
  }

  it should "test static circuits" in {
    test(new IMul(imul_stages = 3),manager) { imul =>
      imul.io.elements.foreach { case (str, dat) => 
        dat match {
          case b : Bool => b.poke(false.B) 
          case a : UInt => a.poke(0.U)  
          case _ => None
        }
      }

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
