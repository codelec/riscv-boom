package boom.unittest.util

import org.scalatest._

import chisel3._
import chisel3.util._
import chisel3.tester._
import chisel3.tester.TestAdapters._

import boom.util._
import boom.unittest.common._

//import freechips.rocketchip.util._

import firrtl_interpreter._
import firrtl.{ExecutionOptionsManager, HasFirrtlOptions}

import scala.collection.mutable.BitSet

// Invoke test with:
//    $ sbt 'testOnly boom.unittest.util.ElasticRegTester'
class ElasticRegTester extends FlatSpec with ChiselScalatestTester 
   with UnittestHelperFunc {

   	val rand = new scala.util.Random
   	def u3w = rand.nextInt(8).U

   	def genericQueueTest1[S <: DecoupledIO[Data]](enq: S, deq: S, c: Clock, count: Data) = {
		val source = new ReadyValidSource(enq, c)
      	val sink = new ReadyValidSink(deq, c)
      	def next = c.step(1)

      	source.enqueueNow(3.U)
      	sink.expectInvalid()
      	next
      	sink.expectDequeueNow(3.U)
      	source.enqueueNow(4.U)
      	next
      	source.enqueueNow(1.U)
      	next
      	enq.ready.expect(false.B)
      	next
      	sink.expectDequeueNow(4.U)
      	enq.ready.expect(false.B)
      	next
      	source.enqueueNow(5.U)
      	sink.expectDequeueNow(1.U)
      	next
      	source.enqueueNow(7.U)
      	count.expect(1.U)
      	next
      	count.expect(2.U)
      	sink.expectDequeueNow(5.U)
      	enq.ready.expect(false.B)
   	}

   	it should "test elastic-reg " in {
		test(new ElasticReg2(UInt(3.W))) { er => 
			genericQueueTest1(er.io.enq, er.io.deq, er.clock, er.io.count) 
		}
	}

	it should "test shift queue " in {
		test(new freechips.rocketchip.util.ShiftQueue(UInt(3.W), 2)) { er => 
			genericQueueTest1(er.io.enq, er.io.deq, er.clock, er.io.count) 
		}
	}
}