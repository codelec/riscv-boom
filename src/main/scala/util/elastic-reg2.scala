package boom.util

import Chisel._


/** Implements the same interface as chisel3.util.Queue.
  * Effectively a two-entry Queue.
  *  */
class ElasticReg2[T <: Data](gen: T) extends Module {
   val entries = 2
   val io = IO(new QueueIO(gen, entries) {})

   private val shaValid = RegInit(false.B)
   private val outValid = RegInit(false.B)
   private val shadow = Reg(gen)
   private val output = Reg(gen)

   // React only if enqueue or dequeue

   when (io.enq.fire() && outValid) {
      shadow := io.enq.bits
      // output flop not getting empty this cycle
      // shadow available for rescue
      shaValid := !io.deq.ready 
   }

   when (io.enq.fire() && (!outValid || io.deq.ready)) {
      output := io.enq.bits
      outValid := true.B
   }

   when (io.deq.fire()) {
      outValid := io.enq.valid || shaValid // nothing incoming and shadow empty
      when (shaValid) {
         output := shadow
         shaValid := false.B
      }
   }

   io.enq.ready := !shaValid
   io.deq.valid := outValid
   io.deq.bits := output

   io.count := Cat(shaValid & outValid, shaValid ^ outValid)
}

object ElasticReg2
{
   def apply[T <: Data](enq: DecoupledIO[T]): DecoupledIO[T] = {
      val q = Module(new ElasticReg2(enq.bits.cloneType))
      q.io.enq <> enq
      q.io.deq
   }
}
