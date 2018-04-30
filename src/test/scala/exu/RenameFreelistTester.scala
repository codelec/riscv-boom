package boom.unittest.exu

import org.scalatest._

import chisel3._
import chisel3.tester._

import boom.exu._
import boom.system._
import boom.unittest.common._

import freechips.rocketchip.config._

import firrtl_interpreter._
import firrtl.{ExecutionOptionsManager, HasFirrtlOptions}

import scala.collection.mutable.BitSet

trait RenameFreeListHelperFunc {

   def checkFree(idx: Int)(implicit ren: RenameFreeListHelper) = ren.io.can_allocate(idx).expect(true.B)
   def checkFull(idx: Int)(implicit ren: RenameFreeListHelper) = ren.io.can_allocate(idx).expect(false.B) 
   def allocatedReg(idx: Int)(implicit ren: RenameFreeListHelper) = ren.io.req_pregs(idx).peek().litValue.toInt
   def checkAllocatedReg(idx: Int, preg: Int)(implicit ren: RenameFreeListHelper) = ren.io.req_pregs(idx).expect(preg.U)
   def freelist(implicit ren: RenameFreeListHelper) = ren.io.debug.freelist.peek()
   def freeReg(idx: Int, preg: Int)(implicit ren: RenameFreeListHelper) = {   
      ren.io.enq_vals(idx).poke(true.B)
      ren.io.enq_pregs(idx).poke(preg.U)
   }
   def freeReqReset(idx: Int)(implicit ren: RenameFreeListHelper) = ren.io.enq_vals(idx).poke(false.B)
   def reqReg(idx: Int)(implicit ren: RenameFreeListHelper) = ren.io.req_preg_vals(idx).poke(true.B)
   def reqRegReset(idx: Int)(implicit ren: RenameFreeListHelper) = ren.io.req_preg_vals(idx).poke(false.B)

   def branchReq(idx: Int, br_tag: Int)(implicit ren: RenameFreeListHelper) = {
      ren.io.ren_br_vals(idx).poke(true.B)
      ren.io.ren_br_tags(idx).poke(br_tag.U)        
   }
   def branchReset(idx: Int)(implicit ren: RenameFreeListHelper) = ren.io.ren_br_vals(idx).poke(false.B)

   def branchMispredict(br_tag: Int)(implicit ren: RenameFreeListHelper) = {
      ren.io.br_mispredict_val.poke(true.B)
      ren.io.br_mispredict_tag.poke(br_tag.U)        
   }
   def branchMispredictReset(implicit ren: RenameFreeListHelper) = ren.io.br_mispredict_val.poke(false.B)

   def rollbackWens(idx: Int, preg: Int)(implicit ren: RenameFreeListHelper) = {
      ren.io.rollback_wens(idx).poke(true.B)
      ren.io.rollback_pdsts(idx).poke(preg.U)        
   }
   def rollbackWensReset(idx: Int)(implicit ren: RenameFreeListHelper) = ren.io.rollback_wens(idx).poke(false.B)

   def flushPipeline(implicit ren: RenameFreeListHelper) = ren.io.flush_pipeline.poke(true.B)
   def flushPipelineReset(implicit ren: RenameFreeListHelper) = ren.io.flush_pipeline.poke(false.B)
}

class withMaxBrCount(n: Int) extends Config((site, here, up) => {
   case BoomTilesKey => up(BoomTilesKey, site) map { r => r.copy(
      core = r.core.copy( maxBrCount = n ) )}
})

class withEnableCommitTable extends Config((site, here, up) => {
   case BoomTilesKey => up(BoomTilesKey, site) map { r => r.copy(
      core = r.core.copy( enableCommitMapTable = true ) )}
})

// Invoke test with:
//    $ sbt 'testOnly boom.unittest.exu.RenameFreeListTester2'
class RenameFreeListTester extends FlatSpec with ChiselScalatestTester 
   with RenameFreeListHelperFunc with UnittestHelperFunc {
   behavior of "exu/rename-freelist"
   val freeregs = BitSet()
   val rand = new scala.util.Random
   // allocations after a branch is detected
   val allocBr = Seq.fill(4)(BitSet())
   var regAllocated : Int = 0 

   val optionsManager = new ExecutionOptionsManager("chisel3") with HasChiselExecutionOptions with HasFirrtlOptions with HasInterpreterSuite {
      chiselOptions = chiselOptions.copy(scalaSimulator = TreadleSimulator)
   }

   var p = new TesterConfig ++ new withMaxBrCount(4)
   it should "test renamefreelisthelper module handling single decode" in {
      test(new RenameFreeListHelper(32,1)(p),optionsManager) { ren =>
   println(ren.io.rollback_pdsts(0))
         implicit val d = ren
         for (i <- 1 until 32) freeregs += i
         initAll(ren.io.elements)


         /// EMPTY & REFILL
         for (i <- 1 until 32) {
            step
            reqReg(0)
            checkFree(0)
            freeregs -= allocatedReg(0)
         }
         step 
         reqRegReset(0)
         checkFull(0)
         for (i <- 1 until 32) {
            step
            freeReg(0, i)
            freeregs += i
         }
         step
         freeReqReset(0)
         checkFree(0)


         for (i <- 1 until 18) {
            step
            reqReg(0)
            checkFree(0)
            freeregs -= allocatedReg(0)
         }
         step
         reqRegReset(0)
         step
         
         // free and reallocate under a branch misprediction
         branchReq(0, 3)
         step
         branchReset(0)
         freeReg(0, 4)
         freeregs += 4
         step
         freeReqReset(0)
         reqReg(0)
         freeregs -= allocatedReg(0)
         allocBr(3) += allocatedReg(0)
         step
         reqRegReset(0)
         branchMispredict(3)
         freeregs ++= allocBr(3)
         allocBr(3).clear
         step
         branchMispredictReset

         step
         branchReq(0, 3)
         reqReg(0)
         freeregs -= allocatedReg(0)
         step 
         branchReset(0)
         reqReg(0)
         freeregs -= allocatedReg(0)
         allocBr(3) += allocatedReg(0)
         step
         reqRegReset(0)
         step
         branchReq(0, 2)
         reqReg(0)
         regAllocated = allocatedReg(0)
         freeregs -= regAllocated
         allocBr(3) += allocatedReg(0)
         step 
         branchReset(0)
         reqReg(0)
         regAllocated = allocatedReg(0)
         freeregs -= regAllocated
         Seq(3,2).map { i => allocBr(i) += regAllocated }
         step
         reqRegReset(0)
         step
         branchReq(0, 1)
         reqReg(0)
         regAllocated = allocatedReg(0)
         freeregs -= regAllocated
         Seq(3,2).map { i => allocBr(i) += regAllocated }
         step 
         branchReset(0)
         reqReg(0)
         regAllocated = allocatedReg(0)
         freeregs -= regAllocated
         Seq(3,2,1).map { i => allocBr(i) += regAllocated }
         step
         reqRegReset(0)
         step
         branchReq(0, 0)
         reqReg(0)
         regAllocated = allocatedReg(0)
         freeregs -= regAllocated
         Seq(3,2,1).map { i => allocBr(i) += regAllocated }
         step 
         branchReset(0)
         reqReg(0)
         regAllocated = allocatedReg(0)
         freeregs -= regAllocated
         Seq(3,2,1,0).map { i => allocBr(i) += regAllocated }
         step
         branchMispredictReset
         reqRegReset(0)
         branchReset(0)
         step 

         branchMispredict(1)
         freeReg(0, 2)
         freeregs += 2
         step
         freeReqReset(0)
         branchMispredictReset
         freeregs ++= allocBr(1)
         Seq(3,2).map { i => allocBr(i) --= allocBr(1) }
         Seq(1,0).map { i => allocBr(i).clear }
         step
         branchMispredict(2)
         step
         branchMispredictReset
         freeregs ++= allocBr(2)
         allocBr(3) --= allocBr(2)
         allocBr(2).clear 
         if (freeregs.toBitMask(0) != freelist.litValue) testerFail("freelist and freeregs differ")
      }
   }
   freeregs.clear
   allocBr.map { x => x.clear }

   it should "test basic functionality in dual decode " in {
   test(new RenameFreeListHelper(48,2)(p)) { ren =>
   implicit val d = ren
   for (i <- 1 until 48) freeregs += i
   // EMPTY & REFILL 
   /* Get to a state where one of the decode pipelines 
   * can be satisfied with a register request and the other can't
   */
   step 
   reqReg(0)
   reqReg(1)
   for (i <- 0 until 22) {
      freeregs -= (allocatedReg(0), allocatedReg(1))
      checkFree(0)
      checkFree(1)
      step
   }
   reqRegReset(1)
   checkFree(0)
   freeregs -= allocatedReg(0)
   step
   checkFree(0)
   freeregs -= allocatedReg(0)
   step
   checkFree(0)
   checkFull(1)
   freeregs -= allocatedReg(0)
   step 
   reqRegReset(0)


   for (i <- 0 until 16) {
      val a = rand.nextInt(47) + 1
      if (!freeregs(a)) {
      freeReg(0, a)
      freeregs += a
      step }
   }
   freeReqReset(0)
   if (freeregs.toBitMask(0) != freelist.litValue) testerFail("freelist and freeregs differ")
   step

   }}
   freeregs.clear
   allocBr.map { x => x.clear }


   it should "test dual decode with random allocations/deallocations done in parallel in separate threads" in {
   test(new RenameFreeListHelper(48,2)(p)) { ren =>
   implicit val d = ren
   for (i <- 1 until 48) freeregs += i
   fork {
      ren.io.req_preg_vals(0).weakPoke(true.B)
      ren.io.req_preg_vals(1).weakPoke(true.B)
      for (i <- 0 until 10) {
      freeregs -= (allocatedReg(0), allocatedReg(1))
      step
      }
      ren.io.req_preg_vals(0).weakPoke(false.B)
      ren.io.req_preg_vals(1).weakPoke(false.B)
      step
   } .fork {
      ren.clock.step(14)
      var a : Int = 0
      var b : Int = 0
      for (i <- 0 until 16) {
         a = rand.nextInt(47) + 1
         if (!freeregs(a)) {
            freeReg(0, a)
            freeregs += a } else freeReqReset(0)
         b = rand.nextInt(47) + 1
         if (!freeregs(b)) {
            freeReg(1, b)
            freeregs += b } else freeReqReset(1)
         step
      }
      freeReqReset(0)
      freeReqReset(1)
   } .fork {
      ren.clock.step(14)
      reqReg(0)
      reqReg(1)
      for (i <- 0 until 16) {
         freeregs -= (allocatedReg(0),allocatedReg(1))
         step
      }
      reqRegReset(0)
      reqRegReset(1)
   } .join
   step
   if (freeregs.toBitMask(0) != freelist.litValue) testerFail("freelist and freeregs differ")

   }}
}
