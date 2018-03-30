package boom.unittest.exu

import org.scalatest._

import chisel3._
import chisel3.tester._
import chisel3.internal.sourceinfo._

import boom._
import boom.system._
import boom.unittest.common._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._

import firrtl_interpreter._
import firrtl.{ExecutionOptionsManager, HasFirrtlOptions}

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

trait RenameHelperFunc {

   def checkFree(idx: Int)(implicit ren: RenameFreeListHelper) = ren.io.can_allocate(idx).expect(true.B)
   def checkFull(idx: Int)(implicit ren: RenameFreeListHelper) = ren.io.can_allocate(idx).expect(false.B) 
   def uopGen(idx: Int, stale_pdst: Int, ldst_val: Int, pdst: Int)(implicit ren: RenameFreeListHelper) = { 
      ren.io.com_uops(idx).dst_rtype.poke(RT_FIX)
      ren.io.com_uops(idx).valid.poke(true.B)
   }
   def exposeI(str: String, ele: Data) = println("" + str + " " + ele.peek().litValue)
   def exposeB(str: String, ele: Data) = println("" + str + " " + ele.peek().litValue.toString(2)) 
   def showAllocatedReg(idx: Int)(implicit ren: RenameFreeListHelper) = exposeI("req_pregs("+idx+")",ren.io.req_pregs(idx))
   def allocatedReg(idx: Int)(implicit ren: RenameFreeListHelper) = ren.io.req_pregs(idx).peek().litValue.toInt
   def checkAllocatedReg(idx: Int, preg: Int)(implicit ren: RenameFreeListHelper) = ren.io.req_pregs(idx).expect(preg.U)
   def showFreeList(implicit ren: RenameFreeListHelper) = exposeB("FreeList",ren.io.debug.freelist)
   def step(implicit ren: RenameFreeListHelper) = {   
      ren.clock.step(1) 
      println("step")
   }
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
class RenameFreeListTester2 extends FlatSpec with ChiselScalatestTester 
   with RenameHelperFunc with UnittestHelperFunc {
   behavior of "Testers2"

   var p = new TesterConfig ++ new withMaxBrCount(4)

   it should "test rename circuits" in {
      test(new RenameFreeListHelper(32,1)(p)) { ren =>
         implicit val d = ren
         val freeregs = ListBuffer[Int]()
         for (i <- 1 until 32) freeregs += i
         initAll(ren.io.elements)
         for (i <- 0 until 18) {
            step
            reqReg(0)
            checkFree(0)
            freeregs -= allocatedReg(0)
            showAllocatedReg(0)
            showFreeList
         }
         
         branchReq(0, 3)
         step
         branchReset(0)
         freeReg(0, 4)
         freeregs += 4
         step
         freeReqReset(0)
         showFreeList
         showAllocatedReg(0)
         reqReg(0)
         freeregs -= allocatedReg(0)
         step
         reqRegReset(0)
         showFreeList
         step
         branchMispredict(3)
         step
         branchMispredictReset
         showFreeList

      }
   }
}
