package boom.unittest.exu

import org.scalatest._

import chisel3._
import chisel3.tester._

import boom._
import boom.system._
import boom.unittest.common._

import freechips.rocketchip.config._

import firrtl_interpreter._
import firrtl.{ExecutionOptionsManager, HasFirrtlOptions}

import scala.collection.mutable.BitSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListMap

trait RenameMapTableHelper {

   def initRtype(rtype: UInt)(implicit ren: RenameMapTable) = {
     (ren.io.ren_uops ++ ren.io.com_uops).map { x => x.dst_rtype.poke(rtype) }
     ren.io.ren_uops.map { x => x.lrs1_rtype.poke(rtype) }
     ren.io.ren_uops.map { x => x.lrs2_rtype.poke(rtype) } 
   } 

   def checkValues1(idx: Int, prs1: Int)(implicit ren: RenameMapTable) = ren.io.values(idx).prs1.expect(prs1.U)
   def checkValues2(idx: Int, prs2: Int)(implicit ren: RenameMapTable) = ren.io.values(idx).prs2.expect(prs2.U)
   def checkValues3(idx: Int, prs3: Int)(implicit ren: RenameMapTable) = ren.io.values(idx).prs3.expect(prs3.U)
   def checkValuesP(idx: Int, stale_pdst: Int)(implicit ren: RenameMapTable) = ren.io.values(idx).stale_pdst.expect(stale_pdst.U)

   def kill(implicit ren: RenameMapTable) = ren.io.kill.poke(true.B)
   def killReset(implicit ren: RenameMapTable) = ren.io.kill.poke(false.B)

   def renUOPValid(idx: Int)(implicit ren: RenameMapTable) = ren.io.ren_will_fire(idx).poke(true.B)
   def renUOPReset(idx: Int)(implicit ren: RenameMapTable) = ren.io.ren_will_fire(idx).poke(false.B)

   def renUOPBr(idx: Int, tag: Int)(implicit ren: RenameMapTable) = { 
      ren.io.ren_br_vals(idx).poke(true.B)
      ren.io.ren_uops(idx).br_tag.poke(tag.U)
   }
   def renUOPBrReset(idx: Int)(implicit ren: RenameMapTable) = ren.io.ren_br_vals(idx).poke(false.B)

   def renUOPLdst(idx: Int, ldst: Int, pdst: Int)(implicit ren: RenameMapTable) = { 
      ren.io.ren_uops(idx).ldst_val.poke(true.B)
      ren.io.ren_uops(idx).ldst.poke(ldst.U)
      ren.io.ren_uops(idx).pdst.poke(pdst.U)
   }
   def renUOPLdstReset(idx: Int)(implicit ren: RenameMapTable) = ren.io.ren_uops(idx).ldst_val.poke(false.B)   

   def renUOPLrs(idx: Int, lrs1: Int, lrs2: Int)(implicit ren: RenameMapTable) = { 
      ren.io.ren_uops(idx).lrs1.poke(lrs1.U)
      ren.io.ren_uops(idx).lrs2.poke(lrs2.U)
   }

   def renUOPRs3(idx: Int, lrs3: Int)(implicit ren: RenameMapTable) = {
      ren.io.ren_uops(idx).lrs3.poke(lrs3.U)
      ren.io.ren_uops(idx).frs3_en.poke(true.B)  
   } 
   def renUOPRs3Reset(idx: Int)(implicit ren: RenameMapTable) = ren.io.ren_uops(idx).frs3_en.poke(false.B)

   def comRbk(idx: Int, ldst: Int, stale_pdst: Int)(implicit ren: RenameMapTable) = {
      ren.io.com_uops(idx).ldst.poke(ldst.U)
      ren.io.com_uops(idx).stale_pdst.poke(stale_pdst.U)
      ren.io.com_rbk_valids(idx).poke(true.B)  
   } 
   def comRbkReset(idx: Int)(implicit ren: RenameMapTable) = ren.io.com_rbk_valids(idx).poke(false.B)   

   def renBrMispredict(tag: Int)(implicit ren: RenameMapTable) = {
      ren.io.brinfo.mispredict.poke(true.B)
      ren.io.brinfo.tag.poke(tag.U)
   }
   def renBrMispredictReset(implicit ren: RenameMapTable) = ren.io.brinfo.mispredict.poke(false.B)
}

//    $ sbt 'testOnly boom.unittest.exu.RenameMapTableTester'
class RenameMapTableTester extends FlatSpec with ChiselScalatestTester 
   with RenameMapTableHelper with UnittestHelperFunc {
   behavior of "exu/rename-maptable"

   val rand = new scala.util.Random
   var lr1: Int = 0
   var lr2: Int = 0
   var pr1: Int = 0
   def rand32 = {lr1 = rand.nextInt(31) + 1 ; lr1}
   def rand48 = {pr1 = rand.nextInt(47) + 1 ; pr1}
   val pregs = BitSet()
   val brmaps = ListMap[Int, ArrayBuffer[Int]]()
   val maps = ListMap[Int, Int]()
   for (i <- 1 until 48) pregs += i
   for (i <- 1 until 32) maps += i -> -1
   for (i <- 1 until 32) brmaps += i -> ArrayBuffer.fill(4)(-1)

   val optionsManager = new ExecutionOptionsManager("chisel3") with HasChiselExecutionOptions with HasFirrtlOptions with HasInterpreterSuite {
      chiselOptions = chiselOptions.copy(scalaSimulator = TreadleSimulator)
   }

   var p = new TesterConfig ++ new withMaxBrCount(4)
   it should " test RenameMapTable module handling dual decode " in {
      test(new RenameMapTable(2,RT_FIX.litValue,32,48)(p),optionsManager) { ren =>
      implicit val d = ren
      initAll(ren.io.elements)
      initRtype(RT_FIX)
      ren.io.debug_inst_can_proceed.map { x => x.poke(true.B) }
      ren.io.debug_freelist_can_allocate.map { x => x.poke(true.B) }

      // consume few pregs 
      // therefore create few mappings
      renUOPValid(0)
      renUOPValid(1)
      for (i <- 1 until 21 by 2) {
         renUOPLdst(0, rand32, i)
         pregs.remove(i)
         maps(lr1) = i
         renUOPLdst(1, rand32, i + 1)
         pregs.remove(i + 1)
         maps(lr1) = i + 1
         step
      }
      // both pipes trying to map 
      // to same lreg
      renUOPLdst(0, rand32, 21)
      pregs.remove(22) 
      maps(lr1) = 21
      renUOPLdst(1, lr1, 22)
      pregs.remove(22)
      maps(lr1) = 22
      step
      renUOPReset(0)
      renUOPReset(1)
      renUOPLdstReset(0)
      renUOPLdstReset(1)

      // only check mappings
      step 
      renUOPValid(0)
      renUOPValid(1)
      for (i <- 0 until 8) {
         while(maps(rand32) == -1) {}
         lr2 = lr1
         while(maps(rand32) == -1) {}
         renUOPLrs(0, lr2, lr1)
         checkValues1(0, maps(lr2))
         checkValues2(0, maps(lr1))
         while(maps(rand32) == -1) {}
         lr2 = lr1
         while(maps(rand32) == -1) {}
         renUOPLrs(1, lr2, lr1)
         checkValues1(1, maps(lr2))
         checkValues2(1, maps(lr1))
         step 
      }
      renUOPReset(0)
      renUOPReset(1)

      // both pipes create and check 
      // for existing mappings
      step 
      renUOPValid(0)
      renUOPValid(1)
      for (i <- 0 until 8) {
         while(maps(rand32) == -1) {}
         lr2 = lr1
         while(maps(rand32) == -1) {}
         renUOPLrs(0, lr2, lr1)
         checkValues1(0, maps(lr2))
         checkValues2(0, maps(lr1))
         pr1 = pregs.head
         renUOPLdst(0, rand32, pr1)
         pregs.remove(pr1)
         maps(lr1) = pr1

         while(maps(rand32) == -1) {}
         lr2 = lr1
         while(maps(rand32) == -1) {}
         renUOPLrs(1, lr2, lr1)
         checkValues1(1, maps(lr2))
         checkValues2(1, maps(lr1))
         pr1 = pregs.head
         renUOPLdst(1, rand32, pr1)
         pregs.remove(pr1)
         maps(lr1) = pr1
         step 
      }
      renUOPLdstReset(0)
      renUOPLdstReset(1)
      renUOPReset(0)
      renUOPReset(1)

      // release few pregs
      step
      for (i <- 0 until 10) {
         while(pregs(rand48)) {}
         pregs += pr1
         while(pregs(rand48)) {}
         pregs += pr1
         step
      }
      comRbkReset(0)
      comRbkReset(1)
      
      // check forwarding
      // by allocating in one pipe
      // checking both the commbinations of reg
      // in the other one
      step 
      renUOPValid(0)
      renUOPValid(1)
      for (i <- 0 until 8) {
         while(maps(rand32) == -1) {}
         pr1 = pregs.head
         pregs.remove(pr1)
         renUOPLdst(0, lr1, pr1)
         maps(lr1) = pr1

         lr2 = lr1
         while(maps(rand32) == -1) {}
         if (i % 2 == 0) {  
            renUOPLrs(1, lr2, lr1) 
            checkValues1(1, maps(lr2))
            checkValues2(1, maps(lr1))
         } else {
            renUOPLrs(1, lr1, lr2)
            checkValues1(1, maps(lr1))
            checkValues2(1, maps(lr2))
         }
         step          
      }
      renUOPLdstReset(0)
      renUOPReset(0)
      renUOPReset(1)

      // normal op in first pipe
      // branch valid in the second one
      step
      renUOPValid(0)
      renUOPValid(1)
      while(maps(rand32) == -1) {}
      lr2 = lr1
      while(maps(rand32) == -1) {}
      renUOPLrs(0, lr2, lr1)
      checkValues1(0, maps(lr2))
      checkValues2(0, maps(lr1))
      pr1 = pregs.head
      renUOPLdst(0, rand32, pr1)
      pregs.remove(pr1)
      maps(lr1) = pr1

      renUOPBr(1, 3)
      pr1 = pregs.head
      renUOPLdst(1, rand32, pr1)
      pregs.remove(pr1)
      maps(lr1) = pr1
      maps.map { case (i, valu) => brmaps(i)(3) = valu }

      // branch valid in the first one
      // normal op in second pipe
      step
      renUOPBrReset(1)
      pr1 = pregs.head
      renUOPLdst(0, rand32, pr1)
      pregs.remove(pr1)
      maps(lr1) = pr1
      maps.map { case (i, valu) => brmaps(i)(2) = valu }
      renUOPBr(0, 2)

      while(maps(rand32) == -1) {}
      lr2 = lr1
      while(maps(rand32) == -1) {}
      renUOPLrs(1, lr2, lr1)
      checkValues1(1, maps(lr2))
      checkValues2(1, maps(lr1))
      pr1 = pregs.head
      renUOPLdst(1, rand32, pr1)
      pregs.remove(pr1)
      maps(lr1) = pr1

      step
      renUOPBrReset(0)
      for (i <- 0 until 5) {
         while(maps(rand32) == -1) {}
         lr2 = lr1
         while(maps(rand32) == -1) {}
         renUOPLrs(0, lr2, lr1)
         checkValues1(0, maps(lr2))
         checkValues2(0, maps(lr1))
         pr1 = pregs.head
         renUOPLdst(0, rand32, pr1)
         pregs.remove(pr1)
         maps(lr1) = pr1

         while(maps(rand32) == -1) {}
         lr2 = lr1
         while(maps(rand32) == -1) {}
         renUOPLrs(1, lr2, lr1)
         checkValues1(1, maps(lr2))
         checkValues2(1, maps(lr1))
         pr1 = pregs.head
         renUOPLdst(1, rand32, pr1)
         pregs.remove(pr1)
         maps(lr1) = pr1
         step 
      }
      renBrMispredict(2)
      renUOPLdstReset(0)
      renUOPLdstReset(1)
      step 
      renBrMispredictReset
      brmaps.map { case (i, arry) => maps(i) = arry(2) }
      for (i <- 0 until 45) {
         while(maps(rand32) == -1) {}
         lr2 = lr1
         while(maps(rand32) == -1) {}
         renUOPLrs(0, lr2, lr1)
         checkValues1(0, maps(lr2))
         checkValues2(0, maps(lr1))
         while(maps(rand32) == -1) {}
         lr2 = lr1
         while(maps(rand32) == -1) {}
         renUOPLrs(1, lr2, lr1)
         checkValues1(1, maps(lr2))
         checkValues2(1, maps(lr1))
         step 
      }
      renUOPReset(0)
      renUOPReset(1)
      }
   }

}
