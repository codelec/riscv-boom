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

trait RenameMapTableHelper {

   def initRtype(rtype: UInt)(implicit ren: RenameMapTable) = {
     (ren.io.ren_uops ++ ren.io.com_uops).map { x => x.dst_rtype.poke(rtype) }
     ren.io.ren_uops.map { x => x.lrs1_rtype.poke(rtype) }
     ren.io.ren_uops.map { x => x.lrs2_rtype.poke(rtype) } 
   } 

   def checkValues(idx: Int, prs1: Int, prs2: Int, prs3: Int, stale_pdst: Int)(implicit ren: RenameMapTable) = {
      ren.io.values(idx).prs1.expect(prs1.U)
      ren.io.values(idx).prs2.expect(prs2.U)
      ren.io.values(idx).prs3.expect(prs3.U)
      ren.io.values(idx).stale_pdst.expect(stale_pdst.U)
   }
   def kill(implicit ren: RenameMapTable) = ren.io.kill.poke(true.B)
   def killReset(implicit ren: RenameMapTable) = ren.io.kill.poke(false.B)

   def renUOPValid(idx: Int)(implicit ren: RenameMapTable) = ren.io.ren_will_fire(idx).poke(true.B)
   def renUOPReset(idx: Int)(implicit ren: RenameMapTable) = ren.io.ren_will_fire(idx).poke(false.B)

   def renUOPBr(idx: Int, tag: Int)(implicit ren: RenameMapTable) = { 
      ren.io.ren_br_vals(idx).poke(true.B)
      ren.io.ren_uops(idx).br_tag.poke(tag.U)
   }
   def renUOPBrReset(idx: Int)(implicit ren: RenameMapTable) = ren.io.ren_br_vals(idx).poke(false.B)

   def renUOPLdst(idx: Int, ldst: Int, pdst: Int, lrs1: Int, lrs2: Int)(implicit ren: RenameMapTable) = { 
      ren.io.ren_uops(idx).ldst_val.poke(true.B)
      ren.io.ren_uops(idx).ldst.poke(ldst.U)
      ren.io.ren_uops(idx).pdst.poke(pdst.U)
      ren.io.ren_uops(idx).lrs1.poke(lrs1.U)
      ren.io.ren_uops(idx).lrs2.poke(lrs2.U)
   }
   def renUOPLdstReset(idx: Int)(implicit ren: RenameMapTable) = ren.io.ren_uops(idx).ldst_val.poke(false.B)   

   def renUOPRs3(idx: Int, lrs3: Int)(implicit ren: RenameMapTable) = {
      ren.io.ren_uops(idx).lrs3.poke(lrs3.U)
      ren.io.ren_uops(idx).frs3_en.poke(true.B)  
   } 
   def renUOPRs3(idx: Int)(implicit ren: RenameMapTable) = ren.io.ren_uops(idx).frs3_en.poke(false.B)

}

//    $ sbt 'testOnly boom.unittest.exu.RenameMapTableHelper'
class RenameMapTableTester extends FlatSpec with ChiselScalatestTester 
   with RenameMapTableHelper with UnittestHelperFunc {
   behavior of "exu/rename-maptable"

   val rand = new scala.util.Random

   val pregs = BitSet()
   val maps = Map[Int, ArrayBuffer[Int]]()

   val optionsManager = new ExecutionOptionsManager("chisel3") with HasChiselExecutionOptions with HasFirrtlOptions with HasInterpreterSuite {
      chiselOptions = chiselOptions.copy(scalaSimulator = TreadleSimulator)
   }

   var p = new TesterConfig ++ new withMaxBrCount(4)
   it should "test RenameMapTable module handling single decode" in {
      test(new RenameMapTable(2,RT_FIX.litValue,32,48)(p),optionsManager) { ren =>
         implicit val d = ren
         for (i <- 1 until 48) pregs += i
         initAll(ren.io.elements)
         initRtype(RT_FIX)

      }
   }

}
