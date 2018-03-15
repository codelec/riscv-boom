package boom.unittest.common

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester, TesterOptionsManager, SteppedHWIOTester}
import chisel3.internal.sourceinfo._
import org.scalatest.{Matchers, FlatSpec}
import boom._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._

// Bare Minimum
class TesterConfig extends Config((site, here, up) => {
   case XLen => 64
   case SystemBusKey => SystemBusParams(beatBytes = site(XLen)/8, blockBytes = site(CacheBlockBytes))
   case TileKey => BoomTileParams(
      core   = BoomCoreParams(
         fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4))))
   case SharedMemoryTLEdge => new TLEdgeOut(TLClientPortParameters(clients = Seq(TLClientParameters(name = s"necessaryPain"))),TLManagerPortParameters(
      Seq(TLManagerParameters(
        address         = Seq(AddressSet(0x00dead00, 0x100 - 1)),
        supportsGet     = TransferSizes(1, 8))), // requests handled in FIFO order
      beatBytes = 8),new Config((site, here, up) => {case ExtIn => Nil}),UnlocatableSourceInfo)
})