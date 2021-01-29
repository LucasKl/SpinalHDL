/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package mylib

import spinal.core._
import spinal.lib._
import spinal.core.GenerationFlags._
import spinal.core.Formal._

import scala.util.Random

//Hardware definition
class NewFormalTester extends Component {
  val io = new Bundle {
    val cond0 = in  Bool
    val cond1 = in  Bool
    val flag  = out Bool
    val state = out UInt(8 bits)
    val a = in Bool
    val b = out Bool
  }
  val counter = Reg(UInt(8 bits)) init(0)

  when(io.cond0){
    counter := counter + 1
  }

  io.state := counter
  io.flag  := (counter === 0) | io.cond1
  io.b := io.a

  // In diesem Teil werden formale properties definiert
  GenerationFlags.formal{
    // im initstate sind past etc. undefiniert
    when(initstate()) {
      assume(clockDomain.isResetActive && clockDomain.readClockWire)
      assume(counter === 0)
    }.otherwise {
      assert((io.a |#2| io.b) |*4|)

      assert(io.a |->1|)
      assert(io.a |->(1,2)|)
      assert(io.a |->(1,$)|)
      assert(io.a |->(1,2)| io.b)

      assert(io.a |*1|)
      assert(io.a |*(1,2)|)
      assert(io.a |*(1,$)|)
      assert(io.a |*(1,2)| io.b)

      assert(io.a |=1|)
      assert(io.a |=(1,2)|)
      assert(io.a |=(1,$)|)
      assert((io.a |=(1,2)) | io.b)

      assert((io.a ==> io.flag) |#4| io.b)
    }
  }
}

//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be resued everywhere
object MySpinalConfig extends SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))

//Generate the MyTopLevel's Verilog using the above custom configuration.
object FormalTop {
  def main(args: Array[String]) {
    MySpinalConfig.includeFormal.generateSystemVerilog(new MyTopLevel)
  }
}