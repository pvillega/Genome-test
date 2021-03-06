/*
 * Copyright 2016 Pere Villega
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.perevillega

import com.perevillega.genome.Genome

object GenomeGenerationTest extends App {
  import Helper._

  val iterations = 300000

  timeRun {
    println("Single Genome")
    val descendants = (0 until iterations).map(_ => Genome())
    val map = descendants.groupBy(Genome.print)
    println(s"> From $iterations iterations we get ${descendants.size} genomes and ${map.keySet.size} groupings")
  }

  timeRun {
    println("Complex Genome")
    val descendants = (0 until iterations).map(_ => Genome(3, 2))
    val map = descendants.groupBy(Genome.print)
    println(s"> From $iterations iterations we get ${descendants.size} genomes and ${map.keySet.size} groupings")
  }
}
