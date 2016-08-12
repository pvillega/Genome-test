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

object MutationRateTest extends App {

  val p1 = Genome()
  val p2 = Genome()

  val iterations = 1000000
  val descendants = (0 to iterations).map { i =>
    val son = Genome.reproduce(p1, p2)
    Genome.print(son)
  }.groupBy(identity)
  val distribution = descendants.map {
    case (k, v) => s"${ v.size } children for ${ k.replaceAll("\n", "") }"
  }.mkString("\n")

  println(s"Distributions")
  println(distribution)
  println(
    s"> From $iterations iterations we get ${ descendants.keySet.size } genome grouping (${descendants.keySet.size*100/iterations.toDouble}%).\n" +
      s"If there was no mutation we would get only 2."
  )
}
