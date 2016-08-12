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

import scala.util.Random

object GaussianDistributionTest extends App {

  // Run to validate the % for to a certain std deviation in a Random Gaussian number
  // Test on 100k as we won't run many more generations at once due to computing limits :)

  val limit  = 100000
  val stdDev = 4
  val count = (0 to limit)
    .map(_ => Random.nextGaussian())
    .count(d => d > stdDev || d < -stdDev)
  println(
      s"Found $count from $limit. This is ${ count * 100 / limit.toDouble }%"
  )

}
