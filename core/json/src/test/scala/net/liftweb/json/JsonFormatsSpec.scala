/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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

package net.liftweb
package json

import org.specs2.mutable._


/**
 * System under specification for JSON Formats.
 */
object JsonFormatsSpec extends Specification with TypeHintExamples {
  "JsonFormats Specification".title
  
  implicit val formats = ShortTypeHintExamples.formats + FullTypeHintExamples.formats.typeHints

  val hintsForFish   = ShortTypeHintExamples.formats.typeHints.hintFor(classOf[Fish])
  val hintsForDog    = ShortTypeHintExamples.formats.typeHints.hintFor(classOf[Dog])
  val hintsForAnimal = FullTypeHintExamples.formats.typeHints.hintFor(classOf[Animal])

  "hintsFor across composite formats" in {
    formats.typeHints.hintFor(classOf[Fish])   must_== (hintsForFish)
    formats.typeHints.hintFor(classOf[Dog])    must_== (hintsForDog)
    formats.typeHints.hintFor(classOf[Animal]) must_== (hintsForAnimal)
  }

  "classFor across composite formats" in {
    formats.typeHints.classFor(hintsForFish)   must_== (ShortTypeHintExamples.formats.typeHints.classFor(hintsForFish))
    formats.typeHints.classFor(hintsForDog)    must_== (ShortTypeHintExamples.formats.typeHints.classFor(hintsForDog))
    formats.typeHints.classFor(hintsForAnimal) must_== (FullTypeHintExamples.formats.typeHints.classFor(hintsForAnimal))
  }
}
