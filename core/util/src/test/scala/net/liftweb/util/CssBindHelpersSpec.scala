/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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
package util
	
import xml._

import org.specs2.mutable._

import common._
import BindHelpers._

object CssBindHelpersSpec extends Specification  {

  "css bind helpers" should {
    "clear clearable" in {
      ClearClearable(<b><span class="clearable"/></b>) must ==/ (<b/>)
    }

    "substitute a String by id" in {
      ("#foo" #> "hello")(<b><span id="foo"/></b>) must ==/ (<b>hello</b>)
    }


    "not duplicate classes" in {

      def anchor(quesType: String, value: String) = {
        <a href="foo" class="selected">(value)</a>
      }
      var page = 1
      var elements = List("1","2","3","4")
      
      val xml = <div class="lift:Bug.attack bug">
      <div id="question" class="question">
      <a href="#" class="L">1</a>
      <a href="#" class="U">1</a>
      <a href="#" class="D">1</a>
      </div>
      <div class="navigation">
      <button class="previous">Previous</button> <button class="next">Next</button>
      </div>
      </div>
      
      val sel = ".question" #> elements.map(value => {
        ".question [id]" #> ("question-" + value) &
        ".question [class]" #> ("question-" + value) &
        ".L" #> anchor("L", value) &
        ".U" #> anchor("U", value) &
        ".D" #> anchor("D", value)
      })
      
      val res = sel(xml)

      ((res \\ "a").head \ "@class").head.text must_== "selected L"      
    }


    "Compound selector" in {
      val res = 
      (".foo [href]" #> "http://dog.com" & ".bar [id]" #> "moo").apply(
        <a class="foo bar" href="#"/>)
      (res \ "@href").text must_== "http://dog.com"
      (res \ "@id").text must_== "moo"
    }

    "not stack overflow on Elem" in {
      val xf = "* [id]" #> "xx" &
      "* [style]" #> "border:thin solid black" &
      "* *" #> <a/>
	  success
    }

    "not stack overflow on Elem" in {
      val xf = "* [id]" #> "xx" &
      "* [style]" #> "border:thin solid black" &
      "* *+" #> <a/>

      xf(<div/>)
	  success
    }

    "not stack overflow on Elem" in {
      val xf = "* [id]" #> "xx" &
      "* [style]" #> "border:thin solid black" &
      "* -*" #> <a/>

      xf(<div/>)
	  success
    }

    "substitute a String by id" in {
      ("#foo" replaceWith "hello")(<b><span id="foo"/></b>) must ==/ (<b>hello</b>)
    }

    "Select a node" in {
      ("#foo ^^" #> "hello")(<div><span id="foo"/></div>) must ==/ (<span id="foo"/>)
    }

    "Another nested select" in {
      val template = <span>
      <div id="meow">
      <lift:loc locid="asset.import.chooseFile"></lift:loc>
      <span id="file_upload"></span>
      <input type="submit" value="import" /><br></br>
      </div>
      <div id="get">
      <lift:loc locid="asset.import.chooseFile"></lift:loc>
      <span id="file_upload"></span>
      <input type="submit" value="import" /><br></br>
      </div>
      </span>

      val xf = "#get ^^" #> "ignore" & "#file_upload" #> <input type="moose"/>

      val ret = xf(template)

      ret(0).asInstanceOf[Elem].label must_== "div"
      ret.length must_== 1
      (ret \ "@id").text must_== "get"

      (ret \\ "input").length must_== 2

      ((ret \\ "input").toList(0) \ "@type").map(_.text) must_== List("moose")
      
    }

    "Child nested select" in {
      val template = <span>
      <div id="meow">
      <lift:loc locid="asset.import.chooseFile"></lift:loc>
      <span id="file_upload"></span>
      <input type="submit" value="import" /><br></br>
      </div>
      <div id="get">
      <lift:loc locid="asset.import.chooseFile"></lift:loc>
      <span id="file_upload"></span>
      <input type="submit" value="import" /><br></br>
      </div>
      </span>

      val xf = "#get ^*" #> "ignore" & "#file_upload" #> <input type="moose"/>

      val ret = xf(template)

      (ret \\ "div").length must_== 0

      (ret \\ "input").length must_== 2

      ((ret \\ "input").toList(0) \ "@type").map(_.text) must_== List("moose")
      
    }

    "Select a node and transform stuff" in {
      val ret = ("#foo ^^" #> "hello" &
                 "span [id]" #> "bar")(<span id="foo"/>)

      ret(0).asInstanceOf[Elem].label must_== "span"
      ret.length must_== 1
      (ret \ "@id").text must_== "bar"
    }

    "Select a node and transform stuff deeply nested" in {
      val ret = ("#foo ^^" #> "hello" &
                 "span [id]" #> "bar")(<div><div><span id="foo"/></div></div>)

      ret(0).asInstanceOf[Elem].label must_== "span"
      ret.length must_== 1
      (ret \ "@id").text must_== "bar"
    }

    "Select a node and transform stuff deeply nested 2" in {
      val ret = ("#foo ^^" #> "hello" &
                 "span [id]" #> "bar")(<div><div><span id="foo2"/><span id="foo3"/><span dog="woof" id="foo"/></div></div>)

      ret(0).asInstanceOf[Elem].label must_== "span"
      ret.length must_== 1
      (ret \ "@id").text must_== "bar"
      (ret \ "@dog").text must_== "woof"
    }

    "substitute multiple Strings by id" in {
      ("#foo" #> "hello" & "#baz" #> "bye")(<b><div id="baz">Hello</div><span id="foo"/></b>).toString must_== <b>byehello</b>.toString
    }

    "bind href and None content" in {
      val opt: Option[String] = None
      val res = ("top *" #> opt &
                 "top [href]" #> "frog")(<top>cat</top>)

      res.length must_== 0
    }

    "bind href and Some content" in {
      val opt: Option[String] = Some("Dog")
      val res = ("top *" #> opt &
                 "top [href]" #> "frog")(<top>cat</top>)

      res.text must_== "Dog"
      (res \ "@href").text.mkString must_== "frog"
    }

    "bind href and Some content with multiple attrs" in {
      val opt: Option[String] = Some("Dog")
      val res = ("top *" #> opt &
                 "top [meow]" #> "woof" &
                 "top [href]" #> "frog")(<top href="#">cat</top>)

      res.text must_== "Dog"
      (res \ "@href").text.mkString must_== "frog"
      (res \ "@meow").text.mkString must_== "woof"
    }

    "option transform on *" in {
      val opt: Option[String] = None
      val res = ("* *" #> opt.map(ignore => "Dog"))(<top>cat</top>)
      res.length must_== 0
    }

    "append attribute to a class with spaces" in {
      val stuff = List("a", "b")
      val res = ("* [class+]" #> stuff)(<top class="q">cat</top>)
      (res \ "@class").text must_== "q a b"
    }

    "append attribute to an href" in {
      val stuff = List("&a=b", "&b=d")
      val res = ("* [href+]" #> stuff)(<top href="q?z=r">cat</top>)
      (res \ "@href").text must_== "q?z=r&a=b&b=d"
    }

    "option transform on *" in {
      val opt: Option[Int] = Full(44)
      val res = ("* *" #> opt.map(ignore => "Dog"))(<top>cat</top>)
      res must ==/ (<top>Dog</top>)
    }


    "option transform on *" in {
      val opt: Box[String] = Empty
      val res = ("* *" #> opt.map(ignore => "Dog"))(<top>cat</top>)
      res.length must_== 0
    }

    "option transform on *" in {
      val opt: Box[Int] = Some(44)
      val res = ("* *" #> opt.map(ignore => "Dog"))(<top>cat</top>)
      res must ==/ (<top>Dog</top>)
    }

    "transform on *" in {
      val res = ("* *" #> "Dog")(<top>cat</top>)
      res must ==/ (<top>Dog</top>)
    }

    "transform child content on *+" in {
      val res = ("* *+" #> "moose")(<a>I like </a>)
      res.text must_== "I like moose"
    }

    "transform child content on -*" in {
      val res = ("* -*" #> "moose")(<a> I like</a>)
      res.text must_== "moose I like"
    }

    "transform on li" in {
      val res = ("li *" #> List("Woof", "Bark") & ClearClearable)(
        <ul><li>meow</li><li class="clearable">a</li><li class="clearable">a</li></ul>)
      res must ==/ (<ul><li>Woof</li><li>Bark</li></ul>)
    }

    "substitute multiple Strings by id" in {
      (("#foo" replaceWith "hello") &
       ("#baz" replaceWith "bye"))(<b><div id="baz">Hello</div><span id="foo"/></b>).toString must_== <b>byehello</b>.toString
    }

    "substitute multiple Strings with a List by id" in {
      ("#foo" #> "hello" &
     "#baz" #> List("bye", "bye"))(<b><div id="baz">Hello</div><span id="foo"/></b>).toString must_== <b>byebyehello</b>.toString
    }

    "substitute multiple Strings with a List by id" in {
      (("#foo" replaceWith "hello") &
       ("#baz" replaceWith List("bye", "bye")))(<b><div id="baz">Hello</div><span id="foo"/></b>).toString must_== <b>byebyehello</b>.toString
    }


    "substitute multiple Strings with a List of XML by id" in {
      val answer = ("#foo" #> "hello" &
     "#baz" #> List[NodeSeq](<i/>, <i>Meow</i>))(<b><div frog="dog" id="baz">Hello</div><span id="foo"/></b>)
      
      (answer \ "i").length must_== 2
      (answer \ "i")(0) must ==/ (<i id="baz" frog="dog"/>)
      (answer \ "i")(1) must ==/ (<i frog="dog">Meow</i>)
    }

    "substitute multiple Strings with a List of XML by id" in {
      val answer = (("#foo" replaceWith "hello") &
                    ("#baz" replaceWith List[NodeSeq](<i/>, <i>Meow</i>)))(<b><div frog="dog" id="baz">Hello</div><span id="foo"/></b>)
      
      (answer \ "i").length must_== 2
      (answer \ "i")(0) must ==/ (<i id="baz" frog="dog"/>)
      (answer \ "i")(1) must ==/ (<i frog="dog">Meow</i>)
    }

    "substitute by name" in {
      val answer = ("name=moose" #> <input name="goof"/>).apply (
        <div><input name="moose" value="start" id="79"/></div>)

      (answer \ "input")(0) must ==/ (<input name="goof" value="start" id="79"/>)
    }
    
    "substitute by name" in {
      val answer = ("name=moose" replaceWith <input name="goof"/>).apply (
        <div><input name="moose" value="start" id="79"/></div>)

      (answer \ "input")(0) must ==/ (<input name="goof" value="start" id="79"/>)
    }
    

    "substitute by name with attrs" in {
      val answer = ("name=moose" #> <input name="goof" value="8" id="88"/>).apply (
        <div><input name="moose" value="start" id="79"/></div>)

      (answer \ "input")(0) must ==/ (<input name="goof" value="8" id="88"/>)
    }
    
    "substitute by name with attrs" in {
      val answer = ("name=moose" replaceWith <input name="goof" value="8" id="88"/>).apply (
        <div><input name="moose" value="start" id="79"/></div>)

      (answer \ "input")(0) must ==/ (<input name="goof" value="8" id="88"/>)
    }
    

    "substitute by a selector with attrs" in {
      val answer = ("cute=moose" #> <input name="goof" value="8" id="88"/>).apply (
        <div><input name="meow" cute="moose" value="start" id="79"/></div>)

      (answer \ "input")(0) must ==/ (<input cute="moose" name="goof" value="8" id="88"/>)
    }
    
    "substitute by a selector with attrs" in {
      val answer = ("cute=moose" replaceWith <input name="goof" value="8" id="88"/>).apply (
        <div><input name="meow" cute="moose" value="start" id="79"/></div>)

      (answer \ "input")(0) must ==/ (<input cute="moose" name="goof" value="8" id="88"/>)
    }

    "Map of funcs" in {
      val func: NodeSeq => NodeSeq = "#horse" #> List(1,2,3).map(".item *" #> _)
      val answer: NodeSeq = func(<span><div id="horse">frog<span class="item">i</span></div></span>)

      answer must ==/ (<span><div id="horse">frog<span class="item">1</span></div><div>frog<span class="item">2</span></div><div>frog<span class="item">3</span></div></span>)
                  
    }
    

    "merge classes" in {
      val answer = ("cute=moose" #> <input class="a" name="goof" value="8" id="88"/>).apply (
        <div><input name="meow" class="b" cute="moose" value="start" id="79"/></div>)

      (answer \ "input")(0) must ==/ (<input class="a b" cute="moose" name="goof" value="8" id="88"/>)
    }
    

    "merge classes" in {
      val answer = ("cute=moose" replaceWith <input class="a" name="goof" value="8" id="88"/>).apply (
        <div><input name="meow" class="b" cute="moose" value="start" id="79"/></div>)

      (answer \ "input")(0) must ==/ (<input class="a b" cute="moose" name="goof" value="8" id="88"/>)
    }
    



    "list of strings" in {
      val answer = ("#moose *" #> List("a", "b", "c", "woof") &
                    ClearClearable).apply (
        <ul>
        <li id="moose">first</li>
        <li class="clearable">second</li>
        <li class="clearable">Third</li>
        </ul>)
        
      val lis = (answer \ "li").toList
      
      lis.length must_== 4

      lis(0) must ==/ (<li id="moose">a</li>)
      lis(3) must ==/ (<li>woof</li>)
    }
    

    "list of Nodes" in {
      val answer = ("#moose *" #> List[NodeSeq](<i>"a"</i>, Text("b"), Text("c"), <b>woof</b>) &
                    ClearClearable).apply (
        <ul>
        <li id="moose">first</li>
        <li class="clearable">second</li>
        <li class="clearable">Third</li>
        </ul>)
        
      val lis = (answer \ "li").toList
      
      lis.length must_== 4

      lis(0) must ==/ (<li id="moose"><i>"a"</i></li>)
      lis(3) must ==/ (<li><b>woof</b></li>)
    }
    

    "set href" in {
      val answer = ("#moose [href]" #> "Hi" &
                    ClearClearable).apply (
        <ul><a id="moose" href="meow">first</a><li class="clearable">second</li><li class="clearable">Third</li></ul>)
        
    
    (answer \ "a" \ "@href").text must_== "Hi"
      (answer \ "li").length must_== 0
    }
    
    "set href and subnodes" in {
      val answer = ("#moose [href]" #> "Hi" &
                    ClearClearable).apply (
        <ul><a id="moose" href="meow">first<li class="clearable">second</li><li class="clearable">Third</li></a></ul>)
        
    
    (answer \ "a" \ "@href").text must_== "Hi"
      (answer \\ "li").length must_== 0
    }


    "list of strings" in {
      val answer = (("#moose *" replaceWith List("a", "b", "c", "woof")) &
                    ClearClearable).apply (
        <ul>
        <li id="moose">first</li>
        <li class="clearable">second</li>
        <li class="clearable">Third</li>
        </ul>)
        
      val lis = (answer \ "li").toList
      
      lis.length must_== 4

      lis(0) must ==/ (<li id="moose">a</li>)
      lis(3) must ==/ (<li>woof</li>)
    }
    
    "bind must bind to subnodes" in {
      val html = <ul class="users">
      <li class="user" userid="">
      <img class="userimg" src=""/>
      </li>
      </ul>

      val lst = List(1,2,3)

      val f = ".users *" #> ("li" #> lst.map(i => ".user [userid]" #> i))

      (f(html) \\ "ul").length must_== 1
      (f(html) \\ "li").length must_== 3
    }

    "list of Nodes" in {
      val answer = (("#moose *" replaceWith List[NodeSeq](<i>"a"</i>, Text("b"), Text("c"), <b>woof</b>)) &
                    ClearClearable).apply (
        <ul>
        <li id="moose">first</li>
        <li class="clearable">second</li>
        <li class="clearable">Third</li>
        </ul>)
        
      val lis = (answer \ "li").toList
      
      lis.length must_== 4

      lis(0) must ==/ (<li id="moose"><i>"a"</i></li>)
      lis(3) must ==/ (<li><b>woof</b></li>)
    }
    

    "set href" in {
      val answer = (("#moose [href]" replaceWith "Hi") &
                    ClearClearable).apply (
        <ul><a id="moose" href="meow">first</a><li class="clearable">second</li><li class="clearable">Third</li></ul>)
        
    
    (answer \ "a" \ "@href").text must_== "Hi"
      (answer \ "li").length must_== 0
    }
    
    "set href and subnodes" in {
      val answer = (("#moose [href]" replaceWith "Hi") &
                    ClearClearable).apply (
        <ul><a id="moose" href="meow">first<li class="clearable">second</li><li class="clearable">Third</li></a></ul>)
        
    
    (answer \ "a" \ "@href").text must_== "Hi"
      (answer \\ "li").length must_== 0
    }
    

  }
}