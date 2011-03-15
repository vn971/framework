/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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
package mongodb

import java.util.UUID
import java.util.regex.Pattern

import com.mongodb._
import org.specs2.specification._
import org.specs2.execute._
import org.specs2.matcher._
import json.DefaultFormats


/**
 * System under specification for Mongodirect.
 */
object MongoDirectSpec extends MongoAcceptance { def is = args(sequential=true, skipAll=(!isMongoRunning))^
  "MongoDirect Specification".title ^
  (if (!isMongoRunning) "MongoDB is not running" else "")                                  ^end^
                                                                                           """
This is a tutorial on how to use MongoDB with Lift
                                                                                           """^p^
    "First we use the DB directly"                                                         ^
      "to save a db object and retrieve it. It should be the same doc we built"            ! direct.e1^
      "we can also upsert the document"                                                    ! direct.e2^
      "we can increment the document or change its type"                                   ! direct.e3^
      "we can also execute some server-side code"                                          ! direct.e4^
                                                                                           p^
    "We can also use a DB collection to"                                                   ^
      "create a list of documents with an index"                                           ! collection.e1^
      "get the count with a query"                                                         ! collection.e2^
      "get the count with a cursor"                                                        ! collection.e3^
      "get a single element with a query"                                                  ! collection.e4^
      "get a count with a 'greater than' query"                                            ! collection.e5^
      "get a count with a 'less than equal' query"                                         ! collection.e6^
      "limit the result set"                                                               ! collection.e7^
      "skip elements"                                                                      ! collection.e8^
      "skip elements and limit the result set"                                             ! collection.e9^
      "get sorted results"                                                                 ! collection.e10^
      "remove elements with a query"                                                       ! collection.e11^
      Step(cleanup("testCollection"))                                                      ^
                                                                                           p^
    "Or we can also use a DB session"                                                      ^
      "to save documents"                                                                  ^
        "with an index on the name"                                                        ! session.e1^
        "saving a document with the same name must return an error"                        ! session.e2^
        "whereas saving a document with a different name should be ok"                     ! session.e3^
        "and documents can be retrieved with a query"                                      ! session.e4^
                                                                                           p^
      "to update documents"                                                                ^
        "with an $inc - increment - function"                                              ! session.e5^
        "unless they can't be found"                                                       ! session.e6^
                                                                                           p^
      "to query for documents"                                                             ^
        "with a regular expression"                                                        ! session.e7^
        "with a regular expression and an object"                                          ! session.e8^
        Step(cleanup("iDoc"))                                                              ^
                                                                                           endp^
    "Documents can be created with an UUID"                                                ^
      "created with the uuid.randomUUID method"                                            ! uuid.e1^
                                                                                           p^
  Step(doAfterSpec)                                                                        ^
                                                                                           end



  def withDb[T <% Result](t: DB =>T): T = MongoDB.use(DefaultMongoIdentifier)(t)
  def withCollection[T <% Result](c: String)(t: DBCollection =>T): T = MongoDB.useCollection(c)(t)
  def withSession[T <% Result](f: (DB, DBCollection) => T): T = MongoDB.useSession { db => f(db, db.getCollection("testCollection")) }
  
  def cleanup(name: String) = if (isMongoRunning) withDb { db => 
    if (!debug) { db.getCollection(name).drop }
	success
  }
  
  object direct {
    def e1 = withDb { db =>
      val coll = db.getCollection("testCollection")
      val doc = createDoc
      coll.save(doc)
      coll.findOne must_== doc
    }

    def e2 = withDb { db =>
      // upsert
      val coll = db.getCollection("testCollection")
      val doc = coll.findOne
      doc.put("type", "document")
      doc.put("count", 2)
      val q = new BasicDBObject("name", "MongoDB") // the query to select the document(s) to update
      val o = doc // the new object to update with, replaces the entire document, except possibly _id
      val upsert = false // if the database should create the element if it does not exist
      val apply = false // if an _id field should be added to the new object

      coll.update(q, o, upsert, apply)

      // get the doc back from the db and compare
	  val retrieved = coll.findOne
      (retrieved.get("type"), retrieved.get("count")) must be_===(("document", 2))
    }
    
    def e3 = withDb { db =>
      val coll = db.getCollection("testCollection")
      // modifier operations $inc, $set, $push...
      val q = new BasicDBObject("name", "MongoDB") // the query to select the document(s) to update
      val o2 = new BasicDBObject
      o2.put("$inc", new BasicDBObject("count", 1)) // increment count by 1
      o2.put("$set", new BasicDBObject("type", "docdb")) // set type
      coll.update(q, o2, false, false)

      // get the doc back from the db and compare
	  val retrieved = coll.findOne
      (retrieved.get("type"), retrieved.get("count")) must be_===(("docdb", 3))
    }
    
    def e4 = withDb { db =>
      db.eval(" function() { return 3+3; } ") must be_===(6)
    }
    
  } 

  object collection extends MustThrownMatchers {
    def e1 = withCollection("iDoc") { coll =>
      for (i <- List.range(1, 101)) {
        coll.insert(new BasicDBObject().append("i", i))
      }
      coll.createIndex(new BasicDBObject("i", 1))  // create index on "i", ascending
      coll.getCount must_== 100
    }
    
    def e2 = withCollection("iDoc") { coll =>
      coll.getCount(new BasicDBObject("i", new BasicDBObject("$gt", 50))) must_== 50
    }

    def e3 = withCollection("iDoc") { coll =>
      coll.find.count must_== 100
    }

    def e4 = withCollection("iDoc") { coll =>
      val query = new BasicDBObject
      query.put("i", 71)
      val cur2 = coll.find(query)

      (cur2.count, cur2.next.get("i")) must be_===((1, 71))
    }

    def e5 = withCollection("iDoc") { coll => 
      coll.find(new BasicDBObject("i", new BasicDBObject("$gt", 50))).count must_== 50
    }
    
    def e6 = withCollection("iDoc") { coll => 
      coll.find(new BasicDBObject("i", new BasicDBObject("$gt", 20).append("$lte", 30))).count must_== 10
    }
    
    def e7 = withCollection("iDoc") { coll => 
      val cur5 = coll.find(new BasicDBObject("i", new BasicDBObject("$gt", 50))).limit(3)
	  var cntr5 = 0
      while(cur5.hasNext) {
        cur5.next
        cntr5 += 1
      }
      cntr5 must_== 3
    }
    
    def e8 = withCollection("iDoc") { coll => 
      val cur6 = coll.find(new BasicDBObject("i", new BasicDBObject("$gt", 50))).skip(10)

      var cntr6 = 0
      while(cur6.hasNext) {
        cntr6 += 1
        cur6.next.get("i") must be_===(60+cntr6)
      }
      cntr6 must_== 40
    }

    def e9 = withCollection("iDoc") { coll => 
      val cur7 = coll.find.skip(10).limit(20)

      var cntr7 = 0
      while(cur7.hasNext) {
        cntr7 += 1
        cur7.next.get("i") must be_===(10+cntr7)
      }
      cntr7 must_== 20
    }
    
    def e10 = withCollection("iDoc") { coll => 
      val cur8 = coll.find.sort(new BasicDBObject("i", -1)) // descending
      var cntr8 = 100
      while(cur8.hasNext) {
        cur8.next.get("i") must be_===(cntr8)
        cntr8 -= 1
      }
      success
    }

    def e11 = withCollection("iDoc") { coll => 
      coll.remove(new BasicDBObject("i", new BasicDBObject("$gt", 50)))
      coll.find.count must_== 50
    }
  }

  object session extends MustThrownMatchers {
    def e1 = withSession { (db, coll) =>
      // create a unique index on name
      coll.ensureIndex(new BasicDBObject("name", 1), new BasicDBObject("unique", true))
      // save the docs to the db
      coll.save(createDoc1)
      db.getLastError.get("err") must beNull
    }

    def e2 = withSession { (db, coll) =>
      coll.save(createDoc2)
      db.getLastError.get("err").toString must startWith("E11000 duplicate key error index")
    }

    def e3 = withSession { (db, coll) =>
      coll.save(createDoc3)
      db.getLastError.get("err") must beNull
    }
    
    def e4 = withSession { (db, coll) =>
      coll.find(new BasicDBObject("type", "db")).count must_== 2
    }
    
    def e5 = withSession { (db, coll) =>
      // modifier operations $inc, $set, $push...
      val o2 = new BasicDBObject
      o2.put("$inc", new BasicDBObject("count", 1)) // increment count by 1
      //o2.put("$set", new BasicDBObject("type", "docdb")) // set type
      coll.update(new BasicDBObject("type", "db"), o2, false, false)
      db.getLastError.get("updatedExisting") must be_===(true)
      /* The update method only updates one document. see:
      http://jira.mongodb.org/browse/SERVER-268
      */
      db.getLastError.get("n") must be_===(1)
    }

    def e6 = withSession { (db, coll) =>
      val o2 = new BasicDBObject
      o2.put("$inc", new BasicDBObject("count", 1)) // increment count by 1

      // this update query won't find any docs to update
      coll.update(new BasicDBObject("name", "None"), o2, false, false)
      db.getLastError.get("updatedExisting") must be_===(false)
      db.getLastError.get("n") must be_===(0)
    }
    
    val key = "name"
    val regex = "^Mongo"
    
    def e7 = withSession { (db, coll) =>
      coll.find(BasicDBObjectBuilder.start.add(key, Pattern.compile(regex)).get).count must_== 2
    }

    def e8 = withSession { (db, coll) =>
      coll.find(BasicDBObjectBuilder.start.add(key, Pattern.compile(regex)).add("count", 1).get).count must_== 1
    }
  }

  object uuid extends MustThrownMatchers {
  
    def e1 = withCollection("examples.uuid") { coll =>
      val uuid = UUID.randomUUID
      val dbo = new BasicDBObject("_id", uuid).append("name", "dbo")
      coll.save(dbo)

      val qry = new BasicDBObject("_id", uuid)
      val dbo2 = coll.findOne(qry)

      (dbo2.get("_id"), dbo2.get("name")) must_== (dbo.get("_id"), dbo.get("name"))
    }
  }
  
  def date(s: String) = DefaultFormats.dateFormat.parse(s).get
  
  def createDoc = {
    // build the DBObject
    val doc = new BasicDBObject

    doc.put("name", "MongoDB")
    doc.put("type", "database")
    doc.put("count", 1)

    val info = new BasicDBObject

    info.put("x", 203)
    info.put("y", 102)

    doc.put("info", info)
    doc
  }
  
  def createDoc1 = {
    val doc = new BasicDBObject
    doc.put("name", "MongoSession")
    doc.put("type", "db")
    doc.put("count", 1)
    doc
  }

  def createDoc2 = {
    val doc2 = new BasicDBObject
    doc2.put("name", "MongoSession")
    doc2.put("type", "db")
    doc2.put("count", 1)
    doc2
  }

  def createDoc3 = {
    val doc3 = new BasicDBObject
    doc3.put("name", "MongoDB")
    doc3.put("type", "db")
    doc3.put("count", 1)
    doc3
  }
}