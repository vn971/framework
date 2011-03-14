/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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

import javax.mail.internet.{MimeMessage, MimeMultipart}

import org.specs2.mutable._
import org.specs2.execute._

import common._


/**
 * Systems under specification for Lift Mailer.
 */
class MailerSpec extends Specification {
  "Mailer Specification".title
  sequential
  
  "A Mailer" should {

    "deliver simple messages as simple messages" in new mailer {
      send(
        From("sender@nowhere.com"),
        Subject("This is a simple email"),
        To("recipient@nowhere.com"),
        PlainMailBodyType("Here is some plain text.")
      ).getContent must haveClass[String]
    }

    "deliver multipart messages as multipart" in new mailer {
      send(
        From("sender@nowhere.com"),
        Subject("This is a multipart email"),
        To("recipient@nowhere.com"),
        PlainMailBodyType("Here is some plain text."),
        PlainMailBodyType("Here is some more plain text.")
      ).getContent must haveClass[MimeMultipart]
    }

    "deliver rich messages as multipart" in new mailer {
      send(
        From("sender@nowhere.com"),
        Subject("This is a rich email"),
        To("recipient@nowhere.com"),
        XHTMLMailBodyType(<html> <body>Here is some rich text</body> </html>)
      ).getContent must haveClass[MimeMultipart]
    }
  }
  implicit def anyToSuccess(a: Any): Success = success
  
  trait mailer extends MailerImpl {
    @volatile var lastMessage: Box[MimeMessage] = Empty
    Props.testMode
    testModeSend.default.set((msg: MimeMessage) => { lastMessage = Full(msg)  })
    def send(f: From, s: Subject, bodies: MailTypes*): MimeMessage = {
      sendMail(f, s, bodies:_*)
      synchronized {
        while (lastMessage.isEmpty) { wait(100) }
        lastMessage.open_!
      }
    }
  }
}


