/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.vapee.main.model

import net.liftweb._
import mapper._
import util._
import _root_.scala.xml.Text

import _root_.java.util.Date
import _root_.java.text.{DateFormat,SimpleDateFormat}

class GuestNote extends LongKeyedMapper[GuestNote] with IdPK{

  def getSingleton = GuestNote

  object title extends MappedPoliteString(this, 128)
  object email extends MappedEmail(this, 200) {
    override def displayName = "电子邮件地址："
  }
  object content extends MappedTextarea(this, 2048) {
    override def textareaRows = 10
    override def textareaCols = 50
    override def displayName  = "Your Note:"
  }
  object createdTime extends MappedDateTime(this) {
    final val dateFormat =
      DateFormat.getDateInstance(DateFormat.SHORT)
    override def asHtml = Text(dateFormat.format(is))
    //override def dbIncludeInForm_? = false
  }
  object repliedByAdmin extends MappedInt(this) {
    //override def dbIncludeInForm_? = false
  }
  object replyContent extends MappedTextarea(this, 2048) {
    override def textareaRows = 5
    override def textareaCols = 50
    override def displayName  = "Your Reply:"
    //override def dbIncludeInForm_? = false
  }
  object replyTime extends MappedDateTime(this) {
    final val dateFormat =
      DateFormat.getDateInstance(DateFormat.SHORT)
    override def asHtml = Text(dateFormat.format(is))
    //override def dbIncludeInForm_? = false
  }

}

object GuestNote extends GuestNote with LongKeyedMetaMapper[GuestNote] {
    override def dbTableName = "guest_notes" // define the DB table name
    override def fieldOrder = List(title, email, content)
}
