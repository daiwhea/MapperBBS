/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.vapee.main.snippet

import java.util.Date
import com.vapee.main._
import model._
import _root_.net.liftweb._
import _root_.net.liftweb.common._
import util._
import Helpers._
import http._
import _root_.net.liftweb.mapper._

import scala.xml._

class GuestBook {

  // Set up a requestVar to track the GuestNote object for edits and adds
  object noteVar extends RequestVar(new GuestNote())
  def note = noteVar.is
  val pageSize: Int = 3
  var curPage = 0

  def addNote(html: NodeSeq): NodeSeq = {
    def isValideNote(toCheck : GuestNote) : Boolean =
      List((if (toCheck.title.length == 0) { S.error("You must provide a title"); false } else true),
        (if (toCheck.email.length == 0) { S.error("You must provide a email address"); false } else true),
        (if (toCheck.content.length == 0) { S.error("You must provide some note content"); false } else true)
      ).forall(_ == true)
    def doAdd() = {
      if (isValideNote(note)) {
        note.createdTime(new Date())
        note.repliedByAdmin(0)
        note.save
        S.redirectTo("/gbook/")
      }
    }

    bind("note", html,
     "title" -> SHtml.text(note.title, note.title(_)),
     "email" -> SHtml.text(note.email, note.email(_)),
     "content" -> SHtml.textarea(note.content, note.content(_)),
     "submit" -> SHtml.submit("Add my Note", doAdd))
  }

  def paginate(html: NodeSeq): NodeSeq = {
    val firstText = "<<"
    val preText = "<"
    val nextText = ">"
    val lastText = ">>"

    var totalNotes = GuestNote.count
    var unrepliedNotes = GuestNote.count(By(GuestNote.repliedByAdmin, 0))
    var maxPid = (Math.ceil(totalNotes.toFloat/pageSize) - 1).toInt
    var curPid = S.param("page").map(_.toInt) openOr 0

    def linkIfOther(destPid: Int, linkText: String): NodeSeq = {
      if (curPid == destPid)
        Text(linkText)
      else {
        //SHtml.link("list?page="+destPid, () => curPid = curPid, Text(linkText))
        val href = "?page="+destPid
        <a href={href}>{Text(linkText)}</a>
      }
    }
    
    bind("notes", html,
      "total" -> totalNotes,
      "unreplied" -> unrepliedNotes,
      "firstPage" -> linkIfOther(0, firstText),
      "prePage" -> linkIfOther(if (curPid-1 > 0) (curPid-1) else 0 , preText),
      "nextPage" -> linkIfOther(if (curPid+1>maxPid) maxPid else curPid+1, nextText),
      "lastPage" -> linkIfOther(maxPid, lastText),
      "pageList" -> <b>{curPid}</b>
    )
  }

  def list(html: NodeSeq): NodeSeq = {
    val paginatedHtml = paginate(html)
    toShow.flatMap(item => {
        bind("item", paginatedHtml,
          "title" -> item.title,
          "email" -> item.email,
          "content" -> item.content,
          "reply" -> getReplyContent(item)
        )
      }
    )
  }

  def replyNote(html: NodeSeq): NodeSeq = {
    def doReply() = {
      if (note.replyContent.length == 0) {
        S.error("No reply content is provided.")
        S.redirectTo("/gbook/")
      }
      note.repliedByAdmin(1)
      if (note.save) {
        S.redirectTo("/gbook/")
      }
    }
    val current = note
    
    bind("note", html,
      "id" -> SHtml.hidden( () => noteVar(current) ),
      "replyContent" -> SHtml.textarea(note.replyContent, note.replyContent(_)),
      "submit" -> SHtml.submit("Reply", doReply)
    )
  }

  private def toShow = {
    val pid = S.param("page").map(_.toInt) openOr 0
    val startAt = pageSize * pid
    GuestNote.findAll(OrderBy(GuestNote.id, Ascending), StartAt(startAt), MaxRows(pageSize))
  }

  private def getReplyContent(note: GuestNote) = {
    if (note.repliedByAdmin == 0) {
      <div><p>Not replied by admin</p>{SHtml.link("reply", () => noteVar(note), Text("Reply"))}</div>
    }
    else
      <div><p>{"Replied by admin at: "+note.replyTime}:</p><p>{note.replyContent.replaceAll("\n", "<br />")}</p>{SHtml.link("reply", () => noteVar(note), Text("Reply"))}</div>
  }

}