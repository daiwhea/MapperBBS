
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
    def doAdd() = {
      if (isValideNote(note)) {
        note.createdTime(new Date())
        note.repliedByAdmin(0)
        note.save
        S.redirectTo("/gbook/")
      }
    }
    def isValideNote(toCheck : GuestNote) : Boolean =
      List((if (toCheck.title.length == 0) { S.error("You must provide a title"); false } else true),
        (if (toCheck.email.length == 0) { S.error("You must provide a email address"); false } else true),
        (if (toCheck.content.length == 0) { S.error("You must provide some note content"); false } else true)
      ).forall(_ == true)

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
    toShow.flatMap(item => {
        curListNote = item
        bind("item", html,
          "title" -> item.title,
          "email" -> item.email,
          "content" -> item.content,
          "reply" -> getReplyContent(curListNote)
        )
      }
    )
  }

  def replyNote(html: NodeSeq): NodeSeq = {

    val localNote = curListNote
    
    def doReply() = {
      if (localNote.replyContent.length == 0) {
        S.error("No reply content is provided.")
        S.redirectTo("/gbook/")
      }
      localNote.repliedByAdmin(1)
      if (localNote.save) {
        S.redirectTo("/gbook/")
      }
    }

    bind("note", html,
      "replyContent" -> SHtml.textarea(localNote.replyContent,
                                    localNote.replyContent(_)),
      "submit" -> SHtml.submit("Reply", doReply)
    )
  }

  private def toShow = {
    val pid = S.param("page").map(_.toInt) openOr 0
    val startAt = pageSize * pid
    GuestNote.findAll(OrderBy(GuestNote.id, Ascending), StartAt(startAt), MaxRows(pageSize))
  }

  var curListNote: GuestNote = _

  private def getReplyContent(item: GuestNote) = {
    if (item.repliedByAdmin == 0) {
      <div><p>Not replied by admin</p><p>
          {item.replyContent split '\n' map { Text(_) ++ <br /> } reduceLeft (_ ++ _) }
                                      </p><lift:embed what="/templates-hidden/gbook/replyForm" /></div>
    }
    else
      <div><p>{"Replied by admin at: "+item.replyTime}</p><p>
          {item.replyContent split '\n' map { Text(_) ++ <br /> } reduceLeft (_ ++ _) }
                                      </p><lift:embed what="/templates-hidden/gbook/replyForm" /></div>
  }

}