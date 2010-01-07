
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
  val pageSize: Int = 3

  def addNote(html: NodeSeq): NodeSeq = {
    val note = GuestNote.create

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

    val totalNotes = GuestNote.count
    val unrepliedNotes = GuestNote.count(By(GuestNote.repliedByAdmin, 0))
    val maxPid = (Math.ceil(totalNotes.toFloat/pageSize) - 1).toInt
    val curPid = S.param("page").map(_.toInt) openOr 0

    def linkIfOther(destPid: Int, linkText: String): NodeSeq = {
      if (curPid == destPid)
      Text(linkText)
      else {
        val href = "?page="+destPid
        <a href={href}>{linkText}</a>
      }
    }

    bind("notes", html,
      "total" -> totalNotes,
      "unreplied" -> unrepliedNotes,
      "firstPage" -> linkIfOther(0, firstText),
      "prePage" -> linkIfOther(if (curPid-1 > 0) (curPid-1) else 0 , preText),
      "nextPage" -> linkIfOther(if (curPid+1>maxPid) maxPid else curPid+1, nextText),
      "lastPage" -> linkIfOther(maxPid, lastText),
      "pageList" -> <b>{curPid+1}</b>
    )
  }


  def list(html: NodeSeq): NodeSeq = 
    toShow.flatMap(item => 
        bind("item", html,
             "title" -> item.title,
             "createdTime" -> item.createdTime,
             "email" -> item.email,
             "content" -> item.content,
             "reply" -> getReplyContent(item)))
 

  private def replyNote(item: GuestNote): NodeSeq = 
  (for {
      html <- TemplateFinder.findAnyTemplate(List("templates-hidden", "gbook", "replyForm"))
    } yield {
      def doReply() = {
        if (item.replyContent.length == 0) {
          S.error("No reply content is provided.")
          S.redirectTo("/gbook/")
        }
        item.repliedByAdmin(1)
        if (item.save) {
          S.redirectTo("/gbook/")
        }
      }

      bind("note", html,
           "replyContent" -> SHtml.textarea(item.replyContent,
                                            item.replyContent(_)),
           "submit" -> SHtml.submit("Reply", doReply)
      )}) openOr NodeSeq.Empty

  
  private def toShow = {
    val pid = S.param("page").map(_.toInt) openOr 0
    val startAt = pageSize * pid
    GuestNote.findAll(OrderBy(GuestNote.id, Ascending), StartAt(startAt), MaxRows(pageSize))
  }

  private def getReplyContent(item: GuestNote) = 
    if (item.repliedByAdmin.is == 0) {
      <div>
        <p>Not replied by admin</p>
        <p>{item.replyContentAsHtml}</p>
        {replyNote(item)}
      </div>
    } else {
      <div>
        <p>{"Replied by admin at: "+item.replyTime}</p>
        <p>{item.replyContentAsHtml}</p>
        {replyNote(item)}
      </div>
    }
}