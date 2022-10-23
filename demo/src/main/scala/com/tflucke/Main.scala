package com.tflucke

import com.tflucke.typeahead.{Dataset,TypeaheadElement}
import org.scalajs.dom.document
import org.scalajs.dom.HTMLInputElement
import scala.concurrent.ExecutionContext.global
import scala.scalajs.js.timers.setTimeout

object Main {
  implicit val ctx = global

  def main(args: Array[String]): Unit = {
    initSync()
    initAsync()
  }

  def initSync() = {
    val input = document.getElementById("typeahead-sync")
      .asInstanceOf[HTMLInputElement]
    TypeaheadElement(input)(Seq(
      Dataset({ str: String => Seq(
        "apple",
        "avocado",
        "orange",
        "grape",
        "grapefruit",
        "lemon",
        "lime"
      ).filter(_.contains(str.toLowerCase)) })
    ))
  }

  def initAsync() = {
    val input = document.getElementById("typeahead-async")
      .asInstanceOf[HTMLInputElement]
    TypeaheadElement(input)(Seq(
      Dataset({ str: String => Nil }, Some({ str: String =>
        val p = scala.concurrent.Promise[Seq[String]]()
        setTimeout(500) {
          p.success(Seq(
            "apple",
            "avocado",
            "orange",
            "grape",
            "grapefruit",
            "lemon",
            "lime"
          ).filter(_.contains(str.toLowerCase)))
        }
        p.future
      }))
    ))
  }
}
