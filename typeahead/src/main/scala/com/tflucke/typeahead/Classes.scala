package com.tflucke.typeahead

import org.scalajs.dom.{HTMLInputElement,HTMLElement}
import org.scalajs.dom.document

// TODO: Split by whitespace
case class Classes(
  val wrapper: String    = "scala-typeahead",
  val input: String      = "st-input",
  val hint: String       = "st-hint",
  val menu: String       = "st-menu",
  val dataset: String    = "st-dataset",
  val suggestion: String = "st-suggestion",
  val selectable: String = "st-selectable",
  val empty: String      = "st-empty",
  val open: String       = "st-open",
  val cursor: String     = "st-cursor",
  val highlight: String  = "st-highlight"
) {
  def buildWrapper(input: HTMLInputElement): HTMLElement = {
    val html = document.createElement("span").asInstanceOf[HTMLElement]
    html.classList.add(wrapper)
    Classes.setCss(html)(Seq(
      "position" -> "relative",
      "display" -> "inline-block"
    ))
    Classes.copyCss(html, input)(Seq(
      "width",
      "max-width",
      "height",
      "max-height"
    ))
    html
  }

  def buildMenu: HTMLElement = {
    val html = document.createElement("div").asInstanceOf[HTMLElement]
    html.classList.add(menu)
    Classes.setCss(html)(Seq(
      "position" -> "absolute",
      "top" -> "100%",
      "left" -> "0",
      "z-index" -> "100",
      "display" -> "none"
    ))
    html
  }

  def buildHintFromInput(input: HTMLInputElement) = {
    val clone = input.cloneNode().asInstanceOf[HTMLInputElement]
    clone.classList.add(hint)
    Classes.setCss(clone)(Seq(
      "position" -> "absolute",
      "top" -> "0",
      "left" -> "0",
      "border-color" -> "transparent",
      "box-shadow" -> "none",
      "opacity" -> "1",
      "background-color" -> "revert"
    ))
    Classes.copyCss(clone, input)(Seq(
      "background-attachment",
      "background-clip",
      "background-color",
      "background-image",
      "background-origin",
      "background-position",
      "background-repeat",
      "background-size"
    ))
    clone.readOnly = true
    clone.spellcheck = false
    clone.tabIndex = -1
    clone.autocomplete = "off"
    Seq(
      "id",
      "name",
      "placeholder",
      "required"
    ).foreach(clone.removeAttribute)
    clone.value = ""
    clone
  }

  def buildOverflowHelper(input: HTMLInputElement) = {
    val pre = document.createElement("pre").asInstanceOf[HTMLElement]
    pre.setAttribute("aria-hidden", "true")
    Classes.setCss(pre)(Seq(
      "position" -> "absolute",
      "visibility" -> "hidden",
      "whiteSpace" -> "pre"
    ))
    Classes.copyCss(pre, input)(Seq(
      "font-family",
      "font-size",
      "font-variant",
      "fontStyle",
      "fontWeight",
      "wordSpacing",
      "letterSpacing",
      "textIndent",
      "textTransform"
    ))
    pre
  }
}

object Classes {
  def setCss(elm: HTMLElement)(css: Iterable[(String, String)]) = {
    css.foreach { case (prop, value) =>
      elm.style.setProperty(prop, value)
    }
    elm
  }

  def copyCss(elm: HTMLElement, ref: HTMLElement)(css: Iterable[String]) = {
    css.foreach {
      prop => elm.style.setProperty(prop, ref.style.getPropertyValue(prop))
    }
    elm
  }
}
