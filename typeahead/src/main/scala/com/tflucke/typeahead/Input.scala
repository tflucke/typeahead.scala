package com.tflucke.typeahead

import org.scalajs.dom._
import org.scalajs.dom.document

case class Input(
  input: HTMLInputElement,
  hint: Option[HTMLInputElement] = None,
  classes: Classes = Classes()
) {
  private var _query = input.value
  private var _queryWhenFocused = if (hasFocus) Some(_query) else None
  private val _overflowHelper = classes.buildOverflowHelper(input)
  private var _dir = "ltr"
  _checkLanguageDirection()
  (Option(input.parentElement), Option(input.nextSibling)) match {
    case (Some(parent), Some(sibling)) => parent.insertBefore(_overflowHelper, sibling)
    case (Some(parent), None) => parent.appendChild(_overflowHelper)
    case (None, _) => ()
  }

  import TypeaheadElement.eventspace

  def hasFocus = input == document.activeElement

  private def _fireEvent(e: Event) = scala.scalajs.js.timers.setTimeout(0) {
    input.dispatchEvent(e)
  }

  private def _managePreventDefault(keyName: String, e: KeyboardEvent) =
    keyName match {
      case "up"|"down" if !Input.withModifier(e) => e.preventDefault()
      case _ => ()
    }

  private def _shouldTrigger(keyName: String, e: KeyboardEvent) =
    keyName match {
      case "tab" => !Input.withModifier(e)
      case _ => true
    }

  private def _checkLanguageDirection() = {
    val dir = input.dir.toLowerCase()
    if (_dir != dir) {
      _dir = dir
      hint.foreach(_.dir = dir)
      _fireEvent(new LanguageDirectionEvent(s"$eventspace:langDirChanged", dir))
    }
  }

  private def _setQuery(value: String, silent: Boolean = false) = {
    val areEquivalent = Input.areQueriesEquivalent(value, _query)
    val hasDifferentWhitespace = areEquivalent && _query.length != value.length

    _query = value

    if (!silent) {
      if (!areEquivalent)
        _fireEvent(new QueryEvent(s"$eventspace:queryChanged", _query))
      else if (hasDifferentWhitespace) {
        _fireEvent(new QueryEvent(s"$eventspace:whitespaceChanged", _query))
      }
    }
  }

  def onBlur(e: Event) = resetInputValue()
  input.addEventListener("blur", onBlur)

  def onFocus(e: Event) = _queryWhenFocused = Some(_query)
  input.addEventListener("focus", onFocus)

  def onKeyDown(e: KeyboardEvent) =
    Input.specialKeyCodeMap.get(e.keyCode).foreach { keyName =>
      _managePreventDefault(keyName, e)
      if (_shouldTrigger(keyName, e))
        _fireEvent(new KeyboardEvent(
          s"$eventspace:${keyName}Keyed",
          // Abusing the JS ducktyping a bit here for convenience
          e.asInstanceOf[KeyboardEventInit]
        ))
    }
  input.addEventListener("keydown", onKeyDown)

  def onInput(e: Event) = {
    _setQuery(getInputValue())
    clearHintIfInvalid()
    _checkLanguageDirection()
  }
  input.addEventListener("input", onInput)

  def focus() = input.focus()

  def blur() = input.blur()

  def getLangDir() = _dir

  def getQuery() = _query

  def setQuery(value: String, silent: Boolean = false) = {
    setInputValue(value)
    _setQuery(value, silent)
  }

  def hasQueryChangedSinceLastFocus = Some(_query) != _queryWhenFocused

  def getInputValue() = input.value

  def setInputValue(value: String) = {
    input.value = value
    clearHintIfInvalid()
    _checkLanguageDirection()
  }

  def resetInputValue() = setInputValue(_query)

  def getHint() = hint.map { _.value }

  def setHint(value: String) = hint.foreach { _.value = value }

  def clearHint() = setHint("")

  def clearHintIfInvalid() = {
    val value = getInputValue()
    getHint().foreach { hint =>
      val valIsPrefixOfHint = value != hint && hint.indexOf(value) == 0
      if (value.isEmpty || !valIsPrefixOfHint || hasOverflow)
        clearHint()
    }
  }

  def hasOverflow = {
    // 2 is arbitrary, just picking a small number to handle edge cases
    val constraint = input.clientWidth - 2
    _overflowHelper.innerText = getInputValue()
    _overflowHelper.clientWidth >= constraint
  }

  def isCursorAtEnd() = input.selectionStart == input.value.length

  def dispose() = {
    input.removeEventListener("blur", onBlur)
    input.removeEventListener("focus", onFocus)
    input.removeEventListener("keydown", onKeyDown)
    input.removeEventListener("input", onInput)
    _overflowHelper.parentElement.removeChild(_overflowHelper)
  }
}

object Input {
  val specialKeyCodeMap = Map(
    9 -> "tab",
    27 -> "esc",
    37 -> "left",
    39 -> "right",
    13 -> "enter",
    38 -> "up",
    40 -> "down"
  )

  def normalizeQuery(str: String) =
    // strips leading whitespace and condenses all whitespace
    str.replaceAll("^\\s*", "").replaceAll("\\s{2,}", " ")

  def areQueriesEquivalent(a: String, b: String) =
    normalizeQuery(a) == normalizeQuery(b)

  def withModifier(e: KeyboardEvent) =
    e.altKey || e.ctrlKey || e.metaKey || e.shiftKey
}
