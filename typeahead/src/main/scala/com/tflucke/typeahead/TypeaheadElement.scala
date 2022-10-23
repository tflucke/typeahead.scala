package com.tflucke.typeahead

import org.scalajs.dom._
import org.scalajs.dom.document
import scala.scalajs.js
import scala.util.matching.Regex

case class TypeaheadElement[T](
  val input: HTMLInputElement =
    document.createElement("input").asInstanceOf[HTMLInputElement],
  val minLength: Int = 1,
  val highlight: Boolean = true,
  val hint: Boolean = true,
  val classes: Classes = Classes()
)(datasets: Iterable[Dataset[_ <: T]])
(implicit ctx: scala.concurrent.ExecutionContext) {  
  if (datasets.isEmpty)
    throw new IllegalArgumentException("At least one dataset required.")

  import TypeaheadElement.eventspace

  /* Overwritten variables for future restore */

  private val _oldDir = input.dir
  private val _oldAutocomplete = input.autocomplete
  private val _oldSpellcheck = input.spellcheck
  private val _oldStyle = input.style

  /* Internal components */

  private val _wrapper = classes.buildWrapper(input)
  private val _hint = if (hint) Some(classes.buildHintFromInput(input)) else None
  private val _menu = _newSelectableDropdown
  private val _input = _newInput

  // Allows this class to be treated as if it was an HTMLElement for both
  // passing this as an argument
  implicit def toElement = _wrapper

  /* Current State */

  private var _enabled = true
  // activate the typeahead on init if the input has focus
  private var _active = false
  private var _dir = _input.getLangDir()

  {
    input.classList.add(classes.input)
    input.autocomplete = "off"
    input.spellcheck = false
    if (input.dir == js.undefined) input.dir = "auto"
    Classes.setCss(input)(Seq(
      "position" -> "relative",
      "verticalAlign" -> "top"
    ))
    if (hint) input.style.backgroundColor = "transparent"

    Option(input.parentElement).map(_.insertBefore(_wrapper, input))
    _hint.foreach(_wrapper.appendChild)
    _wrapper.appendChild(input)
    _wrapper.appendChild(_menu.node)

    _menu.node.addEventListener(s"$eventspace:selectableClicked",
      onSelectableClicked)
    _menu.node.addEventListener(s"$eventspace:asyncRequested", _onAsyncRequested)
    _menu.node.addEventListener(s"$eventspace:asyncCanceled", _onAsyncCanceled)
    _menu.node.addEventListener(s"$eventspace:asyncReceived", _onAsyncReceived)
    _menu.node.addEventListener(s"$eventspace:datasetRendered",_onDatasetRendered)
    _menu.node.addEventListener(s"$eventspace:datasetCleared", _onDatasetCleared)
    _menu.node.addEventListener("mousedown", { e: Event => e.preventDefault() })

    // composed event handlers for input
    def onFocused(e: Event) = if (activate() && open()) _onFocused(e)
    def onBlurred(e: Event) = if (deactivate()) _onBlurred(e)
    def onEnterKeyed(e: Event) = if (isActive && isOpen) _onEnterKeyed(e)
    def onTabKeyed(e: Event) = if (isActive && isOpen) _onTabKeyed(e)
    def onEscKeyed(e: Event) = if (isActive) _onEscKeyed(e)
    def onUpKeyed(e: Event) = if (_openIfActive())  _onUpKeyed(e)
    def onDownKeyed(e: Event) = if (_openIfActive()) _onDownKeyed(e)
    def onLeftKeyed(e: Event) = if (isActive && isOpen) _onLeftKeyed(e)
    def onRightKeyed(e: Event) = if (isActive && isOpen) _onRightKeyed(e)
    def onQueryChanged(e: QueryEvent) = if (_openIfActive()) _onQueryChanged(e)
    def onWhitespaceChanged(e: Event) =
      if (_openIfActive()) _onWhitespaceChanged(e)
    def onSelectableClicked(e: CursorEvent[T]) = e.selectable.map(_select)

    input.addEventListener("focus", onFocused _)
    input.addEventListener("blur", onBlurred _)
    input.addEventListener(s"$eventspace:enterKeyed", onEnterKeyed _)
    input.addEventListener(s"$eventspace:tabKeyed", onTabKeyed _)
    input.addEventListener(s"$eventspace:escKeyed", onEscKeyed _)
    input.addEventListener(s"$eventspace:upKeyed", onUpKeyed _)
    input.addEventListener(s"$eventspace:downKeyed", onDownKeyed _)
    input.addEventListener(s"$eventspace:leftKeyed", onLeftKeyed _)
    input.addEventListener(s"$eventspace:rightKeyed", onRightKeyed _)
    input.addEventListener(s"$eventspace:queryChanged", onQueryChanged _)
    input.addEventListener(s"$eventspace:whitespaceChanged",
      onWhitespaceChanged _)
    input.addEventListener(s"$eventspace:langDirChanged", _onLangDirChanged _)

  }

  if (input == document.activeElement) activate()

  import _menu.SelectableInst

  private def _fireEvent(e: Event) = scala.scalajs.js.timers.setTimeout(0) {
    input.dispatchEvent(e)
  }

  /* Hooks for extending/overwritting */
  protected def _newSelectableDropdown: Dropdown[T] =
    Dropdown[T](datasets, classes, highlight)
  protected def _newInput = Input(input, _hint, classes)

  protected def _onDatasetCleared(e: Event) = _updateHint()

  protected def _onDatasetRendered(e: RenderEvent[T]) = {
    _updateHint()
    _fireEvent(new RenderEvent[T](
      s"$eventspace:render",
      e.dataset,
      e.suggestions,
      e.async
    ))    
  }

  protected def _onAsyncRequested(e: QueryEvent) =
    _fireEvent(new QueryEvent(s"$eventspace:asyncRequest", e.query))

  protected def _onAsyncCanceled(e: QueryEvent) =
    _fireEvent(new QueryEvent(s"$eventspace:asyncCancel", e.query))

  protected def _onAsyncReceived(e: QueryEvent) =
    _fireEvent(new QueryEvent(s"$eventspace:asyncReceived", e.query))

  protected def _onFocused(e: Event) =
    if (_minLengthMet() && _input.hasQueryChangedSinceLastFocus)
      _menu.update(_input.getQuery())
    else
      false

  protected def _onBlurred(e: Event) =
    if (_input.hasQueryChangedSinceLastFocus)
      _fireEvent(new QueryEvent(s"$eventspace:change", _input.getQuery()))
    else false

  protected def _onEnterKeyed(e: Event) = {
    _menu.activeSelectable.map { selectable =>
      if (_select(selectable)) e.preventDefault()
    }
  }

  protected def _onTabKeyed(e: Event) = {
    println(e)
    _menu.activeSelectable.map({ selectable =>
      if (_select(selectable)) e.preventDefault()
    }) match {
      case None => _menu.topSelectable.map({ selectable =>
        if (_autocomplete(selectable)) e.preventDefault()
      })
      case some => some
    }
  }

  protected def _onEscKeyed(e: Event) = close()

  protected def _onUpKeyed(e: Event) = moveCursor(-1)

  protected def _onDownKeyed(e: Event) = moveCursor(+1)

  protected def _onLeftKeyed(e: Event) =
    if (_dir == "rtl" && _input.isCursorAtEnd())
      _menu.topSelectable.foreach(_autocomplete)

  protected def _onRightKeyed(e: Event) =
    if (_dir == "ltr" && _input.isCursorAtEnd())
      _menu.topSelectable.foreach(_autocomplete)

  protected def _onQueryChanged(e: QueryEvent) =
    if (_minLengthMet(e.query)) _menu.update(e.query) else _menu.empty()

  protected def _onWhitespaceChanged(e: Event) = _updateHint()

  protected def _onLangDirChanged(e: LanguageDirectionEvent) =
    if (_dir != e.dir) {
      _dir = e.dir
      _menu.setLanguageDirection(e.dir)
    }

  private def _openIfActive() = if (isActive) open() else false

  private def _minLengthMet(query: String = _input.getQuery()) =
    query.length >= minLength

  private def _updateHint() = {
    val selectable = _menu.topSelectable
    val value = _input.getInputValue()
    selectable.foreach { case SelectableInst(_, display, data) =>
      if (value.trim.nonEmpty && !_input.hasOverflow) {
        val query = Input.normalizeQuery(value)
        val escapedQuery = Regex.quote(query)

        // match input value, then capture trailing text
        val frontMatchRegEx = new Regex("^(?:" + escapedQuery + ")(.+$)")
        frontMatchRegEx.findFirstMatchIn(display).foreach { m =>
          _input.setHint(value + m.group(1))
        }
      }
      else {
        _input.clearHint()
      }
    }    
  }

  private def _isPrevented(e: Event) = {
    _fireEvent(e)
    e.defaultPrevented
  }

  private def _select(selectable: Dropdown.Selectable[T]) = {
    _fireEvent(new CursorEvent[T](s"$eventspace:beforeselected", Some(selectable)))
    _input.setQuery(selectable.display, true)
    _fireEvent(new CursorEvent[T](s"$eventspace:selected", Some(selectable)))
    close()
    true
  }

  private def _autocomplete(selectable: SelectableInst) = {
    val query = _input.getQuery()
    selectable match { case SelectableInst(elm, display, data) =>
      if (query != display && !_isPrevented(
        new CursorEvent[T](s"$eventspace:beforeautocomplete", Some(selectable))
      )) {
        _input.setQuery(display)
        _fireEvent(
          new CursorEvent[T](s"$eventspace:autocomplete", Some(selectable))
        )
        true
      }
      else false
    }
  }

  def isEnabled = _enabled

  def enable() = _enabled = true

  def disable() = _enabled = false

  def isActive = _active

  def activate() =
    // already active
    if (isActive) true
  // unable to activate either due to the typeahead being disabled
  // or due to the active event being prevented
    else if (!isEnabled || _isPrevented(new Event(s"$eventspace:beforeactive")))
      false
  // activate
    else {
      _active = true
      _fireEvent(new Event(s"$eventspace:active"))
      true
    }

  def deactivate() =
    // already idle
    if (!isActive) true
  // unable to deactivate due to the idle event being prevented
    else if (_isPrevented(new Event(s"$eventspace:beforeidle"))) false
  // deactivate
    else {
      _active = false
      close()
      _fireEvent(new Event(s"$eventspace:idle"))
      true
    }

  def hasFocus = input == document.activeElement

  def value_=(str: String) = _input.setQuery(value)

  def value = _input.getQuery()

  def isOpen = _menu.isOpen

  def open() = {
    if (!isOpen && !_isPrevented(new Event(s"$eventspace:beforeopen"))) {
      _menu.open()
      _updateHint()
      _fireEvent(new Event(s"$eventspace:open"))
    }
    isOpen
  }

  def close() = {
    if (isOpen && !_isPrevented(new Event(s"$eventspace:beforeclose"))) {
      _menu.close()
      _input.clearHint()
      _input.resetInputValue()
      _fireEvent(new Event(s"$eventspace:close"))
    }
    !isOpen
  }

  def moveCursor(delta: Int): Boolean = {
    val query = _input.getQuery()
    val candidate = _menu.selectableRelativeToCursor(delta)
    
    // update will return Some when it"s a new query and new suggestions
    // need to be fetched – in this case we don"t want to move the cursor
    val cancelMove = if (_minLengthMet()) Some(_menu.update(query)) else None
    
    if (cancelMove.nonEmpty && !_isPrevented(
      new CursorEvent[T](s"$eventspace:beforecursorchange", candidate)
    )) {
      _menu.setCursor(candidate)
      candidate.map({ case SelectableInst(_, display, _) =>
        // cursor moved to different selectable
        _input.setInputValue(display)
      }).getOrElse({
        // cursor moved off of selectables, back to input
        _input.resetInputValue()
        _updateHint()
      })
      _fireEvent(new CursorEvent[T](s"$eventspace:cursorchange", candidate))
      true
    }
    else false
  }

  def dispose() = {
    _wrapper.removeChild(input)
    Option(_wrapper.parentElement).map(_.insertBefore(input,_wrapper))
    _hint.foreach(_wrapper.removeChild)
    _wrapper.removeChild(_menu.node)
    Option(_wrapper.parentElement).map(_.removeChild(_wrapper))
    _input.dispose()
    _menu.dispose()

    input.classList.remove(classes.input)
    input.autocomplete = _oldAutocomplete
    input.spellcheck = _oldSpellcheck
    input.style = _oldStyle
    input.dir = _oldDir
  }

  /* Pass-through functions */

  def addEventListener[U <: Event](typ: String, fn: Function[U, _]) = 
    input.addEventListener(typ, fn)
}

object TypeaheadElement {
  val eventspace = "typeahead"
}
