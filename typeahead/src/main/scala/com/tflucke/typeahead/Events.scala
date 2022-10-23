package com.tflucke.typeahead

import org.scalajs.dom.{CustomEvent,CustomEventInit,HTMLElement}

class RenderEvent[T](
  typeArg: String,
  val dataset: Dataset[T],
  val suggestions: Seq[T],
  val async: Boolean,
  // TODO: Actual defaults
  init: CustomEventInit = null
) extends CustomEvent(typeArg, init)

class QueryEvent(
  typeArg: String,
  val query: String,
  // TODO: Actual defaults
  init: CustomEventInit = null
) extends CustomEvent(typeArg, init)

class LanguageDirectionEvent(
  typeArg: String,
  val dir: String,
  // TODO: Actual defaults
  init: CustomEventInit = null
) extends CustomEvent(typeArg, init)

class CursorEvent[T](
  typeArg: String,
  val selectable: Option[Dropdown.Selectable[T]],
  // TODO: Actual defaults
  init: CustomEventInit = null
) extends CustomEvent(typeArg, init)
