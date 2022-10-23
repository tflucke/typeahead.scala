package com.tflucke.typeahead

import org.scalajs.dom._
import org.scalajs.dom.document
import scala.util.matching.Regex
import Util._

case class Dropdown[T](
  val datasets: Iterable[Dataset[_ <: T]],
  val classes: Classes,
  highlight: Boolean
)(implicit ctx: scala.concurrent.ExecutionContext) {
  val node = classes.buildMenu

  import TypeaheadElement.eventspace

  private val _datasets = datasets.map(DatasetNode(_)).toSeq
  private var _query: Option[String] = None

  private def _fireEvent(e: Event) = scala.scalajs.js.timers.setTimeout(0) {
    node.dispatchEvent(e)
  }

  def onRendered(e: RenderEvent[T]) = {
    if (_allDatasetsEmpty) _hide()
    else if (isOpen) _show()
    node.classList.toggle(classes.empty, _allDatasetsEmpty)

    _fireEvent(new RenderEvent(
      s"$eventspace:datasetRendered",
      e.dataset,
      e.suggestions,
      e.async,
      // Abusing the JS ducktyping a bit here for convenience
      e.asInstanceOf[CustomEventInit]
    ))
  }

  def onCleared(e: RenderEvent[T]) = {
    if (_allDatasetsEmpty) _hide()
    else if (isOpen) _show()
    node.classList.toggle(classes.empty, _allDatasetsEmpty)

    _fireEvent(new RenderEvent(
      s"$eventspace:datasetCleared",
      e.dataset,
      e.suggestions,
      e.async,
      // Abusing the JS ducktyping a bit here for convenience
      e.asInstanceOf[CustomEventInit]
    ))
  }

  private def _allDatasetsEmpty = _datasets.forall(_.isEmpty)

  private def _selectables = _datasets.flatMap(_._selectables)

  private def _removeCursor() ={
    activeSelectable.foreach { case SelectableInst(elm, _, _) =>
      elm.classList.remove(classes.cursor)
    }}

  private def _ensureVisible(el: HTMLElement) = {
    val elTop = el.offsetTop
    val elBottom = elTop + el.clientHeight
    val nodeScrollTop = node.scrollTop
    val nodeHeight = node.clientHeight

    if (elTop < 0)
      node.scrollTop = nodeScrollTop + elTop
    else if (nodeHeight < elBottom) 
      node.scrollTop = nodeScrollTop + (elBottom - nodeHeight)
  }

  def isOpen = node.classList.contains(classes.open)

  def open() = {
    if (!_allDatasetsEmpty) _show()
    node.classList.add(classes.open)
  }

  def close() = {
    _hide()
    node.classList.remove(classes.open)
    _removeCursor()
  }

  private def _show() = node.style.display = "block"
  private def _hide() = node.style.display = "none"

  def setLanguageDirection(dir: String) = node.dir = dir

  def selectableRelativeToCursor(delta: Int) = {
    val oldCursor = activeSelectable
    val selectables = _selectables

    // shifting before and after modulo to deal with -1 index
    val oldIndex = oldCursor.map(selectables.indexOf).getOrElse(-1)
    val newIndex = (oldIndex + delta + 1) % (selectables.length + 1) - 1

    // wrap new index if less than -1
    if (newIndex < -1)
      selectables.lastOption
    else if (newIndex == -1)
      None
    else
      Some(selectables(newIndex))
  }

  def setCursor(selectable: Option[SelectableInst]) = {
    _removeCursor()
    selectable.map { s =>
      s.elm.classList.add(classes.cursor)
      _ensureVisible(s.elm)
    }
  }

  def getSelectable(el: HTMLElement) = _selectables.find({ x => x.elm == el })

  def activeSelectable = _selectables.find({
    case SelectableInst(elm, _, _) => elm.classList.contains(classes.cursor)
  })

  def topSelectable = _selectables.headOption

  def update(query: String) = if (_query != Some(query)) {
    _query = Some(query)
    _datasets.map(_.update(query))
    true
  }
  else false

  def empty() = {
    _datasets.map(_.clear())
    _query = None
    node.classList.add(classes.empty)
  }

  def dispose() = _datasets.map(_.dispose())

  private case class DatasetNode[U <: T](ds: Dataset[U]) {
    var _selectables: Seq[SelectableInst] = Nil
    val dsNode = document.createElement("div").asInstanceOf[HTMLElement]

    {
      dsNode.classList.add(classes.dataset)
      dsNode.classList.add(s"${classes.dataset}-${ds.name}")
      node.appendChild(dsNode)
      Seq(
        "asyncRequested",
        "asyncCanceled",
        "asyncReceived"
      ).foreach { event =>
        dsNode.addEventListener(s"$eventspace:$event", { e: Event =>
          Dropdown.this._fireEvent(e)
        })
      }
      dsNode.addEventListener(s"$eventspace:rendered", onRendered)
      dsNode.addEventListener(s"$eventspace:cleared", onCleared)
    }

    private def _fireEvent(e: Event) = scala.scalajs.js.timers.setTimeout(0) {
      dsNode.dispatchEvent(e)
    }

    private def _clearNode() = {
      while (dsNode.children.length > 0)
        dsNode.removeChild(dsNode.children(0))
    }

    private def _renderSuggestions(query: String, suggestions: Seq[U]) = {
      _clearNode()
      val selectables = _suggestionsToSelectable(suggestions)
      val fragment = _getSuggestionsFragment(query, selectables)
      selectables.zipWithIndex.map({ case (s, id) => s.elm.id = id.toString })
      _selectables = selectables
      _getHeader(query, suggestions).foreach(dsNode.appendChild)
      dsNode.appendChild(fragment)
      _getFooter(query, suggestions).foreach(dsNode.appendChild)
    }

    private def _getFooter(query: String, suggestions: Seq[U]) =
      ds.templates.footer.map(_(query, suggestions, ds.name))

    private def _getHeader(query: String, suggestions: Seq[U]) =
      ds.templates.header.map(_(query, suggestions, ds.name))

    private def _renderTemplate(query: String)
      (template: Function2[String, String, Element]) = {
      _selectables = Nil
      _clearNode()
      dsNode.appendChild(template(query, ds.name))
    }

    private def _appendSuggestions(query: String, suggestions: Seq[U]) = {
      val selectables = _suggestionsToSelectable(suggestions)
      val fragment = _getSuggestionsFragment(query, selectables)
      selectables.lastOption.flatMap(last => Option(last.elm.nextSibling)) match {
        case Some(footer) => dsNode.insertBefore(fragment, footer)
        case None => dsNode.appendChild(fragment)
      }
      _selectables = _selectables ++ selectables
    }

    private def _empty() = {
      _selectables = Nil
      _clearNode()
    }

    private def _suggestionsToSelectable(suggestions: Seq[U]) =
      suggestions.map({ suggestion => SelectableInst(
        ds.templates.suggestion(suggestion),
        ds.display(suggestion),
        suggestion
      ) })

    private def _getSuggestionsFragment(
      query: String,
      selectables: Seq[SelectableInst]
    ) = {
      val fragment = document.createDocumentFragment()
      selectables.foreach { s =>
        fragment.appendChild(s.elm)
      }
      if (highlight) fragment.childNodes.foreach {child => Highlight(
        child,
        Seq(Regex.quote(query).r),
        className=Some(classes.highlight)
      )}
      fragment
    }

    private def _overwrite(query: String, suggestions: Seq[U]) = {
      // got suggestions: overwrite dom with suggestions
      if (suggestions.nonEmpty)
        _renderSuggestions(query, suggestions)
      // no suggestions, expecting async: overwrite dom with pending
      else
        (if (ds.async.nonEmpty) ds.templates.pending else ds.templates.notFound)
          .map(_renderTemplate(query))
          .getOrElse(_empty())
      _fireEvent(new RenderEvent(
        s"$eventspace:rendered",
        ds,
        suggestions,
        false,
        null
      ))
    }

    private def _append(query: String, suggestions: Seq[U]) = {
      // got suggestions, sync suggestions exist: append suggestions to dom
      if (suggestions.nonEmpty && _selectables.nonEmpty)
        _appendSuggestions(query, suggestions)
      // got suggestions, no sync suggestions: overwrite dom with suggestions
      else if (suggestions.nonEmpty)
        _renderSuggestions(query, suggestions)
      // no async/sync suggestions: overwrite dom with not found
      else if (_selectables.isEmpty)
        ds.templates.notFound.map(_renderTemplate(query))
      _fireEvent(new RenderEvent(
        s"$eventspace:rendered",
        ds,
        suggestions,
        true,
        null
      ))
    }

    private var _cancel: Option[(() => Unit)] = None

    def cancel() = _cancel.foreach(_())

    def update(query: String) = {
      var rendered = 0

      cancel()

      val cancelThisUpdate: () => Unit = { () =>
        _cancel = None
        if (ds.async.nonEmpty)
          _fireEvent(new QueryEvent(s"$eventspace:asyncCanceled", query))
        // BLOCKED: Cancel the future (Scala API limitation)
      }
      _cancel = Some(cancelThisUpdate)

      val syncRes = ds.sync(query).slice(0, ds.limit)
      _overwrite(query, syncRes)
      if (syncRes.length < ds.limit) ds.async.foreach { fn =>
        _fireEvent(new QueryEvent(s"$eventspace:asyncRequested", query))
        fn(query).foreach({ asyncRes =>
          if (_cancel == Some(cancelThisUpdate)) {
            _cancel = None
            _append(query, asyncRes.slice(0, ds.limit - syncRes.length))
            _fireEvent(new QueryEvent(s"$eventspace:asyncRecieved", query))
          }
        })
      }
    }

    def clear() = {
      _empty()
      cancel()
      _fireEvent(new RenderEvent[U](s"$eventspace:cleared", ds, Nil, false, null))
    }

    def isEmpty = dsNode.children.length == 0

    def dispose() = {
      _selectables.foreach(_.dispose())
      cancel()
      clear()
      node.removeChild(dsNode)
    }
  }

  case class SelectableInst(
    val elm: HTMLElement,
    val display: String,
    val data: T
  ) extends Dropdown.Selectable[T] {
    elm.classList.add(s"${classes.suggestion}")
    elm.classList.add(s"${classes.selectable}")
    elm.addEventListener("click", onClick)

    def onClick(e: MouseEvent) = _fireEvent(new CursorEvent[T](
      s"$eventspace:selectableClicked", Some(this)
    ))

    def dispose() = elm.removeEventListener("click", onClick)
  }
}

object Dropdown {
  trait Selectable[+T] {
    val elm: HTMLElement
    val display: String
    val data: T
  }
}
