package com.tflucke.typeahead

import org.scalajs.dom.document
import org.scalajs.dom.{Node,HTMLElement,Element,Text}
import scala.util.matching.Regex
import Util._

object Highlight {
  def apply(
    node: Node,
    patterns: Seq[Regex],
    tagName: String = "strong",
    className: Option[String] = None
  ) = {
    val regex = patterns.map({ r => s"(${r.regex})" }).mkString("|").r
    def _hightlightTextNode(textNode: Text): Node =
      regex.findFirstMatchIn(textNode.data).map({ mat =>
        val parent = textNode.parentNode.asInstanceOf[Element]
        val wrapper = document.createElement(tagName).asInstanceOf[HTMLElement]
        className.map({ name => wrapper.className = name })
        val before = textNode
        val pattern = before.splitText(mat.start)
        val next = pattern.splitText(mat.end - mat.start)
        parent.appendChild(before)
        wrapper.appendChild(pattern)
        parent.appendChild(wrapper)
        parent.appendChild(next)
        _hightlightTextNode(next)
      }).getOrElse(textNode)

    _traverse(node, _hightlightTextNode)

  }

  private def _traverse(el: Node, hightlightTextNode: (Text => Node)): Unit = {
    val TEXT_NODE_TYPE = 3;
    if (el.nodeType == TEXT_NODE_TYPE)
      Option(hightlightTextNode(el.asInstanceOf[Text]).nextSibling)
        .map(_traverse(_, hightlightTextNode))
    else {
      Option(el.firstChild).map(_traverse(_, hightlightTextNode))
      Option(el.nextSibling).map(_traverse(_, hightlightTextNode))
    }
  }
}
