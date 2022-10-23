package com.tflucke.typeahead

import org.scalajs.dom.DOMList

object Util {
  implicit class DomListIterator[T](list: DOMList[T]) extends Iterator[T] {
    private var _i = 0

    def hasNext = _i < list.length

    def next() = {
      val res = list(_i)
      _i = _i + 1
      res
    }
  }
}
