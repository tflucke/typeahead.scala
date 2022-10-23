package com.tflucke.typeahead

import scala.concurrent.Future
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.document

case class Dataset[T](
  val sync: (String => Seq[T]),
  val async: Option[(String => Future[Seq[T]])],
  val templates: Dataset.Templates[T],
  val name: String,
  val limit: Int,
  val display: (T => String)
)

object Dataset {
  private val _nameGenerator = new Iterator[Int] {
    private var _i = 0
    def hasNext = true
    def next(): Int = { _i += 1; _i }
  }

  def apply[T](
    sync: (String => Seq[T]),
    async: Option[(String => Future[Seq[T]])] = None,
    templates: Option[Dataset.Templates[T]] = None,
    name: String = Dataset._nameGenerator.next().toString,
    limit: Int = 5,
    display: (T => String) = {x: T => x.toString}
  ) = new Dataset[T](
    sync,
    async,
    templates.getOrElse(Dataset.Templates(display)),
    name,
    limit,
    display
  )

  case class Templates[T](
    /** Returns an element which represents the argument in a dropdown menu.
      */
    val suggestion: Function1[T, HTMLElement],
    /** Returns an element which informs the user that no results were found.
      * 
      * The first argument is the user input.
      * The second argument is the dataset name.
      */
    val notFound: Option[Function2[String, String, HTMLElement]] = None,
    /** Returns an element which informs the user that asynchronous results are
      * still pending.
      * 
      * The first argument is the user input.
      * The second argument is the dataset name.
      */
    val pending: Option[Function2[String, String, HTMLElement]] = None,
    /** Return an element which will be inserted at the top of each list of results
     * from a dataset.
      * 
      * The first argument is the user input.
      * The second argument contains all of the suggestions found thus far.
      * The third argument is the dataset name.
      */
    val header: Option[Function3[String, Seq[T], String, HTMLElement]] = None,
    /** Return an element which will be inserted at the bottom of each list of
      *  results from a dataset.
      * 
      * The first argument is the user input.
      * The second argument contains all of the suggestions found thus far.
      * The third argument is the dataset name.
      */
    val footer: Option[Function3[String, Seq[T], String, HTMLElement]] = None
  )

  object Templates{
    def apply[T](displayFn: (T => String)): Templates[T] = Templates({ x: T =>
      val html = document.createElement("div").asInstanceOf[HTMLElement]
      html.innerText = displayFn(x)
      html
    })
  }
}
