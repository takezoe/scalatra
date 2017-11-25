package org.scalatra.forms

import javax.servlet.http.HttpServletRequest

import org.scalatra.MultiParams

import scala.xml._

/**
 * Provides view helpers to render form elements in XML literal.
 */
object XmlFormsHelpers {

  /**
   * Render a text field.
   */
  def text(name: String, attributes: (String, String)*)(implicit request: HttpServletRequest): Elem = {
    addAttributes(<input type="text" name={ name } value={ param(name) }></input>, attributes: _*)
  }

  /**
   * Render a password field.
   */
  def password(name: String, attributes: (String, String)*)(implicit request: HttpServletRequest): Elem = {
    addAttributes(<input type="password" name={ name }></input>, attributes: _*)
  }

  /**
   * Render a textarea.
   */
  def textarea(name: String, attributes: (String, String)*)(implicit request: HttpServletRequest): Elem = {
    addAttributes(<textarea name={ name }>{ param(name) }</textarea>, attributes: _*)
  }

  /**
   * Render a checkbox.
   */
  def checkbox(name: String, value: String, attributes: (String, String)*)(implicit request: HttpServletRequest): Elem = {
    val attrs = if (params(name).contains(value)) attributes :+ ("checked" -> "checked") else attributes
    addAttributes(<input type="checkbox" name={ name } value={ value }></input>, attrs: _*)
  }

  /**
   * Render a radio button.
   */
  def radio(name: String, value: String, attributes: (String, String)*)(implicit request: HttpServletRequest): Elem = {
    val attrs = if (params(name).contains(value)) attributes :+ ("checked" -> "checked") else attributes
    addAttributes(<input type="radio" name={ name } value={ value }></input>, attrs: _*)
  }

  /**
   * Render a select box.
   */
  def select(name: String, values: Seq[(String, String)], multiple: Boolean, attributes: (String, String)*)(implicit request: HttpServletRequest): Elem = {
    val attrs = if (multiple) attributes :+ ("multiple" -> "multiple") else attributes
    addAttributes(
      <select name={ name }>
        {
          values.map {
            case (value, label) =>
              addAttributes(<option value={ value }>{ label }</option>, (if (params(name).contains(value)) Seq("selected" -> "selected") else Nil): _*)
          }
        }
      </select>, attrs: _*)
  }

  /**
   * Retrieve an error message of the specified field.
   */
  def error(name: String)(implicit request: HttpServletRequest): Option[String] = {
    Option(request.getAttribute(RequestAttributeErrorsKey)).flatMap { errors =>
      errors.asInstanceOf[Seq[(String, String)]].find(_._1 == name).map(_._2)
    }
  }

  /**
   * Retrieve all error messages of the specified field.
   */
  def errors(name: String)(implicit request: HttpServletRequest): Seq[String] = {
    Option(request.getAttribute(RequestAttributeErrorsKey)).map { errors =>
      errors.asInstanceOf[Seq[(String, String)]].collect { case error if error._1 == name => error._2 }
    }.getOrElse(Nil)
  }

  private def addAttributes(elem: Elem, attributes: (String, String)*): Elem = {
    elem.copy(attributes = elem.attributes.copy(
      attributes.foldLeft(Null: MetaData) { case (next, (key, value)) => Attribute(key, Text(value), next) }))
  }

  private def params(name: String)(implicit request: HttpServletRequest): Seq[String] = {
    Option(request.getAttribute(RequestAttributeParamsKey)).flatMap { params =>
      params.asInstanceOf[MultiParams].get(name)
    }.getOrElse(Nil)
  }

  private def param(name: String)(implicit request: HttpServletRequest): String = {
    params(name).headOption.getOrElse("")
  }

}
