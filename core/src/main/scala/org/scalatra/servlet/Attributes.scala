package org.scalatra.servlet

import org.scalatra.ServletCompat.ServletContext
import org.scalatra.ServletCompat.http.{ HttpServletRequest, HttpSession }

import java.util

/**
 * type class for the various Servlet API objects that have attributes.
 */
private[servlet] trait Attributes[A] {
  def getAttribute(self: A, name: String): AnyRef
  def getAttributeNames(self: A): java.util.Enumeration[String]
  def setAttribute(self: A, name: String, value: AnyRef): Unit
  def removeAttribute(self: A, name: String): Unit
}

private[servlet] object Attributes {
  @inline def apply[A](implicit a: Attributes[A]): Attributes[A] = a

  implicit class AttributesOps[A](private val self: A) extends AnyVal {
    def getAttribute(name: String)(implicit a: Attributes[A]): AnyRef =
      a.getAttribute(self, name)
    def getAttributeNames()(implicit a: Attributes[A]): java.util.Enumeration[String] =
      a.getAttributeNames(self)
    def setAttribute(name: String, value: AnyRef)(implicit a: Attributes[A]): Unit =
      a.setAttribute(self, name, value)
    def removeAttribute(name: String)(implicit a: Attributes[A]): Unit =
      a.removeAttribute(self, name)
  }

  implicit val httpServletRequestAttributes: Attributes[HttpServletRequest] =
    new Attributes[HttpServletRequest] {
      override def getAttribute(self: HttpServletRequest, name: String): AnyRef =
        self.getAttribute(name)
      override def getAttributeNames(self: HttpServletRequest): util.Enumeration[String] =
        self.getAttributeNames()
      override def setAttribute(self: HttpServletRequest, name: String, value: AnyRef): Unit =
        self.setAttribute(name, value)
      override def removeAttribute(self: HttpServletRequest, name: String): Unit =
        self.removeAttribute(name)
    }

  implicit val servletContextAttributes: Attributes[ServletContext] =
    new Attributes[ServletContext] {
      override def getAttribute(self: ServletContext, name: String): AnyRef =
        self.getAttribute(name)
      override def getAttributeNames(self: ServletContext): util.Enumeration[String] =
        self.getAttributeNames()
      override def setAttribute(self: ServletContext, name: String, value: AnyRef): Unit =
        self.setAttribute(name, value)
      override def removeAttribute(self: ServletContext, name: String): Unit =
        self.removeAttribute(name)
    }

  implicit val httpSessionAttributes: Attributes[HttpSession] =
    new Attributes[HttpSession] {
      override def getAttribute(self: HttpSession, name: String): AnyRef =
        self.getAttribute(name)
      override def getAttributeNames(self: HttpSession): util.Enumeration[String] =
        self.getAttributeNames()
      override def setAttribute(self: HttpSession, name: String, value: AnyRef): Unit =
        self.setAttribute(name, value)
      override def removeAttribute(self: HttpSession, name: String): Unit =
        self.removeAttribute(name)
    }
}
