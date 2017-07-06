package org.multibot

case class Msg(channel: String, sender: String, message: String)
object Cmd {
  def unapply(s: String) = if (s.contains(' ')) Some(s.split(" ", 2).toList) else None
}
