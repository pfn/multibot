package org.multibot
object DiscordMultibot {
  def main(args: Array[String]) {
    DiscordMultibot(java.lang.System.getenv("DISCORD_TOKEN"))
  }
}
case class DiscordMultibot(token: String) {
  import sx.blah.discord.api._
  import sx.blah.discord.api.events.Event
  import sx.blah.discord.api.events.IListener
  import sx.blah.discord.handle.obj.IMessage
  import sx.blah.discord.handle.impl.events.guild.channel.message._
  import collection.immutable.ListMap
  sys.props("scala.color") = "false"
  sealed trait Mode
  case object NEW    extends Mode
  case object UPDATE extends Mode
  case object DELETE extends Mode
  val NUMLINES = 5
  val cache = InterpretersCache(List("general"))
  val builder = new ClientBuilder
  builder.withToken(token)
  var messages = ListMap.empty[Long,IMessage]

  def markdownOutputSanitizer(message: String): String =
    "```\n" + message
      .replace("\r", "")
      .replace("`", "\'")
      .split("\n")
      .filter(_.nonEmpty)
      .take(NUMLINES).mkString("\n") + "\n```"
      

  def interp(m: IMessage, mode: Mode) = {
    val h = InterpretersHandler(
      cache, HttpHandler(),
      (x, y) => {
        mode match {
          case NEW =>
            val newmessage = m.reply(markdownOutputSanitizer(y))
            messages += m.getLongID -> newmessage
            messages = messages.takeRight(32)
          case UPDATE =>
            messages.get(m.getLongID).foreach { msg =>
              msg.edit(markdownOutputSanitizer(y))
            }
          case DELETE =>
        }
      },
      GitterInputSanitizer.sanitize)
    h.serve(Msg("sender", "general", m.getContent))
  }

  val client = builder.login()
  val dispatcher = client.getDispatcher
  dispatcher.registerListener(new IListener[MessageEvent] {
    override def handle(event: MessageEvent) = event match {
      case r: MessageReceivedEvent =>
        interp(r.getMessage, NEW)
      case d: MessageDeleteEvent =>
        messages.get(d.getMessage.getLongID).foreach { msg =>
          msg.delete()
        }
        messages -= d.getMessage.getLongID
      case u: MessageUpdateEvent =>
        interp(u.getMessage, UPDATE)
      case _ =>
    }
  })
}
