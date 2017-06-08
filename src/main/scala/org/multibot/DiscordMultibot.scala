package org.multibot
import collection.JavaConverters._
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
              val ms = msg.getMentions.asScala.map(_.mention).mkString(" ")
              msg.edit(ms + " " + markdownOutputSanitizer(y))
            }
          case DELETE =>
        }
      },
      GitterInputSanitizer.sanitize)
    DieOn.error {
      try {
        h.serve(Msg("general", "sender-ignored", m.getContent))
      } catch {
        case e: Exception => e.printStackTrace
      }
    }
  }

  val client = builder.login()
  val dispatcher = client.getDispatcher
  dispatcher.registerListener(new IListener[MessageEvent] {
    override def handle(event: MessageEvent) = event match {
      case r: MessageReceivedEvent =>
        val m = r.getMessage
        if (m.getContent == "*auth") {
          m.reply(
            s"Enable multi-bot on your discord: <https://discordapp.com/oauth2/authorize?client_id=${m.getClient.getOurUser.getStringID}&scope=bot&permissions=0>")
        }
        if (m.getContent == "*help") {
          val nm = m.reply(
            """|```
               |available commands:
               |
               | *scala     evaluate a scala expression
               | *js        evaluate a javascript expression
               | *ruby      evaluate a ruby expression
               | *clj       evaluate a clojure expression
               | *hs        evaluate a haskell expression
               | *idris     evaluate a idris expression
               | *py        evaluate a python expression
               | *groovy    evaluate a groovy expression
               | *type      describe the type of a scala expression
               | *reset     reset javascript and scala evaluator state
               | *auth      show discord authorization link for server invite
               | *help      duh
               |```
               |""".stripMargin)
          messages += m.getLongID -> nm
          messages = messages.takeRight(32)
        }
        interp(m, NEW)
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
