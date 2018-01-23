package org.multibot
import collection.JavaConverters._
object DiscordMultibot {
  def main(args: Array[String]) {
    import sx.blah.discord.Discord4J
    val getenv = java.lang.System.getenv(_: String)
    if (getenv("DISCORD_TRACE") == "true") Discord4J.LOGGER match {
      case d: Discord4J.Discord4JLogger =>
        d.setLevel(Discord4J.Discord4JLogger.Level.TRACE)
      case _ =>
        System.setProperty(
          org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY, "TRACE")
    }
    DiscordMultibot(getenv("DISCORD_TOKEN"))
    //println(ffbehelp())
  }
  def defparam(m: reflect.runtime.universe.Mirror,
    ffbe: reflect.runtime.universe.ModuleSymbol,
    name: String, pos: Int): String = {
    val u = reflect.runtime.universe.asInstanceOf[
      reflect.internal.SymbolTable with reflect.internal.StdNames]
    val n = reflect.runtime.universe.TermName(
      u.nme.defaultGetterName(u.TermName(name), pos + 1).toString)
    val mem = ffbe.typeSignature.member(n)
    val im = m.reflect(m.reflectModule(ffbe).instance)
    im.reflectMethod(mem.asMethod)().toString
  }

  def mapParams(m: reflect.runtime.universe.Mirror,
    ffbe: reflect.runtime.universe.ModuleSymbol,
    name: String,
    xs: List[reflect.runtime.universe.Symbol],
    withtype: Boolean = false): String = {
    "(" + xs.zipWithIndex.map { case(p, i) =>
      (if (p.asTerm.isParamWithDefault) {
        if (withtype)
          s"[${p.name.toString}: ${p.typeSignature} = ${defparam(m, ffbe, name, i)}]"
        else s"[${p.name.toString} = ${defparam(m, ffbe, name, i)}]"
      } else {
        if (withtype)
          p.name.toString + ": " + p.typeSignature
        else p.name.toString
      })
    }.mkString(", ") + ")"
  }
  def ffbehelp(): String = {
    import reflect.runtime.universe._
    val m = runtimeMirror(this.getClass.getClassLoader)
    val ffbe = m.staticModule("ffbe")
    "```scala\n" +
    ffbe.typeSignature.members.filter(s =>
      s.isMethod && s.isPublic && !s.isSynthetic &&
        s.asMethod.paramLists.nonEmpty &&
          s.asMethod.paramLists.head.nonEmpty &&
            s.owner.name.toString == "ffbe").foldLeft(
              (Set.empty[String],List.empty[String])) { case((seen,ac),s) =>
                if (seen(s.name.toString)) (seen,ac)
                else (
                  seen + s.name.toString,
                    "  " + s.fullName +
                      mapParams(m, ffbe, s.name.toString,
                        s.asMethod.paramLists.head) +
                          ": " + s.asMethod.returnType :: ac)
              }._2.mkString("\n") + "\n```"
  }
}
case class DiscordMultibot(token: String) {
  import sx.blah.discord.api._
  import sx.blah.discord.api.events.Event
  import sx.blah.discord.api.events.IListener
  import sx.blah.discord.handle.obj.IMessage
  import sx.blah.discord.handle.impl.events.guild.channel.message._
  import sx.blah.discord.handle.impl.events.ReadyEvent
  import collection.immutable.ListMap
  sys.props("scala.color") = "false"
  sealed trait Mode
  case object NEW    extends Mode
  case object UPDATE extends Mode
  val NUMLINES = 7
  val cache = InterpretersCache(
    "228587804681175041" :: // PM pfn0
    "205384125513859074" :: // r/ffbe
    Nil)
  val builder = new ClientBuilder
  builder.online("*help | https://scala-lang.org/")
  builder.withToken(token)
  var messages = ListMap.empty[Long,IMessage]

  def markdownOutputSanitizer(message: String): String =
    "```\n" + message
      .replace("\r", "")
      .replace("`", "\'")
      .split("\n")
      .filter(_.nonEmpty)
      .take(NUMLINES).mkString("\n") + "\n```"

  def serverID(m: IMessage) = {
    Option(m.getGuild).getOrElse(m.getAuthor).getStringID
  }

  def sourceOf(m: IMessage): String = {
    val s = Option(m.getGuild).fold(m.getAuthor.getName)(g =>
      g.getName + "@" +  m.getAuthor.getDisplayName(g))
    s"<$s>"
  }
  def log(m: IMessage): Unit = {
    val unknown = m.getContent
    import sx.blah.discord.Discord4J
    if (unknown.startsWith("*"))
      Discord4J.LOGGER.info(sourceOf(m) + " " + unknown)
  }

  def associateMessage(incoming: IMessage, response: IMessage) {
    messages += incoming.getLongID-> response
    messages = messages.takeRight(32)
  }

  def messageById(id: Long): Option[IMessage] = messages.get(id)

  def ratelimit[A](limited: => A) {
    import sx.blah.discord.util.RequestBuffer

    RequestBuffer.request(() => limited)
  }

  def scalaInterp(m: IMessage, mode: Mode) = {
    val msg = Msg(serverID(m), "sender-ignored", m.getContent.drop(1))
    DieOn.error {
      val res = cache.scalaInterpreter(msg.channel) { (si, cout) =>
        import scala.tools.nsc.interpreter.Results._

        si interpret msg.message match {
          case Success => cout.toString.replaceAll("(?m:^res[0-9]+: )", "")
          case Error => cout.toString.replaceAll("^<console>:[0-9]+: ", "")
          case Incomplete => "error: unexpected EOF found, incomplete expression"
        }
      }
      mode match {
        case NEW =>
          val ms = m.getMentions.asScala.foldLeft("") { (ac,s) =>
            s.mention + " " + ac }
          val newmessage = m.getChannel.sendMessage(
            m.getAuthor.mention + " " + ms + markdownOutputSanitizer(res))
          associateMessage(m, newmessage)
        case UPDATE =>
          messageById(m.getLongID).foreach { msg =>
            val ms = msg.getMentions.asScala.map(_.mention).mkString(" ")
            msg.edit(ms + " " + markdownOutputSanitizer(res))
          }
      }
    }
  }
  def interp(m: IMessage, mode: Mode) = {
    val h = InterpretersHandler(
      cache,
      (x, y) => {
        mode match {
          case NEW =>
            val ms = m.getMentions.asScala.foldLeft("") { (ac,s) =>
              s.mention + " " + ac }
            val prefix = m.getAuthor.mention + " " + ms
            val newmessage = m.getChannel.sendMessage(prefix +
              markdownOutputSanitizer(y.take(2000 - prefix.length - 8)))
            associateMessage(m, newmessage)
          case UPDATE =>
            messageById(m.getLongID).foreach { msg =>
              val ms = msg.getMentions.asScala.map(_.mention).mkString(" ")
              val prefix = ms + " "
              msg.edit(prefix + markdownOutputSanitizer(
                y.take(2000 - prefix.length - 8)))
            }
        }
      },
      GitterInputSanitizer.sanitize)
    DieOn.error {
      try {
        h.serve(Msg(serverID(m), "sender-ignored", m.getContent))
      } catch {
        case e: Exception => e.printStackTrace
      }
    }
  }

  builder.registerListener(new IListener[ReadyEvent] {
    import sx.blah.discord.Discord4J
    override def handle(event: ReadyEvent) = 
      Discord4J.LOGGER.info("multi-bot ready for action")
  })

  builder.registerListener(new IListener[MessageEvent] {
    override def handle(event: MessageEvent) = ratelimit { event match {
      case r: MessageReceivedEvent => 
        val m = r.getMessage
        val FFBE = """\*ffbe\..+""".r
        val newmessage = m.getContent match {
          case "*source" =>
            Some(m.reply("You can find my sources at: <https://github.com/pfn/multibot>"))
          case "*server" =>
            Some(m.reply(s"Current server ID: ${serverID(m)}"))
          case "*auth" =>
            Some(m.reply(
              s"Enable multi-bot on your discord: <https://discordapp.com/oauth2/authorize?client_id=${m.getClient.getOurUser.getStringID}&scope=bot&permissions=0>"))
          case "*ffbe" =>
            Some(m.reply(DiscordMultibot.ffbehelp()))
          case "*guilds" =>
            Some(m.reply(
              s"Connected guilds: ${m.getClient.getGuilds.asScala.map(_.getName).mkString(", ")}"))
          case "*help" =>
            Some(m.reply(
              """|```
                 |available commands:
                 |
                 | *scala, *s   evaluate a scala expression
                 | *source      show link to github sources
                 | *ffbe        list functions available in `*s ffbe`
                 | *js          evaluate a javascript expression
                 | *type        describe the type of a scala expression
                 | *reset       reset javascript and scala evaluator state
                 | *auth        show discord authorization link for server invite
                 | *help        duh
                 |```
                 |""".stripMargin))
          case FFBE() => 
            log(m)
            scalaInterp(m, NEW)
            None
          case unknown =>
            log(m)
            interp(m, NEW)
            None
        }
        newmessage.foreach(n => associateMessage(m, n))
      case d: MessageDeleteEvent =>
        messageById(d.getMessage.getLongID).foreach { msg =>
          msg.delete()
        }
        messages -= d.getMessage.getLongID
      case u: MessageUpdateEvent =>
        scalaInterp(u.getMessage, UPDATE)
        interp(u.getMessage, UPDATE)
      case _ =>
    }}
  })
  builder.login()
}
