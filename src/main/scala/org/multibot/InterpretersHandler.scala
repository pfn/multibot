package org.multibot

/** This allows users to write code blocks in
  *  gitter that get parsed correctly here */
object GitterInputSanitizer {
  def sanitize(in: String): String = {
    val tripleBackquoted = """^```\S*\r?\n?(?s)(.*)```$""".r
    val singleBackquoted = "^`(.*)`$".r

    in match {
      case tripleBackquoted(s) => s
      case singleBackquoted(s) => s
      case s => s
    }
  }
}

object InterpretersHandler {
  private var pythonSession = "" // ugh
}
case class InterpretersHandler(
  cache: InterpretersCache,
  /*http: HttpHandler,*/
  sendLines: (String, String) => Unit,
  inputSanitizer: String => String) {
  import InterpretersHandler._
  object Sanitize {
    def unapply(s: String): Option[String] = Some(inputSanitizer(s))
  }

  def serve(implicit msg: Msg): Unit = msg.message match {
    case Cmd(("*scala" | "*s") :: Sanitize(m) :: Nil) => sendLines(msg.channel, cache.scalaInterpreter(msg.channel) { (si, cout) =>
      import scala.tools.nsc.interpreter.Results._

      si interpret m match {
        case Success => cout.toString.replaceAll("(?m:^res[0-9]+: )", "")
        case Error => cout.toString.replaceAll("^<console>:[0-9]+: ", "")
        case Incomplete => "error: unexpected EOF found, incomplete expression"
      }
    })

    case Cmd("*type" :: Sanitize(m) :: Nil) => sendLines(msg.channel, cache.scalaInterpreter(msg.channel)((si, cout) => si.typeOfExpression(m).directObjectString))
    case "*reset" =>
      cache.scalaInt invalidate msg.channel
      cache.jsInt invalidate msg.channel
    case "*reset-all" =>
      cache.scalaInt.invalidateAll()
      cache.jsInt.invalidateAll()

    case Cmd("*js" :: Sanitize(m) :: Nil) => sendLines(msg.channel, cache.jsInterpreter(msg.channel) { (ji, cout) =>
      try {
        val result = Option(ji eval m).fold("")(_.toString)
        val out = cout.toString
        ((if (result.nonEmpty) "> " + result else "") + "\n" +
          (if (out.nonEmpty) "out: " + out else "")).trim().take(1024)
      } catch {
        case e: javax.script.ScriptException => e.getMessage
      }
    })

    case _ =>
  }
}
