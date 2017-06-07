package org.multibot

import org.json4s.native.JsonMethods._
import org.json4s.JsonAST._
import org.json4s.JsonDSL._

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
  http: HttpHandler,
  sendLines: (String, String) => Unit,
  inputSanitizer: String => String) {
  import InterpretersHandler._

  def serve(implicit msg: Msg): Unit = msg.message match {
    case Cmd("*scala" :: m :: Nil) => sendLines(msg.channel, cache.scalaInterpreter(msg.channel) { (si, cout) =>
      import scala.tools.nsc.interpreter.Results._

      si interpret inputSanitizer(m) match {
        case Success => cout.toString.replaceAll("(?m:^res[0-9]+: )", "")
        case Error => cout.toString.replaceAll("^<console>:[0-9]+: ", "")
        case Incomplete => "error: unexpected EOF found, incomplete expression"
      }
    })

    case Cmd("*type" :: m :: Nil) => sendLines(msg.channel, cache.scalaInterpreter(msg.channel)((si, cout) => si.typeOfExpression(m).directObjectString))
    case "*reset" =>
      cache.scalaInt invalidate msg.channel
      cache.jsInt invalidate msg.channel
    case "*reset-all" =>
      cache.scalaInt.invalidateAll()
      cache.jsInt.invalidateAll()

    case Cmd("*clj" :: m :: Nil) => http.respond(sendLines).respondJSON(:/("www.tryclj.com") / "eval.json" <<? Map("expr" -> m)) {
      case JObject(JField("expr", JString(_)) :: JField("result", JString(result)) :: Nil) => Some(result)
      case JObject(JField("error", JBool(true)) :: JField("message", JString(message)) :: Nil) => Some(message)
      case e => Some("unexpected: " + e)
    }

    case Cmd("*hs" :: m :: Nil) => http.respond(sendLines).respondJSON(:/("tryhaskell.org") / "eval" <<? Map("exp" -> m)) {
      case JObject(
      JField("success",
      JObject(
      JField("expr", JString(_))
        :: JField("stdout", JArray(out))
        :: JField("value", JString(result))
        :: JField("files", _)
        :: JField("type", JString(xtype))
        :: Nil))
        :: Nil) => Some(s"$result :: $xtype " + out.collect { case JString(s) => s}.mkString("\n", "\n", ""))
      case JObject(JField("error", JString(error)) :: Nil) => Some(error)
      case e => Some("unexpected: " + e)
    }

    case Cmd("*ruby" :: m :: Nil) => http.respond(sendLines).respondJSON(:/("tryruby.org") / "/levels/1/challenges/0" <:<
      Map("Accept" -> "application/json, text/javascript, */*; q=0.01",
        "Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
        "X-Requested-With" -> "XMLHttpRequest",
        "Connection" -> "keep-alive") <<< "cmd=" + java.net.URLEncoder.encode(m, "UTF-8")) {
      case JObject(JField("success", JBool(true)) :: JField("output", JString(output)) :: _) => Some(output)
      case JObject(JField("success", JBool(false)) :: _ :: JField("result", JString(output)) :: _) => Some(output)
      case e => Some("unexpected: " + e)
    }

    case Cmd("*idris" :: m :: Nil) => http.respond(sendLines).respondJSON(:/("tryidris.herokuapp.com") / "interpret" << compact(render("expression", m))) {
      case JArray(List(JArray(List(JString(":return"), JArray(List(JString(_), JString(output), _*)), _*)), _*)) => Some(output)
      case e => Some("unexpected: " + e)
    }
    case Cmd("*js" :: m :: Nil) => sendLines(msg.channel, cache.jsInterpreter(msg.channel) { (ji, cout) =>
      try {
        val result = Option(ji eval inputSanitizer(m)).fold("")(_.toString)
        val out = cout.toString
        ((if (result.nonEmpty) "> " + result else "") + "\n" +
          (if (out.nonEmpty) "out: " + out else "")).trim().take(1024)
      } catch {
        case e: javax.script.ScriptException => e.getMessage
      }
    })

    case Cmd("*py" :: m :: Nil) => http.respond(sendLines).respondJSON2(:/("try-python.appspot.com") / "json" << compact(render(("method", "exec") ~("params", List(pythonSession, m)) ~ ("id" -> "null"))),
      :/("try-python.appspot.com") / "json" << compact(render(("method", "start_session") ~("params", List[String]()) ~ ("id" -> "null")))) {
      case JObject(JField("error", JNull) :: JField("id", JString("null")) :: JField("result", JObject(JField("text", JString(result)) :: _)) :: Nil) => Some(result)
      case e => Some("unexpected: " + e)
    } {
      case JObject(_ :: _ :: JField("result", JString(session)) :: Nil) => pythonSession = session; Some("> python session initialized, try again")
      case e => None
    }

    case Cmd("*groovy" :: m :: Nil) => http.respond(sendLines).respondJSON(:/("groovyconsole.appspot.com") / "executor.groovy" <<? Map("script" -> m), true) {
      case JObject(JField("executionResult", JString(result)) :: JField("outputText", JString(output)) :: JField("stacktraceText", JString("")) :: Nil) => Some(result.trim + "\n" + output.trim)
      case JObject(JField("executionResult", JString("")) :: JField("outputText", JString("")) :: JField("stacktraceText", JString(err)) :: Nil) => Some(err)
      case e => Some("unexpected" + e)
    }

    case _ =>
  }
}
