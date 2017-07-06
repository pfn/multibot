package org.multibot

/*
import org.json4s.JsonAST.JValue
import org.json4s.native.JsonParser
*/

object :/ {
  def apply(host: String) = Request(host, None, Map.empty, Map.empty, None, false)
}

case class Request(host: String, path: Option[String], headers: Map[String,String], query: Map[String,String], post: Option[String], file: Boolean) {
  def /(p: String) = copy(path = Some(path.fold(p) { parent => parent + "/" + p }))
  def <<?(query: Map[String, String]) = copy(query = query ++ query)
  def <<(p: String): Request = copy(post = Some(p))
  def <<<(f: String): Request = copy(post = Some(f), file = true)
  def <:<(h: Map[String, String]) = copy(headers = headers ++ h)
  def asURLConnection: java.net.HttpURLConnection = {
    val base = "http://" + host
    val withpath = path.fold(base) { p => base + "/" + p }
    def enc(s: String) = java.net.URLEncoder.encode(s, "utf-8")
    val q = query.toList.map { case (k,v) => enc(k) + "=" + enc(v) }.mkString("&")
    val withquery = if (q.isEmpty) withpath else withpath + "?" + q
    val uc = new java.net.URL(withquery).openConnection.asInstanceOf[java.net.HttpURLConnection]
    uc.setRequestProperty("User-Agent", "Mozilla")
    headers.toList.foreach { case (k,v) =>
      uc.setRequestProperty(k, v)
    }
    post.foreach { body =>
      uc.setRequestMethod(if (file) "PUT" else "POST")
      uc.setDoOutput(true)
      val osw = new java.io.OutputStreamWriter(uc.getOutputStream, "utf-8")
      osw.write(body)
      osw.close()
    }
    uc
  }
}

/*
case class HttpHandler() {
  private val NUMLINES = 5
  private val INNUMLINES = 8
  private val cookies = scala.collection.mutable.Map[String, String]()

  case class respond(sendMessage: (String, String) => Unit) {

    def respondJSON(req: Request, join: Boolean = false)(response: JValue => Option[String])(implicit msg: Msg) = respond(req, join) {
      line => response(JsonParser.parse(line))
    }

    def respondJSON2(req: Request, init: Request)(response: JValue => Option[String])(initResponse: JValue => Option[String])(implicit msg: Msg) = try {
      respond(req) {
        line => response(JsonParser.parse(line))
      }
    } catch {
      case t: Throwable =>
        respond(init) {
          line => initResponse(JsonParser.parse(line))
        }
        respond(req) {
          line => response(JsonParser.parse(line))
        }
    }

    def respond(req: Request, join: Boolean = false)(response: String => Option[String])(implicit msg: Msg) = {
      val channel = msg.channel
      val host = req.host

      val uc = req.asURLConnection

      cookies.get(channel + host) foreach (c => uc.setRequestProperty("Cookie", c))
      import collection.JavaConverters._
      uc.getHeaderFields.asScala.get("Set-Cookie").foreach(h => h.asScala.foreach(c => cookies(channel + host) = c.split(";").head))
      import scala.concurrent.Future
      import scala.concurrent.ExecutionContext.Implicits.global

      val source = io.Source.fromInputStream(uc.getInputStream)
      val lines = source.getLines.take(NUMLINES)
        (if (join) List(lines.mkString) else lines).foreach { line =>
          response(line).foreach { l =>
            val r = l.split("\n").take(INNUMLINES).mkString("\n")
            sendMessage(channel, r)
          }
      }
    }
  }
}
*/
