package example
import com.twitter.io
import com.twitter.io.Buf
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http
import com.twitter.finagle.http.HeaderMap
import com.twitter.util.{Await, Future}
import com.twitter.finagle.builder
import com.twitter.util.Future
import scala.util.parsing.json.JSON
import com.twitter.finagle
import com.acred.twitterClient.TwitterClient

object Hello extends App {
  println("Hello worlda")

  val twitterReader = new TwitterReader()
  // twitterReader.readRecent("from:twitter")

  def printWithHello(str: String): Boolean = {
    println(str)
    println("Hello")
    str.contains("done")
  } 
  val filterContent = 
    """
    |{
    | "add": [
    |   {"value": "from:twitter"}
    | ]
    |}
    """.stripMargin
  twitterReader.streamTweets(filterContent, printWithHello)
  println("done")
}


class TwittewrWriter {

  def tweet(tweetContent: String) {

  }

  def reply(tweetContent: String, tweetId: String) {

  }
}

class TwitterReader {

  def readTweet(tweetId: String) {
    
  } 

  def readRecent(filter: String) {
    val twitterClient = new TwitterClient(false)
    val uri =  "/2/tweets/search/recent?query=" + filter
    val result = twitterClient.requestGet(uri, false)
    jsonParser(result)
    tweetList.foreach(tweetMap => println(tweetMap.get("text").get))
    twitterClient.close()
  }

  def streamTweets(filter: String, procces: (String) => Boolean) {
    val twitterClient = new TwitterClient(true)
    val postUri = "/2/tweets/search/stream/rules"
    val headerMap = Map("Content-Type" -> "application/json")
    val res = twitterClient.requestPost(postUri, headerMap, filter)
    println("got res" + res + "\n" + res.contentString)
    
    val streamUri = "/2/tweets/search/stream"
    val streamResponse = twitterClient.requestGet(streamUri, true)
    println("reach here")
    val r = streamResponse.chunkReader

    def loop(acc: Buf, trailers: Option[HeaderMap]): Future[(Buf, Option[HeaderMap])] =
      r.read().flatMap {
        case Some(chunk) =>
          if (chunk.isLast && !chunk.trailers.isEmpty)
            loop(acc.concat(chunk.content), Some(chunk.trailers))
          else {
            val Buf.Utf8(str) = chunk.content
            val isDone = procces(str)
            if (!isDone){
              println("not done")
              loop(acc.concat(chunk.content), None)
            }
            else{
              println("done")
              Future.value(acc -> trailers)
            }
          }
        case None =>
          Future.value(acc -> trailers)
      }
    Await.result(loop(Buf.Empty, None))

    println("reach here end!!!")
    twitterClient.close()
  }

  private def jsonParser(result: String): List[Map[String,String]] = {
    val jsonData = JSON.parseFull(result)
    val jsonMap = jsonData.get.asInstanceOf[Map[String,Any]]
    val tweetList = jsonMap.get("data") match {
      case Some(listTweet) => listTweet.asInstanceOf[List[Map[String,String]]]
      case None            => List(Map()).asInstanceOf[List[Map[String,String]]]
    }
  }

}