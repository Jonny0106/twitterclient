package example
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http
import com.twitter.util.{Await, Future}
import com.twitter.finagle.builder
import com.twitter.util.Future
import scala.util.parsing.json.JSON

object Hello extends App {
  println("Hello worlda")

  val twitterReader = new TwitterReader()
  twitterReader.readRecent("from:twitter")

  val filterContent = 
    """
    |{
    | "add": [
    |   {"value": "from:twitter"}
    | ]
    |}
    """.stripMargin
  twitterReader.streamTweets(filterContent)
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
    val uri =  "/2/tweets/search/recent?query=" + filter
    val res = TwitterClient.requestGet(uri)
    res.setContentTypeJson()
    val jsonData = JSON.parseFull(res.contentString)
    val jsonMap = jsonData.get.asInstanceOf[Map[String,Any]]
    val tweetList = jsonMap.get("data") match {
      case Some(listTweet) => listTweet.asInstanceOf[List[Map[String,String]]]
      case None            => List(Map()).asInstanceOf[List[Map[String,String]]]
    }
  
    tweetList.foreach(tweetMap => println(tweetMap.get("text").get))
  }

  def streamTweets(filter: String) {
    val postUri = "/2/tweets/search/stream/rules"
    val headerMap = Map("Content-Type" -> "application/json")
    val res = TwitterClient.requestPost(postUri, headerMap, filter)
    println("got res" + res + "\n" + res.contentString)
    
    val streamUri = "/2/tweets/search/stream"
    val streamResponse = TwitterClient.requestGet(streamUri)
    println("reach here")
    println(streamResponse + "\n" + streamResponse.contentString)
  
  }
}


object TwitterClient {
  private def clientFactory(): Service[http.Request, http.Response] = 
    Http.client
    .withTransport.tls("api.twitter.com")
    .newService("api.twitter.com:443")
  
  private val bearer = "Bearer " + sys.env.get("TWITTER_BEARER").get
  
  def requestGet(uri: String): http.Response = {
    val client = clientFactory()
    val request = http.Request(http.Method.Get, uri)
    request.headerMap.put("Authorization", bearer)
    val response = client(request)
    val rep = Await.result(response)
    client.close()
    rep
  }

  def requestPost(uri: String, headerMap: Map[String,String], contentString: String): http.Response = {
    val client = clientFactory()
    val request = http.Request(http.Method.Post, uri)
    headerMap.foreach(header => request.headerMap.put(header._1, header._2))
    request.headerMap.put("Authorization", bearer)
    request.contentString = contentString
    request.setContentTypeJson()
    val response = client(request)
    val rep = Await.result(response)
    client.close()
    rep
  }
  
}