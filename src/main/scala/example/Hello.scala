package example
import scala.collection.immutable.ListMap
import java.net.URLEncoder
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
import scala.concurrent.ExecutionContext.Implicits.global
import java.net.URLEncoder
import javax.crypto.spec.SecretKeySpec
import javax.crypto.Mac
import org.apache.commons.codec.binary.Base64




object Hello extends App {

  def percentEncode(str: String): String = {
    URLEncoder.encode(str, "UTF-8").replace("+", "%20")
  }
  val oauthMap = Map(
    "include_entities"       -> "true",
    "oauth_consumer_key"     -> sys.env.get("TWITTER_CONSUMER_KEY").get,
    "oauth_nonce"            -> System.nanoTime.toString,
    "oauth_signature_method" -> "HMAC-SHA1",
    "oauth_timestamp"        -> (System.currentTimeMillis / 1000).toString,
    "oauth_token"            -> sys.env.get("TWITTER_ACCESS_TOKEN").get,
    "oauth_version"          -> "1.0",
    "status"                 -> "My First tweet was ok"
  )


  val secretMap = Map(
    "consumer_secret"        -> sys.env.get("TWITTER_CONSUMER_KEY_SECRET").get,
    "oauth_token_secret"     -> sys.env.get("TWITTER_TOKEN_SECRET").get
  )
  def createSignitureBase(oauthMap: Map[String, String]):String = {
    val url = "https://api.twitter.com/1.1/statuses/update.json"
    val postUrl = "POST&" + percentEncode(url)
    val orderedMap = ListMap(oauthMap.toSeq.sortWith(_._1 < _._1):_*)
    val encode = orderedMap.map { pair => 
      (percentEncode(pair._1) + "=" + percentEncode(pair._2))
    }.reduce { 
      (x, y) => x + "&" + y
    }

    postUrl + "&" + percentEncode(encode)
  }

  def createSigningKey(secretMap: Map[String, String]):String = {
    val consumer_key = secretMap.getOrElse("consumer_secret", "")
    val token_secret = secretMap.getOrElse("oauth_token_secret", "")

    percentEncode(consumer_key) + "&" + percentEncode(token_secret)
  }

  def createHeader(oauthMap: Map[String, String]): String = {
  
    
    val header = oauthMap.filter{ x => 
      x._1 != "include_entities" && x._1 != "status"
    }.map { pair => percentEncode(pair._1) + "=\"" + percentEncode(pair._2) + "\""
    }reduce { 
      (str1, str2) => (str1) + "," + (str2)
    }
    "OAuth " + header

  }

  def base64encode(s: Array[Byte]) = {
    Base64.encodeBase64String(s)
  }
  
  def hashing(data: String, key: String): String = {
    val hmacShai = "HmacSHA1"
    val keyStr = new SecretKeySpec(key.getBytes(), hmacShai)
    val sig = {
      val mac = Mac.getInstance(hmacShai)
      mac.init(keyStr)
      new String(base64encode(mac.doFinal(data.getBytes())))
    }
    sig
  }
  
  val base = createSignitureBase(oauthMap)
  val key = createSigningKey(secretMap)
  
  val signiture = hashing(base, key)
  val newOauthMap = oauthMap + ("oauth_signature" -> signiture)
  val header = createHeader(newOauthMap)
  println("base: " + base)
  println("key: "  + key)
  println("signiture: " + signiture)
  println("header: " + header)

  val twitter = new TwitterWriter()

  val status = percentEncode(oauthMap.getOrElse("status", ""))
  twitter.tweet(status, header)
  // println("Hello worlda")

  // val twitterReader = new TwitterReader()
  // // twitterReader.readRecent("from:twitter")

  // def printWithHello(str: String): Boolean = {
  //   println(str)
  //   println("Hello")
  //   str.contains("done")
  // } 
  // val filterContent = 
  //   """
  //   |{
  //   | "add": [
  //   |   {"value": "from:twitter"}
  //   | ]
  //   |}
  //   """.stripMargin

  // twitterReader.streamTweets(filterContent, printWithHello)
  println("done")
}


class TwitterWriter {

  def tweet(tweetContent: String, header:String) {
    val twitterClient = new TwitterClient(false)
    val postUri = "/1.1/statuses/update.json?include_entities=true"
    val headerMap = Map(
      "Content-Type" -> "application/x-www-form-urlencoded",
      "authorization" -> header
    )
    val res = twitterClient.requestPostTweet(postUri, headerMap, tweetContent)
    println(res.contentString)
  }

  def reply(tweetContent: String, tweetId: String) {

  }

  def changeStreamFilter(filter: String): http.Response = {
    val twitterClient = new TwitterClient(false)
    val postUri = "/2/tweets/search/stream/rules"
    val headerMap = Map(
      "Content-Type" -> "application/x-www-form-urlencoded")
    twitterClient.requestPost(postUri, headerMap, filter)
  }
  
}

class TwitterReader {

  def readTweet(tweetId: String) {
    
  } 

  def readRecent(filter: String) {
    val twitterClient = new TwitterClient(false)
    val uri =  "/2/tweets/search/recent?query=" + filter
    val result = twitterClient.requestGet(uri, false)
    val tweetList = jsonParser(result.contentString)
    tweetList.foreach(tweetMap => println(tweetMap.get("text").get))
    twitterClient.close()

  }

  def streamFilter(): List[Map[String,String]] = {
    val twitterClient = new TwitterClient(false)
    val uri =  "/2/tweets/search/stream/rules"
    val result = twitterClient.requestGet(uri, false)
    val filterList = jsonParser(result.contentString)
    twitterClient.close()
    filterList
  }

  def streamTweets(filter: String, procces: (String) => Boolean) {
    val twitterClient = new TwitterClient(true)
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
    println(result)
    val jsonData = JSON.parseFull(result)
    val jsonMap = jsonData.get.asInstanceOf[Map[String,Any]]
    val tweetList = jsonMap.get("data") match {
      case Some(listTweet) => listTweet.asInstanceOf[List[Map[String,String]]]
      case None            => List(Map()).asInstanceOf[List[Map[String,String]]]
    }
    println(tweetList)
    tweetList
  }
}