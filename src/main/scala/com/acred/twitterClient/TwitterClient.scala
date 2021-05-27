package com.acred.twitterClient
import com.twitter.io
import com.twitter.io.{Buf, Reader}
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http
import com.twitter.finagle.http.{Chunk, HeaderMap}
import com.twitter.util.{Await, Future}
import com.twitter.finagle.builder
import com.twitter.util.Future
import scala.util.parsing.json.JSON




class TwitterWriter {

  def tweet(tweetContent:Map[String, String]):http.Response = {
    val twitterClient = new TwitterHttpClient(false)
    val postUri = "/1.1/statuses/update.json?include_entities=true"
    val oauth = new Oauth1vClient()
    val header = oauth(tweetContent)
    val headerMap = Map(
      "Content-Type" -> "application/x-www-form-urlencoded",
      "authorization" -> header
    )
    twitterClient.requestPostTweet(postUri, headerMap, oauth.percentEncodeMap(tweetContent))
  }

  def reply(tweetContent: String, tweetId: String) {

  }

  def changeStreamFilter(filter: String): http.Response = {
    val twitterClient = new TwitterHttpClient(false)
    val postUri = "/2/tweets/search/stream/rules"
    val headerMap = Map(
      "Content-Type" -> "application/x-www-form-urlencoded")
    twitterClient.requestPost(postUri, headerMap, filter)
  }
  
}

class TwitterReader {

  def readTweet(tweetId: String) {
    
  } 

  def readRecent(filter: String):List[Map[String,String]] = {
    val twitterClient = new TwitterHttpClient(false)
    val uri =  "/2/tweets/search/recent?query=" + filter
    val result = twitterClient.requestGet(uri, false)
    val tweetList = jsonParser(result.contentString)
    tweetList.foreach(tweetMap => println(tweetMap.get("text").get))
    twitterClient.close()
    
    tweetList
  }

  def streamFilter(): List[Map[String,String]] = {
    val twitterClient = new TwitterHttpClient(false)
    val uri =  "/2/tweets/search/stream/rules"
    val result = twitterClient.requestGet(uri, false)
    val filterList = jsonParser(result.contentString)
    twitterClient.close()
    
    filterList
  }

  def streamTweets(procces: (String) => Boolean) {
    val twitterClient = new TwitterHttpClient(true)
    val streamUri = "/2/tweets/search/stream"
    val streamResponse = twitterClient.requestGet(streamUri, true)
    println("reach here")
    
    val r = streamResponse.chunkReader


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

  def streamProccess(reader: Reader[Chunk], procces:(String) => Boolean){
    def loop(acc: Buf, trailers: Option[HeaderMap]): Future[(Buf, Option[HeaderMap])] =
    reader.read().flatMap {
      case Some(chunk) =>
        if (chunk.isLast && !chunk.trailers.isEmpty)
          loop(acc.concat(chunk.content), Some(chunk.trailers))
        else {
          val Buf.Utf8(str) = chunk.content
          val isDone = str.trim() match {
            case ""  => false
            case _  => procces(str)
          }
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
  }
}