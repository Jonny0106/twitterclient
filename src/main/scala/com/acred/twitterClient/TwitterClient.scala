package com.acred.twitterClient
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

class TwitterClient(isStream: Boolean) {
    
  val client: Service[http.Request, http.Response] = {
    isStream match {
      case true  => 
        Http.client
        .withTransport.tls("api.twitter.com")
        .withStreaming(true)
        .newService("api.twitter.com:443")
      case false => 
        Http.client
        .withTransport.tls("api.twitter.com")
        .newService("api.twitter.com:443")
    }
  }


  
  private val bearer = "Bearer " + sys.env.get("TWITTER_BEARER").get
  
  def requestGet(uri: String, isStream: Boolean): http.Response = {
  
    val request = http.Request(http.Method.Get, uri)
    request.headerMap.put("Authorization", bearer)
    val response = client(request)

    Await.result(response)
  }

  def requestPost(uri: String, headerMap: Map[String,String], contentString: String): http.Response = {
    val request = http.Request(http.Method.Post, uri)
    headerMap.foreach(header => request.headerMap.put(header._1, header._2))
    request.headerMap.put("Authorization", bearer)
    request.contentString = contentString
    request.setContentTypeJson()
    val response = client(request)
    val rep = Await.result(response)
    rep
  }

  def requestPostTweet(uri: String, headerMap: Map[String,String], content: String): http.Response = {
    val request = http.Request(http.Method.Post, uri)
    headerMap.foreach(header => request.headerMap.put(header._1, header._2))
    request.contentString = "status=" + content
    println(request.contentString)
    val response = client(request)
    val rep = Await.result(response)
    rep
  }

  def close(){
    client.close()
  }
  
}