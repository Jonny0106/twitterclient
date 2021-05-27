package com.acred.twitterClient
import java.net.URLEncoder
import javax.crypto.spec.SecretKeySpec
import javax.crypto.Mac
import org.apache.commons.codec.binary.Base64
import scala.collection.immutable.ListMap

class Oauth1vClient() {
  def apply(content:Map[String, String]):String = {
    
    val oauthMap:Map[String, String] = Map(
      "include_entities"       -> "true",
      "oauth_consumer_key"     -> sys.env.get("TWITTER_CONSUMER_KEY").get,
      "oauth_nonce"            -> System.nanoTime.toString,
      "oauth_signature_method" -> "HMAC-SHA1",
      "oauth_timestamp"        -> (System.currentTimeMillis / 1000).toString,
      "oauth_token"            -> sys.env.get("TWITTER_ACCESS_TOKEN").get,
      "oauth_version"          -> "1.0",
    )

    val secretMap = Map(
      "consumer_secret"        -> sys.env.get("TWITTER_CONSUMER_KEY_SECRET").get,
      "oauth_token_secret"     -> sys.env.get("TWITTER_TOKEN_SECRET").get
    )

    val fullOauthMap = oauthMap ++ content
  

    val base = createSignitureBase(fullOauthMap)
    val key = createSigningKey(secretMap)
    
    val signiture = hashing(base, key)
    val newOauthMap = oauthMap + ("oauth_signature" -> signiture)
    createHeader(newOauthMap)
  }

  def base64encode(s: Array[Byte]) = {
    Base64.encodeBase64String(s)
  }
  
  private def percentEncode(str: String): String = {
    URLEncoder.encode(str, "UTF-8").replace("+", "%20")
  }

  def percentEncodeMap(content:Map[String, String]): String = {
    content.map { pair => 
      (percentEncode(pair._1) + "=" + percentEncode(pair._2))
    }.reduce { 
      (x, y) => x + "&" + y
    }
  }


  def createSignitureBase(oauthMap: Map[String, String]):String = {
    val url = "https://api.twitter.com/1.1/statuses/update.json"
    val postUrl = "POST&" + percentEncode(url)
    val orderedMap = ListMap(oauthMap.toSeq.sortWith(_._1 < _._1):_*)
    val encode = percentEncodeMap(orderedMap)

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
 
}
