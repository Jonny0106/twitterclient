package example
import com.acred.twitterClient.{TwitterReader, TwitterWriter}
import scala.util.parsing.json.JSON

object Hello extends App {

  def reply(str:String):Boolean = {
    println("str:" + str.length + "end")
    val jsonParser = new TwitterJsonParser()
    val tweetContent = jsonParser.streamTweetsParser(str)
    
    val content = Map(
      "status"                        -> "youre right", 
      "in_reply_to_status_id"         -> tweetContent.get("data").get.getOrElse("id", "1397318008389869569"),       
      "auto_populate_reply_metadata"  -> "true"
    )
    val twitterWriter = new TwitterWriter()
    println(twitterWriter.tweet(content))

    true
  }
  val twitterW = new TwitterWriter()
  val twitterR = new TwitterReader()

  twitterR.streamTweets(reply)
  println("done")
}

class TwitterJsonParser {

  def streamTweetsParser(result: String): Map[String,Map[String, String]] = {
    println(result)
    val jsonData = JSON.parseFull(result)
    jsonData.get.asInstanceOf[Map[String, Map[String, String]]]
  }
}


