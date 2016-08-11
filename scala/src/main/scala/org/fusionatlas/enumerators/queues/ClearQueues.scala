package org.fusionatlas.enumerators.queues

object ClearQueues {
  def main(args: Array[String]): Unit = {
    import com.xerox.amazonws.sqs2._

    val amazonAccount = "0D4BTQXQJ7SAKKQHF982"
    val secretKey = "wQsXfibiPzfPFDZ84jWXIjNb9UfqnLh42+FHhqtp"

    val account = new QueueService(amazonAccount, secretKey)
    for (queue <- scala.collection.JavaConversions.asScalaBuffer(account.listMessageQueues("fusionatlas-enumeration-requests"))) {
      queue.deleteQueue
    }

  }
}
