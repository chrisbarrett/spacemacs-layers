package services

import scala.concurrent.Future

class NewService () {
  def hello(): Future[String] = Future.successful {
    print("hello from inside NewService!")
    "hello"
  }
}
