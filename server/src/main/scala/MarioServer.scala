import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.tatlongaso.service.GameService

object MarioServer {

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()

    implicit val executionContext = system.dispatcher

    val gameService = new GameService()

    Http().bindAndHandle(gameService.websocketRoute, "0.0.0.0", 8080)
    println("Listening on ws://0.0.0.0:8080")
    // FIXME: shutdown properly
  }
}
