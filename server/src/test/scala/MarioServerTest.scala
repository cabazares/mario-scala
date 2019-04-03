import akka.http.scaladsl.testkit.{ScalatestRouteTest, WSProbe}
import com.tatlongaso.actor.World
import org.scalatest.{FunSuite, Matchers}
import com.tatlongaso.service.GameService

class MarioServerTest extends FunSuite with Matchers with ScalatestRouteTest {

  test("should create com.tatlongaso.service.GameService") {
    new GameService()
  }

  test ("should be able to connect to websockets") {
    assertWebsocket("One") {
      _: WSProbe =>
        isWebSocketUpgrade shouldEqual true
    }
  }

  test ("should register player") {
    val gameService = new GameService()
    assertWebsocket("Two") {
      wsClient =>
        wsClient.expectMessage(s"""{"type": "world", "data": ${gameService.worldToJson(new World())}}""")
    }
  }

  test ("should register multiple players") {
    val gameService = new GameService()
    val firstClient = WSProbe()
    val secondClient = WSProbe()
    val initialWorld = s"""{"type": "world", "data": ${gameService.worldToJson(new World())}}"""
    WS(s"/?player=first", firstClient.flow) ~> gameService.websocketRoute ~> check {
      firstClient.expectMessage(initialWorld)
      //firstClient.expectMessage("""{"type": "players", "data": [{"direction":"right","jumpEnergy":0.0,"name":"first","position":{"x":64.0,"y":32.0},"state":"stand"}]}""")
    }
    WS(s"/?player=second", secondClient.flow) ~> gameService.websocketRoute ~> check {
      secondClient.expectMessage(initialWorld)
      //secondClient.expectMessage("""{"type": "players", "data": [{"direction":"right","jumpEnergy":0.0,"name":"first","position":{"x":416.0,"y":32.0},"state":"stand"}]}""")
    }
  }

  def assertWebsocket(playerName: String)(assertions: (WSProbe) => Unit): Unit = {
    val gameService = new GameService()
    val wsClient = WSProbe()
    WS(s"/?player=$playerName", wsClient.flow) ~> gameService.websocketRoute ~> check(assertions(wsClient))
  }
}


