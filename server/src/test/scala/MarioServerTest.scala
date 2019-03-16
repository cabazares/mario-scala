import akka.http.scaladsl.testkit.{ScalatestRouteTest, WSProbe}
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
    assertWebsocket("Two") {
      wsClient =>
        wsClient.expectMessage("[{\"direction\":\"right\",\"name\":\"Two\",\"position\":{\"x\":0,\"y\":0}}]")
    }
  }

  test ("should register multiple players") {
    val gameService = new GameService()
    val firstClient = WSProbe()
    val secondClient = WSProbe()
    WS(s"/?player=first", firstClient.flow) ~> gameService.websocketRoute ~> check {
      firstClient.expectMessage("[{\"direction\":\"right\",\"name\":\"first\",\"position\":{\"x\":0,\"y\":0}}]")
    }
    WS(s"/?player=second", secondClient.flow) ~> gameService.websocketRoute ~> check {
      firstClient.expectMessage("[{\"direction\":\"right\",\"name\":\"first\",\"position\":{\"x\":0,\"y\":0}},{\"direction\":\"right\",\"name\":\"second\",\"position\":{\"x\":0,\"y\":0}}]")
    }
  }

  test("should register player and move it up") {
    assertWebsocket("Player") {
      wsClient =>
        wsClient.expectMessage("[{\"direction\":\"right\",\"name\":\"Player\",\"position\":{\"x\":0,\"y\":0}}]")
        wsClient.sendMessage("up")
        wsClient.expectMessage("[{\"direction\":\"right\",\"name\":\"Player\",\"position\":{\"x\":0,\"y\":1}}]")
    }
  }


  test("should register player and move around") {
    assertWebsocket("Player") {
      wsClient =>
        wsClient.expectMessage("[{\"direction\":\"right\",\"name\":\"Player\",\"position\":{\"x\":0,\"y\":0}}]")
        wsClient.sendMessage("up")
        wsClient.expectMessage("[{\"direction\":\"right\",\"name\":\"Player\",\"position\":{\"x\":0,\"y\":1}}]")
        wsClient.sendMessage("left")
        wsClient.expectMessage("[{\"direction\":\"left\",\"name\":\"Player\",\"position\":{\"x\":-1,\"y\":1}}]")
        wsClient.sendMessage("down")
        wsClient.expectMessage("[{\"direction\":\"left\",\"name\":\"Player\",\"position\":{\"x\":-1,\"y\":0}}]")
        wsClient.sendMessage("right")
        wsClient.expectMessage("[{\"direction\":\"right\",\"name\":\"Player\",\"position\":{\"x\":0,\"y\":0}}]")
    }
  }

  def assertWebsocket(playerName: String)(assertions: (WSProbe) => Unit): Unit = {
    val gameService = new GameService()
    val wsClient = WSProbe()
    WS(s"/?player=$playerName", wsClient.flow) ~> gameService.websocketRoute ~> check(assertions(wsClient))
  }
}


