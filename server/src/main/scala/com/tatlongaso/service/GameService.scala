package com.tatlongaso.service

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.http.scaladsl.server.Directives
import akka.stream.scaladsl.{Flow, GraphDSL, Merge, Sink, Source}
import akka.stream.{ActorMaterializer, FlowShape, OverflowStrategy}

import scala.concurrent.duration._
import com.tatlongaso.actor._
import spray.json._
import DefaultJsonProtocol._

import scala.collection.mutable


class GameService(implicit val actorSystem: ActorSystem, implicit val actorMaterializer: ActorMaterializer) extends Directives {
  val websocketRoute = (get & parameter("player"))  {
    playerName =>
      handleWebSocketMessages(flow(playerName))
  }

  val playerInputs = mutable.LinkedHashMap[String, PlayerInput]()
  val gameAreaActor = actorSystem.actorOf(Props(new GameAreaActor()))
  val playerActorSource = Source.actorRef[GameEvent](5, OverflowStrategy.fail)

  def flow(playerName: String): Flow[Message, Message, Any] =
    Flow.fromGraph(GraphDSL.create(playerActorSource){
      implicit builder => playerActor => {
        import GraphDSL.Implicits._

        val player = Player(playerName, World.getRandomPosition(), Right, Stand)
        playerInputs += (playerName -> PlayerInput(false, false, false, false))
        val materialization = builder.materializedValue.map(playerActorRef => PlayerJoined(player, playerActorRef))
        val merge = builder.add(Merge[GameEvent](2))

        // incoming
        val messagesToGameEventsFlow = builder.add(Flow[Message].map {
          case TextMessage.Strict(txt) => {
            val Array(key, event) = txt.split("_")

            // update inputs for player
            playerInputs.get(playerName).map(playerInput => {
              event match {
                case "keydown" | "keyup" => playerInputs.update(playerName, playerInput.update(key, event == "keydown"))
                case _ => playerInput
              }
            })

            // blank game event since we have to return a game event
            PlayerSentInput()
          }
          case _ => PlayerSentInput()
        })

        // outgoing
        val gameEventsToMessagesFlow = builder.add(Flow[GameEvent].map {
          case PlayersChanged(players) => {
            TextMessage(s"""{"type": "players", "data": ${playersToJson(players)}}""")
          }
          case WorldChanged(world) => {
            TextMessage(s"""{"type": "world", "data": ${worldToJson(world)}}""")
          }
        })

        val gameAreaActorSink = Sink.actorRef[GameEvent](gameAreaActor, PlayerLeft(playerName))

        materialization ~> merge ~> gameAreaActorSink
        messagesToGameEventsFlow ~> merge

        playerActor ~> gameEventsToMessagesFlow

        FlowShape(messagesToGameEventsFlow.in, gameEventsToMessagesFlow.out)
      }
    })

  def worldToJson(world: World): String = {
    implicit val positionFormat = jsonFormat2(Position)
    implicit object blockTypeFormat extends JsonFormat[BlockType] {
      def write(blockType: BlockType): JsValue = blockType match {
        case Coin => JsString("Coin")
        case Brick => JsString("Brick")
      }
      override def read(json: JsValue): BlockType = json match{
        case JsString("Coin") => Coin
        case JsString("Brick") => Brick
        case _ => Brick
      }
    }
    implicit val blockFormat = jsonFormat3(Block)
    implicit object worldFormat extends JsonFormat[World] {
      def write(world: World): JsValue = {
        JsArray(world.blocks.map(b => b.toJson.asJsObject).toVector)
      }
      override def read(json: JsValue): World = json match{
        case JsArray(elements) => World(elements.map(e => e.asInstanceOf[Block]))
        case _ => World(World.initialBlocks())
      }
    }
    world.toJson.toString
  }

  def playersToJson(players: Iterable[Player]): String = {
    implicit val positionFormat = jsonFormat2(Position)
    implicit object directionFormat extends JsonFormat[Direction] {
      def write(d: Direction): JsValue = d match {
        case Left => JsString("left")
        case Right => JsString("right")
      }
      def read(json: JsValue) = json match {
        case JsString("left") => Left
        case JsString("light") => Right
        case _ => deserializationError("invalid value")
      }
    }
    implicit object stateFormat extends JsonFormat[State] {
      def write(s: State): JsValue = s match {
        case Stand => JsString("stand")
        case Run => JsString("run")
        case Jump => JsString("jump")
        case Die => JsString("die")
      }
      def read(json: JsValue) = json match {
        case JsString("stand") => Stand
        case JsString("run") => Run
        case JsString("jump") => Jump
        case JsString("die") => Die
        case _ => deserializationError("invalid value")
      }
    }
    implicit val playerFormat = jsonFormat5(Player)
    players.toJson.toString
  }

  // send messages
  val gameTick = actorSystem.scheduler.schedule(
    0 milliseconds,
    16 milliseconds,
    () => {
      gameAreaActor ! GameUpdate(playerInputs.toMap)
    })(actorSystem.dispatcher)
}
