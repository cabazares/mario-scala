package com.tatlongaso.service

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.http.scaladsl.server.Directives
import akka.stream.scaladsl.{Flow, GraphDSL, Merge, Sink, Source}
import akka.stream.{ActorMaterializer, FlowShape, OverflowStrategy}
import scala.concurrent.duration._
import com.tatlongaso.actor._


class GameService(implicit val actorSystem: ActorSystem, implicit val actorMaterializer: ActorMaterializer) extends Directives {
  val websocketRoute = (get & parameter("player"))  {
    playerName =>
      handleWebSocketMessages(flow(playerName))
  }

  val gameAreaActor = actorSystem.actorOf(Props(new GameAreaActor()))
  val playerActorSource = Source.actorRef[GameEvent](5, OverflowStrategy.fail)
  def flow(playerName: String): Flow[Message, Message, Any] =
    Flow.fromGraph(GraphDSL.create(playerActorSource){
      implicit builder => playerActor => {
        import GraphDSL.Implicits._

        val player = Player(playerName, Position(World.GRID_WIDTH * 2, World.GRID_HEIGHT), Right, Stand, -10)
        val materialization = builder.materializedValue.map(playerActorRef => PlayerJoined(player, playerActorRef))
        val merge = builder.add(Merge[GameEvent](2))

        val messagesToGameEventsFlow = builder.add(Flow[Message].map {
          case TextMessage.Strict(txt) => PlayerMoveRequest(playerName, txt)
        })

        val gameEventsToMessagesFlow = builder.add(Flow[GameEvent].map {
          case PlayersChanged(players) => {
            import spray.json._
            import DefaultJsonProtocol._
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
            TextMessage(s"""{"type": "players", "data": ${players.toJson.toString}}""")
          }
          case WorldChanged(world) => {
            import spray.json._
            import DefaultJsonProtocol._
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
            TextMessage(s"""{"type": "world", "data": ${world.toJson.toString}}""")
          }
        })

        val gameAreaActorSink = Sink.actorRef[GameEvent](gameAreaActor, PlayerLeft(playerName))

        materialization ~> merge ~> gameAreaActorSink
        messagesToGameEventsFlow ~> merge

        playerActor ~> gameEventsToMessagesFlow

        FlowShape(messagesToGameEventsFlow.in, gameEventsToMessagesFlow.out)
      }
    })

  // send messages
  val gameTick = actorSystem.scheduler.schedule(
    0 milliseconds,
    50 milliseconds,
    gameAreaActor,
    Tick)(actorSystem.dispatcher)
}