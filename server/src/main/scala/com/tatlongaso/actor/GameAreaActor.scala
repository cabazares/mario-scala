package com.tatlongaso.actor

import akka.actor.{Actor, ActorRef}

trait GameEvent
// incoming events
case class GameUpdate(playerInputs: Map[String, PlayerInput]) extends GameEvent
case class PlayerJoined(name:String, actor: ActorRef) extends GameEvent
case class PlayerLeft(name: String) extends GameEvent
case class PlayerSentInput() extends GameEvent

// outgoing
case class PlayersChanged(players: Iterable[Player]) extends GameEvent
case class WorldChanged(world: World) extends GameEvent

// represents current input for player
case class PlayerInput(left: Boolean = false, right: Boolean = false, up: Boolean = false, down: Boolean = false) {
  def update(key: String, pressed: Boolean): PlayerInput = key match {
    case "up" => this.copy(up=pressed)
    case "down" => this.copy(down=pressed)
    case "left" => this.copy(left=pressed)
    case "right" => this.copy(right=pressed)
    case _ => this
  }
}

case class Player(name: String, position: Position, direction: Direction, state: State, jumpEnergy: Double=0) {
  def collideCheck(players: Iterable[Player]): Iterable[Player] = {
    val width = World.GRID_WIDTH
    val height = World.GRID_HEIGHT
    players.filter(p => p != this).filter(b => {
      b.position.x < position.x + width && b.position.x + width > position.x &&
      b.position.y + height > position.y && b.position.y < position.y + height
    })
  }

  def resetFromCollision(collider: Position): Player = {
    val offset = direction match {
      case Left => Position((collider.x + World.GRID_WIDTH) - position.x, 0)
      case Right => Position((position.x + World.GRID_WIDTH) - collider.x, 0)
      case _ => Position(0, 0)
    }
    this.copy(position=position + offset)
  }
}
object PlayerStats {
  val MOVE_SPEED = 0.16
  val JUMP_STRENGTH = 16
}
case class PlayerWithActor(player: Player, actor: ActorRef)
case class Position(x: Double, y: Double) {
  def + (other: Position): Position = {
    Position(x + other.x, y + other.y)
  }

  def / (other: Int): Position = {
    Position(x / other, y / other)
  }

  def dist(other: Position): Double =
    math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))
}

sealed trait Direction
case object Left extends Direction
case object Right extends Direction

sealed trait State
case object Stand extends State
case object Run extends State
case object Jump extends State
case object Die extends State

case class Block(position: Position, blockType: BlockType, hit: Boolean) {
  def this(position: Position, blockType: BlockType) {
    this(position: Position, blockType: BlockType, false)
  }
}
sealed trait BlockType
case object Brick extends BlockType
case object Coin extends BlockType


case class World(blocks: Iterable[Block] = World.initialBlocks())
case object World {
  val GRAVITY = 0.3
  val HEIGHT = 16
  val WIDTH = 16
  val GRID_WIDTH = 32
  val GRID_HEIGHT = 32

  val startPositions = Array(
    Position(GRID_WIDTH * 2, GRID_HEIGHT),
    Position(WIDTH * GRID_WIDTH - GRID_WIDTH * 3, GRID_HEIGHT)
  )

  def startPosition(players: Iterable[Player]): Position = {
    // find start position farthest away from other players
    val midpoint = players.foldLeft(Position(0, 0))((pos, player) => pos + player.position) / players.size
    startPositions.maxBy(midpoint.dist)
  }

  def initialBlocks(): Iterable[Block] = {
    val blocks = collection.mutable.MutableList[Block]()
    // add initial blocks
    // lower half
    for (i <- 1 to 6) {
      blocks += new Block(Position(GRID_WIDTH * i, 0), Brick)
      blocks += new Block(Position(GRID_WIDTH * (World.WIDTH - i - 1), 0), Brick)
    }
    // second level
    for (i <- 4 to 11) {
      blocks += new Block(Position(GRID_WIDTH * i, GRID_HEIGHT * 4), Brick)
    }
    blocks
  }

  def checkPlayerCollision(world: World, player: Player): Iterable[Block] = {
    // check collision
    val width = World.GRID_WIDTH
    val height = World.GRID_HEIGHT
    world.blocks.filter(b => {
      b.position.x < player.position.x + width && b.position.x + width > player.position.x &&
      b.position.y + height > player.position.y && b.position.y < player.position.y + height
    })
  }
}

class GameAreaActor extends Actor {
  override def receive: Receive = {
    var players: Map[String, PlayerWithActor] = Map[String, PlayerWithActor]()
    val world = new World()

    var cachedPlayers: Map[String, PlayerWithActor] = Map[String, PlayerWithActor]()
    var prevTime: Long = 0

    {
      case PlayerJoined(name, actor) =>
        val player = Player(name, World.startPosition(players.values.map(_.player)), Right, Stand)
        players = players + (name -> PlayerWithActor(player, actor))
        notifyWorldChanged(players.values.map(_.actor), world)
        publishPlayersChanged(players.values.map(_.actor), players.values.map(_.player))

      case PlayerLeft(playerName) =>
        players = players - playerName
        publishPlayersChanged(players.values.map(_.actor), players.values.map(_.player))


      case GameUpdate(playerInputs) =>
        val time = System.currentTimeMillis()
        val timeDelta = if (prevTime == 0) 16 else time - prevTime
        prevTime = time

        players = GameWorld
          .updateGameState(players.values.map(_.player), world, playerInputs, timeDelta)
          .map(p => p.name -> PlayerWithActor(p, players(p.name).actor))
          .toMap
        cachedPlayers = notifyPlayersChanged(players, cachedPlayers)
    }
  }

  // notify players making sure we only publish changes
  def notifyPlayersChanged(
    players: Map[String, PlayerWithActor],
    cachedPlayers: Map[String, PlayerWithActor]
  ): Map[String, PlayerWithActor] = {
    // compute changed players
    val changedPlayers: Iterable[Player] = players.values
      .map(_.player)
      .filter(p => cachedPlayers.get(p.name).exists(_.player != p))

    if (changedPlayers.nonEmpty)
      publishPlayersChanged(players.values.map(_.actor), changedPlayers)

    players
  }

  // send message to all actors that players updated
  def publishPlayersChanged(subscribers: Iterable[ActorRef], playersData: Iterable[Player]): Unit = {
    subscribers.foreach(_ ! PlayersChanged(playersData))
  }

  // send message to all actors that the world changed
  def notifyWorldChanged(subscribers: Iterable[ActorRef], world: World): Unit = {
    subscribers.foreach(_ ! WorldChanged(world))
  }
}
