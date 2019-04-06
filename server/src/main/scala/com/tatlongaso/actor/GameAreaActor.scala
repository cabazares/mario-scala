package com.tatlongaso.actor

import scala.collection.immutable
import scala.collection.mutable
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


case class World(blocks: Iterable[Block]) {
  def this() {
    this(World.initialBlocks())
  }
}
case object World {
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
    val midpoint = players.foldLeft(Position(0,0))((a: Position, b: Player) => a + b.position) / players.size
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
  val GRAVITY = 0.3

  val players: mutable.Map[String, PlayerWithActor] = mutable.LinkedHashMap[String, PlayerWithActor]()
  val world = new World()

  var cachedPlayers: mutable.Map[String, PlayerWithActor] = mutable.LinkedHashMap[String, PlayerWithActor]()
  var cachedWorld = new World()
  var prevTime: Long = 0

  override def receive: Receive = {
    case PlayerJoined(name, actor) =>
      val player = Player(name, World.startPosition(players.values.map(_.player)), Right, Stand)
      players += (name -> PlayerWithActor(player, actor))
      notifyWorldChanged()
      publishPlayersChanged()

    case PlayerLeft(playerName) =>
      players -= playerName
      publishPlayersChanged()

    case GameUpdate(playerInputs) =>
      val time = System.currentTimeMillis()
      val timeDelta = if (prevTime == 0) 16 else time - prevTime
      prevTime = time
      updateGameState(playerInputs, timeDelta)
  }

  def updateGameState(playerInputs: immutable.Map[String, PlayerInput], timeDelta: Long): Unit = {
    // update players based on inputs
    for ((playerName, oldPlayerWithActor) <- players) {
      val oldPlayer = oldPlayerWithActor.player

      val inputForPlayer = playerInputs.get(playerName)
      // stand if no other input
      inputForPlayer
        .filter(pi => pi == PlayerInput() && oldPlayer.state != Jump)
        .foreach(_ => players(playerName) = PlayerWithActor(oldPlayer.copy(state = Stand), oldPlayerWithActor.actor))

      // handle inputs
      inputForPlayer
        .filter(pi => pi != PlayerInput())
        .foreach(playerInput => {
          val actor = oldPlayerWithActor.actor

          // set direction
          val playerDirection = playerInput match {
            case PlayerInput(true, _, _, _) => Left
            case PlayerInput(_, true, _, _) => Right
            case _ => oldPlayer.direction
          }

          // set move speed
          val offset = playerInput match {
            case PlayerInput(true, _, _, _) => Position(-PlayerStats.MOVE_SPEED * timeDelta, 0)
            case PlayerInput(_, true, _, _) => Position(PlayerStats.MOVE_SPEED * timeDelta, 0)
            case _ => Position(0, 0)
          }

          // set state based on movement
          val playerState =
            if (oldPlayer.state == Jump)
              Jump
            else
              playerInput match {
                case PlayerInput(_, _, true, _) => Jump
                case PlayerInput(true, _, _, _) => Run
                case PlayerInput(_, true, _, _) => Run
                case _ => Stand
              }
          //println(s"orig ${oldPlayer.position} offset: $offset state: $playerState input: ${playerInput}")

          // check if can jump
          var player = oldPlayer.copy(position = oldPlayer.position + offset,
                                       direction = playerDirection,
                                       state = playerState)
          val jumpEnergy = if (oldPlayer.state != Jump &&playerState == Jump && hasBlockBelow(player).isDefined)
            PlayerStats.JUMP_STRENGTH
          else
            oldPlayer.jumpEnergy
          player = player.copy(jumpEnergy = jumpEnergy)

          // reset players position based from blocks its colliding with
          World
            .checkPlayerCollision(world, player)
            .map(collision => oldPlayer.resetFromCollision(collision.position))
            .foreach(player = _)

          // don't share same space with other players
          val otherPlayers = players.values.filter(p => p.player.name != player.name).map(p => p.player)
          player
            .collideCheck(otherPlayers)
            // ignore collision if hit on the head
            .filter(collision => {
              val width = World.GRID_WIDTH
              val height = World.GRID_HEIGHT
              val playerMid = player.position.y + height / 2
              collision.position.x < player.position.x + width && collision.position.x + width > player.position.x &&
                collision.position.y + height > playerMid && collision.position.y < (playerMid + height / 2)
            })
            // reset position for player
            .map(collision => oldPlayer.resetFromCollision(collision.position))
            .foreach(player = _)

          // should have changed and no collisions
          if (player != oldPlayer) {
            players(player.name) = PlayerWithActor(player, actor)
          }
        })
    }

    // update players based on state
    players.values.foreach(p => {
      val player = p.player
      var jumpEnergy = player.jumpEnergy
      // apply gravity to player
      val bottom = -GRAVITY * timeDelta
      var newPlayer = player.copy(position = player.position + Position(0, bottom))
      val goingDown = jumpEnergy <= 0

      if (player.state == Jump) {
        // halt jump if up button no longer pressed
        if (jumpEnergy <= PlayerStats.JUMP_STRENGTH * 0.6 && jumpEnergy > 0) {
          playerInputs.get(player.name).filter(pi => !pi.up).map(_ => jumpEnergy = 0)
        }

        // calculate offset for jump
        val jumpMove = (if (goingDown) -GRAVITY * 2 else GRAVITY * 1.5) * timeDelta
        val offset = Position(0, jumpMove.toInt)
        newPlayer = player.copy(position = player.position + offset, jumpEnergy = jumpEnergy - 0.6)

        // check if collide while jumping up
        val collisions = World.checkPlayerCollision(world, newPlayer)
        if (collisions.nonEmpty && !goingDown) {
          val side = collisions.head.position.y - World.GRID_HEIGHT - 1
          // switch to stand
          newPlayer = player.copy(position = Position(player.position.x, side), state = Jump, jumpEnergy = 0)
        }
      }

      // check if frag
      if (goingDown) {
        val otherPlayers = players.values.filter(p => p.player.name != newPlayer.name).map(p => p.player)
        newPlayer.collideCheck(otherPlayers).map(fragged => {
          // reset player position if killed
          val startPosition = World.startPosition(players.values.map(_.player))
          val resetPlayer = fragged.copy(position = startPosition, direction = Right, state = Stand, jumpEnergy = 0)
          players(fragged.name) = PlayerWithActor(resetPlayer, players(fragged.name).actor)
          fragged
        }).take(1).foreach(_ => {
          // jump after fragging
          newPlayer = newPlayer.copy(jumpEnergy = PlayerStats.JUMP_STRENGTH)
        })
      }

      // check bottom collision
      World.checkPlayerCollision(world, newPlayer).foreach(collision => {
        val top = collision.position.y + World.GRID_HEIGHT
        // update player position
        val playerState = if (player.state == Jump) Stand else player.state
        newPlayer = player.copy(position = Position(player.position.x, top), state = playerState)
      })

      players(p.player.name) = PlayerWithActor(newPlayer, p.actor)
    })

    notifyPlayersChanged()
  }

  def hasBlockBelow(player: Player): Option[Block] = {
    val newPlayer = player.copy(position=player.position + Position (0, -GRAVITY))
    World.checkPlayerCollision (world, newPlayer).headOption
  }

  // notify players making sure we only publish changes
  def notifyPlayersChanged(): Unit = {
    // compute changed players
    val changedPlayers = players.values.map(_.player).filter(p => cachedPlayers.get(p.name).exists(_.player != p))
    cachedPlayers = players.clone()

    if (changedPlayers.nonEmpty) {
      publishPlayersChanged(changedPlayers)
    }
  }

  // send message to actor about player data
  def publishPlayersChanged(playersData: Iterable[Player]=players.values.map(_.player)): Unit = {
    players.values.foreach(_.actor ! PlayersChanged(playersData))
  }

  def notifyWorldChanged(): Unit = {
    players.values.foreach(_.actor ! WorldChanged(world))
  }
}
