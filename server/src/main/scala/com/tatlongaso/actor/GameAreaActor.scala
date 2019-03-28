package com.tatlongaso.actor

import akka.actor.{Actor, ActorRef}


trait GameEvent
case object Tick extends GameEvent
case class PlayerJoined(player: Player, actor: ActorRef) extends GameEvent
case class PlayerLeft(name: String) extends GameEvent
case class PlayerMoveRequest(name: String, direction: String) extends GameEvent
case class PlayersChanged(players: Iterable[Player]) extends GameEvent

case class WorldChanged(world: World) extends GameEvent

case class Player(name: String, position: Position, direction: Direction, state: State, jumpEnergy: Double)
object PlayerStats {
  val JUMP_STRENGTH = 12
}
case class PlayerWithActor(player: Player, actor: ActorRef)
case class Position(x: Int, y: Int) {
  def + (other: Position): Position = {
    Position(x + other.x, y + other.y)
  }
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
  val GRAVITY = 8

  val players = collection.mutable.LinkedHashMap[String, PlayerWithActor]()
  val world = new World()

  override def receive: Receive = {
    case PlayerJoined(player, actor) => {
      players += (player.name -> PlayerWithActor(player, actor))
      notifyWorldChanged()
      notifyPlayersChanged()
    }
    case PlayerLeft(playerName) => {
      players -= playerName
      notifyPlayersChanged()
    }
    case PlayerMoveRequest(playerName, direction) => {
      val oldPlayerWithActor = players(playerName)
      val oldPlayer = oldPlayerWithActor.player
      val actor = oldPlayerWithActor.actor

      val playerDirection = direction match {
        case "left" => Left
        case "right" => Right
        case _ => oldPlayer.direction
      }

      val offset = direction match {
        case "left" => Position(-5, 0)
        case "right" => Position(5, 0)
        case _ => Position(0, 0)
      }

      val playerState =
        if (oldPlayer.state == Jump)
          Jump
        else
          direction match {
            case "up" => Jump
            case "down" => Stand
            case "left" => Run
            case "right" => Run
          }

      // check if can jump
      var player = Player(playerName, oldPlayer.position + offset, playerDirection, playerState, oldPlayer.jumpEnergy)
      val jumpEnergy = if (oldPlayer.state != Jump && playerState == Jump && hasBlockBelow(player).isDefined) PlayerStats.JUMP_STRENGTH else oldPlayer.jumpEnergy
      player = Player(playerName, oldPlayer.position + offset, playerDirection, playerState, jumpEnergy)

      // handle collisions
      World.checkPlayerCollision(world, player).map(collision => {
        val offset = direction match {
          case "left" => Position((collision.position.x + World.GRID_WIDTH) - oldPlayer.position.x, 0)
          case "right" => Position((oldPlayer.position.x + World.GRID_WIDTH) - collision.position.x, 0)
          case _ => Position(0, 0)
        }

        // FIX SIDE EFFECT
        // update player position
        player = Player(playerName, oldPlayer.position + offset, player.direction, player.state, jumpEnergy)
      })

      // should have changed and no collisions
      if (player != oldPlayer) {
        players(player.name) = PlayerWithActor(player, actor)
        //notifyPlayerChanged(players(player.name))
        notifyPlayersChanged()
      }
    }
    case Tick => {
      // update world
      players.values.foreach(p => p.player.state match {
        // calculate jumping
        case Jump => {
          val player = p.player
          var newPlayer = player
          val jumpEnergy = player.jumpEnergy

          // check if going up or down
          val jumpMove = if (jumpEnergy <= 0) -GRAVITY * 2 else GRAVITY * 1.5
          val offset = Position(0, jumpMove.toInt)
          newPlayer = Player(player.name, player.position + offset, player.direction, player.state, jumpEnergy - 1)

          var collisions = World.checkPlayerCollision(world, newPlayer)

          // check if collide while jumping up
          if (collisions.size > 0 && jumpEnergy > 0) {
            val side = collisions.head.position.y - World.GRID_HEIGHT - 1
            // switch to stand
            newPlayer = Player(player.name, Position(player.position.x, side), player.direction, Jump, 0)
          }

          // check bottom collision
          World.checkPlayerCollision(world, newPlayer).map(collision => {
            val top = collision.position.y + World.GRID_HEIGHT
            // FIX SIDE EFFECT
            // update player position
            newPlayer = Player(player.name, Position(player.position.x, top), player.direction, Stand, player.jumpEnergy)
          })

          // should have changed
          if (player != newPlayer) {
            players(player.name) = PlayerWithActor(newPlayer, p.actor)
            notifyPlayersChanged()
          }
        }
        case _ => {
          val player = p.player
          val bottom = if (player.position.y - GRAVITY > 0) -GRAVITY else 0
          var newPlayer = Player(player.name, player.position + Position(0, bottom), player.direction, player.state, player.jumpEnergy)
          World.checkPlayerCollision(world, newPlayer).map(collision => {
            val top = collision.position.y + World.GRID_HEIGHT
            // FIX SIDE EFFECT
            // update player position
            newPlayer = Player(player.name, Position(player.position.x, top), player.direction, Stand, player.jumpEnergy)
          })
          // should have changed
          if (p.player != newPlayer) {
            players(p.player.name) = PlayerWithActor(newPlayer, p.actor)
            notifyPlayersChanged()
          }
        }
      })
    }
  }

  def hasBlockBelow(player: Player): Option[Block] = {
    val newPlayer = Player (player.name, player.position + Position (0, -GRAVITY), player.direction, player.state, player.jumpEnergy)
    World.checkPlayerCollision (world, newPlayer).headOption
  }

  def notifyPlayerChanged(playerActor: PlayerWithActor): Unit = {
    playerActor.actor ! PlayersChanged(Array(playerActor.player))
  }

  def notifyPlayersChanged(): Unit = {
    players.values.foreach(_.actor ! PlayersChanged(players.values.map(_.player)))
  }
  def notifyWorldChanged(): Unit = {
    players.values.foreach(_.actor ! WorldChanged(world))
  }
}
