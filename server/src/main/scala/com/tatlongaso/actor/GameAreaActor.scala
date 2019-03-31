package com.tatlongaso.actor

import akka.actor.{Actor, ActorRef}


trait GameEvent
// incoming events
case class GameUpdate(playerInputs: Map[String, PlayerInput]) extends GameEvent
case class PlayerJoined(player: Player, actor: ActorRef) extends GameEvent
case class PlayerLeft(name: String) extends GameEvent
case class PlayerSentInput() extends GameEvent

// outgoing
case class PlayersChanged(players: Iterable[Player]) extends GameEvent
case class WorldChanged(world: World) extends GameEvent

// represents current input for player
case class PlayerInput(left: Boolean, right: Boolean, up: Boolean, down: Boolean) {
  def this() {
    this(false, false, false, false)
  }

  def update(key: String, pressed: Boolean): PlayerInput = key match {
    case "up" => PlayerInput(this.left, this.right, pressed, this.down)
    case "down" => PlayerInput(this.left, this.right, this.up, pressed)
    case "left" => PlayerInput(pressed, this.right, this.up, this.down)
    case "right" => PlayerInput(this.left, pressed, this.up, this.down)
    case _ => this
  }
}

case class Player(name: String, position: Position, direction: Direction, state: State, jumpEnergy: Double)
object PlayerStats {
  val JUMP_STRENGTH = 36
  val MOVE_SPEED = 2.5f
}
case class PlayerWithActor(player: Player, actor: ActorRef)
case class Position(x: Float, y: Float) {
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
  val GRAVITY = 3

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
    case GameUpdate(playerInputs) => {
      // update players
      for ((playerName, oldPlayerWithActor) <- players) {
        // based on inputs
        playerInputs.get(playerName).map(playerInput => {
          val oldPlayer = oldPlayerWithActor.player
          val actor = oldPlayerWithActor.actor

          // set direction
          val playerDirection = playerInput match {
            case PlayerInput(true, _, _, _) => Left
            case PlayerInput(_, true, _, _) => Right
            case _ => oldPlayer.direction
          }

          // set move speed
          val offset = playerInput match {
            case PlayerInput(true, _, _, _) => Position(-PlayerStats.MOVE_SPEED, 0)
            case PlayerInput(_, true, _, _) => Position(PlayerStats.MOVE_SPEED, 0)
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

          // check if can jump
          var player = Player(playerName, oldPlayer.position + offset, playerDirection, playerState, oldPlayer.jumpEnergy)
          val jumpEnergy = if (oldPlayer.state != Jump && playerState == Jump && hasBlockBelow(player).isDefined) PlayerStats.JUMP_STRENGTH else oldPlayer.jumpEnergy
          player = Player(playerName, oldPlayer.position + offset, playerDirection, playerState, jumpEnergy)

          // handle collisions
          World.checkPlayerCollision(world, player).map(collision => {
            val offset = playerInput match {
              case PlayerInput(true, _, _, _) => Position((collision.position.x + World.GRID_WIDTH) - oldPlayer.position.x, 0)
              case PlayerInput(_, true, _, _) => Position((oldPlayer.position.x + World.GRID_WIDTH) - collision.position.x, 0)
              case _ => Position(0, 0)
            }

            // FIX SIDE EFFECT
            // update player position
            player = Player(playerName, oldPlayer.position + offset, player.direction, player.state, jumpEnergy)
          })

          // should have changed and no collisions
          if (player != oldPlayer) {
            players(player.name) = PlayerWithActor(player, actor)
          }
        })
      }


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

          // check if collide while jumping up
          val collisions = World.checkPlayerCollision(world, newPlayer)
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
            newPlayer = Player(player.name, Position(player.position.x, top), player.direction, player.state, player.jumpEnergy)
          })

          players(p.player.name) = PlayerWithActor(newPlayer, p.actor)
          notifyPlayersChanged()
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
