package com.tatlongaso.actor

// represents current input for player
case class PlayerInput(left: Boolean = false, right: Boolean = false, up: Boolean = false, down: Boolean = false) {
  def press(key: String, pressed: Boolean): PlayerInput = key match {
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

case class Position(x: Double, y: Double) {
  def + (other: Position): Position = {
    Position(x + other.x, y + other.y)
  }

  def / (other: Int): Position = {
    Position(x / other, y / other)
  }

  def collides(other: Position, width: Double = World.GRID_WIDTH, height: Double = World.GRID_HEIGHT): Boolean =
    other.x < this.x + width && other.x + width > this.x && other.y + height > this.y && other.y < this.y + height

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
  val WIDTH = 24
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
    for (i <- -1 to World.WIDTH + 1) {
      blocks += new Block(Position(GRID_WIDTH * i, 0), Brick)
    }
    // second level
    for (i <- -1 to 4) {
      blocks += new Block(Position(GRID_WIDTH * i, GRID_HEIGHT * 4), Brick)
    }
    for (i <- 19 to World.WIDTH + 1) {
      blocks += new Block(Position(GRID_WIDTH * i, GRID_HEIGHT * 4), Brick)
    }
    for (i <- 8 to 15) {
      blocks += new Block(Position(GRID_WIDTH * i, GRID_HEIGHT * 6), Brick)
    }
    // third level
    for (i <- -1 to 4) {
      blocks += new Block(Position(GRID_WIDTH * i, GRID_HEIGHT * 10), Brick)
    }
    for (i <- 20 to World.WIDTH + 1) {
      blocks += new Block(Position(GRID_WIDTH * i, GRID_HEIGHT * 10), Brick)
    }
    blocks
  }

  def checkCollisions(world: World, player: Player): Iterable[Block] = {
    world.blocks.filter(_.position collides player.position)
  }

  def update(
      players: Iterable[Player],
      world: World,
      playerInputs: Map[String, PlayerInput],
      timeDelta: Long
   ): Iterable[Player] = {
    // update players based on inputs
    players.map(oldPlayer => {
      val playerName = oldPlayer.name
      val playerInput = playerInputs.get(playerName)

      if (playerInput.contains(PlayerInput()) && oldPlayer.state != Jump)
        // stand if no other input
        oldPlayer.copy(state = Stand)
      else {
        // set direction
        val playerDirection = playerInput match {
          case Some(PlayerInput(true, _, _, _)) => Left
          case Some(PlayerInput(_, true, _, _)) => Right
          case _ => oldPlayer.direction
        }

        // set move speed
        val offset = playerInput match {
          case Some(PlayerInput(true, _, _, _)) => Position(-PlayerStats.MOVE_SPEED * timeDelta, 0)
          case Some(PlayerInput(_, true, _, _)) => Position(PlayerStats.MOVE_SPEED * timeDelta, 0)
          case _ => Position(0, 0)
        }

        // set state based on movement
        val playerState =
          if (oldPlayer.state == Jump)
            Jump
          else
            playerInput match {
              case Some(PlayerInput(_, _, true, _)) => Jump
              case Some(PlayerInput(true, _, _, _)) => Run
              case Some(PlayerInput(_, true, _, _)) => Run
              case _ => Stand
            }
        //println(s"orig ${oldPlayer.position} offset: $offset state: $playerState input: ${playerInput}")

        // check if can jump
        var player = oldPlayer.copy(position = oldPlayer.position + offset,
          direction = playerDirection,
          state = playerState)
        val jumpEnergy = if (oldPlayer.state != Jump &&playerState == Jump && hasBlockBelow(world, player))
          PlayerStats.JUMP_STRENGTH
        else
          oldPlayer.jumpEnergy
        player = player.copy(jumpEnergy = jumpEnergy)

        // reset players position based from blocks its colliding with
        World
          .checkCollisions(world, player)
          .map(collision => oldPlayer.resetFromCollision(collision.position))
          .foreach(player = _)

        // don't share same space with other players
        val otherPlayers = players.filter(p => p.name != playerName)
        player
          .collideCheck(otherPlayers)
          // ignore collision if hit on the head
          .filter(otherPlayer => {
            val playerMid = Position(player.position.x, player.position.y + World.GRID_HEIGHT / 2)
            otherPlayer.position collides playerMid
          })
          // reset position for player
          .map(collision => oldPlayer.resetFromCollision(collision.position))
          .foreach(player = _)

        player
      }
    }).map(player => {
      // update players based on state
      val playerName = player.name
      val otherPlayers = players.filter(p => p.name != playerName)

      // apply gravity to player
      var newPlayer = player.copy(position = player.position + Position(0, -World.GRAVITY * timeDelta))

      // jump state specific calculations
      if (player.state == Jump) {
        // halt jump if up button no longer pressed
        if (newPlayer.jumpEnergy <= PlayerStats.JUMP_STRENGTH * 0.6 && newPlayer.jumpEnergy > 0 &&
            playerInputs.get(player.name).exists(!_.up)) {
          newPlayer = player.copy(jumpEnergy = 0)
        }

        // calculate offset for jump
        val jumpMove = (if (newPlayer.jumpEnergy <= 0) -World.GRAVITY * 2 else World.GRAVITY * 1.5) * timeDelta
        val offset = Position(0, jumpMove.toInt)
        newPlayer = player.copy(position = player.position + offset, jumpEnergy = newPlayer.jumpEnergy - 0.6)

        // check if collide while jumping up
        val collisions = World.checkCollisions(world, newPlayer)
        if (collisions.nonEmpty && newPlayer.jumpEnergy > 0) {
          val side = collisions.head.position.y - World.GRID_HEIGHT - 1
          // switch to stand
          newPlayer = player.copy(position = Position(player.position.x, side), state = Jump, jumpEnergy = 0)
        }

        // jump if killed someone
        if (newPlayer.jumpEnergy <= 0 && newPlayer.collideCheck(otherPlayers).nonEmpty) {
          newPlayer = newPlayer.copy(jumpEnergy = PlayerStats.JUMP_STRENGTH)
        }
      }

      // reset position if killed
      if (otherPlayers.flatMap(_.collideCheck(Array(player))).exists(_.jumpEnergy <= 0)) {
        val startPosition = World.startPosition(players)
        newPlayer = player.copy(position = startPosition, direction = Right, state = Stand, jumpEnergy = 0)
      }

      // check bottom collision
      World.checkCollisions(world, newPlayer).foreach(collision => {
        val top = collision.position.y + World.GRID_HEIGHT
        // update player position
        val playerState = if (player.state == Jump) Stand else player.state
        newPlayer = player.copy(position = Position(player.position.x, top), state = playerState)
      })

      // show up on other side if off screen
      val worldXLimit = World.WIDTH * World.GRID_WIDTH
      if (newPlayer.position.x < -World.GRID_WIDTH) {
        newPlayer = newPlayer.copy(position = Position(worldXLimit, newPlayer.position.y))
      } else if (newPlayer.position.x > worldXLimit) {
        newPlayer = newPlayer.copy(position = Position(-World.GRID_WIDTH, newPlayer.position.y))
      }

      newPlayer
    })
  }

  // check if player is standing on something
  def hasBlockBelow(world: World, player: Player): Boolean = {
    val newPlayer = player.copy(position=player.position + Position (0, -World.GRAVITY))
    World.checkCollisions(world, newPlayer).nonEmpty
  }
}
