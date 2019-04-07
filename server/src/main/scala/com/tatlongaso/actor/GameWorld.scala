package com.tatlongaso.actor

object GameWorld {
  def updateGameState(
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
        val jumpEnergy = if (oldPlayer.state != Jump &&playerState == Jump && hasBlockBelow(world, player).isDefined)
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
        val otherPlayers = players.filter(p => p.name != playerName)
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

        player
      }
    }).map(player => {
      // update players based on state
      val playerName = player.name
      var jumpEnergy = player.jumpEnergy
      // apply gravity to player
      val bottom = -World.GRAVITY * timeDelta
      var newPlayer = player.copy(position = player.position + Position(0, bottom))
      val goingDown = jumpEnergy <= 0

      if (player.state == Jump) {
        // halt jump if up button no longer pressed
        if (jumpEnergy <= PlayerStats.JUMP_STRENGTH * 0.6 && jumpEnergy > 0) {
          playerInputs.get(player.name).filter(pi => !pi.up).map(_ => jumpEnergy = 0)
        }

        // calculate offset for jump
        val jumpMove = (if (goingDown) -World.GRAVITY * 2 else World.GRAVITY * 1.5) * timeDelta
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
        val otherPlayers = players.filter(p => p.name != playerName)
        newPlayer.collideCheck(otherPlayers).map(fragged => {
          // reset player position if killed
          // FIXME: update different player
          // val startPosition = World.startPosition(players)
          // val resetPlayer = fragged.copy(position = startPosition, direction = Right, state = Stand, jumpEnergy = 0)
          // players(fragged.name) = resetPlayer
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

  def hasBlockBelow(world: World, player: Player): Option[Block] = {
    val newPlayer = player.copy(position=player.position + Position (0, -World.GRAVITY))
    World.checkPlayerCollision (world, newPlayer).headOption
  }


}
