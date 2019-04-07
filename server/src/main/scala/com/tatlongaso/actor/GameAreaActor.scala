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

// match Player object and its actor
case class PlayerWithActor(player: Player, actor: ActorRef)

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
        players.values.map(_.actor).foreach(subscriber => {
          subscriber ! WorldChanged(world)
          subscriber ! PlayersChanged(players.values.map(_.player))
        })

      case PlayerLeft(playerName) =>
        players = players - playerName
        players.values.map(_.actor).foreach(_ ! PlayersChanged(players.values.map(_.player)))

      case GameUpdate(playerInputs) =>
        val time = System.currentTimeMillis()
        val timeDelta = if (prevTime == 0) 16 else time - prevTime
        prevTime = time

        // get new player values after we pass them to the world
        players = World
          .update(players.values.map(_.player), world, playerInputs, timeDelta)
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
      players.values.map(_.actor).foreach(_ ! PlayersChanged(changedPlayers))

    players
  }
}
