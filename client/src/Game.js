import Player from './Player';
import Connection from './Connection'
import Block from './Block'
import Audio from './sound'


class Game {
  players = [];
  blocks = [];

  constructor(width, height, context) {
    this.width = width;
    this.height = height;
    this.context = context;

    // create connection class
    this.connection = new Connection()
  }

  setResources(resources) {
    this.resources = resources;
  }

  init(playerName) {
    this.playerName = playerName;
    // make sure resources are loaded before starting
    Promise.all(Object.values(this.resources).map(img => this.checkImage(img.src)))
                  .then(() => this.start());
  }

  playerJoined(name) {
    const player = new Player(this.width, this.height, this.context, this.resources.mario, name);
    this.players.push(player);
    return player
  }

  start() {
    // create player
    this.player = this.playerJoined(this.playerName);

    this.connection.connect(this.playerName)
    this.connection.subscribe('players', (playerActions) => {
        const nameCompare = player => (e) => e.name === player.name

        // create players if necessary
        playerActions
          .map(p => p.name)
          .filter(a => !this.players.find(p => a === p.name))
          .map(p => this.playerJoined(p))

        // loop through all players if they should move
        for (var player of this.players) {
          const playerAction = playerActions.find(nameCompare(player));

          // skip player if no action sent
          if (!playerAction) {
            continue;
          }

          // handle position
          if (playerAction.position) {
            const {x, y} = playerAction.position
            player.setPlayerPosition(x, y)
          }
          // handle direction
          if (playerAction.direction) {
            player.direction = playerAction.direction
          }

          // play jump sound if just jumped
          if (player.state !== 'jump' && playerAction.state === 'jump') {
            Audio.playSound('jump')
          }

          // handle state
          if (playerAction.state) {
            player.state = playerAction.state
          }
        }
    })

    this.connection.subscribe('world', (world) => {
      this.blocks = world.map(block => {
        return new Block(this.width, this.height, this.context, this.resources.blocks,
                          block.position, block.type, block.hit);
      })
    })

    document.onkeydown = document.onkeyup = (e) => {
      e = e || window.event;
      let action;
      if (e.keyCode === 38) {
         action = "up"
      } else if (e.keyCode === 40) {
         action = "down"
      } else if (e.keyCode === 37) {
         action = "left"
      } else if (e.keyCode === 39) {
         action = "right"
      }

      if (action) {
         this.connection.send(`${action}_${e.type}`)
      }
    }

    this.gameLoop();
    //AudioManager.playBG()
  }

  drawBG() {
    this.context.fillStyle = "#5D94FB";
    this.context.fillRect(0, 0, this.width, this.height);
  }

  // main game loop -------------------------
  gameLoop () {

    window.requestAnimationFrame(() => this.gameLoop());

    this.drawBG();
    this.blocks.forEach(b => b.draw());
    this.players.forEach(p => {
      p.update();
      p.draw();
    })
  }

  // UTILS ----------------------------------
  checkImage(path) {
    return new Promise(resolve => {
        const img = new Image();
        img.onload = () => resolve({path, status: 'ok'});
        img.onerror = () => resolve({path, status: 'error'});

        img.src = path;
    });
  }

}

export default Game;
