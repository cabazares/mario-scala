import React from 'react';

import Game from './Game';


const width = 768;
const height = 480;

class Canvas extends React.Component {
  constructor(props) {
    super(props)

    this.state = {
      game: null,
    }
  }

  componentDidMount() {
    const canvas = this.refs.canvas
    const ctx = canvas.getContext("2d")
    const game = new Game(width, height, ctx)

    game.setResources({
      mario: this.refs.mario,
      blocks: this.refs.blocks
    })

    this.setState({ game })
  }

  componentDidUpdate(prevProps) {
    const { startGame, playerName } = this.props
    const { game } = this.state

    if (!prevProps.startGame && startGame) {
      game.init(playerName);
    }
  }

  render() {
    return(
      <div>
        <canvas ref="canvas" width={width} height={height} />
        <img ref="mario" src="mario.png" alt="" style={{display: 'none'}} />
        <img ref="blocks" src="blocks.png" alt="" style={{display: 'none'}} />
      </div>
    )
  }
}
export default Canvas
