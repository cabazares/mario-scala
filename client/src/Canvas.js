import React from 'react';

import Game from './Game';


const width = 512;
const height = 480;

class Canvas extends React.Component {
  componentDidMount() {
    const canvas = this.refs.canvas
    const ctx = canvas.getContext("2d")

    const game = new Game(width, height, ctx);
    game.setResources({
      mario: this.refs.mario,
      blocks: this.refs.blocks
    })
    game.init();
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
