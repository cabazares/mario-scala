import React from 'react';

import Game from './Game';


const width = 768;
const height = 480;

class Canvas extends React.Component {
  constructor(props) {
    super(props)

    this.state = {
      game: null,
      scores: {},
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

    game.onScoreUpdate((scores) => {
      this.setState({ scores })
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
    const { scores } = this.state
    return(
      <div>
        <canvas ref="canvas" width={width} height={height} />
        <img ref="mario" src="mario.png" alt="" style={{display: 'none'}} />
        <img ref="blocks" src="blocks.png" alt="" style={{display: 'none'}} />
        <Scores scores={scores} />
      </div>
    )
  }
}

const Scores = (props) => {
  const { scores } = props
  return (
    <div>
      {Object.keys(scores)
       .sort((a, b) => scores[b] - scores[a])
       .map(k => {
        return (
          <div>{k}: {scores[k]}</div>
        )
      })}
    </div>
  )
}

export default Canvas
