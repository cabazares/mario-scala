import React, { Component } from 'react';

import NameForm from './NameForm';
import Canvas from './Canvas';
import './App.css';

const playerNames = [
  'Mario',
  'Luigi',
  'Peach',
  'Toad',
  'Yoshi',
  'Bowser',
  'Daisy',
  'Wario',
  'Waluigi',
]

class App extends Component {
  constructor(props) {
    super(props)
    this.state = {
      playerName: playerNames[Math.floor(Math.random() * playerNames.length)],
      startGame: false,
    }

    this.startGame = this.startGame.bind(this);
  }

  startGame(playerName) {
    this.setState({
      playerName,
      startGame: true,
    })
  }

  render() {
    const { playerName, startGame } = this.state;

    return (
      <div className="App">
        {!startGame && <NameForm value={playerName} onSubmit={this.startGame} />}
        <Canvas playerName={playerName} startGame={startGame} />
      </div>
    );
  }
}

export default App;
