import React, { Component } from 'react';

import Canvas from './Canvas';
import './App.css';


class App extends Component {
  render() {
    return (
      <div className="App">
        <Canvas text="example" />
      </div>
    );
  }
}

export default App;
