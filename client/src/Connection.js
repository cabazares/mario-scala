
const WS_ADDRESS = process.env.REACT_APP_WS_ADDRESS || 'ws://localhost:8080'

class Connection {
  constructor() {
    this.subscribers = {
      players: [],
      world: [],
    }
  }

  connect(playerName) {
    this.ws = new WebSocket(`${WS_ADDRESS}/?player=${playerName}`);

    this.ws.onopen = (event) => {
       // handle event
    }

    this.ws.onmessage = (event) => {
      const data = JSON.parse(event.data)
      this.subscribers[data.type].forEach(callback => callback(data.data))
    }
  }

  subscribe(type, callback) {
    return this.subscribers[type].push(callback) - 1
  }

  send(message) {
    this.ws.send(message)
  }
}

export default Connection
