export class AudioManager {
  level1BG = new Audio("audio/01-main-theme-overworld.mp3")
  effects = {
    coin: new Audio("audio/Coin.wav"),
    die: new Audio("audio/Die.wav"),
    item: new Audio("audio/Item.wav"),
    powerup: new Audio("audio/Powerup.wav"),
    squish: new Audio("audio/Squish.wav"),
    break: new Audio("audio/Break.wav"),
    bump: new Audio("audio/Bump.wav"),
    jump: new Audio("audio/Jump.wav"),
    flagpole: new Audio("audio/Flagpole.wav"),
  }

  constructor () {
    this.level1BG.loop = true
    this.level1BG.volume = 0.8
  }
  
  isPlaying(item) {
    return (item.currentTime > 0 && !item.paused &&
            !item.ended && item.readyState > 2)
  }

  playBG() {
    this.pauseBG()
    this.level1BG.currentTime = 0
    this.level1BG.play()
  }

  pauseBG() {
    if (this.isPlaying(this.level1BG)) {
      this.level1BG.pause()
    }
  }

  playSound(effect) {
    const sound = this.effects[effect]
    if (sound) {
      if (this.isPlaying(sound)) {
        sound.pause()
      }
      sound.currentTime = 0
      sound.play()
    }
  }
}

export default new AudioManager()
