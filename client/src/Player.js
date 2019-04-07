
const PlayerStates = {
  STAND: "stand",
  RUN: "run",
  JUMP: "jump",
}

const MarioFrames = {
  STAND: {
    height: 32,
    width: 32,
    offsetX: 160,
    offsetY: 68,
    count: 1,
  },
  RUN: {
    height: 32,
    width: 32,
    offsetX: 194,
    offsetY: 68,
    count: 3,
  },
  JUMP: {
    height: 32,
    width: 32,
    offsetX: 330,
    offsetY: 68,
    count: 1,
  },
}

const LuigiFrames = {
  STAND: {
    ...MarioFrames.STAND,
    offsetY: MarioFrames.STAND.offsetY + 130
  },
  RUN: {
    ...MarioFrames.RUN,
    offsetY: MarioFrames.RUN.offsetY + 130
  },
  JUMP: {
    ...MarioFrames.JUMP,
    offsetY: MarioFrames.JUMP.offsetY + 130
  },
}


const PlayerDirection = {
  LEFT: 'left',
  RIGHT: 'right',
}

const SHEET_GAP = 2;


class Player {
  constructor(swidth, sheight, context, sheet, name) {
    this.swidth = swidth;
    this.sheight = sheight;
    this.context = context;
    this.sheet = sheet;
    this.name = name;

    this.position = {
      x: 0,
      y: 0,
    }
    this.state = PlayerStates.RUN;
    this.direction = PlayerDirection.RIGHT;
    this.score = 0;

    this.frameIndex = 0;
    this.tickCount = 0;
    this.ticksPerFrame = 4;

    this.buffer = document.createElement('canvas');
    this.bufferctx = this.buffer.getContext('2d');

    // TODO: generate all images in cache
  }

  setPlayerPosition(x, y) {
    this.position.x = x;
    this.position.y = y;
  }

  setPlayerState(state) {
    this.state = state;
  }

  update() {
    const frameInfo = this.getFrameInfo();
    this.tickCount++;
    if (this.tickCount > this.ticksPerFrame) {
      this.tickCount = 0;
      this.frameIndex++;
      this.frameIndex = (this.frameIndex < frameInfo.count)? this.frameIndex : 0; 
    }
  }

  getFrameInfo() {
    const frames = (this.name.toLowerCase() === "luigi")? LuigiFrames : MarioFrames;
    let frame = frames.STAND;
    if (this.state === PlayerStates.STAND) { 
      frame = frames.STAND;
    } else if (this.state === PlayerStates.RUN) {
      frame = frames.RUN;
    } else if (this.state === PlayerStates.JUMP) {
      frame = frames.JUMP;
    }
    return frame;
  }

  imgCache = {}
  getSheetImage(sheet, x, y, width, height, direction) {
    const key = [x, y, width, height, direction].join('_');

    if (!this.imgCache[key]) {
      this.bufferctx.clearRect(0, 0, this.buffer.width, this.buffer.height);
      this.bufferctx.setTransform(1, 0, 0, 1, 0, 0);
      if (direction === PlayerDirection.LEFT) {
        this.bufferctx.translate(width, 0);
        this.bufferctx.scale(-1, 1);
      }
      this.bufferctx.drawImage(this.sheet, x, y, width, height, 0, 0, width, height);

      const image = new Image();
      image.src = this.buffer.toDataURL();
      this.imgCache[key] = image;
    }
    return this.imgCache[key];
  }

  draw() {
    const frameInfo = this.getFrameInfo();
    // get frame based on index
    const frameX = frameInfo.offsetX + ((frameInfo.width + SHEET_GAP) * this.frameIndex);

    const sheetImage = this.getSheetImage(this.sheet, frameX, frameInfo.offsetY,
                                          frameInfo.width, frameInfo.height,
                                          this.direction);
    this.context.drawImage(sheetImage,
                           this.position.x,
                           this.sheight - this.position.y - frameInfo.height);
  }

  drawDot() {
    const frameInfo = this.getFrameInfo();
    this.context.beginPath();
    this.context.arc(this.position.x + frameInfo.width / 2,
                     this.sheight - this.position.y,
                     2, 0, 2 * Math.PI);
    this.context.fillStyle = 'white';
    this.context.fill();
  }
}

export default Player;
