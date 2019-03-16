
const BrickFrames = {
  DEFAULT: {
    height: 32,
    width: 32,
    offsetX: 544,
    offsetY: 224,
    count: 1,
  }
}

const SHEET_GAP = 0

class Block {
  constructor(swidth, sheight, context, sheet, position, type, hit) {
    this.swidth = swidth;
    this.sheight = sheight;
    this.context = context;
    this.sheet = sheet;

    this.position = position;
    this.type = type;

    this.frameIndex = 0;
    this.tickCount = 0;
    this.ticksPerFrame = 4;

    this.buffer = document.createElement('canvas');
    this.bufferctx = this.buffer.getContext('2d');

    // TODO: generate all images in cache
  }

  update () {

  }

  imgCache = {}
  getSheetImage(sheet, x, y, width, height, direction) {
    const key = [x, y, width, height, direction].join('_');

    if (!this.imgCache[key]) {
      this.bufferctx.clearRect(0, 0, this.buffer.width, this.buffer.height);
      this.bufferctx.setTransform(1, 0, 0, 1, 0, 0);
      this.bufferctx.drawImage(this.sheet, x, y, width, height, 0, 0, width, height);

      const image = new Image();
      image.src = this.buffer.toDataURL();
      this.imgCache[key] = image;
    }
    return this.imgCache[key];
  }

  draw () {
    const frameInfo = BrickFrames.DEFAULT;
    // get frame based on index
    const frameX = frameInfo.offsetX + ((frameInfo.width + SHEET_GAP) * this.frameIndex);

    const sheetImage = this.getSheetImage(this.sheet, frameX, frameInfo.offsetY,
                                          frameInfo.width, frameInfo.height,
                                          this.direction);
    this.context.drawImage(sheetImage,
                           this.position.x,
                           this.sheight - this.position.y - frameInfo.height);
  }
}

export default Block;
