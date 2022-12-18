const version = "0.1.6";

interface Vector2D {
  x_ : number,
  y_ : number,
}

class State {
  canvas_: HTMLCanvasElement;
  dimensions_: Vector2D;
  ctx_: CanvasRenderingContext2D;
  mouse_: Vector2D;

  constructor() {
    this.canvas_ = <HTMLCanvasElement> document.getElementById("canvas");
    this.dimensions_ = { x_ : 0, y_ : 0 };
    this.mouse_ = { x_ : 0, y_ : 0 };
    this.ctx_ = this.canvas_.getContext("2d");

    document.addEventListener("keydown", this.onKeyDown, false);
    document.addEventListener("keyup", this.onKeyUp, false);

    this.canvas_.addEventListener("mousemove", this.setMousePos, true);
    this.canvas_.addEventListener("mousedown", this.onMousePress, true);
    this.canvas_.addEventListener("mouseup", this.onMouseRelease, true);

    let body = <HTMLBodyElement> document.getElementById("body");
    body.onresize = this.updateDimensions;
  }

  setMousePos = (event: MouseEvent) => {
    this.mouse_.x_ = event.pageX;
    this.mouse_.y_ = event.pageY;
  }
 
  onMousePress = (event: MouseEvent) => {
    this.setMousePos(event);
  }

  onMouseRelease = (event: MouseEvent) => {
  }

  onKeyDown = (event: KeyboardEvent) => {
  } 

  onKeyUp = (event: KeyboardEvent) => {
  } 

  updateDimensions = () => {
    this.dimensions_.x_ = this.canvas_.getBoundingClientRect().width;
    this.dimensions_.y_ = this.canvas_.getBoundingClientRect().height;
  }
}

function init() {
  document.title = "ts-canvas-template";
  return new State();
}
