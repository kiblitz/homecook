var version = "0.1.6";
var State = /** @class */ (function () {
    function State() {
        var _this = this;
        this.setMousePos = function (event) {
            _this.mouse_.x_ = event.pageX;
            _this.mouse_.y_ = event.pageY;
        };
        this.onMousePress = function (event) {
            _this.setMousePos(event);
        };
        this.onMouseRelease = function (event) {
        };
        this.onKeyDown = function (event) {
        };
        this.onKeyUp = function (event) {
        };
        this.updateDimensions = function () {
            _this.dimensions_.x_ = _this.canvas_.getBoundingClientRect().width;
            _this.dimensions_.y_ = _this.canvas_.getBoundingClientRect().height;
        };
        this.canvas_ = document.getElementById("canvas");
        this.dimensions_ = { x_: 0, y_: 0 };
        this.mouse_ = { x_: 0, y_: 0 };
        this.ctx_ = this.canvas_.getContext("2d");
        document.addEventListener("keydown", this.onKeyDown, false);
        document.addEventListener("keyup", this.onKeyUp, false);
        this.canvas_.addEventListener("mousemove", this.setMousePos, true);
        this.canvas_.addEventListener("mousedown", this.onMousePress, true);
        this.canvas_.addEventListener("mouseup", this.onMouseRelease, true);
        var body = document.getElementById("body");
        body.onresize = this.updateDimensions;
    }
    return State;
}());
function init() {
    document.title = "ts-canvas-template";
    return new State();
}
