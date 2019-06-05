Keyboard = function(jQuery) {

var $$ = {};

$$.Keyboard = function(canvas, startKey, endKey) {
  this.canvas = canvas;
  this.startKey = startKey;
  this.endKey = endKey;

  this.octaves = Math.ceil(endKey/12) - Math.floor(startKey/12);
  this.keyWidth = canvas.width / (7*this.octaves);
};

$$.Keyboard.prototype._isBlack = function(key) {
  var rel = key % 12;
  return rel == 1 || rel == 3 || rel == 6 || rel == 8 || rel == 10;
};

$$.Keyboard.prototype._getRect = function(key) {
  if (key < this.startKey || key >= this.endKey) {
    return null;
  }

  var rel = key % 12;
  var oct = Math.floor(key / 12) - Math.floor(this.startKey/12);
  var isblack = this._isBlack(key);


  var blackWidth = 0.75;
  var blackHeight = 0.60;
  var blackOffset = 1 - blackWidth / 2;

  var xposes = [0, blackOffset, 1, 1 + blackOffset, 2, 3, 3 + blackOffset, 4, 4 + blackOffset, 5, 5 + blackOffset, 6];

  return [ 7 * this.keyWidth * oct + this.keyWidth * xposes[rel]
         , 0
         , isblack ? this.keyWidth * blackWidth : this.keyWidth
         , isblack ? this.canvas.height * blackHeight : this.canvas.height
         ];

};

$$.Keyboard.prototype.draw = function() {
  var cx = this.canvas.getContext('2d');
  
  for (var k = this.startKey; k < this.endKey; k++) {
    var rect = this._getRect(k);
    if (rect === null) {
      throw "Shouldn't happen bc bounds of for loop";
    }

    if (this._isBlack(k)) {
      cx.fillRect(rect[0], rect[1], rect[2], rect[3]);
    }
    else {
      cx.strokeRect(rect[0], rect[1], rect[2], rect[3]);
    }
  }
};

return $$;

};
