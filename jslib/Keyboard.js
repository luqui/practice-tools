Keyboard = function(jQuery) {

var $$ = {};

$$.Keyboard = function(startKey, endKey, midicallback) {
  var width = window.innerWidth;
  var height = Math.round(width / 4);

  this.canvas = jQuery('<canvas>')
      .attr('width', width)
      .attr('height', height)
      .css('position', 'absolute')
      .css('z-index', 1)[0];
  this.overlay = jQuery('<canvas>')
      .attr('width', width)
      .attr('height', height)
      .css('position', 'absolute')
      .css('z-index', 2)[0];
  this.chordButton = jQuery('<button>')
      .css('width', width)
      .css('height', 100)
      .css('z-index', 0)
      .text('Chord')
      .css('display', 'none');
  this.container = jQuery('<div>')
      .css('position', 'relative')
      .css('width', width)
      .css('height', height+100)
      .append(this.canvas, this.overlay, jQuery('<div>').css('height', height), this.chordButton)[0];

  this.startKey = startKey;
  this.endKey = endKey;

  this.mobileDetected = false;

  this.callback = midicallback || (() => { });

  var octaves = Math.ceil(endKey/12) - Math.floor(startKey/12);
  this.keyWidth = width / (7*octaves);

  this.playingNotes = [];
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

$$.Keyboard.prototype._getKey = function(x, y) {
  // this is a dumb slow way
  for (var k = this.startKey; k < this.endKey; k++) {
    // try black keys first, since they are on top.
    if (!this._isBlack(k)) {
      continue;
    }
    var rect = this._getRect(k);
    if (rect[0] <= x && rect[1] <= y && x < rect[0] + rect[2] && y < rect[1] + rect[3]) {
      return k;
    }
  }
  for (var k = this.startKey; k < this.endKey; k++) {
    var rect = this._getRect(k);
    if (rect[0] <= x && rect[1] <= y && x < rect[0] + rect[2] && y < rect[1] + rect[3]) {
      return k;
    }
  }
  return null;
};

$$.Keyboard.prototype._drawKey = function(cx, ovcx, k, color) {
  if (cx === null) { cx = this.canvas.getContext('2d'); }
  if (ovcx === null) { ovcx = this.overlay.getContext('2d'); }

  var rect = this._getRect(k);
  if (rect === null) { return; }

  var isblack = this._isBlack(k);
  if (color === undefined || color === null) {
    color = isblack ? 'black' : 'white';
  }

  if (isblack) {
    ovcx.fillStyle = color;
    ovcx.fillRect(rect[0], rect[1], rect[2], rect[3]);
    ovcx.strokeRect(rect[0], rect[1], rect[2], rect[3]);
  }
  else {
    cx.fillStyle = color;
    cx.fillRect(rect[0], rect[1], rect[2], rect[3]);
    cx.strokeRect(rect[0], rect[1], rect[2], rect[3]);
  }
};

$$.Keyboard.prototype.draw = function() {
  var cx = this.canvas.getContext('2d');
  var ovcx = this.overlay.getContext('2d');
  
  for (var k = this.startKey; k < this.endKey; k++) {
    this._drawKey(cx, ovcx, k);
  }
};

$$.Keyboard.prototype._transformMousePos = function(x, y) {
  var rect = this.canvas.getBoundingClientRect();
  if (x < rect.left || y < rect.top || x >= rect.left + this.canvas.width || y >= rect.top + this.canvas.height){
    return null;
  }

  return {
    x: x - rect.left,
    y: y - rect.top
  };
};

$$.Keyboard.prototype.installClickHandler = function() {
  var self = this;
  var chordEnabled = false;

  var down = function(pos, shift) {
    if (pos == null) { return; }
    var key = self._getKey(pos.x, pos.y);
    if (key == null) { return; }


    if (!shift) {
      self.playingNotes.push(key);
      self._drawKey(null, null, key, 'orange');
      self.callback([0x90, key, 96]);
    }
    else {
      if (self.playingNotes.includes(key)) {
        var ix = self.playingNotes.findIndex(k => k == key);
        self.playingNotes = self.playingNotes.slice(0,ix).concat(self.playingNotes.slice(ix+1));
        self._drawKey(null, null, key);
      }
      else {
        self.playingNotes.push(key);
        self._drawKey(null, null, key, 'yellow');
      }
    }
  };

  var up = function(shift) {
    if (!shift) {
      for (var k of self.playingNotes) {
        self.callback([0x90, k, 0]);
        self._drawKey(null, null, k);
      }
      self.playingNotes = [];
    }
  };
  
  var playChord = function() {
    var notes = self.playingNotes;

    self.playingNotes = [];

    for (var k of notes) {
      self.callback([0x90, k, 96]);
      self._drawKey(null, null, k, 'orange');
    }

    setTimeout(() => {
      for (var k of notes) {
        self.callback([0x90, k, 0]);
        self._drawKey(null, null, k);
      }
    }, 500);
  };

  jQuery(self.overlay).mousedown(e => {
    var pos = self._transformMousePos(e.clientX, e.clientY);
    if (pos != null) {
      down(pos, e.shiftKey || chordEnabled);
    }
  });

  jQuery(self.overlay).on('touchstart', e => {
    self.chordButton.css('display', 'block');
    self.mobileDetected = true;
    
    var t = e.touches[e.touches.length-1];
    var pos = self._transformMousePos(t.clientX, t.clientY);
    if (pos != null) {
      down(pos, chordEnabled);
    }
    return false;  // don't propagate
  });

  jQuery(window).mouseup(e => {
    if (!self.mobileDetected) {
      up(e.shiftKey || chordEnabled);
    }
  });

  jQuery(window).on('touchend', e => {
    up(chordEnabled);
  });

  jQuery(window).keyup(e => {
    if (e.which == 16) { // shift
      playChord();
    }
  });

  self.chordButton.click(function() {
    if (chordEnabled) {
      self.chordButton.css('background', '');
      chordEnabled = false;
      playChord();
    }
    else {
      self.chordButton.css('background', 'blue');
      chordEnabled = true;
    }
  });
};

return $$;

};
