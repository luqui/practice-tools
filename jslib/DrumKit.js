DrumKit = function(Tone) {

var $$ = {};

var drums = {
  36: "kick",
  37: "snare1",
  38: "snare2",
  40: "snare3",
  42: "hat1",
  44: "hat2",
  46: "hat3",
  43: "tom1",
  45: "tom2",
  47: "tom3",
  50: "ride1",
  53: "ride2",
  55: "crash"
};

$$.Synth = function() {
  this.players = new Tone.Players().toMaster();
  this.players.volume.value = -10;
  this.loadSamples();
};

$$.Synth.prototype.loadSamples = function() {
  for (var noteid in drums) {
    for (var vol of ["soft", "loud"]) {
      (() => {
        var drum = drums[noteid];
        var vel = vol;
        var fname = "assets/Drum-" + drum + "-" + vel + ".mp3";
        this.players.add(drum + "-" + vel, fname);
      })();
    }
  }
};

$$.Synth.prototype.noteOn = function(note, vel) {
  Tone.context.resume();
  
  if (!(note in drums)) {
    return;
  }

  var drumname = drums[note];

  if (vel == 0) {
    return;
  }
  else if (vel < 80) {
    drumname += "-soft";
  }
  else {
    drumname += "-loud";
  }

  this.players.get(drumname).start();
};

$$.Synth.prototype.send = function(dat) {
  if ((dat[0] & 0xf0) == 0x90) {
    this.noteOn(dat[1], dat[2]);
  }
};

return $$;

};


