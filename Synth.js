Synth = function(Tone) {

var $$ = {};

$$.Synth = function() {
  this.synth = new Tone.PolySynth(4, Tone.Synth, { envelope: { attack: 0.01, decay: 5, sustain: 0.1, release: 1 } }).toMaster();
  this.synth.volume.value = -6;
};

var toNoteOctave = function(key) {
  var oct = Math.floor(key/12);
  var notes = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B' ];
  return notes[key % 12] + oct;
};

$$.Synth.prototype.send = function(dat) {
  if ((dat[0] & 0xf0) == 0x90 && dat[2] != 0) { // note on
    this.synth.triggerAttack(toNoteOctave(dat[1]));
  }
  if ((dat[0] & 0xf0) == 0x80 || ((dat[0] & 0xf0) == 0x90 && dat[2] == 0)) {
    this.synth.triggerRelease(toNoteOctave(dat[1]));
  }
};

return $$;

};
