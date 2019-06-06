Synth = function(Tone) {

var $$ = {};

// I guess we're writing our own version of polysynth because Tone's is buggy on android.
$$.Synth = function() {
  this.voices = [];
};

$$.Synth.prototype.noteOn = function(note) {
  Tone.context.resume();

  // check if note is already playing
  for (var v of this.voices) {
    if (v.note === note) {
      console.log("attack ", note);
      v.synth.triggerAttack(note);
      return;
    }
  }

  // otherwise, use a free voice
  for (var v of this.voices) {
    if (v.note === null) {
      console.log("attack reuse ", note);
      v.note = note;
      v.synth.triggerAttack(v.note);
      return;
    }
  }

  // otherwise, allocate a new voice
  var newvoice = new Tone.Synth({ envelope: { attack: 0.01, decay: 5, sustain: 0.1, release: 1 } }).toMaster();
  newvoice.triggerAttack(note);
  console.log("attack alloc", note);
  this.voices.push({ note: note, synth: newvoice });
};

$$.Synth.prototype.noteOff = function(note) {
  for (var v of this.voices) {
    if (v.note === note) {
      v.synth.triggerRelease();
      v.note = null;
      console.log("release ", note)
    }
  }
};

var toNoteOctave = function(key) {
  var oct = Math.floor(key/12);
  var notes = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B' ];
  return notes[key % 12] + oct;
};

$$.Synth.prototype.send = function(dat) {
  if ((dat[0] & 0xf0) == 0x90 && dat[2] != 0) { // note on
    console.log("Note on ", dat[1]);
    this.noteOn(toNoteOctave(dat[1]));
  }
  if ((dat[0] & 0xf0) == 0x80 || ((dat[0] & 0xf0) == 0x90 && dat[2] == 0)) {
    console.log("Note off ", dat[1]);
    this.noteOff(toNoteOctave(dat[1]));
  }
};

return $$;

};
