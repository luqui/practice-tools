PracticeTools = function(VF, $) {

var $$ = {};

$$.MIDIInterface = function() {
  var indevice = null;
  var outdevice = null;

  var setDeviceList = function(sel, devs, onchange) {
    sel.empty();
    sel.append($('<option>').attr('value', null).text("None"));
    for (var i = 0; i < devs.length; i++) {
      sel.append(
        $('<option>').attr('value', i).text(devs[i].name));
    }
    sel.change(onchange);
  };

  var indevs = $('<select>');
  var outdevs = $('<select>');
  var widget = $('<div>').append(
    $('<span>').text('In:'), indevs,
    $('<span>').text('Out:'), outdevs);

  navigator.requestMIDIAccess().then(access => {
    var inputs = Array.from(access.inputs.values());
    var outputs = Array.from(access.outputs.values());
    setDeviceList(indevs, inputs, () => {
      indevice = inputs[indevs.val()];
      if (indevice) {
        indevice.onmidimessage = msg => {
          listeners.map(cb => cb(msg))
        };
      }
    });
    setDeviceList(outdevs, outputs, () => {
      outdevice = outputs[outdevs.val()];
    });

    // Debug TODO remember
    indevs.val(1).change();
    outdevs.val(0).change();
  });

  var listeners = [];

  return {
    widget: widget,
    sendMIDI: data => { 
      if (outdevice) { outdevice.send(data) } 
    },
    listen: cb => listeners.push(cb)
  };
};


$$.Scheduler = function() {
  var startcbs = [];
  var stopcbs = [];

  var tempoBox = $('<input>')
                  .attr('type', 'text')
                  .val(100);
  var startButton = $('<button>');
  var widget = $('<div>').append($('<span>').text('Tempo: '), tempoBox, startButton);

  var started = null;
  var runcount = 0;

  var initButton = () => {
    started = null;
    runcount++;
    tempoBox.prop('disabled', false);

    startButton.text('Start');
    startButton.off('click').on('click', () => {
      tempoBox.prop('disabled', true);
      startButton.text('Stop');
      startButton.off('click').on('click', initButton);
      started = Date.now();
      runcount++;

      var runcbs = startcbs;
      startcbs = [];
      runcbs.map(cb => cb());
    });
    
    var runcbs = stopcbs;
    stopcbs = [];
    runcbs.map(cb => cb());
  };
  initButton();

  var tempo = () => parseFloat(tempoBox.val());
  var now = () => tempo() * (Date.now() - started) / 60000.0;

  return {
    widget: widget,
    running: () => started !== null,
    now: now,
    schedule: (beats, cb) => {
      if (started) {
        // check/runcount is to make sure we abandon all scheduled events,
        // even if stop/run is pressed very quickly
        var check = runcount;
        var target = started + beats / tempo() * 60000.0;
        setTimeout(() => { if (runcount == check) { cb() } }, target - Date.now());
      }
    },
    tempo: tempo,
    onstart: cb => { 
      startcbs.push(cb);
    },
    onstop: cb => {
      stopcbs.push(cb);
    }
  };
};


var metronomePatterns = [
  {
    notation: (cx, stave) => {
      VF.Formatter.FormatAndDraw(cx, stave, [new VF.StaveNote({ clef: "treble", keys: ["b/4"], duration: "qr" })]);
    },
    spacing: 1,
    velocities: [ 0 ]
  },
  {
    notation: (cx, stave) => {
      VF.Formatter.FormatAndDraw(cx, stave, [new VF.StaveNote({ clef: "treble", keys: ["b/4"], duration: "q" })]);
    },
    spacing: 1,  // notes per beat
    velocities: [ 64 ]
  },
  {
    notation: (cx,stave) => {
      var voice = new VF.Voice({num_beats: 1, beat_value: 4});
      var notes = [ new VF.StaveNote({ clef: "treble", keys: ["b/4"], duration: "16" })
                  , new VF.StaveNote({ clef: "treble", keys: ["b/4"], duration: "16" })
                  , new VF.StaveNote({ clef: "treble", keys: ["b/4"], duration: "16" })
                  , new VF.StaveNote({ clef: "treble", keys: ["b/4"], duration: "16" })
                  ];
      voice.addTickables(notes);
      (new VF.Formatter()).joinVoices([voice]).formatToStave([voice], stave);
      var beam = new VF.Beam(notes);
      voice.draw(cx, stave);
      beam.setContext(cx).draw();
    },
    spacing: 4,
    velocities: [ 80, 44, 44, 44, ]
  }
];



$$.Metronome = function(interface, scheduler) {
  var renderPatterns = () => {
    var container = $('<ul>');

    var first = true;
    for (var i = 0; i < metronomePatterns.length; i++) {
      var renderSpan = $('<span>');
      var input = $('<input>').attr('type', 'radio')
                              .attr('name', 'metronomePattern') // TODO uniquify
                              .attr('value', i);
      if (first) {
        input.attr('checked', 'checked');
        first = false;
      }

      $(container)
        .append($('<li>')
          .append(input)
          .append(renderSpan));
        

      var renderer = new VF.Renderer(renderSpan[0], VF.Renderer.Backends.SVG);
      renderer.resize(500, 50);
      var cx = renderer.getContext();

      var stave = new VF.Stave(0, -25, 500);
      //stave.setContext(cx).draw();

      metronomePatterns[i].notation(cx, stave);
    }
    return container;
  };

  var widget = renderPatterns();
  var currentPattern = () => metronomePatterns[widget.find(':checked').val()];

  var schedBeat = beat => {
    var pat = currentPattern();
    for (var i = 0; i < pat.spacing; i++) {
      (() => {
        var j = i;
        scheduler.schedule(beat + j/pat.spacing, () => {
          interface.sendMIDI([0x91, 42, pat.velocities[j]]);
        });
        scheduler.schedule(beat + (j+0.5)/pat.spacing, () => {
          interface.sendMIDI([0x91, 42, 0]);
        });
      })();
    }
    scheduler.schedule(beat+1, () => schedBeat(beat+1));
  };
  var initSched = () => {
    scheduler.onstart(() => {
      schedBeat(0);
      scheduler.onstop(initSched);
    });
  };
  initSched();

  return {
    widget: widget
  };
};


$$.AccuracyFilter = function(interface, scheduler, meter) {
  var subdivs = $('<input>').attr('type', 'text').val(4);
  var accuracy = $('<input>').attr('type', 'text').val(35);
  var enabled = $('<input>').attr('type', 'checkbox');
  var widget = $('<div>').append(
    $('<span>').text('Subdivisions: '), subdivs,
    $('<span>').text('Accuracy (ms): '), accuracy,
    $('<span>').text('Enabled: '), enabled);

  interface.listen(msg => {
    var data = msg.data;
    if (data[0] == 0x90 && data[2] != 0
        && scheduler.running() && enabled.prop('checked')) {
      var divs = subdivs.val();
      var now = scheduler.now();
      var target = Math.round(scheduler.now()*divs)/divs;
      if (60000.0*Math.abs(target-now)/scheduler.tempo() <= accuracy.val()) {
        interface.sendMIDI(data);
        if (meter) {
          meter.hit();
        }
      }
      else {
        if (meter) {
          meter.miss();
        }
      }
    }
    else {
      interface.sendMIDI(data);
    }
  });

  return {
    widget: widget
  }
};


$$.SkillMeter = function(interface) {
  var targetScore = $('<input>').attr('type', 'text').val(100);
  var penalty = $('<input>').attr('type', 'text').val(10);

  var innerBar = $('<div>').addClass('progress');

  var widget = $('<div>').append(
    $('<span>').text('Target:'), targetScore,
    $('<span>').text('Penalty:'), penalty,
    $('<div>').addClass('progressBar').append(innerBar));


  var score = 0;
  var update = () => innerBar.css('width', Math.round(100.0*score/targetScore.val()) + '%');
  update();

  return {
    widget: widget,
    hit: () => {
      var target = targetScore.val();
      if (score >= target-1 && score < target) {
        interface.sendMIDI([0x91, 56, 0x60]); // Win bell!
        setTimeout(() => interface.sendMIDI([0x91, 56, 0]), 30);
        score = 0;
      }
      score++;
      score = Math.min(score, target);
      update();
    },
    miss: () => {
      score = Math.max(0, score-penalty.val());
      update();
    },
  };
};


$$.Recorder = function(interface, scheduler, onrecord) {
  var enabled = $('<input>').attr('type', 'checkbox');
  var widget = $('<div>').append(
    $('<span>').text('Recorder enabled: '), enabled);

  var transcript = [];
  var recording = false;
  var startpoint = 0;

  interface.listen(msg => {
    if (!enabled.prop('checked')) { return; }

    var data = msg.data;
    if (data[0] == 0xb0 && data[1] == 0x43 && data[2] >= 0x40) {
      if (recording) {
        // Don't disable recording immediately, so we can catch the rest
        // of the beat.
        scheduler.schedule(Math.ceil(scheduler.now())-0.01, () => { 
          recording = false; 
          if (onrecord) {
            onrecord(transcript);
          }
        });
      }
      else {
        startpoint = Math.ceil(scheduler.now());
        transcript = [];
        recording = true;
      }
    }

    if (recording) {
      transcript.push([ scheduler.now() - startpoint, data ]);
    }
  });

  return {
    widget: widget
  };
};

$$.Looper = function(interface, scheduler) {
  var widget = $('<div>');

  var playScript = script => {
    var startat = Math.ceil(scheduler.now());
    for (var i = 0; i < script.length; i++) {
      (() => { 
        var j = i;
        scheduler.schedule(script[j][0] + startat, () => {
          interface.sendMIDI(script[j][1]);
          if (j == script.length-1) {
            playScript(script);
          }
        });
      })();
    }
  };

  return {
    widget: widget,
    play: playScript
  };
};

return $$;

};
