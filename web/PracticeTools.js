PracticeTools = function(VF, $) {

var $$ = {};

$$.Event = function() {
  var listeners = [];
  return {
    listen: cb => listeners.push(cb),
    fire: x => {
      listeners.map(cb => cb(x));
    }
  };
};

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

  var inevent = $$.Event();

  navigator.requestMIDIAccess().then(access => {
    var inputs = Array.from(access.inputs.values());
    var outputs = Array.from(access.outputs.values());
    setDeviceList(indevs, inputs, () => {
      indevice = inputs[indevs.val()];
      if (indevice) {
        indevice.onmidimessage = msg => {
          inevent.fire(msg);
        };
      }
    });
    setDeviceList(outdevs, outputs, () => {
      outdevice = outputs[outdevs.val()];
    });

    // Debug TODO remember
    indevs.val(1).change();
    outdevs.val(1).change();
  });

  return {
    widget: widget,
    sendMIDI: data => { 
      if (outdevice) { outdevice.send(data) } 
    },
    event: inevent
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
          interface.sendMIDI([0x90, 96, pat.velocities[j]]);
        });
        scheduler.schedule(beat + (j+0.05)/pat.spacing, () => {
          interface.sendMIDI([0x90, 96, 0]);
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


$$.AccuracyFilter = function(scheduler, listen) {
  var subdivs = $('<input>').attr('type', 'text').val(4);
  var accuracy = $('<input>').attr('type', 'text').val(35);
  var enabled = $('<input>').attr('type', 'checkbox');
  var widget = $('<div>').append(
    $('<span>').text('Subdivisions: '), subdivs,
    $('<span>').text('Accuracy (ms): '), accuracy,
    $('<span>').text('Enabled: '), enabled);

  var hitevent = $$.Event();
  var missevent = $$.Event();
  var noteevent = $$.Event();

  listen.listen(msg => {
    var data = msg.data;
    if (data[0] == 0x90 && data[2] != 0
        && scheduler.running() && enabled.prop('checked')) {
      var divs = subdivs.val();
      var now = scheduler.now();
      var target = Math.round(scheduler.now()*divs)/divs;
      if (60000.0*Math.abs(target-now)/scheduler.tempo() <= accuracy.val()) {
        hitevent.fire();
        noteevent.fire(msg);
      }
      else {
        missevent.fire();
      }
    }
    else {
      noteevent.fire(msg);
    }
  });

  return {
    widget: widget,
    hitevent: hitevent,
    missevent: missevent,
    noteevent: noteevent
  }
};


$$.SkillMeter = function(interface, hitevent, missevent) {
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

  hitevent.listen(() => {
    var target = targetScore.val();
    if (score >= target-1 && score < target) {
      interface.sendMIDI([0x91, 56, 0x60]); // Win bell!
      setTimeout(() => interface.sendMIDI([0x91, 56, 0]), 30);
      score = 0;
    }
    score++;
    score = Math.min(score, target);
    update();
  });

  missevent.listen(() => {
    score = Math.max(0, score-penalty.val());
    update();
  });

  return {
    widget: widget
  };
};


$$.Recorder = function(interface, scheduler, onrecord) {
  var recorderLight = $('<div>').addClass('recorderLight');
  var widget = $('<div>').append(recorderLight);

  var transcript = [];
  var recording = false;
  var startpoint = 0;

  interface.listen(msg => {
    if (!scheduler.running()) {
      return;
    }

    var data = msg.data;
    if (data[0] == 0xb0 && data[1] == 0x43 && data[2] >= 0x40) {
      if (recording) {
        // Don't disable recording immediately, so we can catch the rest
        // of the beat.
        scheduler.schedule(Math.ceil(scheduler.now())-0.01, () => { 
          recording = false; 
          recorderLight.removeClass('recording');
          if (onrecord) {
            onrecord(transcript);
          }
        });
      }
      else {
        startpoint = Math.ceil(scheduler.now());
        transcript = [];
        recording = true;
        recorderLight.addClass('recording');
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
