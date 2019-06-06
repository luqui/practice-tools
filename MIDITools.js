MIDITools = function($, Keyboard, Synth) {

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

$$.EventSwitcher = function() {
  var event = $$.Event();
  var sourceid = {};  // blank object as unique id
  
  return {
    listen: event.listen,
    setSource: source => {
      var newsourceid = {};
      sourceid = newsourceid;
      source.listen(x => {
        if (sourceid === newsourceid) {
          event.fire(x);
        }
      });
    },
  };
};

$$.MIDIInput = function(indev) {
  this.event = $$.Event();
  this.widget = $('<span>').text(indev.name)[0];
  this.name = indev.name;
  var event = this.event;

  indev.onmidimessage = msg => event.fire(msg.data);  // I don't understand `this` scoping well enough to inline `event`
};

$$.KeyboardInput = function() {
  this.event = $$.Event();

  var event = this.event;
  var keyboard = new Keyboard.Keyboard(600, 200, 36, 72, m => event.fire(m));
  
  this.widget = keyboard.container;
  this.name = "Virtual Keyboard";

  keyboard.draw();
  keyboard.installClickHandler();
};

$$.InputSelector = function() {
  this.name = "Input Selector";
  this.event = $$.EventSwitcher();
  var event = this.event;

  var inputs = [new $$.KeyboardInput()];
  var select = $('<select>');
  var container = $('<div>');
  this.widget = $('<div>').append(select, container)[0];

  var makeSelect = () => {
    for (var i = 0; i < inputs.length; i++) {
      select.append($('<option>').attr('value', i).text(inputs[i].name));
      container.append(inputs[i].widget);
    }

    var dochange = () => {
      var i = select.val();
      for (var j = 0; j < inputs.length; j++) {
        $(inputs[j].widget).css('display', 'none');
      }
      $(inputs[i].widget).css('display', 'block');
      event.setSource(inputs[i].event);
    };
    select.change(dochange);
    select.val(0);
    dochange();
  };

  navigator.requestMIDIAccess().then(access => {
    for (var i of access.inputs.values()) {
      inputs.push(new $$.MIDIInput(i));
    }

    makeSelect();
  }, reason => {
    makeSelect();
  });
};


$$.SynthOutput = function() {
  this.name = "JS Synth";
  var synth = new Synth.Synth();
  this.send = dat => synth.send(dat);
  this.widget = $('<span>').text('JS Synth')[0];
};

$$.MIDIOutput = function(dev) {
  this.name = dev.name;
  this.send = dat => dev.send(dat);
  this.widget = $('<span>').text(dev.name)[0];
};


$$.OutputSelector = function() {
  this.name = "Output Selector";
  this.send = dat => {};
  var self = this;

  var outputs = [new $$.SynthOutput()];
  var select = $('<select>');
  var container = $('<div>');
  this.widget = $('<div>').append(select, container)[0];

  var makeSelect = () => {
    for (var i = 0; i < outputs.length; i++) {
      select.append($('<option>').attr('value', i).text(outputs[i].name));
      container.append(outputs[i].widget);
    }

    var dochange = () => {
      var i = select.val();
      for (var j = 0; j < outputs.length; j++) {
        $(outputs[j].widget).css('display', 'none');
      }
      $(outputs[i].widget).css('display', 'block');
      self.send = dat => outputs[i].send(dat);
    };
    select.change(dochange);
    select.val(0);
    dochange();
  };

  navigator.requestMIDIAccess().then(access => {
    for (var i of access.outputs.values()) {
      outputs.push(new $$.MIDIOutput(i));
    }

    makeSelect();
  }, reason => {
    makeSelect();
  });
};

return $$;

};
