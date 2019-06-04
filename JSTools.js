JSTools = function($) {

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

return $$;

};
