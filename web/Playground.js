Playground = function($, Stepper) {

var $$ = {};

$$.displayStepper = function(stepper) {
  var c = $('<span>').append(stepper.get());
  const update = () => {
    c.empty().append(stepper.get());
    stepper.watch(update);
  };
  update();
  return c;
};

$$.Name = function(name) {
  return {
    display: () => $('<span>').addClass('name').text(name)
  };
};

$$.Env = function(names) {
  return {
    display: () => {
      const div = $('<div>');
      for (var i = 0; i < names.length; i++) {
        div.append(names[i].display());
      }
      return div;
    }
  };
};

$$.Id = function() {
  return {
    display: () => $('<div>').text('Id'),
    envmap: env => env
  };
};

$$.ConsumeAll = function() {
  return {
    display: () => $('<div>').text('ConsumeAll'),
    envmap: env => Stepper.static($$.Env([]))
  }
};

// Dyanmic is a Block not a function because it needs to generate its own
// change events.
$$.Dynamic = function() {
  var inner = Stepper.cell(null);

  return {
    display: () => {
      const input = $('<input>').attr('type', 'text');
      const meta = $('<div>').append(inner);

      input.keydown(e => {
        if (e.keyCode == 13) {
          try {
            var newinner = eval(input.val());
          }
          catch (ex) {
            alert(ex);
          }
          inner.set(newinner);
        }
      });

      const update = () => {
        meta.empty();
        const i = inner.get();
        if (i) {
          meta.append(i.display());
        }
        inner.watch(update);
      };
      update();

      return $('<div>').append(input, meta);
    },
    envmap: env => {
      const apply = i => {
        if (i) {
          return i.envmap(env);
        }
        else {
          return env;
        }
      };
      return Stepper.join(Stepper.map(apply, inner));
    },
  };
};

return $$;

};
