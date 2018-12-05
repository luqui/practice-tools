Stepper = function() {

var $$ = {};

// A stepper has the methods:
//   - get: get the current value
//   - watch: subscribe to *one* update

$$.static = function(v) {
  return {
    get: () => v,
    watch: cb => null
  }
};

$$.cell = function(v0) {
  var value = v0;

  var listeners = [];

  return {
    get: () => value,
    watch: cb => listeners.push(cb),
    set: vi => {
      value = vi;
      var notify = listeners;
      listeners = [];
      notify.map(cb => cb());
    }
  };
};

$$.zip = function(steppers) {
  return {
    get: () => steppers.map(s => s.get()),
    watch: cb => $$.watchMany(steppers, cb)
  };
};

$$.watchMany = function(steppers, cb) {
  var seen = false;
  var see = () => {
    if (!seen) {
      seen = true;
      cb();
    }
  };
  for (var i = 0; i < steppers.length; i++) {
    steppers[i].watch(see);
  }
};

$$.map = function(f, stepper) {
  return {
    get: () => f(stepper.get()),
    watch: cb => stepper.watch(cb)
  }
};

$$.join = function(stepper) {
  return {
    get: () => stepper.get().get(),
    watch: cb => {
      $$.watchMany([stepper, stepper.get()], cb);
    }
  }
};

return $$;

};
