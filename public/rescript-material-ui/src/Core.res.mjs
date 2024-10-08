// Generated by ReScript, PLEASE EDIT WITH CARE


var UNSAFE_INTERNAL = {};

function getAsString(theme, breakpoint) {
  var bpGet = breakpoint.NAME === "up" ? (function (v) {
        return v.up;
      }) : (function (v) {
        return v.down;
      });
  var bpValue;
  if (breakpoint.NAME === "up") {
    var match = breakpoint.VAL;
    bpValue = typeof match === "object" ? match.VAL : (
        match === "md" ? "md" : (
            match === "sm" ? "sm" : (
                match === "xl" ? "xl" : (
                    match === "xs" ? "xs" : "lg"
                  )
              )
          )
      );
  } else {
    var match$1 = breakpoint.VAL;
    bpValue = typeof match$1 === "object" ? match$1.VAL : (
        match$1 === "md" ? "md" : (
            match$1 === "sm" ? "sm" : (
                match$1 === "xl" ? "xl" : (
                    match$1 === "xs" ? "xs" : "lg"
                  )
              )
          )
      );
  }
  var func = bpGet(theme.breakpoints);
  return func(bpValue);
}

function get(theme, breakpoint) {
  return getAsString(theme, breakpoint);
}

var Breakpoint = {
  UNSAFE_INTERNAL: UNSAFE_INTERNAL,
  getAsString: getAsString,
  get: get
};

var ServerStyleSheets = {};

export {
  Breakpoint ,
  ServerStyleSheets ,
}
/* No side effect */
