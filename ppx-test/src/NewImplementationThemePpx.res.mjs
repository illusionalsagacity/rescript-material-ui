// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Core from "@material-ui/core";
import * as JsxRuntime from "react/jsx-runtime";
import * as Styles from "@material-ui/core/styles";

var useStyles = Styles.makeStyles((function (_theme) {
        return {
                rounded: {},
                root: {}
              };
      }), {
      name: "test"
    });

var Styles1 = {
  useStyles: useStyles
};

var useStyles$1 = Styles.makeStyles(function (theme) {
      return {
              rounded: {
                backgroundColor: theme.palette.background.paper,
                color: theme.palette.text.primary,
                padding: "15px"
              },
              root: {
                margin: "0 auto",
                maxWidth: "970px",
                width: "100%"
              }
            };
    });

var Styles2 = {
  useStyles: useStyles$1
};

var useStyles$2 = Styles.makeStyles({
      rounded: {
        backgroundColor: "#eee",
        color: "#333",
        padding: "15px"
      },
      root: {
        margin: "0 auto",
        maxWidth: "970px",
        width: "100%"
      }
    });

var Styles3 = {
  useStyles: useStyles$2
};

var useStyles$3 = Styles.makeStyles({
      rounded: {
        backgroundColor: "#eee",
        color: "#333",
        padding: "15px"
      },
      root: {
        margin: "0 auto",
        maxWidth: "970px",
        width: "100%"
      }
    }, {
      name: "test"
    });

var Styles4 = {
  useStyles: useStyles$3
};

function NewImplementationThemePpx(props) {
  useStyles();
  useStyles$1();
  useStyles$2();
  useStyles$3();
  return JsxRuntime.jsx(Core.Paper, {
              children: Caml_option.some(JsxRuntime.jsx(Core.Typography, {
                        children: "Some Content"
                      }))
            });
}

var make = NewImplementationThemePpx;

export {
  Styles1 ,
  Styles2 ,
  Styles3 ,
  Styles4 ,
  make ,
}
/* useStyles Not a pure module */
