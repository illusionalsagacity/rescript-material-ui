// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Core from "@material-ui/core";
import * as JsxRuntime from "react/jsx-runtime";
import * as Styles from "@material-ui/core/styles";

var useStyles = Styles.makeStyles({
      root: {
        margin: "0 auto",
        maxWidth: "970px",
        width: "100%"
      },
      rounded: {
        backgroundColor: "grey",
        color: "white",
        padding: "15px"
      }
    });

var Styles$1 = {
  useStyles: useStyles
};

function NewImplementation(props) {
  var classes = useStyles();
  return JsxRuntime.jsx(Core.Paper, {
              children: Caml_option.some(JsxRuntime.jsx(Core.Typography, {
                        children: "Some Content"
                      })),
              classes: {
                root: classes.root,
                rounded: classes.rounded
              }
            });
}

var make = NewImplementation;

export {
  Styles$1 as Styles,
  make ,
}
/* useStyles Not a pure module */
