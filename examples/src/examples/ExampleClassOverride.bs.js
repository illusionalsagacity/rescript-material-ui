// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core from "@material-ui/core";
import * as JsxRuntime from "react/jsx-runtime";
import * as Styles from "@material-ui/styles";
import * as Colors from "@material-ui/core/colors";

var useStyles = Styles.makeStyles({
      fontSize: {
        fontSize: "30px"
      },
      bgColor: {
        backgroundColor: Colors.red[300]
      }
    });

function ExampleClassOverride(props) {
  var classes = useStyles();
  return JsxRuntime.jsx(Core.Button, {
              children: "Example Button",
              classes: {
                root: classes.fontSize,
                containedPrimary: classes.bgColor
              },
              color: "primary",
              variant: "contained"
            });
}

var make = ExampleClassOverride;

export {
  useStyles ,
  make ,
}
/* useStyles Not a pure module */
