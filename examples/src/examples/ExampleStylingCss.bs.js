// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core from "@material-ui/core";
import * as JsxRuntime from "react/jsx-runtime";
import * as Styles from "@material-ui/core/styles";

function ExampleStylingCss(props) {
  return JsxRuntime.jsx(Styles.StylesProvider, {
              injectFirst: true,
              children: JsxRuntime.jsx(Core.Button, {
                    onClick: (function (prim) {
                        
                      }),
                    children: "Hello, plain CSS & MUI!",
                    className: "my-global-class-name",
                    color: "primary"
                  })
            });
}

var make = ExampleStylingCss;

export {
  make ,
}
/* @material-ui/core Not a pure module */
