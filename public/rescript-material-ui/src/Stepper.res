module Component = {
  type t
  external string: string => t = "%identity"
  external callback: (unit => React.element) => t = "%identity"
  external element: React.element => t = "%identity"
}

type variant = [#elevation | #outlined]

module Classes = {
  type t = {
    "root": option<string>,
    "horizontal": option<string>,
    "vertical": option<string>,
    "alternativeLabel": option<string>,
  }
  @obj
  external make: (
    ~root: string=?,
    ~horizontal: string=?,
    ~vertical: string=?,
    ~alternativeLabel: string=?,
    unit,
  ) => t = ""
}

type orientation = [#horizontal | #vertical]

@react.component @module("@material-ui/core")
external make: (
  ~component: Component.t=?,
  ~elevation: Number.t=?,
  ~square: bool=?,
  ~variant: variant=?,
  ~id: string=?,
  ~style: ReactDOM.Style.t=?,
  ~activeStep: Number.t=?,
  ~alternativeLabel: bool=?,
  ~children: React.element=?,
  ~classes: Classes.t=?,
  ~className: string=?,
  ~connector: React.element=?,
  ~nonLinear: bool=?,
  ~orientation: orientation=?,
  ~key: string=?,
  ~ref: ReactDOM.domRef=?,
) => React.element = "Stepper"
