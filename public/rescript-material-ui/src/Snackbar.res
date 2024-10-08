type horizontal = [#center | #left | #right]

type vertical = [#bottom | #top]

module AnchorOrigin = {
  type t = {"horizontal": option<horizontal>, "vertical": option<vertical>}
  @obj external make: (~horizontal: horizontal=?, ~vertical: vertical=?, unit) => t = ""
}

module Classes = {
  type t = {
    "root": option<string>,
    "anchorOriginTopCenter": option<string>,
    "anchorOriginBottomCenter": option<string>,
    "anchorOriginTopRight": option<string>,
    "anchorOriginBottomRight": option<string>,
    "anchorOriginTopLeft": option<string>,
    "anchorOriginBottomLeft": option<string>,
  }
  @obj
  external make: (
    ~root: string=?,
    ~anchorOriginTopCenter: string=?,
    ~anchorOriginBottomCenter: string=?,
    ~anchorOriginTopRight: string=?,
    ~anchorOriginBottomRight: string=?,
    ~anchorOriginTopLeft: string=?,
    ~anchorOriginBottomLeft: string=?,
    unit,
  ) => t = ""
}

module TransitionComponent = {
  type t
  external string: string => t = "%identity"
  external transitionComponent_func: Any.t => t = "%identity"
  external element: React.element => t = "%identity"
}

module TransitionDuration_shape = {
  type t = {"appear": option<Number.t>, "enter": option<Number.t>, "exit": option<Number.t>}
  @obj external make: (~appear: Number.t=?, ~enter: Number.t=?, ~exit: Number.t=?, unit) => t = ""
}

module TransitionDuration = {
  type t
  external int: int => t = "%identity"
  external float: float => t = "%identity"
  external shape: TransitionDuration_shape.t => t = "%identity"
}

@react.component @module("@material-ui/core")
external make: (
  ~action: React.element=?,
  ~anchorOrigin: AnchorOrigin.t=?,
  ~autoHideDuration: Number.t=?,
  ~children: React.element=?,
  ~classes: Classes.t=?,
  ~className: string=?,
  ~\"ClickAwayListenerProps": {..}=?,
  ~\"ContentProps": {..}=?,
  ~disableWindowBlurListener: bool=?,
  ~key: string=?,
  ~message: React.element=?,
  ~onClose: (ReactEvent.Synthetic.t, string) => unit=?,
  ~onMouseEnter: ReactEvent.Mouse.t => unit=?,
  ~onMouseLeave: ReactEvent.Mouse.t => unit=?,
  ~\"open": bool=?,
  ~resumeHideDuration: Number.t=?,
  ~\"TransitionComponent": TransitionComponent.t=?,
  ~transitionDuration: TransitionDuration.t=?,
  ~\"TransitionProps": {..}=?,
  ~id: string=?,
  ~style: ReactDOM.Style.t=?,
  ~ref: ReactDOM.domRef=?,
) => React.element = "Snackbar"
