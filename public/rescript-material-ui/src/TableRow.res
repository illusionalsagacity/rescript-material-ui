module Classes = {
  type t = {
    "root": option<string>,
    "selected": option<string>,
    "hover": option<string>,
    "head": option<string>,
    "footer": option<string>,
  }
  @obj
  external make: (
    ~root: string=?,
    ~selected: string=?,
    ~hover: string=?,
    ~head: string=?,
    ~footer: string=?,
    unit,
  ) => t = ""
}

module Component = {
  type t
  external string: string => t = "%identity"
  external callback: (unit => React.element) => t = "%identity"
  external element: React.element => t = "%identity"
}

@react.component @module("@material-ui/core")
external make: (
  ~children: React.element=?,
  ~classes: Classes.t=?,
  ~className: string=?,
  ~component: Component.t=?,
  ~hover: bool=?,
  ~selected: bool=?,
  ~id: string=?,
  ~style: ReactDOM.Style.t=?,
  ~onClick: ReactEvent.Mouse.t => unit=?,
  ~onDoubleClick: ReactEvent.Mouse.t => unit=?,
  ~key: string=?,
  ~ref: ReactDOM.domRef=?,
) => React.element = "TableRow"
