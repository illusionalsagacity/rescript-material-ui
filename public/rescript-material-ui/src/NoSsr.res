@react.component @module("@material-ui/core")
external make: (
  ~children: React.element=?,
  ~defer: bool=?,
  ~fallback: React.element=?,
  ~id: string=?,
  ~style: ReactDOM.Style.t=?,
  ~key: string=?,
  ~ref: ReactDOM.domRef=?,
) => React.element = "NoSsr"
