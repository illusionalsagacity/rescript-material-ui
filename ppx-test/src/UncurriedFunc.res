type styles = {
  root: ReactDOM.Style.t,
  rounded: ReactDOM.Style.t,
}
type classes = {
  root: string,
  rounded: string,
}

// let arity1 = _a => {
//   root: ReactDOM.Style.make(),
//   rounded: ReactDOM.Style.make(),
// }

@module("mui/styles")
external makeStyles: Mui.Theme.t => styles => unit => classes = "makeStyles"

@module("mui/styles")
external makeStyles2: (Mui.Theme.t, {..}) => styles => unit => classes = "makeStyles"
