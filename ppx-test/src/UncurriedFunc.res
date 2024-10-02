type styles = {
  root: ReactDOM.Style.t,
  rounded: ReactDOM.Style.t,
}
type classes = {
  root: string,
  rounded: string,
}

@module("mui/styles")
external makeStyles: (Mui.Theme.t => styles) => unit => classes = "makeStyles"

// let useStyles = makeStyles(theme => {
//   root: ReactDOM.Style.make(~width="100%", ~maxWidth="970px", ~margin="0 auto", ()),
//   rounded: ReactDOM.Style.make(
//     ~backgroundColor=theme.palette.background.paper,
//     ~color=theme.palette.text.primary,
//     ~padding="15px",
//     (),
//   ),
// })