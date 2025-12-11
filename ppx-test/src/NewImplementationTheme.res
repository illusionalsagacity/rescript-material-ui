module Styles: {
  type classes = {
    root: string,
    rounded: string,
  }
  let useStyles: unit => classes
} = {
  type styles = {
    root: ReactDOM.Style.t,
    rounded: ReactDOM.Style.t,
  }
  type classes = {
    root: string,
    rounded: string,
  }
  type useStyles = unit => classes
  @module("@material-ui/core/styles")
  external makeStyles: (Mui.Theme.t => styles) => useStyles = "makeStyles"
  let useStyles = makeStyles(theme => {
    root: {width: "100%", maxWidth: "970px", margin: "0 auto"},
    rounded: {
      backgroundColor: theme.palette.background.paper,
      color: theme.palette.text.primary,
      padding: "15px",
    },
  })
}

@react.component
let make = () => {
  let classes = Styles.useStyles()
  open Mui
  <Paper classes={root: classes.root, rounded: classes.rounded}>
    <Typography> {"Some Content"->React.string} </Typography>
  </Paper>
}
