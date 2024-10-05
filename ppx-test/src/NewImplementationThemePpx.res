module Styles1 = %makeStyles((
  _theme => {
    root: ReactDOM.Style.make(),
    rounded: ReactDOM.Style.make(),
  },
  {name: "test"},
))
module Styles2 = %makeStyles(
  theme => {
    root: ReactDOM.Style.make(~width="100%", ~maxWidth="970px", ~margin="0 auto", ()),
    rounded: ReactDOM.Style.make(
      ~backgroundColor=theme.palette.background.paper,
      ~color=theme.palette.text.primary,
      ~padding="15px",
      (),
    ),
  })
module Styles3 = %makeStyles({
  root: ReactDOM.Style.make(~width="100%", ~maxWidth="970px", ~margin="0 auto", ()),
  rounded: ReactDOM.Style.make(~backgroundColor="#eee", ~color="#333", ~padding="15px", ()),
})
module Styles4 = %makeStyles((
  {
    root: ReactDOM.Style.make(~width="100%", ~maxWidth="970px", ~margin="0 auto", ()),
    rounded: ReactDOM.Style.make(~backgroundColor="#eee", ~color="#333", ~padding="15px", ()),
  },
  {name: "test"},
))

@react.component
let make = () => {
  let _classes1 = Styles1.useStyles()
  let _classes2 = Styles2.useStyles()
  let _classes3 = Styles3.useStyles()
  let _classes4 = Styles4.useStyles()
  open Mui
  <Paper>
    <Typography> {"Some Content"->React.string} </Typography>
  </Paper>
}
