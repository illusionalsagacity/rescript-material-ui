module Styles = %makeStyles((
  {
    root: {width: "100%", maxWidth: "970px", margin: "0 auto"},
    rounded: {backgroundColor: "grey", color: "white", padding: "15px"},
  },
  {
    classNamePrefix: "xxx",
    // generateId: () => "blasdnsad" ++ Math.Int.random(0, 100)->Int.toString,
  },
))

@react.component
let make = () => {
  let classes = Styles.useStyles()
  open Mui
  <Paper classes={{root: classes.root, rounded: classes.rounded}}>
    <Typography> {"Some Content"->React.string} </Typography>
  </Paper>
}
