module type Styles = {
  type classes = {
    root: string,
    rounded: string,
  }
  let useStyles: unit => classes
}

type styles = {
  root: ReactDOM.Style.t,
  rounded: ReactDOM.Style.t,
}
type classes = {
  root: string,
  rounded: string,
}

@module("mui/styles")
external makeStyles: styles => unit => classes = "makeStyles"

let useStyles = makeStyles({
  root: ReactDOM.Style.make(),
  rounded: ReactDOM.Style.make(),
})

let classes = useStyles()