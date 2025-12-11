module App = {
  open Mui

  @react.component
  let make = () => <>
    <NewImplementation />
    <br />
    <br />
    <NewImplementationTheme />
    <br />
    <br />
    <center>
      <Typography variant=H4> {React.string("PPX Result")} </Typography>
    </center>
    <br />
    <br />
    <NewImplementationPpx />
    <br />
    <br />
    <NewImplementationThemePpx />
  </>
}

let el = ReactDOM.querySelector("#app")->Option.getOrThrow

ReactDOM.Client.createRoot(el)->ReactDOM.Client.Root.render(<App />)
