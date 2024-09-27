# Reason PPX addition for ReScript Material-UI

This library creates a code extension for conveniently using the `withStyles` functionality of MaterialUi in a typesafe way.

Check out the documentation here: [https://rescript-material-ui.cca.io/](https://rescript-material-ui.cca.io/)

Testing printing:

```shell
node_modules/rescript/bsc \
  -only-parse \
  -bs-jsx 4 \
  -bs-jsx-mode automatic \
  -ppx public/rescript-material-ui-ppx/_build/default/src/Bin.exe \
  -dsource \
  ppx-test/src/NewImplementationThemePpx.res
```

```shell
node_modules/rescript/bsc \
  -only-parse \
  -bs-jsx 4 \
  -bs-jsx-mode automatic \
  -ppx public/rescript-material-ui-ppx/_build/default/src/Bin.exe \
  -dsource \
  -uncurried \
  ppx-test/src/NewImplementationThemePpx.res
```