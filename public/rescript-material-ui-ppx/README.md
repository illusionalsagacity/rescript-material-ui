# Reason PPX addition for ReScript Material-UI

This library creates a code extension for conveniently using the `withStyles` functionality of MaterialUi in a typesafe way.

Testing printing:

```shell
yarn run test:source -uncurried ../../ppx-test/src/NewImplementationThemePpx.res
yarn run test:parsetree -uncurried ../../ppx-test/src/NewImplementationThemePpx.res
```

or

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