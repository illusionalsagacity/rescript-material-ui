# Reason bindings for material-ui

This library provides Reason bindings for
[material-ui](https://material-ui-next.com/). It's automatically generated by the tools contained in this repo.

# Important: Version 1.3.0 (!! Breaking Changes !!)

There are some **breaking changes**, please make sure to check out the [changelog](https://github.com/jsiebern/bs-material-ui/blob/master/CHANGELOG.md).

I am also working on some optimized / improved bindings that make heavy use of the latest bucklescript features. The goal is to get the bindings as close to zero cost as possible. Some debt is unavoidable unfortunately, but code optimization should be able to compile a lot of that overhead away. I'd greatly appreciate some help in testing this new version. It contains many breaking changes though and is still prone to changes in the new API. You can install it by running this command:

```
yarn add @jsiebern/bs-material-ui@zero
```

# Important: Version 1.2.0

The master version is now using the hooks system instead of the old component API.

# Important: Version 1.1.0

There are a few backsteps and important changes in 1.1.0:

- I have removed the icons again in favor of this package: [@mscharley/bs-material-ui-icons](https://github.com/mscharley/bs-material-ui-icons)
  This package basically contains them in the way I would have published them in their own package, so I can't see any need to do so anymore.
- This repo is now a mono-repo managed by lerna as managing this project got a little out of hand.
  _Important: The packages still should be treated separately when playing around with the code, as there are a great many steps involved in actually getting to the end result._
  _Even more important: I do not guarantee that the build process will work for you. I have tried to generalize it a bit but it's still quite messy. It's good enough for me if I can use it personally to publish the bindings. Feel free to send PRs that alleviate the pain for others a bit._

**Please make sure to look at the [changelog](https://github.com/jsiebern/bs-material-ui/blob/master/CHANGELOG.md) for the breaking changes!**

## Installation (for your Reason project)

Run:

    yarn add @jsiebern/bs-material-ui

to add the library to your project dependencies. And add `@jsiebern/bs-material-ui` to the `bs-dependencies` node of your `bsconfig.json`.

## Installation of the `withStyles` code extension (ppx)

Install the package: `yarn add --dev @jsiebern/bs-material-ui-ppx`.

(The PPX builds on the fly using `bsb-native`. This is a quite heavy dependency and takes a while but will ensure that the PPX runs on your system)

Add the entry `./node_modules/@jsiebern/bs-material-ui-ppx/ppx` to the `ppx-flags` node of your `bsconfig.json`.

## Example

Please see [the examples folder](https://github.com/jsiebern/bs-material-ui/tree/master/public/bs-material-ui/examples).
(Running the example code: `yarn examples`)

## withStyles

In material-ui, the `withStyles` [HOC](https://reactjs.org/docs/higher-order-components.html) takes care of turning React styles into CSS via [react-jss](https://github.com/cssinjs/react-jss). It passes a `classes` prop onto the component with the first level keys of the style object passed on.

HOC do not translate well into Reason which is why we are using a [render prop](http://reactpatterns.com/#render-callback) to make things easier. [(More information on the topic).](https://www.youtube.com/watch?v=BcVAq3YFiuc)

## withStyles Example PPX (typesafe)

**Make sure you have implemented the ppx file (see installation for reference)**

**Important: In order to use `theme => styles` you need to provide a `<MaterialUi_ThemeProvider theme={MaterialUi_Theme.create()}>` at the top of the tree!**

The code extension allows you to write a typesafe styled component with ease. It follows the format `[%mui.withStyles "ComponentName"({ className: ReactDOMRe.Style.t })]`. The generated Component has a render function which passes on a `record` with the class keys. See the example below.

```reason
let component = ReasonReact.statelessComponent("Example");

[%mui.withStyles
  "StyledExample"({
    alignRight:
      ReactDOMRe.Style.make(~width="100%", ~textAlign="right", ()),
  })
];

let make = _children => {
  ...component,
  render: _self =>
    <StyledExample>
      ...{
        classes =>
          <div className={classes.alignRight}>
            "Example text - aligned to the right"->ReasonReact.string
          </div>
      }
    </StyledExample>,
};
```

## withStyles Example (unsafe)

You need to pass a `classes` prop of type `list( { name: string, styles: ReactDOMRe.Style.t } )` and a `render` function to the component. See the following example:

```reason
let component = ReasonReact.statelessComponent("Example");

let make = _children => {
  ...component,
  render: _self =>
    <MaterialUi.WithStyles
      classes=[
        {
          name: "alignRight",
          styles:
            ReactDOMRe.Style.make(~width="100%", ~textAlign="right", ()),
        },
      ]
      render={
        classes =>
          <div className=classes##alignRight>
            "Example text - aligned to the right"->ReasonReact.string
          </div>
      }
    />,
};
```

## Colors

All Colors are accessible in Submodules of the Module `Colors`. Color keys that are a pure number begin with a `c`. [(MUI Docs Reference).](https://material-ui-next.com/style/color/)

Example:

```reason
[%mui.withStyles
  "ColorExample"({
    bgColor: ReactDOMRe.Style.make(~backgroundColor=MaterialUi.Colors.Red.c300, ())
  })
];
```

## Overriding with classes

To take advantage of Reasons type system when overriding classes directly on components they have been converted into Variants and need to be passed as a `list` to the components `classes` prop. It is best used in combination with the `MaterialUi.WithStyles` component.

[(MUI Docs Reference).](https://material-ui-1dab0.firebaseapp.com/customization/overrides/#overriding-with-classes)

Example:

```reason
let component = ReasonReact.statelessComponent("Example");

[%mui.withStyles
  "OverrideExample"({
    fontSize: ReactDOMRe.Style.make(~fontSize="30px", ()),
    bgColor:
      ReactDOMRe.Style.make(
        ~backgroundColor=MaterialUi.Colors.Red.c300,
        (),
      ),
  })
];

let make = _children => {
  ...component,
  render: _self =>
    <OverrideExample>
      ...{
        classes =>
          <MaterialUi.Button
            color=`Primary
            variant=`Contained
            classes=[
              Root(classes.fontSize),
              RaisedPrimary(classes.bgColor),
            ]>
            "Example Button"
          </MaterialUi.Button>
      }
    </OverrideExample>,
};
```
