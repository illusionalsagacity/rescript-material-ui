[@bs.obj]
external makePropsMui:
  (
    ~children: 'children=?,
    ~defaultValue: 'union_r9bp=?,
    ~name: string=?,
    ~onChange: 'any_rlgb=?,
    ~value: 'any_rpa3=?,
    ~key: string=?,
    ~ref: ReactDOMRe.domRef=?,
    ~className: string=?,
    ~row: bool=?,
    unit
  ) =>
  _;

let makeProps =
    (
      ~children: option('children)=?,
      ~defaultValue:
         option(
           [
             | `Array(array(string))
             | `Int(int)
             | `Float(float)
             | `String(string)
           ],
         )=?,
      ~name: option(string)=?,
      ~onChange: option(ReactEvent.Form.t => unit)=?,
      ~value: option('any_rpa3)=?,
      ~key: option(string)=?,
      ~ref: option(ReactDOMRe.domRef)=?,
      ~className: option(string)=?,
      ~row: option(bool)=?,
      (),
    ) =>
  makePropsMui(
    ~children?,
    ~defaultValue=?
      defaultValue->(Belt.Option.map(v => MaterialUi_Helpers.unwrapValue(v))),
    ~name?,
    ~onChange?,
    ~value?,
    ~key?,
    ~ref?,
    ~className?,
    ~row?,
    (),
  );

[@bs.module "@material-ui/core"]
external make: React.component('a) = "RadioGroup";
