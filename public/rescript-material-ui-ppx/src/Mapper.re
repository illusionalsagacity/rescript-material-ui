open Migrate_parsetree;
open Ast_410;
open Asttypes;
open Parsetree;

let withStylesMapper = (_argv, _) => {
  ...Ast_mapper.default_mapper,
  module_expr: (mapper, mexpr) => {
    switch (mexpr) {
    | {pmod_desc: Pmod_extension(({txt: "makeStyles", loc}, pstr)), _} =>
      switch (pstr) {
      // just a record
      | PStr([
          {
            pstr_desc:
              Pstr_eval({pexp_desc: Pexp_record(fields, None), _}, _),
            _,
          },
        ]) =>
        UncurriedImplementation.rewriteMakeStyles(fields, None)
      // record with options
      | PStr([
          {
            pstr_desc:
              Pstr_eval(
                {
                  pexp_desc:
                    Pexp_tuple([
                      {pexp_desc: Pexp_record(fields, None), _},
                      {pexp_desc: Pexp_record(options, None), _},
                    ]),
                  _,
                },
                _,
              ),
            _,
          },
        ]) =>
        UncurriedImplementation.rewriteMakeStyles(fields, Some(options))
      // theme function
      | PStr([
          {
            pstr_desc:
              Pstr_eval(
                {
                  pexp_desc:
                    Pexp_fun(
                      _,
                      _,
                      _,
                      {pexp_desc: Pexp_record(fields, None), _},
                    ),
                  _,
                } as fn,
                _,
              ),
            _,
          },
        ]) =>
        NewImplementation.rewriteMakeStylesWithTheme(fields, fn, None)
      // theme function with options
      | PStr([
          {
            pstr_desc:
              Pstr_eval(
                {
                  pexp_desc:
                    Pexp_tuple([
                      {
                        pexp_desc:
                          Pexp_fun(
                            _,
                            _,
                            _,
                            {pexp_desc: Pexp_record(fields, None), _},
                          ),
                        _,
                      } as fn,
                      {pexp_desc: Pexp_record(options, None), _},
                    ]),
                  _,
                },
                _,
              ),
            _,
          },
        ]) =>
        NewImplementation.rewriteMakeStylesWithTheme(
          fields,
          fn,
          Some(options),
        )
      // uncurried theme function
      | PStr([
          {
            pstr_desc:
              Pstr_eval(
                {
                  pexp_desc:
                    Pexp_construct(
                      _,
                      Some({
                        pexp_desc:
                          Pexp_fun(
                            _,
                            _,
                            _,
                            {pexp_desc: Pexp_record(fields, None), _},
                          ),
                        _,
                      }),
                    ),
                  pexp_attributes: [
                    {
                      // attr_payload: Ppat_var("arity1"),
                      attr_name: {txt: "res.arity", _},
                      attr_payload:
                        PStr([
                          {
                            pstr_desc:
                              Pstr_eval(
                                {
                                  pexp_desc:
                                    Pexp_constant(Pconst_integer("1", None)),
                                  _,
                                },
                                _,
                              ),
                            _,
                          },
                        ]),
                      _,
                    },
                  ],
                  _,
                } as fn,
                _attributes,
              ),
            _,
          },
        ]) =>
        UncurriedImplementation.rewriteMakeStylesWithTheme(fields, fn, None)
      // uncurried function with options
      | PStr([
          {
            pstr_desc:
              Pstr_eval(
                {
                  pexp_desc:
                    Pexp_tuple([
                      {
                        pexp_desc:
                          Pexp_construct(
                            _,
                            Some({
                              pexp_desc:
                                Pexp_fun(
                                  _,
                                  _,
                                  _,
                                  {pexp_desc: Pexp_record(fields, None), _},
                                ),
                              _,
                            }),
                          ),
                        pexp_attributes: [
                          {
                            // attr_payload: Ppat_var("arity1"),
                            attr_name: {txt: "res.arity", _},
                            attr_payload:
                              PStr([{pstr_desc: Pstr_eval(_, _), _}]),
                            _,
                          },
                        ],
                        _,
                      } as fn,
                      {pexp_desc: Pexp_record(options, None), _},
                    ]),
                  _,
                },
                _,
              ),
            _,
          },
        ]) =>
        UncurriedImplementation.rewriteMakeStylesWithTheme(
          fields,
          fn,
          Some(options),
        )
      | _ => Utils.raiseError(~loc, None)
      }

    | other => Ast_mapper.default_mapper.module_expr(mapper, other)
    };
  },
};
