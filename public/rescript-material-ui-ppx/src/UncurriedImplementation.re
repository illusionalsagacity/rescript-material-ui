open Migrate_parsetree;
open Ast_410;
open Ast_helper;

type rawFields = list((Asttypes.loc(Longident.t), Parsetree.expression));

let parseFields = (fields: rawFields) => {
  fields
  |> List.fold_left(
       (prev, field) => {
         switch (field) {
         | (
             {txt: Longident.Lident(name), loc}: Asttypes.loc(Longident.t),
             _,
           ) => [
             (name, loc),
             ...prev,
           ]
         | _ => prev
         }
       },
       [],
     );
};

let createOptionsWithTypeConstraint = (fields: rawFields) => {
  let keys = parseFields(fields);
  [
    Str.type_(
      Nonrecursive,
      [
        Type.mk(
          ~kind=
            Ptype_record(
              keys
              |> List.fold_left(
                   (prev, (name, loc)) =>
                     switch (name) {
                     | "defaultTheme" => [
                         Type.field(
                           ~loc,
                           Location.mkloc(name, loc),
                           Typ.constr(
                             Longident.unflatten(["Mui", "Theme", "t"])
                             |> Option.get
                             |> Location.mknoloc,
                             [],
                           ),
                         ),
                         ...prev,
                       ]
                     | "name" => [
                         Type.field(
                           ~loc,
                           Location.mkloc(name, loc),
                           Typ.constr(
                             Longident.Lident("string") |> Location.mknoloc,
                             [],
                           ),
                         ),
                         ...prev,
                       ]
                     | "flip" => [
                         Type.field(
                           ~loc,
                           Location.mkloc(name, loc),
                           Typ.constr(
                             Longident.unflatten(["Js", "nullable"])
                             |> Option.get
                             |> Location.mknoloc,
                             [
                               Typ.constr(
                                 Longident.Lident("bool") |> Location.mknoloc,
                                 [],
                               ),
                             ],
                           ),
                         ),
                         ...prev,
                       ]
                     | "generateId" => [
                         Type.field(
                           ~loc,
                           Location.mkloc(name, loc),
                           Typ.arrow(
                             Nolabel,
                             Typ.constr(
                               Longident.Lident("unit") |> Location.mknoloc,
                               [],
                             ),
                             Typ.constr(
                               Longident.Lident("string") |> Location.mknoloc,
                               [],
                             ),
                           ),
                         ),
                         ...prev,
                       ]
                     | "classNamePrefix" => [
                         Type.field(
                           ~loc,
                           Location.mkloc(name, loc),
                           Typ.constr(
                             Longident.Lident("string") |> Location.mknoloc,
                             [],
                           ),
                         ),
                         ...prev,
                       ]
                     | "media" => [
                         Type.field(
                           ~loc,
                           Location.mkloc(name, loc),
                           Typ.constr(
                             Longident.Lident("string") |> Location.mknoloc,
                             [],
                           ),
                         ),
                         ...prev,
                       ]
                     | "meta" => [
                         Type.field(
                           ~loc,
                           Location.mkloc(name, loc),
                           Typ.constr(
                             Longident.Lident("string") |> Location.mknoloc,
                             [],
                           ),
                         ),
                         ...prev,
                       ]
                     | "index" => [
                         Type.field(
                           ~loc,
                           Location.mkloc(name, loc),
                           Typ.constr(
                             Longident.Lident("int") |> Location.mknoloc,
                             [],
                           ),
                         ),
                         ...prev,
                       ]
                     | "link" => [
                         Type.field(
                           ~loc,
                           Location.mkloc(name, loc),
                           Typ.constr(
                             Longident.Lident("bool") |> Location.mknoloc,
                             [],
                           ),
                         ),
                         ...prev,
                       ]
                     | _ => prev
                     },
                   [],
                 ),
            ),
          Location.mknoloc("options"),
        ),
      ],
    ),
    Str.value(
      Nonrecursive,
      [
        Vb.mk(
          Pat.constraint_(
            Pat.var(Location.mknoloc("options")),
            Typ.constr(
              Longident.unflatten(["options"])
              |> Option.get
              |> Location.mknoloc,
              [],
            ),
          ),
          Exp.record(fields, None),
        ),
      ],
    ),
  ];
};

/**
This is a helper function to create a record type expression mapping over the fields given, with the types being mapped to the given list of strings (to flatten)
 */
let createRecordTypeExpression =
    (~keys: list((string, Location.t)), ~mapTo: list(string), name: string) => {
  Type.mk(
    ~kind=
      Ptype_record(
        keys
        |> List.map(((name, loc)) => {
             Type.field(
               ~loc,
               Location.mkloc(name, loc),
               Typ.constr(
                 Longident.unflatten(mapTo) |> Option.get |> Location.mknoloc,
                 [],
               ),
             )
           }),
      ),
    Location.mknoloc(name),
  );
};

let arity1 =
  Typ.variant(
    [Ast_helper.Rf.mk(Rtag(Location.mknoloc("Has_arity1"), true, []))],
    Closed,
    None,
  );

let arity2 =
  Typ.variant(
    [Ast_helper.Rf.mk(Rtag(Location.mknoloc("Has_arity2"), true, []))],
    Closed,
    None,
  );

  // arity2 -> Ptyp_constr([Ptyp_arrow, ])

let getMakeStylesTypeUncurried = (options) =>
  Typ.constr(
    Longident.unflatten(["function$"]) |> Option.get |> Location.mknoloc,
    [
      Typ.arrow(
        Nolabel,
        Typ.constr(
          Longident.unflatten(["Mui", "Theme", "t"]) |> Option.get |> Location.mknoloc,
          [],
        ),
        Typ.constr(
          Longident.unflatten(["function$"]) |> Option.get |> Location.mknoloc,
          [
            Typ.arrow(
              Nolabel,
              Typ.constr(
                Longident.unflatten(["styles"])
                |> Option.get
                |> Location.mknoloc,
                [],
              ),
              Typ.constr(
                Longident.unflatten(["function$"])
                |> Option.get
                |> Location.mknoloc,
                [
                  Typ.arrow(
                    Nolabel,
                    Typ.constr(
                      Longident.unflatten(["unit"])
                      |> Option.get
                      |> Location.mknoloc,
                      [],
                    ),
                    Typ.constr(
                      Longident.unflatten(["classes"])
                      |> Option.get
                      |> Location.mknoloc,
                      [],
                    ),
                  ),
                  arity1,
                ],
              ),
            ),
            arity1,
          ],
        ),
      ),
      arity1,
    ],
  );

let getTypeExpressions = (fields: rawFields) => {
  let keys = fields |> parseFields;
  (
    createRecordTypeExpression(~keys, ~mapTo=["string"], "classes"),
    createRecordTypeExpression(
      ~keys,
      ~mapTo=["ReactDOM", "Style", "t"],
      "styles",
    ),
  );
};

let rewriteMakeStyles = (fields: rawFields, options: option(rawFields)) => {
  let (classTypeExpression, styleTypeExpression) =
    getTypeExpressions(fields);

  let options2 = options|>Option.map(createOptionsWithTypeConstraint);

  Mod.constraint_(
    Mod.mk(
      Pmod_structure(
        List.append(
          switch (options) {
          | None => []
          | Some(options) => createOptionsWithTypeConstraint(options)
          },
          [
            Str.module_(
              Mb.mk(
                Location.mknoloc(Some("Styles")),
                Mod.mk(
                  Pmod_structure([
                    Str.type_(Nonrecursive, [styleTypeExpression]),
                  ]),
                ),
              ),
            ),
            Str.type_(
              Recursive,
              [
                classTypeExpression,
                Type.mk(
                  ~kind=Ptype_abstract,
                  ~manifest=getMakeStylesTypeUncurried(),
                  Location.mknoloc("useStyles"),
                ),
              ],
            ),
            Str.primitive(
              Val.mk(
                ~attrs=[
                  Attr.mk(
                    Location.mknoloc("bs.module"),
                    PStr([
                      Str.eval(
                        Exp.constant(
                          Const.string("@material-ui/core/styles"),
                        ),
                      ),
                    ]),
                  ),
                ],
                ~prim=["makeStyles"],
                Location.mknoloc("makeStyles"),
                Typ.arrow(
                  Nolabel,
                  Typ.constr(
                    Longident.unflatten(["Styles", "styles"])
                    |> Option.get
                    |> Location.mknoloc,
                    [],
                  ),
                  switch (options) {
                  | None =>
                    Typ.constr(
                      Longident.unflatten(["useStyles"])
                      |> Option.get
                      |> Location.mknoloc,
                      [],
                    )
                  | Some(_) =>
                    Typ.arrow(
                      Nolabel,
                      Typ.constr(
                        Longident.unflatten(["options"])
                        |> Option.get
                        |> Location.mknoloc,
                        [],
                      ),
                      Typ.constr(
                        Longident.unflatten(["useStyles"])
                        |> Option.get
                        |> Location.mknoloc,
                        [],
                      ),
                    )
                  },
                ),
              ),
            ),
            Str.value(
              Nonrecursive,
              [
                Vb.mk(
                  Ast_helper.Pat.var(Location.mknoloc("useStyles")),
                  Exp.apply(
                    Exp.ident(
                      Longident.unflatten(["makeStyles"])
                      |> Option.get
                      |> Location.mknoloc,
                    ),
                    [
                      (
                        Nolabel,
                        Exp.constraint_(
                          Exp.record(fields, None),
                          Typ.constr(
                            Longident.unflatten(["Styles", "styles"])
                            |> Option.get
                            |> Location.mknoloc,
                            [],
                          ),
                        ),
                      ),
                      ...switch (options) {
                         | None => []
                         | Some(_) => [
                             (
                               Nolabel,
                               Longident.unflatten(["options"])
                               |> Option.get
                               |> Location.mknoloc
                               |> Exp.ident,
                             ),
                           ]
                         },
                    ],
                  ),
                ),
              ],
            ),
          ],
        ),
      ),
    ),
    Mty.mk(
      Pmty_signature([
        Sig.type_(Nonrecursive, [classTypeExpression]),
        Sig.value(
          Val.mk(Location.mknoloc("useStyles"), getMakeStylesTypeUncurried()),
        ),
      ]),
    ),
  );
};

let rewriteMakeStylesWithTheme =
    (
      fields: rawFields,
      funcExpr: Parsetree.expression,
      options: option(rawFields),
    ) => {
  let (classTypeExpression, styleTypeExpression) =
    getTypeExpressions(fields);
  
  Mod.constraint_(
    Mod.mk(
      Pmod_structure(
        List.append(
          switch (options) {
          | None => []
          | Some(options) => createOptionsWithTypeConstraint(options)
          },
          [
            Str.module_(
              Mb.mk(
                Location.mknoloc(Some("Styles")),
                Mod.mk(
                  Pmod_structure([
                    Str.type_(Nonrecursive, [styleTypeExpression]),
                  ]),
                ),
              ),
            ),
            Str.type_(
              Recursive,
              [
                classTypeExpression,
                Type.mk(
                  ~kind=Ptype_abstract,
                  ~manifest=getMakeStylesTypeUncurried(), // FIXME: this isn't right, makeStyles needs a second argument for options
                  Location.mknoloc("useStyles"),
                ),
              ],
            ),
            // binding for makeStyles
            Str.primitive(
              Val.mk(
                ~attrs=[
                  Attr.mk(
                    Location.mknoloc("bs.module"),
                    PStr([
                      Str.eval(
                        Exp.constant(
                          Const.string("@material-ui/core/styles"),
                        ),
                      ),
                    ]),
                  ),
                ],
                ~prim=["makeStyles"],
                Location.mknoloc("makeStyles"),
                Typ.arrow(
                  Nolabel,
                  Typ.constr(
                    Longident.unflatten(["themeFunc"])
                    |> Option.get
                    |> Location.mknoloc,
                    [],
                  ),
                  switch (options) {
                  | None =>
                    Typ.constr(
                      Longident.unflatten(["useStyles"])
                      |> Option.get
                      |> Location.mknoloc,
                      [],
                    )
                  | Some(_) =>
                    Typ.arrow(
                      Nolabel,
                      Typ.constr(
                        Longident.unflatten(["options"])
                        |> Option.get
                        |> Location.mknoloc,
                        [],
                      ),
                      Typ.constr(
                        Longident.unflatten(["useStyles"])
                        |> Option.get
                        |> Location.mknoloc,
                        [],
                      ),
                    )
                  },
                ),
              ),
            ),
            Str.value(
              Nonrecursive,
              [
                Vb.mk(
                  Ast_helper.Pat.var(Location.mknoloc("useStyles")),
                  Exp.apply(
                    Exp.ident(
                      Longident.unflatten(["makeStyles"])
                      |> Option.get
                      |> Location.mknoloc,
                    ),
                    [
                      (Nolabel, funcExpr),
                      ...switch (options) {
                         | None => []
                         | Some(_) => [
                             (
                               Nolabel,
                               Longident.unflatten(["options"])
                               |> Option.get
                               |> Location.mknoloc
                               |> Exp.ident,
                             ),
                           ]
                         },
                    ],
                  ),
                ),
              ],
            ),
          ],
        ),
      ),
    ),
    Mty.mk(
      Pmty_signature([
        Sig.type_(Nonrecursive, [classTypeExpression]),
        Sig.value(
          Val.mk(Location.mknoloc("useStyles"), getMakeStylesTypeUncurried()), // FIXME: type is incorrect
        ),
      ]),
    ),
  );
};
