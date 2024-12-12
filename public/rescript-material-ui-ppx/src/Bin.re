let () = {
  let import_path = ref("@mui/styles");
  let spec_list = [
    (
      "-importPath",
      Arg.Set_string(import_path),
      "Set the import path for the makeStyles function",
    ),
  ];

  let input_file = ref("");
  let output_file = ref("");
  Arg.parse(
    spec_list,
    arg => {
      switch (input_file^, output_file^) {
      | ("", _) => input_file := arg
      | (_, "") => output_file := arg
      | _ => failwith("Invalid arguments")
      }
    },
    "usage",
  );

  let argv = [|
    Sys.argv[0],
    input_file^,
    "-o",
    output_file^,
    "--dump-ast",
  |];

  Migrate_parsetree.Driver.register(
    ~name="withStyles",
    ~args=spec_list,
    Migrate_parsetree.Versions.ocaml_410,
    Mapper.withStylesMapper(import_path^),
  );

  Migrate_parsetree.Driver.run_main(~argv, ());
};
