open Ast

let () =
  let p =
    {
      patt =
        Ptuple
          [
            {
              patt =
                Ptuple
                  [
                    { patt = Pid { name = "x_0" }; pmeta = () };
                    { patt = Pid { name = "x" }; pmeta = () };
                  ];
              pmeta = ();
            };
            { patt = Pid { name = "obs" }; pmeta = () };
          ];
      pmeta = ();
    }
  in
  let g =
    {
      expr =
        Eapp
          ( { expr = Evar { name = "gaussian" }; emeta = () },
            {
              expr =
                Etuple
                  [
                    { expr = Evar { name = "x" }; emeta = () };
                    { expr = Econst (Cfloat "1."); emeta = () };
                  ];
              emeta = ();
            } );
      emeta = ();
    }
  in
  let e =
    {
      expr =
        Efun
          ( p,
            {
              expr =
                Elet
                  ( { patt = Pid { name = "x" }; pmeta = () },
                    { expr = Esample g; emeta = () },
                    {
                      expr =
                        Elet
                          ( { patt = Pid { name = "y" }; pmeta = () },
                            g,
                            {
                              expr =
                                Elet
                                  ( { patt = Ptuple []; pmeta = () },
                                    {
                                      expr =
                                        Eobserve
                                          ( {
                                              expr = Evar { name = "y" };
                                              emeta = ();
                                            },
                                            {
                                              expr = Evar { name = "obs" };
                                              emeta = ();
                                            } );
                                      emeta = ();
                                    },
                                    {
                                      expr =
                                        Etuple
                                          [
                                            {
                                              expr = Evar { name = "x_0" };
                                              emeta = ();
                                            };
                                            {
                                              expr =
                                                Etuple
                                                  [
                                                    {
                                                      expr =
                                                        Evar { name = "x_0" };
                                                      emeta = ();
                                                    };
                                                    {
                                                      expr = Evar { name = "x" };
                                                      emeta = ();
                                                    };
                                                  ];
                                              emeta = ();
                                            };
                                          ];
                                      emeta = ();
                                    } );
                              emeta = ();
                            } );
                      emeta = ();
                    } );
              emeta = ();
            } );
      emeta = ();
    }
  in
  let alloc =
    {
      expr =
        Elet
          ( { patt = Pid { name = "x_0" }; pmeta = () },
            {
              expr =
                Esample
                  {
                    expr =
                      Eapp
                        ( { expr = Evar { name = "gaussian" }; emeta = () },
                          {
                            expr =
                              Etuple
                                [
                                  { expr = Econst (Cfloat "0."); emeta = () };
                                  { expr = Econst (Cfloat "1.0"); emeta = () };
                                ];
                            emeta = ();
                          } );
                    emeta = ();
                  };
              emeta = ();
            },
            {
              expr =
                Etuple
                  [
                    { expr = Evar { name = "x_0" }; emeta = () };
                    { expr = Evar { name = "x_0" }; emeta = () };
                  ];
              emeta = ();
            } );
      emeta = ();
    }
  in
  let p =
    [
      { decl = Ddecl ({ patt = Pid { name = "f_init" }; pmeta = () }, alloc) };
      { decl = Ddecl ({ patt = Pid { name = "f_step" }; pmeta = () }, e) };
    ]
  in
  let c = Compiler.compile_program p in
  Format.printf "%a@." Pprintast.structure c
