let insn_selection vertices tag =
  let Block.Block(astlist, term) = tag.Block.block in
  let flattened = Flatten.flatten astlist in
  let tfermerged = Mergetransfer.mergetransfer vertices flattened term in
  let slist, _ =
    List.fold_right (fun ast (acc,num) ->
                      try
                        Select.accum_insns_for_ast ast acc num
                      with
                        Select.SelectFailed x ->
                          Printf.fprintf stderr
                            "Instruction selection failed for:\n%s\n"
                            (Disast.writeop x);
                          flush stderr;
                          failwith "Stop."
                       | Ast.AstNotRegisterLike x ->
                           Printf.fprintf stderr
                             "AST '%s' not register-like selecting for:\n%s\n"
                             (Disast.writeop x)
                             (Disast.writeop ast);
                           flush stderr;
                           failwith "Stop.")
                    tfermerged
                    ([], 0)
  in
    List.rev slist
