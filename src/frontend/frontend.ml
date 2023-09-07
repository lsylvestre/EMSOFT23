open Ast
open Prelude.Errors

let read_phrase () =
  (* phrases terminate with ";;" *)
  let rec loop acc =
    let s = read_line () in
    let new_acc = acc^"\n"^s in (* todo (improvment): use a buffer to avoid multiple concatenation *)
    if String.contains s '.' then new_acc else
    match String.index_opt s ';' with
    | None -> loop new_acc
    | Some n ->
        if n < String.length s && s.[n] = ';' then new_acc else
        loop new_acc
  in loop ""

let frontend ~(inputs : string list) repl ?(when_repl=(fun _ -> ())) ?(relax=false) main str_arg : pi * e list =
  let gss_from_files,dss_from_files =
    List.map (fun path ->
                let ic = open_in path in
                begin try
                      Current_filename.current_file_name := path;
                      let gs,ds = Parser.pi Lexer.token (Lexing.from_channel ic) in
                      close_in ic;
                      gs,ds
                    with excp ->
                      close_in_noerr ic;
                      (match excp with
                      | Parser.Error ->
                          error (fun fmt ->
                            Format.fprintf fmt "Syntax error (file %s)" path)
                      | _ -> ());
                      raise excp
                end
               ) inputs |> List.split in
  let gs_from_files,ds_from_files = List.concat gss_from_files, List.concat dss_from_files in
  let ds = (if repl || ds_from_files = [] then
            let () = List.iter when_repl ds_from_files in
            (Current_filename.current_file_name := "%stdin";
             Printf.printf "=== mixc ===";
             flush stdout;
             let rec loop ds =
               Printf.printf "\n> ";
               let l = read_phrase () in
               if String.contains l '#' then ds else
               try
                 (match Parser.decl_opt Lexer.token (Lexing.from_string l) with
                 | Some d -> begin
                    caml_error_handler ~on_error:(fun _ ->
                      Format.print_flush ();
                      let () = List.iter when_repl (List.rev ds) in
                      (* todo: reset when_repl and retype all previous declaration, ignoring the last (eronous) one *)
                      loop ds
                    )
                    (fun () -> when_repl d; loop (d::ds)) ()
                  end
                 | None -> loop ds)
                with End_of_file -> ds
            in
             loop (List.rev ds_from_files))
             |> List.rev
            else ds_from_files) in

  let values_list =
    String.split_on_char ';' str_arg |>
    (function | [""] -> [] | l -> l) |>
    List.map (fun s -> Parser.exp_eof Lexer.token (Lexing.from_string s))
  in
  let entry_point = if relax then (E_var main) else (Ast.ty_annot ~ty:(Ast_mk.fresh_node ()) (E_var main)) in
  let ds = List.map (function ((P_var x,e),loc) -> (x,e) | _ -> assert false) ds in
  ({statics=gs_from_files;ds;main=entry_point}, values_list)


(*


  (** check that given inputs are combinatorial *)
  List.iteri (fun i a ->
    if Combinatorial.combinatorial a then ()
    else error (fun fmt ->
                  Format.fprintf fmt
                    "@[<v>Input %d should be combinatorial, but I found:@,%a@,@]" i
                       (emph_pp purple Ast_pprint.pp_exp) a)) values_list;

  (* return both parsed program and its inputs *)
  (e,values_list) *)
