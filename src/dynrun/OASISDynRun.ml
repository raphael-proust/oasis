
(* Dynamic runtime for building setup.ml.

   We use this to reduce setup.ml replaceable section to a few lines
   everything else is dynamically generated.
 *)

let () = OASISBuiltinPlugins.init ()

open OASISTypes
open BaseSetup

let oasis_fn = "_oasis"

let setup_t =
  let pkg = OASISParse.from_file ~ctxt:!BaseContext.default oasis_fn in
  let _, setup_t = BaseSetup.of_package ~setup_update:false pkg in
    setup_t

(* Re-export BaseSetup.setup so one can modify setup_t before passing it
   to setup(). *)
module BaseSetup = struct
  include BaseSetup

  let setup setup_t =
    let tmp_setup_fn =
      Filename.temp_file (setup_t.package.name^"-setup") ".ml" in
    let restored = ref false in
    let cleanup () =
      if not !restored then begin
        restored := true;
        BaseGenerate.restore ();
        if Sys.file_exists tmp_setup_fn then
          Sys.remove tmp_setup_fn
      end
    in
    try
      let _lst: 'a list =
        BaseGenerate.generate
          ~backup:true
          ~setup_fn:tmp_setup_fn
          ~restore:true
          BaseGenerate.NoUpdate
          (OASISParse.from_file
             ~ctxt:!BaseContext.default oasis_fn)
      in
      at_exit cleanup;
      let setup_t =
        (* Override distclean, because it remove setup.log and we need it for
         * BaseGenerate.restore
         *)
        {setup_t with distclean = setup_t.distclean @ [fun _ _ -> cleanup ()]}
      in
        BaseSetup.setup setup_t;
        cleanup ()
    with e ->
      cleanup ();
      raise e
end

let setup () = BaseSetup.setup setup_t
