
let pf = Printf.printf
let idstr (x:string) = x

let fn = Sys.argv.(1)

let o = OASISAst.to_package
	{OASISRecDescParser.
		oasisfn = (Some fn);
		ctxt = !OASISContext.default;
	}
	(Stream.of_channel (open_in fn))

let section s = let open OASISTypes in
	match s with
	| Library (cs, bs, lib) -> begin
		let compiled_object = bs.bs_compiled_object in
		let internal_depends, findlib_depends =
			List.fold_left
				(fun (id, fd) d -> match d with
					| FindlibPackage (f, _) -> (id, f :: fd)
					| InternalLibrary n -> (n :: id, fd)
				)
				([], [])
				bs.bs_build_depends
		in
		let internal_tool, external_tool =
			List.fold_left
				(fun (it, et) t -> match t with
					| ExternalTool n -> (it, n :: et)
					| InternalExecutable n -> (n :: it, et)
				)
				([], [])
				bs.bs_build_tools
		in
		let byteopt = OASISExpr.choose idstr bs.bs_byteopt in
		let nativeopt = OASISExpr.choose idstr bs.bs_nativeopt in

		let builder = match compiled_object with
		| Byte -> "ocamlc"
		| Native | Best -> "ocamlopt"
		in
		let args =
			(match compiled_object with
				| Byte -> String.concat " " byteopt
				| Native | Best -> String.concat " " nativeopt
			) ::
			[] (*TODO: includes and such*)
		in
		let modules = lib.lib_modules @ lib.lib_internal_modules in
		let rule = Printf.sprintf "%s %s" builder (String.concat " " args) in

		(*TODO: lib_pack*)
		(bs.bs_path,
		 [(cs.cs_name, internal_depends @ modules, [])]
		)
	end

	| Object (cs, bs, o) -> assert false
	| Executable (cs, bs, e) -> begin
		let compiled_object = bs.bs_compiled_object in
		let internal_depends, findlib_depends =
			List.fold_left
				(fun (id, fd) d -> match d with
					| FindlibPackage (f, _) -> (id, f :: fd)
					| InternalLibrary n -> (n :: id, fd)
				)
				([], [])
				bs.bs_build_depends
		in
		let internal_tool, external_tool =
			List.fold_left
				(fun (it, et) t -> match t with
					| ExternalTool n -> (it, n :: et)
					| InternalExecutable n -> (n :: it, et)
				)
				([], [])
				bs.bs_build_tools
		in
		let byteopt = OASISExpr.choose idstr bs.bs_byteopt in
		let nativeopt = OASISExpr.choose idstr bs.bs_nativeopt in

		let builder = match compiled_object with
		| Byte -> "ocamlc"
		| Native | Best -> "ocamlopt"
		in
		let args =
			(match compiled_object with
				| Byte -> String.concat " " byteopt
				| Native | Best -> String.concat " " nativeopt
			) ::
			[] (*TODO: includes and such*)
		in
		let arg = Printf.sprintf "%s" e.exec_main_is in
		let rule = Printf.sprintf "%s %s %s"
			builder (String.concat " " args) arg in

		(bs.bs_path,
		 [(cs.cs_name, internal_depends, [rule])]
		)
	end
	| Flag (cs, f) -> assert false
	| SrcRepo (cs, sr) -> assert false
	| Test (cs, t) -> assert false
	| Doc (cs, d) -> assert false

let package2makefile
	{ OASISTypes.
		name;             (*package_name*)
		version;          (*OASISVersion.t*)

		conf_type;        (*[`Configure] plugin*)
		conf_custom;      (*custom*)

		build_type;       (*[`Build] plugin*)
		build_custom;     (*custom*)

		install_type;     (*[`Install] plugin*)
		install_custom;   (*custom*)
		uninstall_custom; (*custom*)

		clean_custom;     (*custom*)
		distclean_custom; (*custom*)

		files_ab;         (*unix_filename list*)
		sections;         (*section list*)
		plugins;          (*[`Extra] plugin list*)
		schema_data;      (*PropList.Data.t*)
		plugin_data;      (*plugin_data*)
	} =
	let variables = [
		("NAME", name);
		("VERSION", OASISVersion.string_of_version version);
	]
	in
	let sections = List.map section sections in
	List.iter
		(fun (path, rules) ->
			pf "%s/Makefile:\n\n" path;
			List.iter (fun (s, v) -> pf "%s=%s\n" s v)  variables ;
			pf "\n";
			List.iter
				(fun (prereq, depends, cmds) ->
					pf "%s: %s\n%s\n" prereq
						(String.concat " " depends)
						(match cmds with
							| [] -> ""
							| cmds -> "\t" ^ String.concat "\n\t" cmds)
				)
				rules;
			pf "\n"
		)
		sections


let () = package2makefile o
