include "mexpr/ast.mc"
include "ocaml/mcore.mc"
include "ast.mc"

type LibCompileResult = {
  cleanup : () -> (),
  libPath : String
}

let compileOcamlLibrary : [String] -> [String] -> String -> LibCompileResult =
  lam libs. lam clibs. lam ocamlProg.

  let td = sysTempDirMake () in
  let dir = sysTempDirName td in
  let tempfile = lam f. sysJoinPath dir f in
  let t = tempfile ("plugin.ml") in
  writeFile t ocamlProg;
  -- Assume that needed dependencies are present in the cwd
  let includePath = sysGetCwd () in

  let command = ["ocamlfind", "ocamlopt -O3 -package boot -shared -I ", includePath,
                 " -o plugin.cmxs ", t] in
  let r = sysRunCommand command "" dir in
  (if neqi r.returncode 0 then
    print (join ["Something went wrong when compiling the plugin\n",
                 r.stdout, "\n", r.stderr, "\n"]);
    exit 1
  else ()); 
  {
  libPath = tempfile ("plugin.cmxs"),
  cleanup = lam. sysTempDirDelete td (); ()
  }

let processExternal = use OCamlAst in use ExternalsAst in
  lam ext : (String, Expr).
  let name = ext.0 in
  let tm = ext.1 in
  match tm with TmExtBind {e = e, deps = deps} in
    {
      name = name,
      top = OTopLet {
        ident = nameSym "main",
        tyBody = tyTm tm,
        body = OTmLam {
          label = None (),
          ident = nameNoSym "",
          body = OTmExprExt {expr = tmSeq2String e}
        }
      },
      libs = deps
    }

let processExternals : [(String, Expr)] -> ([(String, Top)], [String], [String]) =
  lam exts.
  let results = map processExternal exts in
  let nametops = map (lam r. (r.name, r.top)) results in
  let libs = setToSeq (setOfSeq cmpString (
    foldl (lam acc. lam r. concat r.libs acc) [] results)) in
  let clibs = [] in -- TODO
  (nametops, libs, clibs)

let jitCompile : [(String, Expr)] -> () =
  lam externals.
  match processExternals externals with (nametops, libs, clibs) in
  let p =
    use MCoreCompileLang in
    compileMCorePlugin nametops libs clibs (mkEmptyHooks (compileOcamlLibrary))
  in
  loadLibraries p.libPath;
  p.cleanup ();
  ()
