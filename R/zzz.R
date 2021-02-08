.onLoad <- function(libname, pkgname) {
  assign("v8_global_context", V8::v8(), environment(.onLoad))
  tronweb_js_path <- system.file("js/tronweb-bundle.js", package = pkgname)
  invisible(v8_global_context$source(tronweb_js_path))

  v8_global_context$get(
    V8::JS('const HttpProvider = tronweb.providers.HttpProvider;
           const fullNode = new HttpProvider("https://api.trongrid.io");
           const solidityNode = new HttpProvider("https://api.trongrid.io");
           const tronWeb = new tronweb(fullNode,solidityNode);')
  )
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("R toolbox for the TRON blockchain\nDeveloped by Next Game Solutions (http://nextgamesolutions.com)")
}
