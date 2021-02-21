.onLoad <- function(libname, pkgname) {
  assign("v8_global_context", V8::v8(), environment(.onLoad))
  tronaddr_js_path <- system.file("js/tron-address-bundle.js", package = pkgname)
  invisible(v8_global_context$source(tronaddr_js_path))
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("R toolbox to explore the TRON blockchain\nDeveloped by Next Game Solutions (http://nextgamesolutions.com)")
}
