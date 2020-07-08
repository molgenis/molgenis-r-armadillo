.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the MOLGENIS Armadillo client")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Tommy de Boer",
    devtools.desc.author = "Tommy de Boer <t.de.boer01@umcg.nl>",
    devtools.desc.license = "LGPL-3.0",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if (any(toset)) options(op.devtools[toset])
  
  invisible()
}