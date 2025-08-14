# Filename: zzz.R
# This file contains the .onAttach hook, which is run when a user loads the package.

.onAttach <- function(libname, pkgname) {

  # Call the main setup function when the package is attached via library()
  setup_lims_environment()
}

