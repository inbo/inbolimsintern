.onAttach <- function(libname, pkgname){
  packageStartupMessage("Om de scripts actief te maken, voer eenmalig inbolimsintern::copy_scripts_to_lims_path('x:/yyy/zzz/...) uit")
}
.onLoad <- function(libname, pkgname){
  packageStartupMessage("Om de scripts actief te maken, voer eenmalig inbolimsintern::copy_scripts_to_lims_path('x:/yyy/zzz/...) uit")
}
