.onLoad <- function(libname, pkgname){
  packageStartupMessage("De routine zal de scripts op de lims server aanpassen als deze niet up to date zijn met het package. De locatie waar de scripts gezocht worden is de 4e regel in het dbcredentials.txt bestand dat altijd aanwezig moet zijn in de werkdirectory (de directory waar de scripts bestaan op de server, de projectdirectory bij het lokaal uittesten van het package)")
  #path <- "\\\\limsbgops.inbo.be\\Labware7\\LabWare-7\\Data\\R_SCRIPTS\\dbcredentials.txt"
  #creds <- try(utils::read.csv(path, header = FALSE)[4,1])
  #if (class(creds)[1] != "try-error") {
  #
  #}
  path_prd <- "\\\\limsbgops.inbo.be\\Labware7\\LabWare-7\\Data\\R_SCRIPTS"
  path_dev <- "D:\\LWL8DEV\\Data\\R_SCRIPTS"
  path_uat <- "D:\\LWL8UAT\\Data\\R_SCRIPTS"

  suppressWarnings(dev <- try(copy_scripts_to_lims_path(path_dev)))
  if (all(!dev)) suppressWarnings(uat <- try(copy_scripts_to_lims_path(path_uat)))
  if (all(!uat)) suppressWarnings(prd <-  try(copy_scripts_to_lims_path(path_prd)))
}

