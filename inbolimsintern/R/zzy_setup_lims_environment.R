
# Filename: R/zz_session_setup.R
# This file contains the main setup function that is called when the library is attached.

#' Run the LIMS script setup process.
#'
#' This function iterates through a predefined list of LIMS script paths.
#' It stops and updates the first accessible path it finds.
#'
#' @keywords internal
setup_lims_environment <- function() {
  # Define all possible LIMS paths in order of priority
  lims_paths_to_try <- c(
    LW7 = "D:/V7/LabWare-7/Data/R_SCRIPTS",
    DEV = "D:/LWL8DEV/Data/R_SCRIPTS",
    UAT = "D:/LWL8UAT/Data/R_SCRIPTS",
    PRD = "D:/LWL8PRD/Data/R_SCRIPTS"
  )

  packageStartupMessage("Checking if LIMS scripts need updating...")

  # Loop through each potential path
  for (env_name in names(lims_paths_to_try)) {
    path <- lims_paths_to_try[env_name]

    # Check if the directory exists and is accessible before trying anything else
    if (dir.exists(path)) {
      packageStartupMessage(paste("Found accessible LIMS path for environment:", env_name))

      # Attempt to update the scripts for this path.
      # The `copy_and_update_scripts` function will handle all logic and messaging.
      success <- copy_and_update_scripts(target_path = path)

      # If the update was successful, we are done and can exit the loop.
      if (success) {
        packageStartupMessage("LIMS script check complete.")
        return(invisible(NULL)) # Exit the function
      }
    }
  }

  # This message only appears if the loop finishes without finding any accessible paths
  packageStartupMessage("Could not find any accessible LIMS script directories to update.")
}
