# Filename: R/copy_scripts_to_lims_path.R
# This is a helper function. It's not exported and is called by the main setup routine.

#' Copy package scripts to a single LIMS path if they are outdated.
#'
#' This function compares MD5 hashes of scripts in the package's 'inst/lims_scripts'
#' directory with those in the target LIMS path. If they differ, it backs up the
#' existing scripts and copies the new ones over.
#'
#' @param target_path The single LIMS directory path to check and update.
#' @return A single logical value: `TRUE` if scripts were successfully updated
#'   or were already up-to-date, `FALSE` on failure.
copy_and_update_scripts <- function(target_path) {
  # --- 1. Get Package and Target Script Paths ---

  # Find the directory containing the source scripts within the package installation
  package_script_dir <- system.file("lims_scripts", package = "inbolimsintern")
  if (!dir.exists(package_script_dir)) {
    warning("Internal error: 'inst/lims_scripts' directory not found in package.")
    return(FALSE)
  }

  package_script_files <- list.files(package_script_dir, pattern = "^[^_].*\\.R$", full.names = FALSE)
  if (length(package_script_files) == 0) {
    # This is not an error, just means there's nothing to sync.
    return(TRUE)
  } else {
    package_script_files <- file.path(package_script_dir, package_script_files)
  }

  # Get paths for existing scripts at the target location
  target_script_files <- list.files(target_path, pattern = "\\.R$", full.names = TRUE)

  # --- 2. Compare Hashes to See if an Update is Needed ---

  # Use `try` to safely get hashes, as files might not exist or be accessible
  md5_pkg <- try(tools::md5sum(package_script_files), silent = TRUE)
  md5_target <- try(tools::md5sum(target_script_files), silent = TRUE)

  # Rename the hash vectors with just the base filenames for correct comparison
  if (!inherits(md5_pkg, "try-error")) names(md5_pkg) <- basename(names(md5_pkg))
  if (!inherits(md5_target, "try-error")) names(md5_target) <- basename(names(md5_target))

  # Check if hashes are identical. If so, no update is needed for this path.
  if (!inherits(md5_pkg, "try-error") && !inherits(md5_target, "try-error") &&
      identical(sort(names(md5_pkg)), sort(names(md5_target))) &&
      identical(md5_pkg[sort(names(md5_pkg))], md5_target[sort(names(md5_target))])) {
    message(" -> Scripts are already up-to-date.")
    return(TRUE)
  }

  message(" -> Scripts are outdated or missing. Starting update process...")

  # --- 3. Backup Existing Scripts ---

  if (length(target_script_files) > 0) {
    # Create a unique backup directory name
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    backup_path <- file.path(target_path, paste0("_backup_", timestamp))

    message(paste("    -> Backing up", length(target_script_files), "existing scripts to:", backup_path))

    # Use `try` for robustness against permission errors
    backup_result <- try({
      dir.create(backup_path, showWarnings = FALSE, recursive = TRUE)
      file.copy(from = target_script_files, to = backup_path)
    }, silent = TRUE)

    if (inherits(backup_result, "try-error") || !all(backup_result)) {
      warning(" -> Backup failed. Aborting update for this path to prevent data loss.")
      return(FALSE)
    }

    # Remove original files only after successful backup
    file.remove(target_script_files)
  }

  # --- 4. Copy New Scripts from Package ---

  message(paste("    -> Copying", length(package_script_files), "new scripts from package..."))

  copy_success <- try(
    file.copy(from = package_script_files, to = target_path, overwrite = TRUE),
    silent = TRUE
  )

  if (inherits(copy_success, "try-error") || !all(copy_success)) {
    warning(" -> Failed to copy new scripts. The target directory might be incomplete.")
    return(FALSE)
  }

  message(" -> Update successful.")
  return(TRUE)
}
