# Filename: zzz.R
# This file contains the .onAttach hook, which is run when a user loads the package.

# .onAttach <- function(libname, pkgname) {
# 
#   # Call the main setup function when the package is attached via library()
#   setup_lims_environment()
# }
# 


# Filename: zzz.R

.onLoad <- function(libname, pkgname) {
  # We run this silently. If it fails (e.g., read-only filesystem), 
  # the package still loads so the LIMS doesn't crash.
  try(silent_hash_sync(pkgname), silent = TRUE)
}

silent_hash_sync <- function(pkgname) {
  # 1. Define Paths
  source_path <- system.file("lims_scripts", package = pkgname)
  if (source_path == "") return(NULL) # Folder doesn't exist in inst/
  
  target_paths <- c(
    "D:/LWL8PRD/Data/R_SCRIPTS",
    "D:/LWL8DEV/Data/R_SCRIPTS",
    "D:/LWL8UAT/Data/R_SCRIPTS"
  )
  
  # 2. Generate Hash of the Source Folder
  # We look at file names, sizes, and mtime to create a unique fingerprint
  files <- list.files(source_path, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return(NULL)
  
  # Digesting the file info is faster than digesting the actual file content 
  # of every file, but for small scripts, digesting content is safer.
  current_hash <- digest::digest(lapply(files, digest::digest, file = TRUE))
  
  for (path in target_paths) {
    if (dir.exists(path)) {
      hash_file <- file.path(path, ".sync_hash")
      
      # Read existing hash
      last_hash <- if(file.exists(hash_file)) readLines(hash_file, n = 1, warn = FALSE) else ""
      
      # 3. Only Sync if the scripts themselves have changed
      if (current_hash != last_hash) {
        # Perform the silent copy
        success <- file.copy(
          from = list.files(source_path, full.names = TRUE),
          to = path,
          overwrite = TRUE,
          recursive = TRUE,
          copy.date = TRUE
        )
        
        if (success) {
          # Write the new hash to prevent re-syncing next time
          writeLines(current_hash, hash_file)
        }
      }
      break # Exit after the first valid/active LIMS path is handled
    }
  }
}