# Initialize a cruise
initialize_cruise <- function(path, ...) {

  # ensure path exists but dont overwrite project
  if(!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  } else {
    stop("Project already exists")
  }

  # copy contents of template directory to project folder
  file.copy(list.files(system.file("extdata","initial_dir",package="seaprocess"), full.names = TRUE),
            path,
            recursive = TRUE)

  ## TODO add cruise metadata contents

  ## TODO append cruise ID to all files
  cruise <- basename(path)
  old_names <- list.files(path, full.names = T, recursive = T)
  new_names <- file.path(dirname(old_names),paste0(cruise,"_",basename(old_names)))
  file.rename(old_names, new_names)


}


# Initialize a cruise
initialize_example <- function(path, ...) {

  # ensure path exists but dont overwrite project
  if(!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  } else {
    stop("Project already exists")
  }

  # copy contents of template directory to project folder
  file.copy(list.files(system.file("extdata","initial_example",package="seaprocess"), full.names = TRUE),
            path,
            recursive = TRUE)

  ## TODO add cruise metadata contents

  ## TODO append cruise ID to all files
  cruise <- basename(path)
  old_names <- list.files(path, full.names = T, recursive = T)
  new_names <- file.path(dirname(old_names),paste0(cruise,"_",basename(old_names)))
  file.rename(old_names, new_names)


}
