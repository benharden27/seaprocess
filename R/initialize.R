# Initialize a cruise
initialize_cruise <- function(path, ...) {

  initialize_master(path, ...)

}


# Initialize an example cruise
initialize_example <- function(path, ...) {

  initialize_master(path, cruiseID = "C285C",
                    initial_folder = "initial_example", ...)


}

# master intializing function
initialize_master <- function(path, cruiseID = NULL,
                              initial_folder =  "initial_dir",
                              ...) {

  if(is.null(cruiseID)) {
    cruiseID <- basename(path)
  }

  # ensure path exists but dont overwrite project
  if(!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  } else {
    stop("Project already exists")
  }

  # copy contents of template directory to project folder
  file.copy(list.files(system.file("extdata",initial_folder,package="seaprocess"), full.names = TRUE),
            path,
            recursive = TRUE)

  if(!dir.exists(file.path(path,"ctd"))) {
    dir.create(file.path(path,"ctd"))
  }

  if(!dir.exists(file.path("output"))) {
    dir.create(file.path(path,"output"))
    dir.create(file.path(path,"output","csv"))
    dir.create(file.path(path,"output","odv"))
    dir.create(file.path(path,"output","odv","neuston"))
    dir.create(file.path(path,"output","odv","bottle"))
    dir.create(file.path(path,"output","odv","adcp"))
    dir.create(file.path(path,"output","odv","ctd"))
    dir.create(file.path(path,"output","odv","elg"))
    dir.create(file.path(path,"output","odv","meter"))
  }


  ## TODO add cruise metadata contents
  lines <- readr::read_lines(file.path(path,"process_data.R"))

  # add cruiseID as master processing param
  ii <- stringr::str_which(lines, "^cruiseID \\<-")
  lines[ii] <- stringr::str_replace(lines[ii], '\\"\\"', paste0('\\"',cruiseID,'\\"'))

  # add cruise ID to all datasheets
  ii <- stringr::str_which(lines, "datasheets/")
  lines[ii] <- stringr::str_replace_all(lines[ii], "datasheets/", paste0("datasheets/",cruiseID,"_"))

  # write this to file
  readr::write_lines(lines, file.path(path,"process_data.R"))

  ## TODO append cruise ID to all files
  cruise <- basename(path)
  old_names <- c(list.files(file.path(path,"datasheets"), full.names = T, recursive = T),
                 list.files(path, full.names = T, recursive = F, pattern = "*.R"))
  new_names <- file.path(dirname(old_names),paste0(cruiseID,"_",basename(old_names)))
  file.rename(old_names, new_names)


}
