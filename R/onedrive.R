onedrive <- function(path = NULL) {
  onedrive_path <- "~/OneDrive - Karolinska Institutet"
  if (is.null(path)) {
    print(list.files(onedrive_pat))
    return(onedrive_path)
  }
  full_path <- paste0(onedrive_path, "/", path)
  is_dir <- dir.exists(full_path)
  if (is_dir) {
    message(paste(list.files(full_path), collapse = "\n"))
  } else {
    stop(paste0(full_path, " does not exist"))
  }

  return(full_path)
}
