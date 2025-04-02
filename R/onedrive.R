#' Access OneDrive - Karolinska Institutet Directory
#'
#' @description
#' A helper function to access files and directories in the Karolinska Institutet
#' OneDrive folder. If no path is provided, it lists the contents of the root
#' OneDrive directory. If a path is provided, it checks if the path exists and
#' lists its contents if it's a directory.
#'
#' @param path Character string. Optional. The relative path within the OneDrive
#'   directory. If NULL (default), returns the root OneDrive path.
#'
#' @return Returns the full path as a character string. Additionally:
#'   * If path is NULL: prints and returns the root OneDrive path
#'   * If path is a directory: prints its contents and returns the full path
#'   * If path doesn't exist: throws an error
#'
#' @examples
#' \dontrun{
#' # Get root OneDrive path and list contents
#' onedrive()
#'
#' # Access a specific directory
#' onedrive("Documents")
#'
#' # Access a nested directory
#' onedrive("Documents/Projects")
#' }
#'
#' @export
onedrive <- function(path = NULL) {
  onedrive_path <- "~/OneDrive - Karolinska Institutet"
  if (is.null(path)) {
    print(list.files(onedrive_path))
    return(onedrive_path)
  }
  full_path <- paste0(onedrive_path, "/", path)
  is_dir <- dir.exists(full_path)
  if (is_dir) {
    message(paste(list.files(full_path), collapse = "\n"))
  }
  if (!is_dir) {
    is_file <- file.exists(full_path)
    if (!is_file) {
      stop("Onedrive path does not exist")
    }
  }
  return(full_path)
}
