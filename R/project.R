#' Create a project for a variable collection
#'
#' @param project_name The name of the project to create. The project name
#' \itemize{
#'   \item{cannot be empty.}
#'   \item{must be no more than 56 characters.}
#'   \item{cannot end with a \code{-}.}
#'   \item{must consist of lowercase letters and numbers.}
#'   }
#' @return TRUE if successful
#'
#' @importFrom httr PUT
#'
#' @examples
#' \dontrun{
#' armadillo.create_project("gecko")
#' }
#'
#' @export
armadillo.create_project <- function(project_name) { # nolint
  .check_project_name(project_name)

  handle = getOption("MolgenisArmadillo.armadillo.handle")
  
  response <- httr::PUT(
    handle = handle,
    path = "/admin/projects",
    body = list(name = project_name),
    encode = "json"
  )
  .handle_request_error(response)
  
  message(paste0("Created project '", project_name, "'"))
}

#' Delete project
#'
#' A project represents usually a study or collection of variables
#'
#' @param project_name the name of the study or collection of variables name
#' @return NULL
#'
#' @examples
#' \dontrun{
#' armadillo.delete_project(project_name = "gecko")
#' }
#'
#' @export
armadillo.delete_project <- function(project_name) { # nolint
  handle = getOption("MolgenisArmadillo.armadillo.handle")
  
  response <- httr::DELETE(
    handle = handle,
    path = paste0("/admin/projects/", project_name)
  )
  .handle_request_error(response)
  
  message(paste0("Deleted project '", project_name, "'"))
}

#' List the projects
#'
#' @return the projects
#'
#' @examples
#' \dontrun{
#' armadillo.list_projects()
#' }
#'
#' @export
armadillo.list_projects <- function() { # nolint
  handle = getOption("MolgenisArmadillo.armadillo.handle")
  
  response <- httr::GET(
    handle = handle,
    path = "/admin/projects"
  )
  .handle_request_error(response)
  
  content <- httr::content(response, as = "parsed")
  sapply(content, function(project) project$name)
}
