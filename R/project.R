#' Create a project for a variable collection
#'
#' @param project_name The name of the project to create. The project name
#' \itemize{
#'   \item{cannot be empty.}
#'   \item{must be no more than 56 characters.}
#'   \item{cannot end with a \code{-}.}
#'   \item{must consist of lowercase letters and numbers.}
#'   }
#' @param users A list collection of the users that should have access to the project
#' @param overwrite_existing Character, specifying action to take if project still exists: 'choose'
#' (default) displays a menu giving the option to overwrite or not, 'yes' overwrites the
#' existing project and 'no' exists the function with a message.
#' @return NULL
#'
#' @importFrom httr PUT
#' @importFrom dplyr case_when
#'
#' @examples
#' \dontrun{
#' armadillo.create_project("gecko")
#' }
#'
#' @export
armadillo.create_project <- function(project_name = NULL, users = NULL, overwrite_existing = "choose") { # nolint
  askYesNo <- NULL

  if (is.null(users)) {
    users <- list()
  }

  project_exists <- project_name %in% armadillo.list_projects()

  choice <- .get_overwrite_choice(project_name, project_exists, overwrite_existing)

  if (choice == TRUE & !project_exists) {
    .create_project(project_name, users)
    message(.create_project_message(project_name, users))
  } else if (choice == TRUE & project_exists) {
    .create_project(project_name, users)
    message(paste0(.create_project_message(project_name, users), " with overwrite set to 'yes'"))
  } else if (choice == "cancel" & project_exists) {
    message("Did not create project: operation cancelled by user")
  } else {
    message("Did not create project: ", "'", project_name, "'", " already exists and overwrite is set to 'no'")
  }
}

.create_project <- function(project_name, users, overwrite_existing) {
  .check_project_name(project_name)

  response <- httr::PUT(
    url = .get_url(),
    path = "/access/projects",
    body = list(name = project_name, users = users),
    config = c(
      httr::content_type_json(),
      httr::add_headers(.get_auth_header())
    ),
    encode = "json"
  )
  .handle_request_error(response)
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
  response <- httr::DELETE(
    url = .get_url(),
    path = paste0("/access/projects/", project_name),
    config = httr::add_headers(.get_auth_header())
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
  content <- .get_projects_content()
  sapply(content, function(project) project$name)
}

#' Gets the Projects information
#'
#' @return the projects and their information
#'
#' @examples
#' \dontrun{
#' armadillo.get_projects_info()
#' }
#'
#' @export
armadillo.get_projects_info <- function() { # nolint
  return(.get_projects_content())
}


#' Gets the users of an given project name
#'
#' @param project_name the name of the project to extract the users from
#' @return List of all users within "project_name"
#'
#' @import rlist
#'
#' @examples
#' \dontrun{
#' armadillo.get_project_users("some-project")
#' }
#'
#' @export
armadillo.get_project_users <- function(project_name) { # nolint
  # workaround for NOTE: no binding for global variable name
  name <- NULL
  content <- .get_projects_content()

  filtered <- rlist::list.filter(content, name == project_name)
  if (length(filtered) == 0) {
    stop(paste0("Project ", project_name, " not found."))
  }
  return(filtered[[1]]$users)
}

.get_projects_content <- function() {
  response <- httr::GET(
    url = .get_url(),
    path = "/access/projects",
    config = httr::add_headers(.get_auth_header())
  )
  .handle_request_error(response)
  content <- httr::content(response, as = "parsed")
  return(content)
}

#' Displays a menu with choices if the project already exists
#'
#' @param project_name the name of the target project
#' @return Either 1, 2, or 0 depending on menu choice
#'
#' @importFrom utils askYesNo
#' @noRd
.make_overwrite_menu <- function(project_name) {
  choice <- NULL
  while (is.null(choice)) {
    tryCatch(
      {
        choice <- askYesNo(
          paste0("Project ", "'", project_name, "'", " already exists: do you want to overwrite?"), 
          default = NA)
      },
      error = function(cond) {
        print("Unrecognised response: please choose from 'yes', 'no' or 'cancel'")
      }
    )
  }

  if (is.na(choice)) {
    choice <- "cancel"
  }
  return(choice)
}

.get_overwrite_choice <- function(project_name, project_exists, overwrite_existing) {
  if (!any(c("choose", "yes", "no") %in% overwrite_existing)) {
    stop("`overwrite` must be one of 'choose', 'yes', 'no'")
  }
  if (project_exists & overwrite_existing == "choose") {
    choice <- .make_overwrite_menu(project_name)
  } else if (!project_exists | (project_exists & overwrite_existing == "yes")) {
    choice <- TRUE
  } else {
    choice <- FALSE
  }
  return(choice)
}

#' Provides an exit message
#'
#' @param project_name the name of the target project
#' @param users A list collection of the users that should have access to the project
#' @return A message indicating what operation has been performed
#'
#' @noRd
.create_project_message <- function(project_name, users) {
  if (length(users) == 0) {
    usermessage <- "without users"
  } else {
    usermessage <- paste0("with users: ", paste(unlist(users), collapse = ", "))
  }
  paste0("Created project '", project_name, "' ", usermessage)
}
