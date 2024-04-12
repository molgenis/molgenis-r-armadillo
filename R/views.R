#' Describes data available to subset and makes subset
#'
#' This automates the process of:
#' \enumerate{
#'  \item Checking what data is available to create subsets
#'  \item Make the subset
#' }
#' @param source_project project from which to subset data
#' @param new_project project to upload subset to. Will be created if it doesn't
#' exist.
#' @param subset_def R object containing subset definition created by
#' \code{armadillo.subset_definition()}
#' @param dry_run you can dry-run the function to which variables are missing
#'
#' @return missing variables provided in the subset definition
#'
#' @examples
#' \dontrun{
#' armadillo.subset(
#'   source_project = "gecko",
#'   new_project = "study1",
#'   subset_def = local_subset
#' )
#' }
#'
#' @export
armadillo.make_views <- function(source_project = NULL, target_project, new_project = NULL, reference_csv = NULL,
                                 dry_run = "asdsa") {
  
  

  #change column names but allow old ones
  
  .check_args_valid(source_project, new_project, subset_def, dry_run)
  view_reference <- armadillo.subset_definition(subset_def)
  
  view_reference <- view_reference %>%
    mutate(
      target_folder = folder,
      target_table = table
    )
  
  armadillo.create_project(new_project)
  posts <- .loop_make_views(reference, source_project, target_project)
  statuses <- .get_status(posts)
  
  if(any(status == 404)){
    warning("One or more views were not created correctly, see details below.", 
            immediate. = T)
    .get_messages(posts, statuses, view_reference)
  }
  
  .get_messages <- function(posts, statuses, reference){
    messages <- posty %>%
      map_chr(function(x){
        content(x)[["message"]]
        })
  messages <- view_reference %>%
    mutate(
      status = unlist(status),
      message = unlist(messages)) %>%
    dplyr::select(folder, table, target_folder, target_table, status, message)
  
  return(messages)
  }
  
}


        
# .check_missing_vars <- function(source_project, requested_vars){
#   . <- folder <- NULL
#   
#   existing_tables <- .get_tables(source_project, requested_vars)
#   existing_vars <- existing_tables %>% map(colnames)
#   expected_vars <- requested_vars$vars_to_subset %>% map(~.x$variable)
#   
#   missing_vars <- list(expected_vars, existing_vars) %>%
#     pmap(function(x, y){
#       unique(x[!x %in% y])
#     })
#   
#   return(missing_vars)
# }
# 
# .format_missing_vars <- function(requested_vars, missing_vars_list){
#   missing_neat <- requested_vars %>%
#     dplyr::select(folder, table) %>%
#     mutate(missing = missing_vars_list) %>%
#     unnest(missing)
#   
#   return(missing_neat)
# }

#' Checks the input arguments are complete
#'
#' @param source_project project from which to subset data
#' @param new_project project to upload subset to. Will be created if it doesn't
#' exist.
#' @param subset_def R object containing subset definition created by
#' @noRd
.check_args_valid <- function(source_project, new_project, subset_def, dry_run){
  
  if (is.null(source_project)) {
    stop(
      paste0("You must provide the name of the source project from ", "which you will subset")
    )
  }
  
  if (is.null(new_project)) {
    stop("You must provide a name for the new project")
  }
  
  if (is.null(subset_def)) {
    stop(
      paste0("You must provide an object created by ",
             "armadillo.subset_definition containing details of the ", 
             "variables and tables to include in the subset"
      )
    )
    
    if(!is.null(dry_run)){
      message("Argument `dry_run` is now defunct")
    }
    
    if(!is.null(new_project)){
      message("Argument `new project` has now been depricated: please use `target_project` instead")
    }
    
  }
  
}

#' Builds an R object containing info required to make subsets
#'
#' This file must contain three columns with the headers 'folder', 'table' and
#' 'variables'. 'Folder' must refer to a folder in the armadillo project to be
#' subsetted. 'Table' must refer to a table within that folder. 'variables' must
#' refer to variables within that table.
#'
#' @param vars \code{.csv} file containing vars to subset.
#'
#' @importFrom tidyr nest
#'
#' @return a dataframe containing variables that is used for input in the
#' \code{armadillo.subset()} method
#'
#' @examples
#' \dontrun{
#' armadillo.subset_definition(
#'   vars = "C:/tmp/vars.csv"
#' )
#' }
#'
#' @export
armadillo.subset_definition <- function(vars = NULL) { # nolint
  variable <- folder <- . <- subset_vars <- vars_to_subset <- NULL # nolint
  reference <- .read_view_reference(vars)
  reference <- .rename_reference_columns(reference, "folder", "source_folder")
  reference <- .rename_reference_columns(reference, "project", "source_project")
  .check_reference_columns(reference)
  reference_clean <- .format_reference(reference)
  return(reference_clean)
}

#' Reads in .csv file containing variables 
#' 
#' @param vars .csv file containing vars to subset.
#' @importFrom readr read_csv
#'
#' @noRd
.read_view_reference <- function(vars) {
  variable <- subset_vars <- NULL
  
  if (is.null(vars)) {
    stop("You must provide a .csv file with variables and tables to subset")
  }
  
  reference <- suppressWarnings(
    read_csv(file = vars, show_col_types = FALSE, trim_ws = TRUE)
  )
  return(reference)
}


.rename_reference_columns <- function(reference, old_name, new_name){
  
  colname_message <- "Renaming .csv column name `%s` to `%s`: please update your .csv file to silence this message."
  
  if (any(colnames(reference) %in% old_name)) {
    message(sprintf(colname_message, old_name, new_name))
    colnames(reference) <- str_replace(colnames(reference), old_name, new_name)
  }
  
  return(reference)
}

#' Checks imported file for correct column names
#'
#' The imported file must contain three columns with the headers 'folder', 'table' & 'variables'. 
#' 'Folder' must refer to a folder in the armadillo project which contains the master file with the 
#' data from which to create a view. 'Table' must refer to a table within that folder. 
#' 'variables' must refer to variables within that that table.
#'
#' @param vars .csv file containing vars to subset.
#' @importFrom readr read_csv
#'
#' @noRd
.check_reference_columns <- function(reference) {
  
  if(!all(colnames(reference) %in% c("source_folder", "source_table", "variable"))){
    stop(".csv file must contain columns entitled 'source_folder', 'source_table' and 'variable'")
  }
  
  allowed_cols <- c("source_folder", "source_table", "variable", "target_folder", "target_table")
  
  if (any(colnames(reference) %in% allowed_cols == FALSE))  {
    incorrect_name <- colnames(reference)[!colnames(reference) %in% allowed_cols]
    stop(paste0(
      ".csv column name ", "'", incorrect_name, "'", " is not permitted: allowed names are ", "'",
      paste0(allowed_cols, collapse = ", "), "'"))
  }  
    
  if(any(reference %>% unlist %>% is.na)){
    stop("The input .csv file contains empty cells: please check and try again")
  }
  
}

#' Formats the reference file that has been imported
#'
#' @param vars .csv file containing vars to subset.
#' @importFrom dplyr %>% filter select
#' @importFrom tidyr nest
#' @importFrom purrr map_lgl
#'
#' @noRd
.format_reference <- function(reference){

  reference_out <- reference %>%
  nest(subset_vars = c(variable)) %>%
  dplyr::select(folder, table, vars_to_subset = subset_vars)
  
return(reference_out)
}

#' Creates a local subset of data based on reference object, and uploads this to
#' server
#'
#' @param source_project project from which to subset data
#' @param tables R object containing armadillo tables created by
#' \code{.get_tables()}
#'
#' @importFrom dplyr %>% select any_of
#' @importFrom purrr pmap pwalk
#'
#' @noRd
.make_views <- function(source_project, source_folder, source_table, target_project, target_folder, 
                        target_table, target_vars) {
  
  target_path <- paste0(target_folder, "/", target_name)
  
  body <- .make_json_body(source_project, source_folder, source_table, target_project, target_path, 
                          requested_vars)
  
  post_url <- .make_post_url(armadillo_url, target_project)
  
  response <- POST(
    url = post_url,
    body = json_body,
    encode = "json",
    config = c(httr::content_type_json(), httr::add_headers(.get_auth_header()))
  )
  
  return(response)
}

.loop_make_views <- function(reference, source_project, target_project){
  
  reference %>%
    pmap(function(folder, table, vars_to_subset){
      .make_views(
        source_project = source_project, 
        source_folder = folder,
        source_table = table,
        target_project = target_project, 
        target_folder = target_folder,
        target_table = target_table,
        target_vars = unlist(vars_to_subset)
        )
    })
  
}
  
handle_post_errors <- function(){
if(content(response)$status == 204){
  message("View ", "'", new_project, "/", new_path, "'", " successfully created")
} else {
  stop(content(response)$message)
}

.make_post_url <- function(armadillo_url, target_project){
  return(sprintf("%sstorage/projects/%s/objects/link", armadillo_url, target_project))
}

}

.make_json_body <- function(source_project, source_folder, source_table, target_project, target_path, 
                            target_vars){  
  
  json_body <- jsonlite::toJSON(
    list(sourceObjectName = paste0(source_folder, "/", source_table),
         sourceProject = source_project,
         linkedObject = target_path,
         variables = target_vars, 
         auto_unbox = TRUE))
  
  return(json_body)
}

.get_status <- function(post){
  status <- post %>% 
    map_int(function(x){
      content(x)[["status"]]
    })
  return(status)
}
