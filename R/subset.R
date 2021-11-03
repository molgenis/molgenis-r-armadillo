#' Describes data available to subset and makes subset
#'
#' This automates the process of (i) checking what data is available to create
#' subsets, and (ii) makes the subsets.
#'
#' @param source_project project from which to subset data
#' @param new_project project to upload subset to. Will be created if it doesn't exist.
#' @param subset_def R object containing subset definition created by newSubsetDefinition()
#' @param type Either 'describe' to describe data not available, 'subset' to subset
#' the data, (iii) 'both' to describe and subset the data
#' @param include_meta wheter or not to automatically include key variables (defined in subset_def)
#' @return if type == 'describe' or 'both' returns a tibble detailing variables not
#' available for subsetting
#'
#' @importFrom rlang arg_match
#' @importFrom dplyr %>% mutate filter select bind_rows distinct any_of 
#' @importFrom tidyr nest unnest
#' @importFrom stringr str_split
#' @importFrom tibble as_tibble
#' @importFrom purrr set_names map_lgl pmap pwalk
#' @importFrom utils read.csv
#'
#' @export
armadillo.subset <- function(source_project = NULL,
                                 new_project = NULL,
                                 subset_def = NULL,
                                 type = NULL,
                                 include_meta = NULL) {

folder <- subset_vars <- . <- NULL

  ## SIDO: HERE WE NEED A CHECK TO SEE IF THE PERSON HAS LOGGED IN CORRECTLY

  if (is.null(source_project)) {
    stop("You must specify the name of your source project from which you will subset")
  }

  if (type %in% c("subset", "both") & is.null(new_project)) {
    stop("You must provide a name of the new project bucket")
  }

  if (is.null(new_project)) {
    stop("You must specify the name of the new project to contain the subset")
  }

  if (is.null(subset_def)) {
    stop("You must provide an object created by newSubsetDefinition containing details
  of the variables and tables to include in the subset")
  }

  if (is.null(type)) {
    stop("Please specify a value for argument type: either 'describe', 'subset', or 'both'")
  }

  type <- arg_match(type, c("describe", "subset", "both"))

  if (is.null(include_meta)) {
    stop("Please specify whether you want to include metadata in your subset")
  }

  if (source_project %in% armadillo.list_projects() == FALSE) {
    stop("The source project specified does not exist")
  }

  ## SIDO: DO WE WANT TO INCLUDE CHECKS TO MAKE SURE THE REFERENCE OBJECT IS CORRECT?

  source_tables <- armadillo.list_tables("alspac") %>%
    str_split("/", simplify = TRUE) %>%
    as_tibble() %>%
    set_names("folder", "table") %>%
    mutate(type = "source")

  missing_tables <- left_join(subset_def, source_tables, by = c("folder", "table")) %>%
    dplyr::filter(is.na(type)) %>%
    dplyr::select(folder, table)

  if (nrow(missing_tables) > 0) {
    stop("The following folders & tables are included in your reference object, but don't exist
  within the specified project")

    ## HERE WE NEED TO RETURN THE TABLE IN THE ERROR MESSAGE, BUT I'VE NEVER WORKED OUT HOW TO DO THIS.
  }

  subset_obj <- subset_def %>%
    dplyr::filter(!map_lgl(subset_vars, is.null))

  if (include_meta == TRUE) {
    subset_obj <- subset_obj %>%
      mutate(vars_to_subset = pmap(., function(meta_vars, subset_vars, ...) {
        bind_rows(meta_vars, subset_vars) %>% distinct()
      }))
  } else if (include_meta == FALSE) {
    subset_obj <- subset_obj %>%
      mutate(vars_to_subset = subset_vars)
  }

  subset_obj <- subset_obj %>%
    mutate(data = pmap(., function(folder, table, ...) {
      armadillo.load_table(source_project, folder, table)
    }))

  subset_obj <- subset_obj %>%
    mutate(missing = pmap(
      .,
      function(vars_to_subset, data, ...) {
        setdiff(
          x = vars_to_subset$variable,
          y = colnames(data)
        )
      }
    ))

  missing_out <- subset_obj %>%
    dplyr::select(folder, table, missing) %>%
    unnest(cols = missing)

  if (type %in% c("subset", "both")) {
    if (source_project %in% armadillo.list_projects() == FALSE) {
      armadillo.create_project(source_project)
    }

    subset_obj <- subset_obj %>%
      mutate(data_to_upload = pmap(
        .,
        function(data, vars_to_subset, ...) {
          data %>% dplyr::select(any_of(vars_to_subset$variable))
        }
      ))

    subset_obj %>% pwalk(function(folder, table, data_to_upload, ...) {
      armadillo.upload_table(
        project = source_project,
        folder = folder,
        table = data_to_upload,
        name = table
      )
      return(missing_out)
    })
  }
}

#' Builds an R object containing info required to make subsets
#'
#' @param vars .csv file containing vars to subset. This file must contain
#' three columns with the headers 'folder', 'table' & 'variables'. 'Folder' must
#' refer to a folder in the armadillo project to be subsetted. 'Table' must refer
#' to a table within that folder. 'variables' must refer to variables within that
#' that table.
#' @param metadata .csv file containing meta variables to subset (optional)
#'
#' @importFrom dplyr %>% filter left_join
#' @importFrom tidyr nest
#' @importFrom utils read.csv
#'
#' @export
armadillo.subset_definition <- function(vars = NULL, metadata = NULL) {
  
  variable <- folder <- NULL
  
  if (is.null(vars)) {
    stop("You must provide a .csv file with variables and tables to subset")
  }
  
  subset_vars <- read.csv(file = vars)
  
  if (any(colnames(subset_vars) %in% c("folder", "table", "variable") == FALSE)) {
    stop(".csv file must contain exactly three columns entitled 'folder', 'table' and 'variable'")
  }
  
  if (length(colnames(subset_vars)) != 3) {
    stop(".csv file must contain exactly three columns entitled 'folder', 'table' and 'variable'")
  }
  
  subset_vars <- subset_vars %>%
    dplyr::filter(!is.na(variable)) %>%
    nest(subset_vars = c(variable))
  
  if (!is.null(metadata)) {
    meta_vars <- read.csv(file = metadata)
    
    if (any(colnames(meta_vars) %in% c("folder", "table", "variable") == FALSE)) {
      stop(".csv file must contain three columns titled 'folder', 'table' and 'variable'")
    }
    
    if (length(colnames(meta_vars)) != 3) {
      stop(".csv file must contain exactly three columns titled 'folder', 'table' and 'variable'")
    }
    
    folders_dont_exist <- subset_vars$folder[subset_vars$folder %in% meta_vars$folder == FALSE]
    
    if (length(folders_dont_exist) > 0) {
      stop(paste0(
        "The following folders are specified in your variables to subset, but don't exist in your
  metadata:  ",
        paste0(folders_dont_exist, collapse = ", ")
      ))
    }
    
    tabs_dont_exist <- subset_vars$table[subset_vars$table %in% meta_vars$table == FALSE]
    
    if (length(tabs_dont_exist) > 0) {
      stop(paste0(
        "The following tables are specified in your variables to subset, but don't exist in your
  metadata:  ",
        paste0(tabs_dont_exist, collapse = ", ")
      ))
    }
    
    meta_vars <- meta_vars %>%
      dplyr::filter(!is.na(variable)) %>%
      nest(meta_vars = c(variable))
    
    out <- left_join(meta_vars, subset_vars, by = c("folder", "table"))
  } else {
    out <- subset_vars %>%
      mutate(meta_vars = NA) %>%
      dplyr::select(folder, table, meta_vars, subset_vars)
  }
  
  return(out)
}

