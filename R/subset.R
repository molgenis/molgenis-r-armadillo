#' Describes data available to subset and makes subset
#'
#' This automates the process of (i) checking what data is available to create
#' subsets, and (ii) makes the subsets.
#'
#' @param project directory from which to subset data
#' @param core_folder path of folder containing core data
#' @param outcome_folder path of folder containing outcome data
#' @param ref_csv csv file detailing the variables required
#' in the subset. It must contain three columns:
#'
#' 1. type (either "core" or "outcome")
#' 2. table (if type == "core", either "non_rep", "monthly_rep", "yearly_rep"
#'           of "trimester". if type == outcome, either "non_rep", "weekly_rep",
#'           "monthly_rep" or "yearly_rep"))
#' 3. variable (corresponding to LifeCycle variable name)
#' @param type Either 'describe' to describe data not available, 'subset' to subset
#' the data, (iii) 'both' to describe and subset the data
#' @param subset_name name of the subset to create
#' @return if type == 'describe' or 'both' returns a tibble detailing variables not
#' available for subsetting
#'
#' @importFrom dplyr %>% mutate filter arrange distinct pull bind_rows select any_of group_by group_split
#' @importFrom tidyr extract
#' @importFrom readr read_csv
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr pmap map imap
#' @importFrom stringr str_detect str_remove
#'
#' @export
armadillo.makeSubset <- function(
                                 source_project = NULL,
                                 new_project = NULL,
                                 subset_def = NULL,
                                 type = NULL,
                                 include_meta = NULL) {

## HERE WE NEED A CHECK TO SEE IF THE PERSON HAS LOGGED IN CORRECTLY 

if(is.null(source_project)){

stop("You must specify the name of your source project from which you will subset")
}

if (type %in% c("subset", "both") & is.null(new_project)) {
    stop("You must provide a name of the new project bucket")
  }

if(is.null(new_project)){

stop("You must specify the name of the new project to contain the subset")
}

if(is.null(subset_def)){

stop("You must provide an object created by newSubsetDefinition containing details
  of the variables and tables to include in the subset")
}

if(is.null(type)){

stop("Please specify a value for argument type: either 'describe', 'subset', or 'both'")

}

type <- arg_match(type, c("describe", "subset", "both"))

if(is.null(include_meta)){

stop("Please specify whether you want to include metadata in your subset")

}

if(source_project %in% armadillo.list_projects() == FALSE){

stop("The source project specified does not exist")

}

## DO WE WANT TO INCLUDE CHECKS TO MAKE SURE THE REFERENCE OBJECT IS CORRECT?

source_tables <- armadillo.list_tables("alspac") %>%
str_split("/", simplify = TRUE) %>%
as_tibble %>%
set_names("folder", "table") %>%
mutate(type = "source")

missing_tables <- left_join(input, source_tables, by = c("folder", "table")) %>%
dplyr::filter(is.na(type)) %>%
dplyr::select(folder, table)

if(nrow(missing_tables) > 0){

stop("The following folders & tables are included in your reference object, but don't exist
  within the specified project")

## HERE WE NEED TO RETURN THE TABLE, BUT I'VE NEVER WORKED OUT HOW TO DO THIS.

}

subset_obj <- ref_object %>% 
dplyr::filter(!map_lgl(subset_vars, is.null))

if(include_meta == TRUE){

subset_obj <- subset_obj %>%
mutate(vars_to_subset = pmap(., function(meta_vars, subset_vars, ...){

bind_rows(meta_vars, subset_vars) %>% distinct

})
)

} else_if(include_meta == FALSE){

subset_obj <- subset_obj %>%
mutate(vars_to_subset = subset_vars)

}

subset_obj <- subset_obj %>%
mutate(data = pmap(., function(folder, table, ...) {
      armadillo.load_table(project, folder, table)
    })
)

subset_obj <- subset_obj %>%
mutate(missing = pmap(., 
  function(vars_to_subset, data, ...){
      setdiff(
        x = vars_to_subset$variable,
        y = colnames(data)
      )
    })
)

missing_out <- subset_obj %>%
dplyr::select(folder, table, missing) %>%
unnest(cols = missing)

  if (type %in% c("subset", "both")) {

    if(source_project %in% armadillo.list_projects() == FALSE){

  armadillo.create_project(source_project)
}

subset_obj <- subset_obj %>%
mutate(data_to_upload = pmap(., 
  function(data, vars_to_subset, ...) {
        data %>% dplyr::select(any_of(vars_to_subset$variable))
      })
)

subset_obj %>% pwalk(function(folder, table, data_to_upload, ...){

  armadillo.upload_table(
    project = source_project, 
    folder = folder, 
    table = data_to_upload, 
    name = table
  )

})

  if (type %in% c("describe", "both")) {
    return(missing_out)
  }
}
