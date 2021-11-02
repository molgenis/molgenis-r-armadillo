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
#'
#' @export
armadillo.newSubsetDefinition <- function(vars = NULL, metadata = NULL) {
  
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
