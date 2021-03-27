#' Describes data available to subset and makes subset
#'
#' This automates the process of (i) checking what data is available to create
#' subsets, and (ii) makes the subsets.
#'
#' @param master_bucket directory from which to subset data
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
                                 master_bucket = "alspac",
                                 core_folder = "2_1_core_1_0",
                                 outcome_folder = "1_1_outcome_1_0",
                                 ref_csv,
                                 type = c("describe", "subset", "both"),
                                 subset_name = NULL) {
  if (type %in% c("subset", "both") & is.null(subset_name)) {
    stop("You must provide a name of the new bucket")
  }
  ################################################################################
  # 1. Read in reference csv data
  ################################################################################
  study_vars <- read_csv(file = ref_csv) %>%
    filter(!is.na(variable)) %>%
    mutate(long_name = paste0(type, "_", table))

  ################################################################################
  # 2. Check .csv data is in correct format
  ################################################################################
  if (any(colnames(study_vars) %in% c("type", "table", "variable", "long_name") == FALSE)) {
    stop(".csv file must contain three columns titled 'type', 'table' and 'variable'")
  }

  if (length(colnames(study_vars)) != 4) {
    stop(".csv file must contain three columns titled 'type', 'table' and 'variable'")
  }

  if (any(study_vars$type %in% c("outcome", "core") == FALSE)) {
    stop("Value of column 'type' in .csv file must be either 'core' or 'outcome")
  }

  core_tmp <- study_vars %>% filter(type == "core")

  if (any(
    core_tmp$table %in% c("non_rep", "monthly_rep", "yearly_rep", "trimester")
    == FALSE
  )) {
    stop("Value of column 'table' in .csv file can only be 'non_rep', 'monthly_rep', 
	 'yearly_rep' or 'trimester' when type == 'core'")
  }

  outcome_tmp <- study_vars %>% filter(type == "outcome")

  if (
    any(
      outcome_tmp$table %in% c("non_rep", "monthly_rep", "yearly_rep", "weekly_rep")
      == FALSE
    )) {
    stop("Value of column 'table' in .csv file can only be 'non_rep', 'weekly_rep', 
	 'monthly_rep', or 'yearly_rep' when type == 'core'")
  }

  ################################################################################
  # 3. Create reference table
  ################################################################################
  ref_tab <- tibble(
    type = c(rep("core", 4), rep("outcome", 4)),
    table = c(
      "non_rep", "monthly_rep", "yearly_rep", "trimester", "non_rep",
      "monthly_rep", "yearly_rep", "weekly_rep"
    ),
    name = c(rep(core_folder, 4), rep(outcome_folder, 4)),
    long_name = paste0(type, "_", table)
  ) %>%
    arrange(long_name)


  ################################################################################
  # 4. Create vector of required tables for view
  ################################################################################

  ## First we get the different tables required for this project
  required <- study_vars %>%
    distinct(long_name) %>%
    pull() %>%
    sort()

  ## Now we subset our reference tibble of tables by those required
  ref_sub <- ref_tab %>% filter(long_name %in% required)

  ################################################################################
  # 5. Create list containing downloaded required tables
  ################################################################################

  ##  Here we fetch the cohort tables that are required to subset
  cohort_sub <- ref_sub %>%
    pmap(function(name, table, ...) {
      armadillo.load_table(master_bucket, name, table)
    })

  names(cohort_sub) <- required

  ################################################################################
  # 6. See what data we have available
  ################################################################################

  ## Here we create a parallel list of the required variables which matches the
  ## corresponds to the list we just created of downloaded tables
  required_vars <- study_vars %>%
    group_by(long_name) %>%
    group_split() %>%
    map(~ select(., variable))

  names(required_vars) <- required

  ## Now we check which of the required variables don't exist in the data we
  ##  just downloaded
  dif_list <- list(required_vars, cohort_sub) %>%
    pmap(function(x, y) {
      setdiff(
        x = x$variable,
        y = colnames(y)
      )
    })

  dif_out <- dif_list %>%
    map(., as_tibble) %>%
    bind_rows(.id = "long_name") %>%
    extract(col = "long_name", regex = "([^_]*)_(.*)", into = c("type", "table"))

  if (type %in% c("subset", "both")) {

    ################################################################################
    # 7. Create subsets
    ################################################################################
    armadillo.create_project(subset_name)

    sub_list <- list(required_vars, cohort_sub) %>%
      pmap(function(x, y) {
        y %>% select(any_of(x$variable))
      })


    ################################################################################
    # 8. Create core and outcome subsets
    ################################################################################

    ## In order to upload the tables we need to split them into core and outcome.
    sub_core <- sub_list[str_detect(names(sub_list), "core")]
    sub_outcome <- sub_list[str_detect(names(sub_list), "outcome")]


    ################################################################################
    # 9. Correct names
    ################################################################################

    ## To keep a simple naming convention we remove the prefixes 'core' and 'outcome'
    names(sub_core) <- str_remove(names(sub_core), "core_")
    names(sub_outcome) <- str_remove(names(sub_outcome), "outcome_")


    ################################################################################
    # 10. Upload subsets
    ################################################################################

    ## ---- Core -------------------------------------------------------------------
    sub_core %>%
      imap(~ armadillo.upload_table(subset_name, core_folder, .x, .y))

    ## ---- Outcome ----------------------------------------------------------------
    sub_outcome %>%
      imap(~ armadillo.upload_table(subset_name, outcome_folder, .x, .y))
  }

  if (type %in% c("describe", "both")) {
    return(dif_out)
  }
}
