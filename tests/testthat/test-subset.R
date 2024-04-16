library(tibble)

test_that("read_view_reference handles missing .csv file", {
  missing_csv <- "thispathdoesnotexist"
  expect_error(
    .read_view_reference(missing_csv),
    "'thispathdoesnotexist' does not exist in current working directory ('/Users/tcadman/Library/Mobile Documents/com~apple~CloudDocs/work/repos/molgenis-r-armadillo/tests/testthat').", 
  fixed = T)
})

test_that("read_view_reference handles existing .csv file", {
  example_csv <- tibble(
    "variable" = c("var1", "var2", "var3"),
    "source_folder" = c("folder1", "folder2", "folder3"),
    "source_table" = c("table1", "table2", "table3"))
  csv_test_path <- "csv_for_testing.csv"
  write.csv(example_csv, csv_test_path, row.names = FALSE)
  expect_is(.read_view_reference(csv_test_path), "tbl_df")
  })

test_that("read_view_reference handles null value for `reference_csv` argument", {
  expect_error(
    .read_view_reference(NULL),
    "You must provide a .csv file with variables and tables to subset",
    info = "Function should throw an error when the .csv file path is NULL"
  )
})

test_that("It renames 'folder' column to 'source_folder'", {
  reference <- data.frame(folder = c(1, 2, 3), table = c('A', 'B', 'C'))
  renamed_reference <- .rename_reference_columns(reference, "folder", "source_folder")
  expect_equal("source_folder" %in% colnames(renamed_reference), TRUE)
  expect_equal("folder" %in% colnames(renamed_reference), FALSE)
})

test_that("It renames 'table' column to 'source_table'", {
  reference <- data.frame(folder = c(1, 2, 3), table = c('A', 'B', 'C'))
  renamed_reference <- .rename_reference_columns(reference, "table", "source_table")
  expect_equal("source_table" %in% colnames(renamed_reference), TRUE)
  expect_equal("table" %in% colnames(renamed_reference), FALSE)
})

test_that("It throws an error when required columns are missing", {
  df <- data.frame(variable = c("var1", "var2"),
                   target_folder = c("folder1", "folder2"))
  expect_error(.check_reference_columns(df), ".csv file must contain columns entitled 'source_folder', 'source_table' and 'variable'")
})

test_that("It throws an error when extra columns are present", {
  df <- data.frame(source_folder = c("folder1", "folder2"),
                   source_table = c("table1", "table2"),
                   variable = c("var1", "var2"),
                   extra_column = c("extra1", "extra2"))
  expect_error(.check_reference_columns(df), ".csv column name 'extra_column' is not permitted: allowed names are 'source_folder, source_table, variable, target_folder, target_table'")
})

test_that("It throws an error when there are missing cells", {
  df <- data.frame(source_folder = c("folder1", NA, "folder_2"),
                   source_table = c("table1", "table2", NA),
                   variable = c(NA, "var1", "var2"))
  expect_error(.check_reference_columns(df), "The input .csv file contains empty cells: please check and try again")
})

test_that("It nests the 'target_vars' column in the dataframe", {
  df <- data.frame(source_folder = c("folder1", "folder2"),
                   source_table = c("table1", "table2"),
                   variable = c("var1", "var2"))
  
  subset_ref <- as_tibble(df)
  formatted_df <- .format_reference(subset_ref)
  expect_is(formatted_df$target_vars, "list")
  expect_equal(length(formatted_df$target_vars), nrow(formatted_df))
})

test_that("It sets default values for 'target_folder' and 'target_table'", {
  df <- tibble(source_folder = c("folder1", "folder2"),
               source_table = c("table1", "table2"),
               variable = c("var1", "var2"))
  modified_df <- .set_default_targets(df)
  expect_true("target_folder" %in% colnames(modified_df))
  expect_true("target_table" %in% colnames(modified_df))
  expect_equal(unique(modified_df$target_folder), unique(df$source_folder))
  expect_equal(unique(modified_df$target_table), unique(df$source_table))
})

test_that("It does not modify the dataframe if 'target_folder' and 'target_table' columns are already present", {
  df <- tibble(source_folder = c("folder1", "folder2"),
               source_table = c("table1", "table2"),
               target_folder = c("folderA", "folderB"),
               target_table = c("tableA", "tableB"),
               variable = c("var1", "var2"))
  modified_df <- .set_default_targets(df)
  expect_identical(df, modified_df)
})

test_that("It returns a tibble when a valid .csv file path is provided", {
  example_csv <- tibble(
    "variable" = c("var1", "var2", "var3"),
    "source_folder" = c("folder1", "folder2", "folder3"),
    "source_table" = c("table1", "table2", "table3"))
  csv_test_path <- "csv_for_testing.csv"
  write.csv(example_csv, csv_test_path, row.names = FALSE)
  output_df <- armadillo.subset_definition(reference_csv = csv_test_path)
  expect_is(output_df, "tbl_df")
  expect_true(all(c("source_folder", "source_table", "target_vars", "target_folder", "target_table") %in% colnames(output_df)))
  expect_equal(nrow(output_df), 3)
})

test_that("It throws an error if source_project is NULL", {
  expect_error(.check_args_valid(input_source = "subset_def", source_project = NULL, 
                                 subset_def = "subset_def", source_folder = "source_folder", 
                                 source_table = "source_table", target_project = "target_project", 
                                 target_folder = "target_folder", target_table = "target_table", 
                                 target_variables = "target_variables", new_project = NULL, 
                                 dry_run = NULL), 
               "You must provide the name of the source project from which you will subset")
})

test_that("It throws an error if target_project is NULL", {
  expect_error(.check_args_valid(input_source = "subset_def", source_project = "source_project", 
                                 target_project = NULL, subset_def = "subset_def", 
                                 source_folder = "source_folder", source_table = "source_table", 
                                 target_folder = "target_folder", target_table = "target_table", 
                                 target_variables = "target_variables", new_project = NULL, 
                                 dry_run = NULL), 
               "You must provide a name for the target project")
})

test_that("It throws an error if input_source is 'subset_def' but subset_def is NULL", {
  expect_error(.check_args_valid(input_source = "subset_def", source_project = "source_project", 
                                 target_project = "target_project", subset_def = NULL, 
                                 source_folder = "source_folder", source_table = "source_table", 
                                 target_folder = "target_folder", target_table = "target_table", 
                                 target_variables = "target_variables", new_project = NULL, 
                                 dry_run = NULL), 
               "You have specified `input_source = subset_ref` but you have not provided an object created by armadillo.subset_definition containing details of the variables and tables to include in the subset")
})

test_that("It throws an error if input_source is 'arguments' but required arguments are NULL", {
  expect_error(.check_args_valid(input_source = "arguments", source_project = "source_project", 
                                 target_project = "target_project", subset_def = NULL, 
                                 source_folder = NULL, source_table = NULL, 
                                 target_folder = "target_folder", target_table = "target_table", 
                                 target_variables = "target_variables", new_project = NULL, 
                                 dry_run = NULL), 
               "You must provide source_folder, source_table, target_folder, target_table and target_variables if input_source = 'arguments'")
})

test_that("It displays a message if new_project is provided (deprecated)", {
  expect_message(.check_args_valid(input_source = "subset_def", source_project = "source_project", 
                                   target_project = "target_project", new_project = "new_project", 
                                   subset_def = "subset_def", source_folder = "source_folder", 
                                   source_table = "source_table", target_folder = "target_folder", 
                                   target_table = "target_table", target_variables = "target_variables", 
                                   dry_run = NULL), 
                 "Argument `new project` has now been depricated: please use `target_project` instead")
})

test_that("It displays a message if dry_run is provided (defunct)", {
  expect_message(.check_args_valid(input_source = "subset_def", source_project = "source_project", 
                                   target_project = "target_project", dry_run = TRUE, 
                                   subset_def = "subset_def", source_folder = "source_folder", 
                                   source_table = "source_table", target_folder = "target_folder", 
                                   target_table = "target_table", target_variables = "target_variables", 
                                   new_project = "new_project"), 
                 "Argument `dry_run` is now defunct")
})

test_that("It creates a subset definition object with the specified arguments", {
  target_vars <- c("var1", "var2", "var3")
  source_folder <- "source_folder"
  source_table <- "source_table"
  target_folder <- "target_folder"
  target_table <- "target_table"
  
  expected_subset_def <- tibble(
    target_vars = list(tibble(target_vars)),
    source_folder = source_folder,
    source_table = source_table,
    target_folder = target_folder,
    target_table = target_table
  )
  
  actual_subset_def <- .create_subset_def_from_arguments(target_vars, source_folder, 
                                                         source_table, target_folder, 
                                                         target_table)
  
  expect_equal(actual_subset_def, expected_subset_def)
})

test_that("It creates the URL for the API request with the specified target project", {
  expect_equal(
    with_mocked_bindings(
      .make_post_url(target_project = "test_project"),
      ".get_url" = function() "https://armadillo-demo.molgenis.net/"
    ), 
    "https://armadillo-demo.molgenis.net/storage/projects/test_project/objects/link"
  )
})
  
context(".make_json_body function")

test_that("It creates JSON body for the API request with the specified parameters", {

  source_project <- "source_project"
  source_folder <- "source_folder"
  source_table <- "source_table"
  target_project <- "target_project"
  target_folder <- "target_folder"
  target_table <- "target_table"
  target_vars <- c("var1", "var2", "var3")
  
  expected_body <- list(
    sourceObjectName = paste0(source_folder, "/", source_table),
    sourceProject = source_project,
    linkedObject = paste0(target_folder, "/", target_table),
    variables = paste0(target_vars, collapse = ",")
  )
  
  actual_body <- .make_json_body(source_project, source_folder, source_table, 
                                 target_project, target_folder, target_table, 
                                 target_vars)
  
    expect_equal(actual_body, expected_body)
  })

test_that("It creates the URL for the API request with the specified target project", {
  expect_equal(
    with_mocked_bindings(
      .make_post_url(target_project = "test_project"),
      ".get_url" = function() "https://armadillo-demo.molgenis.net/"
    ), 
    "https://armadillo-demo.molgenis.net/storage/projects/test_project/objects/link"
  )
})

test_that("It makes headers for API requests", {
  expect_equal(
    with_mocked_bindings(
      .make_headers(),
      ".get_auth_header" = function() "Basic YWRtaW46YWRtaW4="
    ),
    list(
      "Accept" = "*/*",
      "Content-Type" = "application/json",
      "Authorization" = "Basic YWRtaW46YWRtaW4="
    )
  )
  })











# 
# core <- data.frame(
#   row_id = c(1, 2, 3, 4, 5),
#   mother_id = c(0, 1, 2, 3, 4),
#   child_id = c(1, 4, 8, 15, 1),
#   cohort_id = c(110, 106, 114, 102, 109),
#   preg_no = c(0, 1, 2, 3, 4),
#   child_no = c(0, 1, 2, 3, 4),
#   recruit_age = c(4, 3, 13, 13, 4),
#   ethn1_m = c(NA, NA, NA, NA, NA),
#   agebirth_m_y = c(245, 128, 222, 90, 50),
#   agebirth_m_d = c(513, 1457, 1363, 848, 1060),
#   death_m = c(NA, NA, NA, NA, NA),
#   death_m_age = c(233, 950, 1096, 303, 454)
# )
# 
# tables <- tibble(
#   project = "gecko",
#   folder = "2_1_core_1_0",
#   table = "non_rep",
#   vars_to_subset = list(
#     as_tibble_col(
#       c(
#         "child_id", "mother_id",
#         "ethn1_m", "cohort_id",
#         "non_existant"
#       ),
#       column_name = "variable"
#     )
#   ),
#   data = list(core)
# )
# 
# subset_def <- tibble(
#   folder = c("2_1_core_1_0"),
#   table = c("non_rep"),
#   vars_to_subset = c(
#     list(as_tibble_col(
#       c(
#         "child_id",
#         "mother_id",
#         "ethn1_m",
#         "cohort_id"
#       ),
#       column_name =
#         "variable"
#     ))
#   )
# )
# 
# projects <- c("alspac", "gecko", "inma", "ninfea", "my_project")
# 
# missing <- tibble(
#   folder = "2_1_core_1_0", table = "non_rep",
#   missing = "non_existant"
# )
# 
# test_that(".read_subset will throw error when one of headers is missing", {
#   df <- data.frame(matrix(ncol = 3, nrow = 0))
#   cols <- c("folder", "table", "var")
#   colnames(df) <- cols
#   message <- paste0(
#     ".csv file must contain exactly three columns entitled ",
#     "'folder', 'table' and 'variable'"
#   )
#   with_mock(read.csv = mock(df), {
#     expect_error(.read_subset("broken.csv"), message, fixed = TRUE)
#   })
# })
# 
# test_that(".read_subset will throw error when number of columns incorrect", {
#   df <- data.frame(matrix(ncol = 4, nrow = 0))
#   cols <- c("folder", "table", "variable", "something else")
#   colnames(df) <- cols
# 
#   with_mock(read.csv = mock(df), {
#     message <- paste0(
#       ".csv file must contain exactly three columns entitled ",
#       "'folder', 'table' and 'variable'"
#     )
#     expect_error(.read_subset("broken.csv"), message,
#       fixed = TRUE
#     )
#   })
# })
# 
# test_that(".read_subset will return correct output", {
#   df <- data.frame(
#     folder = c("outcome", "outcome", "outcome"),
#     table = c("yearlyrep", "yearlyrep", "yearlyrep"),
#     variable = c("row_id", "child_id", "int_raw_3")
#   )
# 
#   expected <- tibble(
#     folder = "outcome", table = "yearlyrep",
#     subset_vars = list(as_tibble_col(
#       c(
#         "row_id", "child_id",
#         "int_raw_3"
#       ),
#       column_name = "variable"
#     ))
#   )
# 
#   with_mock(read.csv = mock(df), {
#     obj <- .read_subset("data.csv")
#     expect_identical(obj, expected)
#   })
# })
# 
# test_that("armadillo.subset_definition should return proper
#           subset definition", {
#   df <- data.frame(
#     folder = c("outcome", "outcome", "outcome"),
#     table = c("yearlyrep", "yearlyrep", "yearlyrep"),
#     variable = c("row_id", "child_id", "int_raw_3")
#   )
# 
#   expected <- tibble(
#     folder = "outcome", table = "yearlyrep",
#     vars_to_subset = list(as_tibble_col(
#       c(
#         "row_id",
#         "child_id",
#         "int_raw_3"
#       ),
#       column_name =
#         "variable"
#     ))
#   )
# 
#   with_mock(read.csv = mock(df), {
#     obj <- armadillo.subset_definition("data.csv")
#     expect_identical(obj, expected)
#   })
# })
# 
# test_that("armadillo.subset_definition will throw error when vars are NULL", {
#   df <- data.frame(
#     folder = c("outcome", "outcome", "outcome"),
#     table = c("yearlyrep", "yearlyrep", "yearlyrep"),
#     variable = c("row_id", "child_id", "int_raw_3")
#   )
# 
#   with_mock(read.csv = mock(df), {
#     message <- paste0(
#       "You must provide a .csv file with variables and tables ",
#       "to subset"
#     )
#     expect_error(armadillo.subset_definition(NULL), message, fixed = TRUE)
#   })
# })
# 
# test_that(".get_tables throws error when tables are missing", {
#   tables <- c(
#     "gecko/1_1_outcome_1_0/non_rep",
#     "gecko/1_1_outcome_1_0/yearly_rep",
#     "gecko/2_1_core_1_0/monthly_rep",
#     "gecko/2_1_core_1_0/non_rep",
#     "gecko/2_1_core_1_0/trimester_rep",
#     "gecko/2_1_core_1_0/yearly_rep"
#   )
# 
#   subset_def <- tibble(
#     folder = c("outcome"),
#     table = c("yearly_rep"),
#     vars_to_subset = c(
#       list(as_tibble_col(
#         c(
#           "row_id",
#           "child_id",
#           "int_raw_3"
#         ),
#         column_name =
#           "variable"
#       ))
#     )
#   )
#   message <- paste0(
#     "The following folders & tables: [ outcome] are included",
#     " in your reference object, but don't exist within the ",
#     "specified projectThe following folders & tables: ",
#     "[ yearly_rep] are included in your reference object, ",
#     "but don't exist within the specified project"
#   )
# 
#   with_mock(armadillo.list_tables = mock(tables), {
#     expect_error(.get_tables("test-project", subset_def),
#       message,
#       fixed = TRUE
#     )
#   })
# })
# 
# test_that(".get_tables returns tables", {
#   table_list <- c(
#     "gecko/1_1_outcome_1_0/non_rep",
#     "gecko/1_1_outcome_1_0/yearly_rep",
#     "gecko/2_1_core_1_0/monthly_rep",
#     "gecko/2_1_core_1_0/non_rep",
#     "gecko/2_1_core_1_0/trimester_rep",
#     "gecko/2_1_core_1_0/yearly_rep"
#   )
# 
#   expected <- tibble(
#     folder = "2_1_core_1_0",
#     table = "non_rep",
#     vars_to_subset = list(
#       as_tibble_col(
#         c(
#           "child_id",
#           "mother_id",
#           "ethn1_m",
#           "cohort_id"
#         ),
#         column_name =
#           "variable"
#       )
#     ),
#     data = list(core)
#   )
# 
#   with_mock(armadillo.list_tables = mock(table_list), {
#     with_mock(armadillo.load_table = mock(core), {
#       obj <- .get_tables("test-project", subset_def)
#       expect_identical(obj, expected)
#     })
#   })
# })
# 
# test_that("armadillo.subset fails if source project is NULL", {
#   message <- paste0(
#     "You must provide the name of the source ",
#     "project from which you will subset"
#   )
#   expect_error(armadillo.subset(NULL, "my_project", subset_def),
#     message,
#     fixed = TRUE
#   )
# })
# 
# test_that("armadillo.subset fails if new project is NULL", {
#   expect_error(armadillo.subset("gecko", NULL, subset_def),
#     "You must provide a name for the new project",
#     fixed = TRUE
#   )
# })
# 
# test_that("armadillo.subset fails if subset_def is NULL", {
#   message <- paste0(
#     "You must provide an object created by ",
#     "armadillo.subset_definition containing details of the ",
#     "variables and tables to include in the subset"
#   )
#   expect_error(armadillo.subset("gecko", "my_project", NULL),
#     message,
#     fixed = TRUE
#   )
# })
# 
# test_that(".check_available_vars will return table with non existant variable", {
#   expected <- tibble(
#     folder = "2_1_core_1_0", table = "non_rep",
#     missing = "non_existant"
#   )
#   expect_identical(.check_available_vars(tables), expected)
# })
# 
# test_that("armadillo.subset will return missing and call make_subset", {
#   with_mock("MolgenisArmadillo:::.get_tables" = mock(tables), {
#     with_mock(armadillo.list_projects = mock(projects), {
#       with_mock("MolgenisArmadillo:::.check_available_vars" = mock(missing), {
#         with_mock("MolgenisArmadillo:::.make_subset" = mock(), {
#           expect_identical(
#             armadillo.subset("gecko", "my_project", subset_def),
#             missing
#           )
#           expect_equal(length(.make_subset), 1)
#         })
#       })
#     })
#   })
# })
# 
# test_that("armadillo.subset returns missing and make_subset for dryrun", {
#   with_mock("MolgenisArmadillo:::.get_tables" = mock(tables), {
#     with_mock(armadillo.list_projects = mock(projects), {
#       with_mock(
#         "MolgenisArmadillo:::.check_available_vars" =
#           mock(missing),
#         {
#           with_mock(
#             "MolgenisArmadillo:::.make_subset" =
#               mock(),
#             {
#               expect_identical(
#                 armadillo.subset(
#                   "gecko",
#                   "my_project",
#                   subset_def,
#                   TRUE
#                 ),
#                 missing
#               )
#               expect_equal(length(.make_subset), 0)
#             }
#           )
#         }
#       )
#     })
#   })
# })
# 
# test_that(".make_subset creates new project if provided project doesn't exist
#           and uploads table", {
#   with_mock(armadillo.list_projects = mock(projects), {
#     with_mock(armadillo.create_project = mock(), {
#       with_mock(armadillo.upload_table = mock(), {
#         .make_subset("test_project", tables)
#         expect_equal(length(armadillo.create_project), 1)
#         expect_equal(length(armadillo.upload_table), 1)
#       })
#     })
#   })
# })
# 
# test_that(".make_subset creates doesn't create project if provided project
#           exists and uploads table", {
#   with_mock(armadillo.list_projects = mock(projects), {
#     with_mock(armadillo.create_project = mock(), {
#       with_mock(armadillo.upload_table = mock(), {
#         .make_subset("my_project", tables)
#         expect_equal(length(armadillo.create_project), 0)
#         expect_equal(length(armadillo.upload_table), 1)
#       })
#     })
#   })
# })
