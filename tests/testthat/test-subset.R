library("tibble")

core <- data.frame(
                   row_id = c(1, 2, 3, 4, 5),
                   mother_id = c(0, 1, 2, 3, 4),
                   child_id = c(1, 4, 8, 15, 1),
                   cohort_id = c(110, 106, 114, 102, 109),
                   preg_no = c(0, 1, 2, 3, 4),
                   child_no = c(0, 1, 2, 3, 4),
                   recruit_age = c(4, 3, 13, 13, 4),
                   ethn1_m = c(NA, NA, NA, NA, NA),
                   agebirth_m_y = c(245, 128, 222, 90, 50),
                   agebirth_m_d = c(513, 1457, 1363, 848, 1060),
                   death_m = c(NA, NA, NA, NA, NA),
                   death_m_age = c(233, 950, 1096, 303, 454))

tables <- tibble(
                 project = "gecko",
                 folder = "2_1_core_1_0",
                 table = "non_rep",
                 vars_to_subset = list(
                                       as_tibble_col(c("child_id", "mother_id",
                                                       "ethn1_m", "cohort_id",
                                                       "non_existant"),
                                                     column_name = "variable")),
                 data = list(core))

subset_def <- tibble(
                     folder = c("2_1_core_1_0"),
                     table = c("non_rep"),
                     vars_to_subset = c(
                                        list(as_tibble_col(c("child_id",
                                                             "mother_id",
                                                             "ethn1_m",
                                                             "cohort_id"),
                                                           column_name =
                                                             "variable"))))

projects <- c("alspac", "gecko", "inma", "ninfea", "my_project")

missing <- tibble(folder = "2_1_core_1_0", table = "non_rep",
                  missing = "non_existant")

test_that(".read_subset will throw error when one of headers is missing", {
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  cols <- c("folder", "table", "var")
  colnames(df) <- cols
  message <- paste0(".csv file must contain exactly three columns entitled ",
                    "'folder', 'table' and 'variable'")
  with_mock(read.csv = mock(df), {
    expect_error(.read_subset("broken.csv"), message, fixed = TRUE)
  })
})

test_that(".read_subset will throw error when number of columns incorrect", {
  df <- data.frame(matrix(ncol = 4, nrow = 0))
  cols <- c("folder", "table", "variable", "something else")
  colnames(df) <- cols

  with_mock(read.csv = mock(df), {
    message <- paste0(".csv file must contain exactly three columns entitled ",
                      "'folder', 'table' and 'variable'")
    expect_error(.read_subset("broken.csv"), message,
                 fixed = TRUE)
  })
})

test_that(".read_subset will return correct output", {
  df <- data.frame(
                   folder = c("outcome", "outcome", "outcome"),
                   table = c("yearlyrep", "yearlyrep", "yearlyrep"),
                   variable = c("row_id", "child_id", "int_raw_3"))

  expected <- tibble(folder = "outcome", table = "yearlyrep",
                     subset_vars = list(as_tibble_col(c("row_id", "child_id",
                                                        "int_raw_3"),
                       column_name = "variable"
                     )))

  with_mock(read.csv = mock(df), {
    obj <- .read_subset("data.csv")
    expect_identical(obj, expected)
  })
})

test_that("armadillo.subset_definition should return proper
          subset definition", {
            df <- data.frame(
                             folder = c("outcome", "outcome", "outcome"),
                             table = c("yearlyrep", "yearlyrep", "yearlyrep"),
                             variable = c("row_id", "child_id", "int_raw_3"))

            expected <- tibble(folder = "outcome", table = "yearlyrep",
                               vars_to_subset = list(as_tibble_col(c("row_id",
                                                                     "child_id",
                                                                     "int_raw_3"
                                                                   ),
                                                                   column_name =
                                                                     "variable")
                               ))

            with_mock(read.csv = mock(df), {
              obj <- armadillo.subset_definition("data.csv")
              expect_identical(obj, expected)
            })
          })

test_that("armadillo.subset_definition will throw error when vars are NULL", {
  df <- data.frame(
                   folder = c("outcome", "outcome", "outcome"),
                   table = c("yearlyrep", "yearlyrep", "yearlyrep"),
                   variable = c("row_id", "child_id", "int_raw_3"))

  with_mock(read.csv = mock(df), {
    message <- paste0("You must provide a .csv file with variables and tables ",
                      "to subset")
    expect_error(armadillo.subset_definition(NULL), message, fixed = TRUE)
  })
})

test_that(".get_tables throws error when tables are missing", {
  tables <- c("gecko/1_1_outcome_1_0/non_rep",
              "gecko/1_1_outcome_1_0/yearly_rep",
              "gecko/2_1_core_1_0/monthly_rep",
              "gecko/2_1_core_1_0/non_rep",
              "gecko/2_1_core_1_0/trimester_rep",
              "gecko/2_1_core_1_0/yearly_rep")

  subset_def <- tibble(
                       folder = c("outcome"),
                       table = c("yearly_rep"),
                       vars_to_subset = c(
                                          list(as_tibble_col(c("row_id",
                                                               "child_id",
                                                               "int_raw_3"),
                                                             column_name =
                                                               "variable"))))
  message <- paste0("The following folders & tables: [ outcome] are included",
                    " in your reference object, but don't exist within the ",
                    "specified projectThe following folders & tables: ",
                    "[ yearly_rep] are included in your reference object, ",
                    "but don't exist within the specified project")

  with_mock(armadillo.list_tables = mock(tables), {
    expect_error(.get_tables("test-project", subset_def),
                 message,
                 fixed = TRUE)
  })
})

test_that(".get_tables returns tables", {
  table_list <- c("gecko/1_1_outcome_1_0/non_rep",
                  "gecko/1_1_outcome_1_0/yearly_rep",
                  "gecko/2_1_core_1_0/monthly_rep",
                  "gecko/2_1_core_1_0/non_rep",
                  "gecko/2_1_core_1_0/trimester_rep",
                  "gecko/2_1_core_1_0/yearly_rep")

  expected <- tibble(
                     folder = "2_1_core_1_0",
                     table = "non_rep",
                     vars_to_subset = list(
                                           as_tibble_col(c("child_id",
                                                           "mother_id",
                                                           "ethn1_m",
                                                           "cohort_id"),
                                                         column_name =
                                                           "variable")),
                     data = list(core))

  with_mock(armadillo.list_tables = mock(table_list), {
    with_mock(armadillo.load_table = mock(core), {
      obj <- .get_tables("test-project", subset_def)
      expect_identical(obj, expected)
    })
  })
})

test_that("armadillo.subset fails if source project is NULL", {
  message <- paste0("You must provide the name of the source ",
                    "project from which you will subset")
  expect_error(armadillo.subset(NULL, "my_project", subset_def),
               message,
               fixed = TRUE)
})

test_that("armadillo.subset fails if new project is NULL", {
  expect_error(armadillo.subset("gecko", NULL, subset_def),
               "You must provide a name for the new project",
               fixed = TRUE)
})

test_that("armadillo.subset fails if subset_def is NULL", {
  message <- paste0("You must provide an object created by ",
                    "armadillo.subset_definition containing details of the ",
                    "variables and tables to include in the subset")
  expect_error(armadillo.subset("gecko", "my_project", NULL),
               message,
               fixed = TRUE)
})

test_that(".check_available_vars will return table with non existant variable",
          {
            expected <- tibble(folder = "2_1_core_1_0", table = "non_rep",
                               missing = "non_existant")
            expect_identical(.check_available_vars(tables), expected)
          })

test_that("armadillo.subset will return missing and call make_subset", {
  with_mock("MolgenisArmadillo:::.get_tables" = mock(tables), {
    with_mock(armadillo.list_projects = mock(projects), {
      with_mock("MolgenisArmadillo:::.check_available_vars" = mock(missing), {
        with_mock("MolgenisArmadillo:::.make_subset" = mock(), {
          expect_identical(armadillo.subset("gecko", "my_project", subset_def),
                           missing)
          expect_equal(length(.make_subset), 1)
        })
      })
    })
  })
})

test_that("armadillo.subset returns missing and make_subset for dryrun", {
  with_mock("MolgenisArmadillo:::.get_tables" = mock(tables), {
    with_mock(armadillo.list_projects = mock(projects), {
      with_mock("MolgenisArmadillo:::.check_available_vars" =
                  mock(missing), {
                  with_mock("MolgenisArmadillo:::.make_subset" =
                              mock(), {
                              expect_identical(armadillo.subset("gecko",
                                                                "my_project",
                                                                subset_def,
                                                                TRUE),
                                               missing)
                              expect_equal(length(.make_subset), 0)
                            })
                })
    })
  })
})

test_that(".make_subset creates new project if provided project doesn't exist
          and uploads table", {
            with_mock(armadillo.list_projects = mock(projects), {
              with_mock(armadillo.create_project = mock(), {
                with_mock(armadillo.upload_table = mock(), {
                  .make_subset("test_project", tables)
                  expect_equal(length(armadillo.create_project), 1)
                  expect_equal(length(armadillo.upload_table), 1)
                })
              })
            })
          })

test_that(".make_subset creates doesn't create project if provided project
          exists and uploads table", {
            with_mock(armadillo.list_projects = mock(projects), {
              with_mock(armadillo.create_project = mock(), {
                with_mock(armadillo.upload_table = mock(), {
                  .make_subset("my_project", tables)
                  expect_equal(length(armadillo.create_project), 0)
                  expect_equal(length(armadillo.upload_table), 1)
                })
              })
            })
          })
