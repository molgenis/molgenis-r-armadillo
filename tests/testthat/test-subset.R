test_that("armadillo.subset calls .get_tables()", {
  source_project <- "subset_folder"

  subset_definition <- tibble(folder = c("subset_folder"), table = c("subset_table"), vars_to_subset = tibble(variable = c("subset_variable")))
  expected_result <- tibble(folder = c("subset_folder"), table = c("subset_table"), vars_to_subset = tibble(variable = c("subset_variable")), data = list(NULL))

  with_mock(
    "armadillo.list_tables" = mock(list("subset_folder/subset_table")),
    "armadillo.load_table" = mock(),
    "MolgenisArmadillo:::.use_https" = mock(TRUE),
    {
      result <- .get_tables(
        source_project,
        subset_definition
      )
      expect_equal(result, expected_result)
    }
  )
})

test_that("armadillo.subset calls .get_tables()", {
  source_project <- "subset_folder"

  subset_definition <- tibble(folder = c("subset_folder"), table = c("subset_table"), vars_to_subset = tibble(variable = c("subset_variable")))
  expected_result <- tibble(folder = c("subset_folder"), table = c("subset_table"), vars_to_subset = tibble(variable = c("subset_variable")), data = list(NULL))

  with_mock(
    "armadillo.list_tables" = mock(list("subset_folder/subset_table")),
    "armadillo.load_table" = mock(),
    "MolgenisArmadillo:::.use_https" = mock(TRUE),
    {
      result <- .get_tables(
        source_project,
        subset_definition
      )
      expect_equal(result, expected_result)
    }
  )
})
