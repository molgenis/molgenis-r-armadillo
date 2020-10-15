test_that(".check_project_name checks for single character argument", {
  expect_error(.check_project_name(NA), "is.character\\(name\\) is not TRUE")
  expect_error(.check_project_name(NULL), "is.character\\(name\\) is not TRUE")
  expect_error(
    .check_project_name(list("abc", "def")),
    "is.character\\(name\\) is not TRUE"
  )
})

test_that(".check_project_name checks name length", {
  expect_error(.check_project_name(""), "Project name cannot be empty")
  expect_error(
    .check_project_name(strrep("x", 57)),
    "Project name has max 56 characters\\."
  )
})

test_that(".check_project_name checks for invalid characters", {
  expect_error(
    .check_project_name("A"),
    "Project name must consist of lowercase letters and numbers\\."
  )
  expect_error(
    .check_project_name("\U72B0"),
    "Project name must consist of lowercase letters and numbers\\."
  )
  expect_error(
    .check_project_name("ármadïllø"),
    "Project name must consist of lowercase letters and numbers\\."
  )
})


test_that(".check_project_name allows valid names", {
  expect_silent(.check_project_name(strrep("x", 56)))
  expect_silent(.check_project_name("x"))
})

test_that(".check_full_table_name checks folder for single character arg", {
  expect_error(
    .check_full_name(folder = NA, name = "name"),
    "is.character\\(folder\\) is not TRUE"
  )
  expect_error(
    .check_full_name(folder = NULL, name = "name"),
    "is.character\\(folder\\) is not TRUE"
  )
  expect_error(
    .check_full_name(folder = list("abc", "def"), name = "name"),
    "is.character\\(folder\\) is not TRUE"
  )
})

test_that(".check_full_table_name checks name for single character argument", {
  expect_error(
    .check_full_name(folder = "folder", name = NA),
    "is.character\\(name\\) is not TRUE"
  )
  expect_error(
    .check_full_name(folder = "folder", name = NULL),
    "is.character\\(name\\) is not TRUE"
  )
  expect_error(
    .check_full_name(folder = "folder", name = list("abc", "def")),
    "is.character\\(name\\) is not TRUE"
  )
})

test_that(".check_full_name checks name length", {
  expect_error(
    .check_full_name("", "name"),
    "Folder, table or resource name cannot be empty"
  )
  expect_error(
    .check_full_name("folder", ""),
    "Folder, table or resource name cannot be empty"
  )
  expect_error(
    .check_full_name(strrep("x", 1000), strrep("y", 24)),
    "Folder \\+ table/resource name cannot be longer than 1024 characters\\."
  )
})

test_that(".check_full_name checks for valid characters", {
  expected <- paste0(
    "Valid name characters are ",
    "ASCII letters, digits, '_', '-' and ':'"
  )
  expect_error(.check_full_name("folder", "\U72B0"), expected)
  expect_error(.check_full_name("folder", "ármadïllø"), expected)
  expect_error(.check_full_name("folder", "A b"), expected)
})

test_that(".check_full_table_name allows valid names", {
  expect_silent(.check_full_name("folder", "Example_0-9:__BACKUP__"))
  expect_silent(.check_full_name(strrep("x", 1000), strrep("y", 23)))
})
