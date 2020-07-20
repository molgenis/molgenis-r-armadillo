test_that(".check_folder_name checks for single character argument", {
  expect_error(.check_folder_name(NA), "is.character\\(name\\) is not TRUE")
  expect_error(.check_folder_name(NULL), "is.character\\(name\\) is not TRUE")
  expect_error(
    .check_folder_name(list("abc", "def")),
    "is.character\\(name\\) is not TRUE"
  )
})

test_that(".check_folder_name checks name length", {
  expect_error(.check_folder_name(""), "Folder name cannot be empty")
  expect_error(
    .check_folder_name(strrep("x", 57)),
    "Folder name has max 56 characters\\."
  )
})

test_that(".check_folder_name checks for invalid characters", {
  expect_error(
    .check_folder_name("A"),
    "Folder name must consist of lowercase letters and numbers\\."
  )
  expect_error(
    .check_folder_name("\U72B0"),
    "Folder name must consist of lowercase letters and numbers\\."
  )
  expect_error(
    .check_folder_name("ármadïllø"),
    "Folder name must consist of lowercase letters and numbers\\."
  )
})


test_that(".check_folder_name allows valid names", {
  expect_silent(.check_folder_name(strrep("x", 56)))
  expect_silent(.check_folder_name("x"))
})

test_that(".check_workspace_name checks for single character argument", {
  expect_error(.check_workspace_name(NA), "is.character\\(name\\) is not TRUE")
  expect_error(.check_workspace_name(NULL),
               "is.character\\(name\\) is not TRUE")
  expect_error(
    .check_workspace_name(list("abc", "def")),
    "is.character\\(name\\) is not TRUE"
  )
})

test_that(".check_workspace_name checks name length", {
  expect_error(.check_workspace_name(""), "Workspace name cannot be empty")
  expect_error(
    .check_workspace_name(strrep("x", 1019)),
    "Workspace name cannot be longer than 1018 characters\\."
  )
})

test_that(".check_workspace_name checks for valid characters", {
  expected <- paste0(
    "Valid workspace name characters are ",
    "ASCII letters, digits, '_', '-' and ':'"
  )
  expect_error(.check_workspace_name("\U72B0"), expected)
  expect_error(.check_workspace_name("ármadïllø"), expected)
  expect_error(.check_workspace_name("A b"), expected)
})

test_that(".check_workspace_name allows valid names", {
  expect_silent(.check_workspace_name("Example_0-9:__BACKUP__"))
  expect_silent(.check_workspace_name(strrep("x", 1018)))
})
