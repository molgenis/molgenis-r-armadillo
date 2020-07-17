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
    "Folder name has max 56 characters"
  )
})

test_that(".check_folder_name checks for invalid characters", {
  expect_error(
    .check_folder_name("A"),
    "Folder name must consist of lowercase letters and numbers"
  )
  expect_error(
    .check_folder_name("\U72B0"),
    "Folder name must consist of lowercase letters and numbers"
  )
  expect_error(
    .check_folder_name("ármadïllø"),
    "Folder name must consist of lowercase letters and numbers"
  )
})


test_that(".check_folder_name allows valid names", {
  expect_silent(.check_folder_name(strrep("x", 56)))
  expect_silent(.check_folder_name("x"))
})
