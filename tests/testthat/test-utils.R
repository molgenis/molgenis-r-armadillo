test_that(".handle_request_error handles 401", {
  expect_error(
    .handle_request_error(list(status_code = 401)),
    "Unauthorized"
  )
})

test_that(".handle_request_error handles 500", {
  response <- list(status_code = 500)
  httr_content <- mock(list(message = "Error"))
  with_mock(
    expect_error(
      .handle_request_error(response),
      "Internal server error: Error"
    ),
    "httr::content" = httr_content
  )
})

test_that("split_and_unlist splits strings correctly", {
  expect_equal(.split_and_unlist("a,b,c", ","), c("a", "b", "c"))
  expect_equal(.split_and_unlist("a|b|c", "|"), c("a", "b", "c"))
  expect_equal(.split_and_unlist("one two three", " "), c("one", "two", "three"))
  expect_equal(.split_and_unlist("apple;orange;banana", ";"), c("apple", "orange", "banana"))
  expect_equal(.split_and_unlist("a,,b,,c", ","), c("a", "", "b", "", "c"))
  expect_equal(.split_and_unlist("", ","), character(0))
})


test_that("split_and_unlist handles no separator", {
  expect_equal(.split_and_unlist("abcd", ""), c("a", "b", "c", "d"))  # Each character as a separate element
})

test_that("split_and_unlist returns character vector", {
  result <- .split_and_unlist("hello|world", "|")
  expect_type(result, "character")
})
