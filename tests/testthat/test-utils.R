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
