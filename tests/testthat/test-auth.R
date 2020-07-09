test_that("login retrieves session info", {
  response <- structure(list(status_code = 200), class = "response")
  httr_post <- mock(response)
  content <- xml2::as_xml_document(list(AssumeRoleWithWebIdentityResponse = structure(list(
    AssumeRoleWithWebIdentityResult = list(
      AssumedRoleUser = list(Arn = list(), AssumeRoleId = list()),
      Credentials = list(
        AccessKeyId = list("UNWUHGSS9OOE1L7I49X9"),
        SecretAccessKey = list("TiAe4MEuwgU+urcM94kLEORsk8PZtu+JJ6mmmOAj"),
        Expiration = list("2020-07-10T10:55:53Z"), SessionToken = list("edcba")
      ),
      SubjectFromWebIdentityToken = list("87ff7211-5050-4565-9137-f2547207c852")
    ),
    ResponseMetadata = list(RequestId = list("16205C5CE86AE656"))
  ), xmlns = "https://sts.amazonaws.com/doc/2011-06-15/")))

  httr_content <- mock(content)

  with_mock(
    session <- login("https://example.org", "abcde", 120),
    "httr::POST" = httr_post,
    "httr::content" = httr_content
  )

  expect_equal(session, list(
    key = "UNWUHGSS9OOE1L7I49X9",
    secret = "TiAe4MEuwgU+urcM94kLEORsk8PZtu+JJ6mmmOAj",
    session_token = "edcba"
  ))

  expect_args(httr_post, 1,
    "https://example.org",
    query = list(
      Action = "AssumeRoleWithWebIdentity",
      DurationSeconds = 120,
      WebIdentityToken = "abcde",
      Version = "2011-06-15"
    )
  )
})
