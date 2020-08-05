content <- xml2::as_xml_document(
  list(
    AssumeRoleWithWebIdentityResponse =
      structure(list(
        AssumeRoleWithWebIdentityResult = list(
          AssumedRoleUser = list(Arn = list(), AssumeRoleId = list()),
          Credentials = list(
            AccessKeyId = list("UNWUHGSS9OOE1L7I49X9"),
            SecretAccessKey =
              list("TiAe4MEuwgU+urcM94kLEORsk8PZtu+JJ6mmmOAj"),
            Expiration = list("2020-07-10T10:55:53Z"),
            SessionToken = list("edcba")
          ),
          SubjectFromWebIdentityToken =
            list("87ff7211-5050-4565-9137-f2547207c852")
        ),
        ResponseMetadata = list(RequestId = list("16205C5CE86AE656"))
      ), xmlns = "https://sts.amazonaws.com/doc/2011-06-15/")
  )
)

test_that("assume_role_with_webidentity retrieves credentials", {
  response <- structure(list(status_code = 200), class = "response")
  httr_post <- mock(response)
  httr_content <- mock(content)

  with_mock(
    session <- armadillo.assume_role_with_web_identity(
      "abcde", "https://example.org", 900
    ),
    "httr::POST" = httr_post,
    "httr::content" = httr_content
  )

  expect_equal(session, list(
    AccessKeyId = "UNWUHGSS9OOE1L7I49X9",
    SecretAccessKey = "TiAe4MEuwgU+urcM94kLEORsk8PZtu+JJ6mmmOAj",
    SessionToken = "edcba"
  ))

  expect_args(httr_post, 1,
    "https://example.org",
    query = list(
      Action = "AssumeRoleWithWebIdentity",
      DurationSeconds = 900,
      WebIdentityToken = "abcde",
      Version = "2011-06-15"
    )
  )

  expect_equal(Sys.getenv("AWS_S3_ENDPOINT"), "example.org")
})

test_that("assume_role_with_webidentity sets port in AWS_S3_ENDPOINT", {
  response <- structure(list(status_code = 200), class = "response")
  httr_post <- mock(response)
  httr_content <- mock(content)

  with_mock(
    session <- armadillo.assume_role_with_web_identity(
      "abcde", "https://example.org:9000", 900
    ),
    "httr::POST" = httr_post,
    "httr::content" = httr_content
  )

  expect_equal(Sys.getenv("AWS_S3_ENDPOINT"), "example.org:9000")
})
