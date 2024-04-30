library(mockery)

buckets <- structure(list(Bucket = c(
  "shared-diabetes", "shared-gecko",
  "user-admin",
  "user-c5dde1e1-95c5-4eb7-8c34-1296ac53562e"
), CreationDate = c(
  "2020-07-15T18:13:53.697Z", "2020-07-15T09:02:53.880Z",
  "2020-07-15T11:31:02.810Z", "2020-07-16T12:04:06.826Z"
)), row.names = c(NA, 4L), class = "data.frame")

tables <- structure(
  list(
    Contents = structure(list(
      Key = "folder/patient.parquet",
      LastModified = "2020-07-15T18:14:35.324Z",
      ETag = "\"8479a0fce51a4bb731cbe99a8287510b\"",
      Size = 3312, Owner = list(
        ID = "02d6176db174dc93cb1b899f7c6078f08654445fe8cf1b6ce98d8855f66bdbf4",
        DisplayName = list()
      ), StorageClass = "STANDARD", Bucket = "shared-diabetes"
    ), class = "s3_object"),
    Contents = structure(list(
      Key = "folder/test.parquet", LastModified = "2020-07-20T13:30:41.015Z",
      ETag = "\"a27785cb22af9dba331b53dadfa136b2\"", Size = 1040,
      Owner = list(
        ID = "02d6176db174dc93cb1b899f7c6078f08654445fe8cf1b6ce98d8855f66bdbf4",
        DisplayName = list()
      ), StorageClass = "STANDARD",
      Bucket = "shared-diabetes"
    ), class = "s3_object")
  ),
  server = "nginx/1.17.10", date = "Mon, 20 Jul 2020 13:30:48 GMT",
  "`content-type`" = "application/xml", "`transfer-encoding`" = "chunked",
  connection = "keep-alive", vary = "Origin",
  "`content-security-policy`" = "block-all-mixed-content",
  "`x-amz-request-id`" = "1623791ABE6FF3E2",
  "`x-xss-protection`" = "1; mode=block",
  "`content-encoding`" = "gzip", class = "s3_bucket",
  Name = "shared-diabetes", Prefix = list(), Marker = list(),
  MaxKeys = "50000", Delimiter = list(), IsTruncated = "false",
  CommonPrefixes = character(0)
)
