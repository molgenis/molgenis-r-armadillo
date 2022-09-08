# Setup HTTP stubbing
library(webmockr)
webmockr::enable()
webmockr::httr_mock()

# Run cleanup after all tests
withr::defer(stub_registry_clear(), teardown_env())