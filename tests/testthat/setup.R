# Setup HTTP stubbing
webmockr::enable()
webmockr::httr_mock()

# Run cleanup after all tests
withr::defer(stub_registry_clear(), teardown_env())
