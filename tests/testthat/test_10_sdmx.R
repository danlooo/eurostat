test_that("get_eurostat_sdmx works", {
  skip_on_cran()
  skip_if_offline()
  
  prodcom <- get_eurostat_sdmx(id = "ds-059327", agency = "eurostat_comext", 
                               filters = 
                                 list(
                                   FREQ = c("A"),
                                   REPORTER = "FR",
                                   PARTNER = "US",
                                   PRODUCT = c("291"),
                                   FLOW = "2",
                                   INDICATORS = "VALUE_EUR"), verbose = FALSE)
  
  expect_equal(unique(prodcom$freq), "A")
    
  prodcom2 <- get_eurostat_sdmx(id = "ds-059327", agency = "eurostat_comext", 
                             filters = 
                               list(
                                 FREQ = c("M"),
                                 REPORTER = "FR",
                                 PARTNER = "US",
                                 PRODUCT = c("291"),
                                 FLOW = "2",
                                 INDICATORS = "VALUE_EUR"), verbose = FALSE, use.data.table = TRUE)
  
  expect_equal(unique(prodcom2$freq), "M")
})
