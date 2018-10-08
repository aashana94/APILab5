context("test")

test_that("Input by user works",{
  expect_error(latlon(address=c("&","%","$","#","@","!","(",")","_","*","+",";","^"), apikey="12345"))
})
