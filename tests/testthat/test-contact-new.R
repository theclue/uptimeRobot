test_that("uptimerobot.account.details", {
  
  skip_on_cran()
  
  api.key <- Sys.getenv("KEY", "")
  

  # Error in case of invalid api.key
  expect_error(uptimerobot.contact.new("blahblah"), "apiKey not mentioned or in a wrong format")
  
  # Error in case the api.key is NULL or empty
  expect_error(uptimerobot.contact.new(""), "api.key cannot be empty or NULL")
  
  # Error in case the type is empty or not recognized
  expect_error(uptimerobot.contact.new(""), "contact type missing or not recognized.")
  
  # Clean the environment
  rm(list = ls())
  
})
