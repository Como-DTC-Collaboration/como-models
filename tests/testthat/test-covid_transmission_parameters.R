test_that("non-age-structured transmission parameters call works",{
  params <- covid_transmission_parameters()
  expect_length(params$kappa, 1)
  expect_length(params$gamma, 1)
  expect_length(params$mu, 1)
  expect_length(params$R0, 1)
  
  # check delta different
  params_d <- covid_transmission_parameters(variant="delta")
  expect_true(params$R0 != params_d$R0)
  expect_true(params$kappa != params_d$kappa)
  
  # check omicron different
  params_o <- covid_transmission_parameters(variant="omicron")
  expect_true(params$R0 < params_o$R0)
  expect_true(params_d$R0 < params_o$R0)
  expect_true(params$kappa != params_o$kappa)
  expect_true(params_d$kappa != params_o$kappa)
})

test_that("age-structured transmission parameters call works", {
  params <- covid_transmission_parameters(is_age_structured = TRUE)
  params_d <- covid_transmission_parameters(is_age_structured = TRUE,
                                            variant="delta")
  params_o <- covid_transmission_parameters(is_age_structured = TRUE,
                                            variant="omicron")
  expect_equal(dim(params$gamma), dim(params_d$gamma))
  expect_equal(dim(params$gamma), dim(params_o$gamma))
  expect_equal(dim(params$mu), dim(params_d$mu))
  expect_equal(dim(params$mu), dim(params_o$mu))
  expect_length(params$R0, 1)
  expect_length(params_d$R0, 1)
  expect_length(params_o$R0, 1)
  expect_true(params$R0 < params_d$R0)
  expect_true(params_d$R0 < params_o$R0)
})
