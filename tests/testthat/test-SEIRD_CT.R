inits <- list(
  S0=0.999, E0=0, P0=0, A0=0, I0=0.001,
  Et0=0, Pt0=0, At0=0, It0=0, R0=0)
params <- list(beta=1,
               beta_a=2,
               gamma=3,
               mu=4,
               chi=0.5,
               phi=0.1,
               omega=2,
               eta_a=0.3,
               psi=5.3)

test_that("SEIRD_CT model is instantiated correctly", {
  my_model <- SEIRD_CT()
  
  expect_length(my_model@initial_conditions, 10)
  expect_length(my_model@transmission_parameters, 9)
})

test_that("Initial conditions can be set and retrieved", {
  my_model <- SEIRD_CT()
  initial_conditions(my_model) <- inits # Test output is correct
  expect_equal(initial_conditions(my_model),
               inits)
  
  # Check error is raised when sum of initial conditions is not 1
  inits1 <- inits
  inits1$S0 <- 0.98
  expect_error(initial_conditions(my_model) <- inits1)
})

test_that("Transmission parameters can be set and retrieved", {
  my_model <- SEIRD_CT()
  transmission_parameters(my_model) <- params
  
  # Test output is correct
  expect_equal(transmission_parameters(my_model),
               params)
})

test_that("SEIR model runs correctly", {
  my_model <- SEIRD_CT()
  initial_conditions(my_model) <- inits
  transmission_parameters(my_model) <- params
  
  # Check output shape
  t <- seq(0, 10, by = 0.1)
  out_df <- run(my_model, t)
  expect_identical(dim(out_df$changes), as.integer(c(((10 - 0) / 0.1 + 1) * 2, 4)))
  expect_identical(dim(out_df$state), as.integer(c(((10 - 0) / 0.1 + 1) * 11, 4)))
  
  # Test input errors
  expect_error(run(my_model, "a"))
})

test_that("Running model before setting parameters fails", {
  t <- seq(0, 10, by = 0.1)
  my_model <- SEIRD_CT()
  initial_conditions(my_model) <- inits
  expect_error(run(my_model, t), "Transmission parameters must be set before running.")
  my_model <- SEIRD_CT()
  transmission_parameters(my_model) <- params
  expect_error(run(my_model, t), "Initial conditions must be set before running.")
})

test_that("R0 works for SEIRD_CT model", {
  my_model <- SEIRD_CT()
  initial_conditions(my_model) <- inits
  transmission_parameters(my_model) <- params
  expect_true(R0(my_model) > 0)
})
