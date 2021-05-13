test_that("SEIRD model is instantiated correctly", {
  my_model <- SEIRD()

  expect_length(my_model@initial_conditions, 4)
  expect_length(my_model@transmission_parameters, 4)
})

test_that("Initial conditions can be set and retrieved", {
  my_model <- SEIRD()
  initial_conditions(my_model) <- list(S0=0.9, E0=0, I0=0.1, R0=0)

  # Test output is correct
  expect_equal(initial_conditions(my_model),
               list("S0" = 0.9, "E0" = 0, "I0" = 0.1, "R0" = 0))

  # Check error is raised when initial states are not doubles
  expect_error(
    initial_conditions(my_model) <- list(S0=0.6, E0=0.2, I0=list(0.1, 0.1), R0=0))
  expect_error(
    initial_conditions(my_model) <- list(S0="a", E0=0.9, I0=0.1, R0=0))

  # Check error is raised when sum of initial conditions is not 1
  expect_error(initial_conditions(my_model) <- list(S0=0.6, E0=0.2, I0=0, R0=0))
})

test_that("Transmission parameters can be set and retrieved", {
  my_model <- SEIRD()
  transmission_parameters(my_model) <- list(beta=1, kappa=0.5, gamma=0.5, mu=0.1)

  # Test output is correct
  expect_equal(transmission_parameters(my_model),
               list(beta = 1, kappa = 0.5, gamma = 0.5, mu = 0.1))

  # Check error is raised when transmission parameters are not 1-dimensional
  expect_error(
    transmission_parameters(my_model) <- list(beta=1, kappa=0.5, gamma=list(0.1, 0.1), mu=0.1))
  expect_error(
    transmission_parameters(my_model) <- list(beta=1, kappa=list(0.1, 0.1), gamma=0.5, mu=0.1))
})

test_that("SEIR model runs correctly", {
  my_model <- SEIRD()
  initial_conditions(my_model) <- list(S0=0.9, E0=0, I0=0.1, R0=0)
  transmission_parameters(my_model) <- list(beta=0, kappa=0, gamma=0, mu=0)

  # Check output shape
  t <- seq(0, 10, by = 0.1)
  out_df <- run(my_model, t)
  expect_identical(dim(out_df$changes), as.integer(c(((10 - 0) / 0.1 + 1) * 2, 4)))
  expect_identical(dim(out_df$state), as.integer(c(((10 - 0) / 0.1 + 1) * 5, 4)))

  # Check output value for rates equal 0s
  expected_data <- data.frame(
    S = 0.9, E = 0, I = 0.1, R = 0, Incidence = 0, Deaths = 0)
  out_df_SEIR <- dcast(out_df$states, time ~ compartment, value.var = "value")
  out_df_CD <- dcast(out_df$changes, time ~ compartment, value.var = "value")
  test_data_SEIR <- out_df_SEIR[101, 2:5]
  test_data_CD <- out_df_CD[101,2:3]
  row.names(test_data_SEIR) <- NULL
  row.names(test_data_CD) <- NULL
  expect_identical(test_data_SEIR, expected_data[1:4])
  expect_identical(test_data_CD, expected_data[5:6])

  # Check that sum of states is sufficiently close to one at all times
  transmission_parameters(my_model) <- list(beta=0.9, kappa=0.2, gamma=0.01, mu=0.1)
  out_df <- run(my_model, seq(0, 10, by = 0.1))
  out_df_SEIR <- dcast(out_df$states, time ~ compartment, value.var = "value")
  out_df_CD <- dcast(out_df$changes, time ~ compartment, value.var = "value")
  out_df_CD$Deaths <- cumsum(out_df_CD$Deaths)
  test <- rowSums(out_df_SEIR[, c(2:5)]) + out_df_CD$Deaths
  expected <- as.double(rep(1, 101))
  expect_equal(test, expected)

  # Test input errors
  expect_error(run(my_model, "a"))
})

test_that("Running model before setting parameters fails", {
  t <- seq(0, 10, by = 0.1)
  my_model <- SEIRD()
  initial_conditions(my_model) <- list(S0=0.9, E0=0, I0=0.1, R0=0)
  expect_error(run(my_model, t), "Transmission parameters must be set before running.")
  my_model <- SEIRD()
  transmission_parameters(my_model) <- list(beta=0.9, kappa=0.2, gamma=0.01, mu=0.1)
  expect_error(run(my_model, t), "Initial conditions must be set before running.")
})

test_that("R0 works for SEIRD model", {
  my_model <- SEIRD()
  initial_conditions(my_model) <- list(S0=0.9, E0=0, I0=0.1, R0=0)
  beta <- 1.1
  gamma <- 0.4
  transmission_parameters(my_model) <- list(beta=beta, kappa=0.1,
                                            gamma=gamma, mu=0.3)
  expect_equal(R0(my_model), beta/gamma)
  
  beta <- 1.4
  gamma <- 0.8
  transmission_parameters(my_model) <- list(beta=beta, kappa=0.1,
                                            gamma=gamma, mu=0.3)
  expect_equal(R0(my_model), beta/gamma)
})
