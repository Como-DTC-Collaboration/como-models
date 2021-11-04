library(tidyverse)

test_that("SEIRDV model is instantiated correctly", {
  my_model <- SEIRDV()
  
  expect_length(my_model@initial_conditions, 5)
  expect_length(my_model@transmission_parameters, 7)
  expect_length(my_model@intervention_parameters, 3)
})

test_that("Initial conditions can be set and retrieved", {
  my_model <- SEIRDV()
  initial_conditions(my_model) <- list(S0=0.9, E0=0, I0=0.1, R0=0, V0=0)
  
  # Test output is correct
  expect_equal(initial_conditions(my_model),
               list("S0" = 0.9, "E0" = 0, "I0" = 0.1, "R0" = 0, "V0" = 0))
  
  # Check error is raised when initial states are not doubles
  expect_error(
    initial_conditions(my_model) <- list(
      S0=0.6, E0=0.2, I0=list(0.1, 0.1), R0=0, V0=0))
  expect_error(
    initial_conditions(my_model) <- list(
      S0="a", E0=0.9, I0=0.1, R0=0, V0=0))
  
  # Check error is raised when sum of initial conditions is not 1
  expect_error(initial_conditions(my_model) <- list(
    S0=0.6, E0=0.2, I0=0, R0=0, V0=0))
})

test_that("Transmission parameters can be set and retrieved", {
  my_model <- SEIRDV()
  transmission_parameters(my_model) <- list(
    beta=1, kappa=0.5, gamma=0.5, mu=0.1, nu = 0.1, delta_V = 0.1, delta_R = 0.1)
  
  # Test output is correct
  expect_equal(transmission_parameters(my_model),
               list(beta = 1, kappa = 0.5, gamma = 0.5, mu = 0.1, nu = 0.1,
                    delta_V = 0.1, delta_R = 0.1))
  
  # Check error is raised when transmission parameters are not 1-dimensional
  expect_error(
    transmission_parameters(my_model) <- list(
      beta=1, kappa=0.5, gamma=list(0.1, 0.1), mu=0.1,
      nu = 0.1, delta_V = 0.1, delta_R = 0.1))
  expect_error(
    transmission_parameters(my_model) <- list(
      beta=1, kappa=list(0.1, 0.1), gamma=0.5, mu=0.1,
      nu = 0.1, delta_V = 0.1, delta_R = 0.1))
  expect_error(
    transmission_parameters(my_model) <- list(
      beta=1, kappa=0.5, gamma=0.5, mu=0.1,
      nu = c(0.1, 0.2), delta_V = 0.1, delta_R = 0.1))
  expect_error(
    transmission_parameters(my_model) <- list(
      beta=1, kappa=0.5, gamma=0.5, mu=0.1,
      nu = 0.1, delta_V = c(0.1, 0), delta_R = 0.1))
  expect_error(
    transmission_parameters(my_model) <- list(
      beta=1, kappa=0.5, gamma=0.5, mu=0.1,
      nu = 0.1, delta_V = 0.1, delta_R = c(0.1, 0.2)))
})

test_that("Intervention parameters can be set and retrieved", {
  my_model <- SEIRDV()
  intervention_parameters(my_model) <- list(
    starts=10, stops=20, coverages=0.5)
  
  # Test output is correct
  expect_equal(intervention_parameters(my_model),
               list(starts=10, stops=20, coverages=0.5))
  
  # Check error is raised when transmission parameters are not 1-dimensional
  expect_error(
    intervention_parameters(my_model) <- list(
      starts=c(10, 15), stops=c(20), coverages=c(0.5)))
  expect_error(
    intervention_parameters(my_model) <- list(
      starts=c(5), stops=c(10, 20), coverages=c(0.5)))
  expect_error(
    intervention_parameters(my_model) <- list(
      starts=c(10), stops=c(20), coverages=c(0.5, 1)))
})

test_that("SEIRDV model runs correctly", {
  my_model <- SEIRDV()
  initial_conditions(my_model) <- list(S0=0.9, E0=0, I0=0.1, R0=0, V0=0)
  transmission_parameters(my_model) <- list(beta=0, kappa=0, gamma=0, mu=0,
                                            nu = 0, delta_V = 0, delta_R = 0)
  intervention_parameters(my_model) <- list(starts=0, stops=10, coverages=0.5)
  
  # Check output shape
  t <- seq(0, 10, by = 0.1)
  out_df <- run(my_model, t)
  expect_identical(dim(out_df$changes), as.integer(c(((10 - 0) / 0.1 + 1) * 2, 4)))
  expect_identical(dim(out_df$state), as.integer(c(((10 - 0) / 0.1 + 1) * 6, 4)))
  
  # Check output value for rates equal 0s
  expected_data <- data.frame(
    S = 0.9, E = 0, I = 0.1, R = 0, V = 0, Incidence = 0, Deaths = 0)
  out_df_SEIRV <- dcast(out_df$states, time ~ compartment, value.var = "value")
  out_df_CD <- dcast(out_df$changes, time ~ compartment, value.var = "value")
  test_data_SEIRV <- out_df_SEIRV[101, 2:6]
  test_data_CD <- out_df_CD[101,2:3]
  row.names(test_data_SEIRV) <- NULL
  row.names(test_data_CD) <- NULL
  expect_identical(test_data_SEIRV, expected_data[1:5])
  expect_identical(test_data_CD, expected_data[6:7])
  
  # Check that sum of states is sufficiently close to one at all times
  transmission_parameters(my_model) <- list(beta=0.9, kappa=0.2, gamma=0.01, mu=0.1,
                                            nu = 0, delta_V = 0, delta_R = 0)
  out_df <- run(my_model, seq(0, 10, by = 0.1))
  out_df_SEIRV <- dcast(out_df$states, time ~ compartment, value.var = "value")
  out_df_CD <- dcast(out_df$changes, time ~ compartment, value.var = "value")
  out_df_CD$Deaths <- cumsum(out_df_CD$Deaths)
  test <- rowSums(out_df_SEIRV[, c(2:6)]) + out_df_CD$Deaths
  expected <- as.double(rep(1, 101))
  expect_equal(test, expected)
  
  # Test input errors
  expect_error(run(my_model, "a"))
  
  # Test errors when intervention parameters do not make sense
  intervention_parameters(my_model) <- list(
    starts=c(10, 5), stops=c(20, 25), coverages=c(0.5, 1))
  expect_error(
    run(my_model, t), "Each subsequent intervention must start after the last ends.")
  
  intervention_parameters(my_model) <- list(
    starts=c(10, 15), stops=c(5, 20), coverages=c(0.5, 1))
  expect_error(
    run(my_model, t), "Intervention must start before it ends.")
})

test_that("Running model before setting parameters fails", {
  t <- seq(0, 10, by = 0.1)
  my_model <- SEIRDV()
  initial_conditions(my_model) <- list(S0=0.9, E0=0, I0=0.1, R0=0, V0=0)
  intervention_parameters(my_model) <- list(starts=0, stops=10, coverages=0.5)
  expect_error(run(my_model, t), "Transmission parameters must be set before running.")
  
  my_model <- SEIRDV()
  transmission_parameters(my_model) <- list(beta=0.9, kappa=0.2, gamma=0.01, mu=0.1,
                                            nu = 0.1, delta_V = 0.1, delta_R = 0.1)
  intervention_parameters(my_model) <- list(starts=0, stops=10, coverages=0.5)
  expect_error(run(my_model, t), "Initial conditions must be set before running.")
  
  my_model <- SEIRDV()
  initial_conditions(my_model) <- list(S0=0.9, E0=0, I0=0.1, R0=0, V0=0)
  transmission_parameters(my_model) <- list(beta=0.9, kappa=0.2, gamma=0.01, mu=0.1,
                                            nu = 0.1, delta_V = 0.1, delta_R = 0.1)
  expect_error(run(my_model, t), "Intervention parameters must be set before running.")
})
