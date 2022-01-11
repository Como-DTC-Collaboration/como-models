test_that("SEmIRD model is instantiated correctly", {
  my_model <- SEmIRD()
  
  expect_length(my_model@initial_conditions, 4)
  expect_length(my_model@transmission_parameters, 4)
})

test_that("Initial conditions can be set and retrieved", {
  my_model <- SEmIRD()
  initial_conditions(my_model) <- list(S0=0.9, E0=as.list(rep(0, 5)), I0=0.1, R0=0)
  
  # Test output is correct
  expect_equal(initial_conditions(my_model),
               list("S0" = 0.9, "E0"= as.list(rep(0, 5)), "I0" = 0.1, "R0" = 0))
  
  # Check error is raised when initial states are not doubles
  expect_error(
    initial_conditions(my_model) <- list(S0=0.6, E0=as.list(c(0.2, rep(0, 4))), I0=list(0.1, 0.1), R0=0))
  expect_error(
    initial_conditions(my_model) <- list(S0="a", E0=0.9, I0=0.1, R0=0))
  
  # Check error is raised when sum of initial conditions is not 1
  expect_error(initial_conditions(my_model) <- list(S0=0.6, as.list(c(0.2, rep(0, 4))), I0=0, R0=0))
})

test_that("Transmission parameters can be set and retrieved", {
  my_model <- SEmIRD()
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

test_that("SEmIRD model runs correctly", {
  my_model <- SEmIRD()
  initial_conditions(my_model) <- list(S0=0.9, E0=as.list(rep(0, 5)), I0=0.1, R0=0)
  transmission_parameters(my_model) <- list(beta=1, kappa=1, gamma=1, mu=1)
  
  # Check output shape
  t <- seq(0, 10, by = 0.1)
  out_df <- run(my_model, t)
  expect_identical(dim(out_df$changes), as.integer(c(((10 - 0) / 0.1 + 1) * 2, 4)))
  expect_identical(dim(out_df$state), as.integer(c(((10 - 0) / 0.1 + 1) * 9, 4)))
  
  # Check that sum of states is sufficiently close to one at all times
  transmission_parameters(my_model) <- list(beta=0.9, kappa=0.2, gamma=0.01, mu=0.1)
  out_df <- run(my_model, seq(0, 10, by = 0.1))
  states <- out_df$states %>% 
    dplyr::group_by(time) %>% 
    dplyr::summarise(value=sum(value))
  expect_equal(1, mean(states$value))
  
  # Test input errors
  expect_error(run(my_model, "a"))
})

test_that("Running model before setting parameters fails", {
  t <- seq(0, 10, by = 0.1)
  my_model <- SEmIRD()
  initial_conditions(my_model) <- list(S0=0.9, E0=as.list(rep(0, 5)), I0=0.1, R0=0)
  expect_error(run(my_model, t), "Transmission parameters must be set before running.")
  my_model <- SEmIRD()
  transmission_parameters(my_model) <- list(beta=0.9, kappa=0.2, gamma=0.01, mu=0.1)
  expect_error(run(my_model, t), "Initial conditions must be set before running.")
})

test_that("R0 works for SEmIRD model", {
  my_model <- SEmIRD()
  initial_conditions(my_model) <- list(S0=0.9, E0=as.list(rep(0, 5)), I0=0.1, R0=0)
  beta <- 1.1
  gamma <- 0.4
  mu <- 0.2
  transmission_parameters(my_model) <- list(beta=beta, kappa=0.1,
                                            gamma=gamma, mu=mu)
  expect_equal(R0(my_model), beta/(gamma+mu))
  
  beta <- 1.4
  gamma <- 0.8
  mu <- 0.1
  transmission_parameters(my_model) <- list(beta=beta, kappa=0.1,
                                            gamma=gamma, mu=mu)
  expect_equal(R0(my_model), beta/(gamma+mu))
})

test_that("ode_model_structure works", {
  model <- SEmIRD()
  g <- ode_structure_diagram(model)
  expect_s3_class(g, "html")
})

