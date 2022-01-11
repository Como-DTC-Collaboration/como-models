test_that("SEIRD_BD model is instantiated correctly", {
  my_model <- SEIRD_BD()

  expect_length(my_model@initial_conditions, 4)
  expect_length(my_model@transmission_parameters, 7)
})

test_that("Initial conditions can be set and retrieved", {
  my_model <- SEIRD_BD()
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
  my_model <- SEIRD_BD()
  params <- list(beta=1, kappa=0.5, gamma=0.5, mu=0.1,
                 nu=0.1, delta=0.4, lambda=2)
  transmission_parameters(my_model) <- params

  # Test output is correct
  expect_equal(transmission_parameters(my_model),
               params)

  # Check error is raised when transmission parameters are not 1-dimensional
  expect_error(
    transmission_parameters(my_model) <- list(beta=1, kappa=0.5, gamma=list(0.1, 0.1), mu=0.1, nu=0.1, delta=0.4, lambda=2))
  expect_error(
    transmission_parameters(my_model) <- list(beta=1, kappa=list(0.1, 0.1), gamma=0.5, mu=0.1, nu=0.1, delta=0.4, lambda=2))
})

test_that("SEIR model runs correctly", {
  my_model <- SEIRD_BD()
  initial_conditions(my_model) <- list(S0=0.9, E0=0, I0=0.1, R0=0)
  transmission_parameters(my_model) <- list(beta=0, kappa=0, gamma=0, mu=0, nu=0.1, delta=0.4, lambda=2)

  # Check output shape
  t <- seq(0, 10, by = 0.1)
  out_df <- run(my_model, t)
  expect_identical(dim(out_df$changes), as.integer(c(((10 - 0) / 0.1 + 1) * 2, 4)))
  expect_identical(dim(out_df$state), as.integer(c(((10 - 0) / 0.1 + 1) * 5, 4)))

  # Test input errors
  expect_error(run(my_model, "a"))
})

test_that("Running model before setting parameters fails", {
  t <- seq(0, 10, by = 0.1)
  my_model <- SEIRD_BD()
  initial_conditions(my_model) <- list(S0=0.9, E0=0, I0=0.1, R0=0)
  expect_error(run(my_model, t), "Transmission parameters must be set before running.")
  my_model <- SEIRD_BD()
  transmission_parameters(my_model) <- list(beta=0.9, kappa=0.2, gamma=0.01, mu=0.1, nu=0.1, delta=0.4, lambda=2)
  expect_error(run(my_model, t), "Initial conditions must be set before running.")
})

test_that("R0 works for SEIRD_BD model", {
  my_model <- SEIRD_BD()
  initial_conditions(my_model) <- list(S0=0.9, E0=0, I0=0.1, R0=0)
  beta <- 1.1
  gamma <- 0.4
  mu <- 0.2
  kappa <- 0.3
  nu <- 0.3
  lambda <- 1
  delta <- 0
  transmission_parameters(my_model) <- list(beta=beta, kappa=kappa,
                                            gamma=gamma, mu=mu, nu=nu,
                                            lambda=lambda, delta=delta)
  expect_equal(R0(my_model), beta * kappa / (kappa + nu) * 1 / (gamma + mu + nu))
  
  beta <- 1.4
  gamma <- 0.8
  mu <- 0.1
  kappa <- 0.5
  transmission_parameters(my_model) <- list(beta=beta, kappa=kappa,
                                            gamma=gamma, mu=mu, nu=nu,
                                            lambda=lambda, delta=delta)
  expect_equal(R0(my_model), beta * kappa / (kappa + nu) * 1 / (gamma + mu + nu))
})

test_that("ode_model_structure works", {
  model <- SEIRD_BD()
  g <- ode_structure_diagram(model)
  expect_s3_class(g, "html")
})
