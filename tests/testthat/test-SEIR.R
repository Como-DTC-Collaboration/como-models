test_that("SEIR model is set up correctly", {
  my_model <- new("SEIR_model", name = "my_model",
                  parameter_names = list("S0", "E0", "I0", "R0",
                                         "beta", "kappa", "gamma"))
  my_model <- set_parameters(my_model, 0.9, 0, 0.1, 0, 0.1, 0.2, 0.3)
  test_data <- get_parameters(my_model)
  expect_identical(test_data$kappa[0], numeric(0.2))
})

test_that("Errors are raised properly", {
  my_model <- new("SEIR_model", name = "my_model")

  # Check error is raised when initial states are not doubles
  expect_error(set_parameters(my_model, "a", 0, 0.1, 0, 0.1, 0.2, 0.3),
               "S0 must be of type double.")

  # Check error is raised when rates of changes are not 1-dimensional
  expect_error(set_parameters(my_model, 0.9, 0, 0.1, 0,
                              list(0.1, 0.1), 0.2, 0.3),
               "The rates of change between compartments are 1-dimensional.")

  # Check error is raised when sum of initial conditions is not 1
  expect_error(set_parameters(my_model, 0.9, 0, 0.1, 0.5, 0.1, 0.2, 0.3),
               "Initial conditions do not add up to 1.")
})

test_that("SEIR model runs correctly", {
  my_model <- new("SEIR_model", name = "my_model")

  # Check output shape
  my_model <- set_parameters(my_model, 0.9, 0, 0.1, 0, 0, 0, 0)
  out_df <- simulate(my_model, seq(0, 10, by = 0.1))
  expect_identical(dim(out_df), as.integer(c((10 - 0) / 0.1 + 1, 5)))

  # Check output value of rates equal 0s
  expected_data <- data.frame(S = 0.9, E = 0, I = 0.1, R = 0)
  test_data <- out_df[101, 2:5]
  row.names(test_data) <- NULL
  expect_identical(test_data, expected_data)

  # Check that sum of states is sufficiently close to one at all times
  my_model <- set_parameters(my_model, 0.9, 0, 0.1, 0, 0.9, 0.2, 0.01)
  out_df <- simulate(my_model, seq(0, 10, by = 0.1))
  test <- rowSums(out_df[, 2:5])
  expected <- as.double(rep(1, 101))
  expect_equal(test, expected)
})
