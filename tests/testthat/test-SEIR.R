test_that("SEIR model is instantiated correctly", {
  my_model <- new("SEIR_model")
  
  expect_length(my_model@initial_conditions, 4)
  expect_length(my_model@transmission_parameters, 3)
})

test_that("Initial conditions can be set and retrieved", {
  my_model <- new("SEIR_model")
  my_model <- `initial_conditions<-`(my_model, 0.9, 0, 0.1, 0)
  
  # Test output is correct
  expect_equal(initial_conditions(my_model), 
               list("S0"=0.9, "E0"=0, "I0"=0.1, "R0"=0))
  
  # Check error is raised when initial states are not doubles
  expect_error({
    my_model <- `initial_conditions<-`(my_model, 0.6, 0.2, list(0.1, 0.1), 0)
    my_model <- `initial_conditions<-`(my_model, "a", 0.9, 0.1, 0)
  })
  
  # Check error is raised when sum of initial conditions is not 1
  expect_error( my_model <- `initial_conditions<-`(my_model, 0.6, 0.2, 0, 0))
})

test_that("Transmission parameters can be set and retrieved", {
  my_model <- new("SEIR_model")
  my_model <- `transmission_parameters<-`(my_model, 1, 0.5, 0.5)
  
  # Test output is correct
  expect_equal(transmission_parameters(my_model),
               list("b"=1, "k"=0.5, "g"=0.5))
  
  # Check error is raised when transmission parameters are not 1-dimensional
  expect_error({
    my_model <- `transmission_parameters<-`(my_model, 1, 0.5, list(0.1, 0.1))
    my_model <- `transmission_parameters<-`(my_model, 1, list(0.1, 0.1), 0.5)
  })
})

test_that("SEIR model runs correctly", {
  my_model <- new("SEIR_model")
  my_model <- `initial_conditions<-`(my_model, 0.9, 0, 0.1, 0)
  my_model <- `transmission_parameters<-`(my_model, 0, 0, 0)

  # Check output shape
  out_df <- simulate_SEIR(my_model, seq(0, 10, by = 0.1))
  expect_identical(dim(out_df), as.integer(c((10 - 0) / 0.1 + 1, 6)))

  # Check output value for rates equal 0s
  expected_data <- data.frame(S = 0.9, E = 0, I = 0.1, R = 0, Incidence = 0)
  test_data <- out_df[101, 2:6]
  row.names(test_data) <- NULL
  expect_identical(test_data, expected_data)

  # Check that sum of states is sufficiently close to one at all times
  my_model <- `transmission_parameters<-`(my_model, 0.9, 0.2, 0.01)
  out_df <- simulate_SEIR(my_model, seq(0, 10, by = 0.1))
  test <- rowSums(out_df[, 2:5])
  expected <- as.double(rep(1, 101))
  expect_equal(test, expected)
  
  # Test input errors
  expect_error(simulate_SEIR(my_model, "a"))
})
