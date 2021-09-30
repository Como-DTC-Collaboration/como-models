test_that("SEIRD model is instantiated correctly", {
  my_model <- SEIRD_nonpharma()
  
  expect_length(my_model@initial_conditions, 7)
  expect_length(my_model@transmission_parameters, 8)

  expect_equal(my_model@output_names,
               list('S', 'E', 'I', 'I_isolated', 'R', 'D', 'Incidence', 'Deaths'))

  expect_equal(my_model@initial_condition_names,
               list('S0', 'E0', 'I0', 'I0_isolated', 'R0'))

  expect_equal(my_model@transmission_parameter_names,
               list('beta', 'beta_isolated', 'kappa', 'gamma', 'mu', 'heta1', 'heta2'))

})

test_that("can retrieve SEIRD_nonpharma initial conditions", {
  my_model <- SEIRD_nonpharma()

  expect_equal(initial_conditions(my_model),
               vector(mode = "list", length = 7))
})

test_that("can retrieve SEIRD_nonpharma transmission parameters", {
  my_model <- SEIRD_nonpharma()

  expect_equal(transmission_parameters(my_model),
               vector(mode = "list", length = 8))
})

test_that("can set new initial conditions for the SEIRD_nonpharma", {
  my_model <- SEIRD_nonpharma()
  initial_conditions(my_model)<-list(S0=0.73,
                                     E0=0.21,
                                     I0=0.05,
                                     I0_isolated=0.01,
                                     R0=0)

  # Test output is correct
  expected_ics <- list('S0'=0.73,
                       'E0'=0.21,
                       'I0'=0.05,
                       'I0_isolated'=0.01,
                       'R0'=0)

  expect_equal(initial_conditions(my_model),
               expected_ics)

  # Test input errors
  expect_error({
    initial_conditions(my_model) <- list(S0=c(0.73),
                                         E0=c(0.21, 0),
                                         I0=c(0.05),
                                         I0_isolated=c(0.01),
                                         R0=c(0))
  })

  

    expect_error({
    initial_conditions(my_model) <- list(S0=c(0.73),
                                         E0=c(0.21),
                                         I0=c('0.05'),
                                         R0=c(0.01),
                                         D0 = c(0))
    })

    expect_error(
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.5, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    
    )
})

test_that("can set new transmission parameters for the SEIRD_nonpharma", {
  my_model <- SEIRD_nonpharma()
  transmission_parameters(my_model) <- list(beta=1, beta_isolated=1, kappa=0.5, gamma=0.5, mu = 0.01, heta1=0.8, heta2=0.3)

  # Test output is correct
  expected_transpar <- list('beta'=1,
                            'beta_isolated'=1,
                            'kappa'=0.5,
                            'gamma'=0.5,
                            'mu'= 0.01,
                            'heta1'=0.8,
                            'heta2'=0.3)

  expect_equal(transmission_parameters(my_model),
               expected_transpar)

  # Test input errors
  expect_error(
    transmission_parameters(my_model) <- list(beta=c(1, 0), kappa=0.5, gamma=0.5, mu = 0.01, heta1=0.8, heta2=0.3)
  )

})

test_that("can run simulation for the SEIRD_nonpharma", {
  my_model <- SEIRD_nonpharma()
  
  initial_conditions(my_model) <- list(S0=c(1),
                                       E0=c(0),
                                       I0=c(0),
                                       I0_isolated=c(0),
                                       R0=c(0))
  
  transmission_parameters(my_model) <- list(beta=1, beta_isolated=1, kappa=0.5, gamma=0.5, mu = 0.01, heta1=0.6, heta2=0.1)

 
  # Test input errors
  expect_error(run(my_model, "a"))
  
  
  times <- seq(0, 180, by = 1)
  t_intervention_1_2 <- 10
  t_intervention_2_3 <- 70
  
  output <- run(my_model, times, t_intervention_1_2, t_intervention_2_3)
  
  
  #Test whether names of outputs are as expected
  list_names <- names(output)
  expected_names <- c("states", "changes")
  expect_true(all.equal(list_names[1:2], expected_names))
  
  
  #Test whether names of states are as expected
  states <- output$states
  compartments <- unique(states$compartment)
  #compartments above is given as a factor, so convert to strings
  compartments_char <-as.character(compartments)
  
  expected_compartments <- c("S", "E", "I", "I_isolated", "R", "D")
  expect_true(all.equal(compartments_char, expected_compartments))
  
  #Test whether output times are as expected
  times <- unique(states$time)
  expected_times <- seq(0, 180, by = 1)
  #compartments above is given as a factor, so convert to strings
  expect_true(all.equal(times, expected_times))
  
})

