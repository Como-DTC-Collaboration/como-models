test_that("SEIRAge gets instantiated", {
  my_model <- new('SEIRAge', name = 'my_model', n_age_categories = 2)

  # Test output is correct
  expect_equal(my_model@name,
               'my_model')

  expect_equal(my_model@output_names,
               list('S', 'E', 'I', 'R', 'Incidence'))

  expect_equal(my_model@initial_condition_names,
               list('S0', 'E0', 'I0', 'R0'))
  
  expect_equal(my_model@transmission_parameter_names,
               list('b', 'k', 'g'))
  
  expect_length(my_model@initial_conditions, 4)
  
  expect_length(my_model@transmission_parameters, 3)

  expect_equal(my_model@n_age_categories, 2)
})

test_that("can retrieve SEIRAge initial conditions", {
  my_model <- new('SEIRAge', name = 'my_model', n_age_categories = 2)
  
  expect_equal(initial_conditions(my_model),
               vector(mode = "list", length = 4))
})

test_that("can retrieve SEIRAge transmission parameters", {
  my_model <- new('SEIRAge', name = 'my_model', n_age_categories = 2)

  expect_equal(transmission_parameters(my_model),
               vector(mode = "list", length = 3))
})

test_that("can set new initial conditions for the SEIRAge", {
  my_model <- new('SEIRAge', name = 'my_model', n_age_categories = 2)
  my_model <- `initial_conditions<-`(my_model, c(0.4, 0.4), c(0, 0),
                                   c(0.05, 0.15), c(0, 0))

  # Test output is correct
  expected_ics <- list('S0'=c(0.4, 0.4),
                       'E0'=c(0, 0),
                       'I0'=c(0.05, 0.15),
                       'R0'=c(0, 0))

  expect_equal(initial_conditions(my_model),
               expected_ics)

  # Test input errors
  expect_error({
    my_model <- `initial_conditions<-`(my_model, c(0.4, 0, 0.4), c(0, 0),
                                       c(0.5, 0.15), c(0, 0))
    my_model <- `initial_conditions<-`(my_model, c(0.4, 0, 0.4), c(0, 0),
                                       c(0.05, 0.15), c(0, 0))
    my_model <- `initial_conditions<-`(my_model, c(0.4, 0.4), c(0, 0, 0),
                                       c(0.05, 0.15), c(0, 0))
    my_model <- `initial_conditions<-`(my_model, c(0.4, 0.4), c(0, 0),
                                       c(0.05, 0, 0.15), c(0, 0))
    my_model <- `initial_conditions<-`(my_model, c(0.4, 0.4), c(0, 0),
                                       c(0.05, 0.15), c(0, 0, 0))
    my_model <- `initial_conditions<-`(my_model, 0, c(0, 0), c(0.05, 0.15),
                                       c(0, 0))
    my_model <- `initial_conditions<-`(my_model, c(0.4, 0.4), 0, c(0.05, 0.15),
                                       c(0, 0))
    my_model <- `initial_conditions<-`(my_model, c(0.4, 0.4), c(0, 0), 0,
                                       c(0, 0))
    my_model <- `initial_conditions<-`(my_model, c(0.4, 0.4), c(0, 0),
                                       c(0.05, 0.15), 0)
  })
})

test_that("can set new transmission parameters for the SEIRAge", {
  my_model <- new('SEIRAge', name = 'my_model', n_age_categories = 2)
  my_model <- `transmission_parameters<-`(my_model, 1, 0.5, 0.5)
  
  # Test output is correct
  expected_transpar <- list('b'=1,
                            'k'=0.5,
                            'g'=0.5)
  
  expect_equal(transmission_parameters(my_model),
               expected_transpar)
  
  # Test input errors
  expect_error({
    my_model <- `transmission_parameters<-`(my_model, c(1, 0), 0.5, 0.5)
    
    my_model <- `transmission_parameters<-`(my_model, 1, c(1, 0), 0.5)
    
    my_model <- `transmission_parameters<-`(my_model, 1, 0.5, c(1, 0))
  })
})

test_that("can run simulation for the SEIRAge", {
  my_model <- new('SEIRAge', name = 'my_model', n_age_categories = 2)
  my_model <- `initial_conditions<-`(my_model, c(0.6, 0.4), c(0, 0), c(0, 0),
                                     c(0, 0))
  
  my_model <- `transmission_parameters<-`(my_model, 1, 0.5, 0.5)

  expected_output <- data.frame(time=0:2,
                                S1=rep(0.6,3),
                                S2=rep(0.4,3),
                                E1=rep(0,3),
                                E2=rep(0,3),
                                I1=rep(0,3),
                                I2=rep(0,3),
                                R1=rep(0,3),
                                R2=rep(0,3))
  expected_output$Incidence <- cbind(rep(0,3), rep(0,3))

  # Test output is correct
  expect_equal(simulate_SEIRAge(my_model, seq(0, 2, by = 1)),
               expected_output)

  # Test input errors
  expect_error({
    simulate_SEIRAge(my_model, '0')
    simulate_SEIRAge(my_model, seq(0, 2, by = 1))})

  # Snapshot testing to check that outputs to the command line as expected
  expect_snapshot_output(simulate_SEIRAge(my_model, seq(0, 2, by = 1)))

})
