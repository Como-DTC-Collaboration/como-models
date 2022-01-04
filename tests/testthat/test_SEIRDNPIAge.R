test_that("SEIRDNPIAge gets instantiated", {
  my_model <- SEIRDNPIAge(n_age_categories = 2,
                       age_ranges = list('0-50', '50-100'))
  
  # Test output is correct
  
  expect_equal(my_model@age_ranges,
               list('0-50', '50-100'))
  
  expect_equal(my_model@output_names,
               list('S', 'E', 'I', 'R', 'D', 'Incidence'))
  
  expect_equal(my_model@initial_condition_names,
               list('S0', 'E0', 'I0', 'R0', 'D0'))
  
  expect_equal(my_model@transmission_parameter_names,
               list('beta_npi', 'beta', 'kappa', 'gamma', 'mu'))
  
  expect_length(my_model@initial_conditions, length(my_model@initial_condition_names))

  expect_length(my_model@transmission_parameters, length(my_model@transmission_parameter_names))
  
  expect_length(my_model@age_ranges, my_model@n_age_categories)
  
  expect_equal(my_model@n_age_categories, 2)
})


test_that("Transmission parameters can be set and retrieved", {
  my_model <- SEIRDNPIAge(n_age_categories = 2)
  
  # Single gamma & mu
  transmission_parameters(my_model) <- list(beta_npi = 0.5,
                                            beta = 1,
                                            kappa = 0.5,
                                            gamma = 0.5,
                                            mu = 0)
  
  
  expected_ics <- list(beta_npi = 0.5,
                       beta = 1,
                       kappa = 0.5,
                       gamma = 0.5,
                       mu = 0)

  expect_equal(transmission_parameters(my_model),
               expected_ics)
  
  # Multiple gamma & mu vals
  transmission_parameters(my_model) <- list(beta_npi = 0.5,
                                            beta = 1,
                                            kappa = 0.5,
                                            gamma = c(0.8, 0.5),
                                            mu = c(0.01, 0.1))
  
  
  expected_ics <- list(beta_npi = 0.5,
                       beta = 1,
                       kappa = 0.5,
                       gamma = c(0.8, 0.5),
                       mu = c(0.01, 0.1))
  
  expect_equal(transmission_parameters(my_model),
               expected_ics)

  # Test output length equal to the length of names
  expect_equal(length(transmission_parameters(my_model)), 
               length(my_model@transmission_parameter_names)
  )
  # Test output names are correct
  expect_equal(sort(names(transmission_parameters(my_model))), 
               sort(unlist(my_model@transmission_parameter_names))
  )
  # expect_equal(transmission_parameters(my_model),
  #              vector(mode = "list", length = length(my_model@transmission_parameter_names)))

  # Test input errors
  ## missing params (mu)
  expect_error(
    transmission_parameters(my_model) <- list(beta_npi = 0.5,
                                              beta = 1,
                                              kappa = 0.5,
                                              gamma = 0.5)
  )
  ## bad length (beta)
  expect_error(
    transmission_parameters(my_model) <- list(beta_npi = 0.5,
                                              beta = c(1, 0.5),
                                              kappa = 0.5,
                                              gamma = 0.5, 
                                              mu = 0)
  )
  ## bad length (beta_npi)
  expect_error(
    transmission_parameters(my_model) <- list(beta_npi = c(1, 0.5),
                                              beta = 1,
                                              kappa = 0.5,
                                              gamma = 0.5, 
                                              mu = 0)
  )
  ## bad length (kappa)
  expect_error(
    transmission_parameters(my_model) <- list(beta_npi = 0.5,
                                              beta = 1,
                                              kappa = c(1, 0.5),
                                              gamma = 0.5, 
                                              mu = 0)
  )

})

test_that("Initial conditions for SEIRDNPIAge can be set and retrieved", {
  my_model <- SEIRDNPIAge(n_age_categories = 2)
  initial_conditions(my_model)<-list(S0=c(0.4, 0.4),
                                     E0=c(0, 0),
                                     I0=c(0.05, 0.15),
                                     R0=c(0, 0), 
                                     D0 = c(0, 0))
  
  # Test output is correct
  expected_ics <- list('S0'=c(0.4, 0.4),
                       'E0'=c(0, 0),
                       'I0'=c(0.05, 0.15),
                       'R0'=c(0, 0),
                       'D0' = c(0, 0))
  
  expect_equal(initial_conditions(my_model),
               expected_ics)
  
  # Test input errors
  expect_error(
    initial_conditions(my_model) <- list(S0=c(0.4, 0, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.5, 0.15),
                                         R0=c(0, 0),                                   
                                         D0 = c(0, 0))
  )
  expect_error(
    initial_conditions(my_model) <- list(S0=c(0.4, 0, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
  )
  expect_error(
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    
  )
  expect_error( 
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    
  )
  expect_error(
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0, 0),
                                         D0 = c(0, 0))
    
  )
  expect_error( 
    initial_conditions(my_model) <- list(S0=0,
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    
  )
  expect_error( 
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=0,
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    
  )
  expect_error( 
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=0,
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    
  )
  expect_error( 
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=0,
                                         D0 = c(0, 0))
    
  )
})

test_that("R0 can be calculated correctly", {
  my_model <- SEIRDNPIAge(n_age_categories = 2,
                          n_npi = 2,
                          age_ranges = list('0-50', '50-100'), 
                          contact_matrix = matrix(c(1,0,0,1), nrow = 2),
                          contact_matrix_npi = list(matrix(c(1,0,0,1), nrow = 2),
                                                    matrix(c(1,0,0,1), nrow = 2))
                          )
  
  initial_conditions(my_model)<-list(S0=c(0.4, 0.4),
                                     E0=c(0, 0),
                                     I0=c(0.05, 0.15),
                                     R0=c(0, 0), 
                                     D0 = c(0, 0))
  
  transmission_parameters(my_model) <- list(beta_npi = 0.5,
                                            beta = 1,
                                            kappa = 0.5,
                                            gamma = c(0.8, 0.5),
                                            mu = c(0.01, 0.1))
  
  # check R0 is calculated correctly
  expect_equal(round(R0(my_model), 2), 0.92)
  
  transmission_parameters(my_model) <- list(beta_npi = 0.5,
                                            beta = 1,
                                            kappa = 0.5,
                                            gamma = 0.5,
                                            mu = 0.01)
  
  # check R0 is calculated correctly
  expect_equal(round(R0(my_model), 2), 1.08)
})


test_that("Interventions can be set and retrieved", {
  my_model <- SEIRDNPIAge(n_age_categories = 2,
                          n_npi = 2,
                          age_ranges = list('0-50', '50-100'), 
                          contact_matrix = matrix(c(1,0,0,1), nrow = 2),
                          contact_matrix_npi = list(matrix(c(1,0,0,1), nrow = 2),
                                                    matrix(c(1,0,0,1), nrow = 2))
                          )
  # A single intervention period
  interventions(my_model) <- list(list(starts=10, stops=20))
  expect_equal(interventions(my_model),
               list(list(starts=10, stops=20)))
  
  # Multiple intervention periods
  interventions(my_model) <- list(list(starts=10, stops=20),
                                  list(starts=30, stops=45))
  expect_equal(interventions(my_model),
               list(list(starts=10, stops=20),
                    list(starts=30, stops=45))
               )
  
  # Check error is raised when the lengths of "starts" or "stops" is not a single value
  expect_error(
    interventions(my_model) <- list(list(starts=c(10, 15), stops=20))
    )
  expect_error(
    interventions(my_model) <- list(list(starts=5, stops=c(10, 20)))
    )
  expect_error(
    interventions(my_model) <- list(list(starts=c(10, 15), stops=c(10, 20)))
    )
  # Check error is raised when "starts" or "stops" are not single values
  expect_error(
    interventions(my_model) <- list(list(starts=10, stops=20,
                                    list(starts=c(25,30), stops=c(30, 35))))
  )
})

test_that("Simulation runs correctly", {
  my_model <- SEIRDNPIAge(n_age_categories = 2,
                          n_npi = 1,
                          age_ranges = list('0-50', '50-100'), 
                          contact_matrix = matrix(c(1,0,0,1), nrow = 2),
                          contact_matrix_npi = list(matrix(c(0,0,0,0), nrow = 2))
  )
  
  transmission_parameters(my_model) <- list(beta_npi = 0.5,
                                            beta = 1,
                                            kappa = 0.5,
                                            gamma = 1,
                                            mu = 0.5)
  
  initial_conditions(my_model)<-list(S0=c(0.6, 0.4),
                                     E0=c(0, 0),
                                     I0=c(0, 0),
                                     R0=c(0, 0), 
                                     D0 = c(0, 0))
  
  interventions(my_model) <- list(list(starts=2, stops=4))
  times = seq(0, 8, by = 1)
  out_df <- run(my_model, times)
  
  expected_output_states <- data.frame('time'=rep(times, 5*my_model@n_age_categories),
                                       'value'=c(rep(0.6, length(times)), 
                                                 rep(0.4, length(times)), 
                                                 rep(0.0, length(times)*((5-1)*my_model@n_age_categories))
                                                 ),
                                       'compartment' = lapply(c("S", "E", "I", "R", "D"), function(x)rep(x, length(times)*my_model@n_age_categories)) %>% unlist,
                                       'age_range' = rep(lapply(c('0-50', '50-100'), function(x)rep(x, length(times))) %>% unlist, 5)
                                       )
  
  expected_output_changes <- data.frame('time'=rep(times, 2*my_model@n_age_categories),
                                        'value'=rep(0.0, length(times)*(2*my_model@n_age_categories)),
                                        'compartment' = lapply(c("Deaths", "Incidence"), function(x)rep(x, length(times)*my_model@n_age_categories)) %>% unlist,
                                        'age_range' = rep(lapply(c('0-50', '50-100'), function(x)rep(x, length(times))) %>% unlist, 2)
  )
  for (state in c("S", "E", "I", "R", "D")){
    expect_equal(out_df$states[out_df$states$compartment==state, "value"],
                 expected_output_states[expected_output_states$compartment==state, "value"])
  }
  
  for (state in c("Deaths", "Incidence")){
    expect_equal(out_df$changes[out_df$changes$compartment==state, "value"],
                 expected_output_changes[expected_output_changes$compartment==state, "value"])
  }
  
  
})

test_that("Running model before setting parameters fails", {
  times = seq(0, 8, by = 1)
  my_model <- SEIRDNPIAge(n_age_categories = 2,
                          n_npi = 1,
                          age_ranges = list('0-50', '50-100'), 
                          contact_matrix = matrix(c(1,0,0,1), nrow = 2),
                          contact_matrix_npi = list(matrix(c(0,0,0,0), nrow = 2))
  )
  initial_conditions(my_model)<-list(S0=c(0.6, 0.4),
                                     E0=c(0, 0),
                                     I0=c(0, 0),
                                     R0=c(0, 0), 
                                     D0 = c(0, 0))
  interventions(my_model) <- list(list(starts=2, stops=4))
  expect_error(run(my_model, times), "Transmission parameters must be set before running.")
  
  
  my_model <- SEIRDNPIAge(n_age_categories = 2,
                          n_npi = 1,
                          age_ranges = list('0-50', '50-100'), 
                          contact_matrix = matrix(c(1,0,0,1), nrow = 2),
                          contact_matrix_npi = list(matrix(c(0,0,0,0), nrow = 2))
  )
  
  transmission_parameters(my_model) <- list(beta_npi = 0.5,
                                            beta = 1,
                                            kappa = 0.5,
                                            gamma = 1,
                                            mu = 0.5)
  interventions(my_model) <- list(list(starts=2, stops=4))
  expect_error(run(my_model, times), "Initial conditions must be set before running.")
  
  my_model <- SEIRDNPIAge(n_age_categories = 2,
                          n_npi = 1,
                          age_ranges = list('0-50', '50-100'), 
                          contact_matrix = matrix(c(1,0,0,1), nrow = 2),
                          contact_matrix_npi = list(matrix(c(0,0,0,0), nrow = 2))
  )
  
  transmission_parameters(my_model) <- list(beta_npi = 0.5,
                                            beta = 1,
                                            kappa = 0.5,
                                            gamma = 1,
                                            mu = 0.5)
  
  initial_conditions(my_model)<-list(S0=c(0.6, 0.4),
                                     E0=c(0, 0),
                                     I0=c(0, 0),
                                     R0=c(0, 0), 
                                     D0 = c(0, 0))
  expect_error(run(my_model, times), "Interventions must be set before running.")
})


