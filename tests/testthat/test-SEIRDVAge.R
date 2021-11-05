library(tidyverse)

test_that("SEIRDVAge model is instantiated correctly", {
  my_model <- SEIRDVAge(n_age_categories = 2,
                        age_ranges = list('0-50', '50-100'))
  
  # Test output is correct
  
  expect_equal(my_model@age_ranges,
               list('0-50', '50-100'))
  
  expect_equal(my_model@output_names,
               list('S', 'E', 'I', 'R', 'V', 'VR', 'D', 'Incidence'))
  
  expect_equal(my_model@initial_condition_names,
               list('S0', 'E0', 'I0', 'R0', 'V0', 'VR0', 'D0'))
  
  expect_equal(my_model@transmission_parameter_names,
               list('beta', 'kappa', 'gamma','mu', 'nu', 'delta_V', 'delta_R',
                    'delta_VR'))
  
  expect_length(my_model@initial_conditions, 7)
  expect_length(my_model@transmission_parameters, 8)
  expect_length(my_model@intervention_parameters, 3)
  
  expect_length(my_model@age_ranges, my_model@n_age_categories)
  
  expect_equal(my_model@n_age_categories, 2)
})

test_that("Initial conditions can be set and retrieved", {
  my_model <- SEIRDVAge(n_age_categories = 2)
  initial_conditions(my_model)<-list(S0=c(0.4, 0.4),
                                     E0=c(0, 0),
                                     I0=c(0.05, 0.15),
                                     R0=c(0, 0),
                                     V0=c(0, 0),
                                     VR0=c(0, 0),
                                     D0 = c(0, 0))
  
  # Test output is correct
  expected_ics <- list('S0'=c(0.4, 0.4),
                       'E0'=c(0, 0),
                       'I0'=c(0.05, 0.15),
                       'R0'=c(0, 0),
                       'V0'=c(0, 0),
                       'VR0'=c(0, 0),
                       'D0' = c(0, 0))
  
  expect_equal(initial_conditions(my_model),
               expected_ics)
  
  # Check error is raised when initial states are not doubles
  expect_error({
    initial_conditions(my_model) <- list(S0=c(0.4, 0, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0, 0))
  })
  
  expect_error({
    initial_conditions(my_model) <- list(S0=0,
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=0,
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=0,
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=0,
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=0,
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=0,
                                         D0 =c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = 0)
  })
  
  expect_error({
    initial_conditions(my_model) <- list(S0=c('0.4', 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, '0'),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, '0.15'),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c('0', 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c('0', 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c('0', 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, '0'))
  })
  
  expect_error(
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.5, 0.15),
                                         R0=c(0, 0),
                                         V0=c(0, 0),
                                         VR0=c(0, 0),
                                         D0 = c(0, 0))
    
  )
})

test_that("Transmission parameters can be set and retrieved", {
  my_model <- SEIRDVAge(n_age_categories = 2)
  transmission_parameters(my_model) <- list(
    beta=1, kappa=0.5, gamma=0.5, mu=0.1, nu = 0.1, delta_V = 0.1, delta_R = 0.1,
    delta_VR = 0.1)
  
  # Test output is correct
  expect_equal(transmission_parameters(my_model),
               list(beta = 1, kappa = 0.5, gamma = 0.5, mu = 0.1, nu = 0.1,
                    delta_V = 0.1, delta_R = 0.1, delta_VR = 0.1))
  
  # For multidimensional mu
  transmission_parameters(my_model) <- list(
    beta=1, kappa=0.5, gamma=0.5, mu=c(0.1, 0.2), nu=0.1, delta_V = 0.1, delta_R = 0.1,
    delta_VR = 0.1)
  
  # Test output is correct
  expect_equal(transmission_parameters(my_model),
               list(beta = 1, kappa = 0.5, gamma = 0.5, mu = c(0.1, 0.2), nu = 0.1,
                    delta_V = 0.1, delta_R = 0.1, delta_VR = 0.1))
  
  # For multidimensional nu
  transmission_parameters(my_model) <- list(
    beta=1, kappa=0.5, gamma=0.5, mu=0.1, nu = c(0.1, 0.2), delta_V = 0.1, delta_R = 0.1,
    delta_VR = 0.1)
  
  # Test output is correct
  expect_equal(transmission_parameters(my_model),
               list(beta = 1, kappa = 0.5, gamma = 0.5, mu = 0.1, nu = c(0.1, 0.2),
                    delta_V = 0.1, delta_R = 0.1, delta_VR = 0.1))
  
  # Check error is raised when transmission parameters are not 1-dimensional
  expect_error(
    transmission_parameters(my_model) <- list(
      beta=1, kappa=0.5, gamma=list(0.1, 0.1), mu=0.1,
      nu = 0.1, delta_V = 0.1, delta_R = 0.1, delta_VR = 0.1))
  expect_error(
    transmission_parameters(my_model) <- list(
      beta=1, kappa=list(0.1, 0.1), gamma=0.5, mu=0.1,
      nu = 0.1, delta_V = 0.1, delta_R = 0.1, delta_VR = 0.1))
})

test_that("Intervention parameters can be set and retrieved", {
  my_model <- SEIRDVAge(n_age_categories = 2)
  intervention_parameters(my_model) <- list(
    starts=10, stops=20, coverages=0.2)
  
  # Test output is correct
  expect_equal(intervention_parameters(my_model),
               list(starts=10, stops=20, coverages=0.2))
  
  # Multiple age-groups coverages
  intervention_parameters(my_model) <- list(
    starts=10, stops=20, coverages=list(0.2, 0.5))
  
  # Test output is correct
  expect_equal(intervention_parameters(my_model),
               list(starts=10, stops=20, coverages=list(0.2, 0.5)))
  
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
  expect_error(
    intervention_parameters(my_model) <- list(
      starts=c(10), stops=c(20), coverages=list(0.5)))
})

test_that("SEIRDVAge model runs correctly", {
  my_model <- SEIRDVAge(n_age_categories = 2,
                        age_ranges = list('0-50', '50-100'), 
                        contact_matrix = matrix(c(1,0,0,1), nrow = 2))
  
  initial_conditions(my_model) <- list(S0=c(0.6, 0.4),
                                       E0=c(0, 0),
                                       I0=c(0, 0),
                                       R0=c(0, 0),
                                       V0=c(0, 0),
                                       VR0=c(0, 0),
                                       D0=c(0,0))
  transmission_parameters(my_model) <- list(beta=0, kappa=0, gamma=0, mu=0,
                                            nu = 0, delta_V = 0, delta_R = 0,
                                            delta_VR = 0)
  intervention_parameters(my_model) <- list(starts=c(0, 19), stops=c(10, 20), coverages=list(c(0.5, 0), c(0, 0.4)))
  
  expected_output_states <- data.frame('time'=rep(0:2, 14),
                                       'value'=c(rep(0.6, 3), rep(0.4, 3), rep(0, 36)))
  
  expected_output_changes <- data.frame('time'=rep(0:2, 4),
                                        'value'=rep(0, 12))
  
  times = seq(0, 2, by = 1)
  expected_output_states$compartment = c(replicate(length(times)*2, "S"),
                                         replicate(length(times)*2, "E"),
                                         replicate(length(times)*2, "I"),
                                         replicate(length(times)*2, "R"),
                                         replicate(length(times)*2, "V"),
                                         replicate(length(times)*2, "VR"),
                                         replicate(length(times)*2, "D"))
  expected_output_states$age_range = unlist(rep(my_model@age_ranges, each=3))
  expected_output_states <- expected_output_states %>% 
    dplyr::mutate(compartment=as.factor(compartment)) %>% 
    dplyr::mutate(compartment=forcats::fct_relevel(compartment, "S", "E", "I",
                                                   "R", "V", "VR", "D")) %>% 
    dplyr::mutate(age_range=as.factor(age_range)) %>% 
    dplyr::mutate(age_range=forcats::fct_relevel(age_range, my_model@age_ranges))
  expected_output_changes$age_range <- unlist(rep(my_model@age_ranges, each=3))
  expected_output_changes <- expected_output_changes %>% 
    dplyr::mutate(age_range=as.factor(age_range)) %>% 
    dplyr::mutate(age_range=forcats::fct_relevel(age_range, my_model@age_ranges)) %>% 
    dplyr::mutate(compartment = c(replicate(length(times)*2, "Deaths"),
                                  replicate(length(times)*2, "Incidence"))) %>% 
    dplyr::select("time", "value", "compartment", "age_range")
  
  expected_output = list("states" = expected_output_states,
                         "changes" = expected_output_changes)
  # Test output is correct
  actual_output <- run(my_model, seq(0, 2, by = 1))
  expect_equal(actual_output, expected_output)
  
  # Test input errors
  expect_error({
    run(my_model, '0')
    run(my_model, c('0', 1, 2, 3))
    run(my_model, c(0, 1, 2, 3, 3, 4.5))
    run(my_model, seq(0, 2, by = 1))})
})

test_that("Running model before setting parameters fails", {
  t <- seq(0, 10, by = 0.1)
  my_model <- SEIRDVAge(n_age_categories = 2)
  initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                       E0=c(0, 0),
                                       I0=c(0.05, 0.15),
                                       R0=c(0, 0),
                                       V0=c(0, 0),
                                       VR0=c(0, 0),
                                       D0 = c(0, 0))
  intervention_parameters(my_model) <- list(starts=0, stops=10, coverages=0.5)
  expect_error(run(my_model, t), "Transmission parameters must be set before running.")
  
  my_model <- SEIRDVAge(n_age_categories = 2)
  transmission_parameters(my_model) <- list(beta=0.9, kappa=0.2, gamma=0.01, mu=0.1,
                                            nu = 0.1, delta_V = 0.1, delta_R = 0.1,
                                            delta_VR = 0.1)
  intervention_parameters(my_model) <- list(starts=0, stops=10, coverages=0.5)
  expect_error(run(my_model, t), "Initial conditions must be set before running.")
  
  my_model <- SEIRDVAge(n_age_categories = 2)
  initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                       E0=c(0, 0),
                                       I0=c(0.05, 0.15),
                                       R0=c(0, 0),
                                       V0=c(0, 0),
                                       VR0=c(0, 0),
                                       D0 = c(0, 0))
  transmission_parameters(my_model) <- list(beta=0.9, kappa=0.2, gamma=0.01, mu=0.1,
                                            nu = 0.1, delta_V = 0.1, delta_R = 0.1,
                                            delta_VR = 0.1)
  expect_error(run(my_model, t), "Intervention parameters must be set before running.")
})
