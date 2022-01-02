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


test_that("can retrieve SEIRDNPIAge transmission parameters", {
  my_model <- SEIRDNPIAge(n_age_categories = 2)
  
  transmission_parameters(my_model) <- list(beta_npi = c(0.5, 0.5),
                                            beta = 1,
                                            kappa = c(1, 0.5),
                                            gamma = 0.5,
                                            mu = 0)

  # Test output is correct
  expected_ics <- list(beta_npi = c(0.5, 0.5),
                       beta = 1,
                       kappa = c(1, 0.5),
		       gamma = 0.5,
                       mu = 0)

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
    transmission_parameters(my_model) <- list(beta_npi = c(0.5, 0.5),
                                              beta = 1,
                                              kappa = c(1, 0.5),
                                              gamma = 0.5)
  )
  ## bad length (kappa)
  expect_error(
    transmission_parameters(my_model) <- list(beta_npi = c(0.5, 0.5),
                                              beta = 1,
                                              kappa = c(1, 0.5, 0.5),
                                              gamma = 0.5, 
                                              mu = 0)
  )

})

test_that("can set new initial conditions for the SEIRDNPIAge", {
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

test_that("can calculate the basic reproduction number R0 for the SEIRDNPIAge", {
  my_model <- SEIRDNPIAge(n_age_categories = 2,
                           n_npi = 2,
                           age_ranges = list('0-50', '50-100'), 
                           contact_matrix= matrix(c(1,0,0,1), nrow = 2),
                           contact_matrix_npi = list(
                             matrix(c(1,0,0,1), nrow = 2),
                             matrix(c(1,0,0,1), nrow = 2)
                             )
                           )
                                     
  transmission_parameters(my_model) <- list(#isolated_interventions = 0.4,
                                            beta_npi = c(0.5, 0.5), beta = 1, 
                                            kappa = 0.5, gamma = 0.5, mu = 0.01)
  initial_conditions(my_model)<-list(S0=c(0.4, 0.4),
                                     E0=c(0, 0),
                                     I0=c(0.05, 0.15),
                                     R0=c(0, 0), 
                                     D0 = c(0, 0))
  
  print(R0(my_model))
})
# 
# test_that("can run simulation for the SEIRDNPIAge", {
#   my_model <- SEIRDNPIAge(n_age_categories = 2,
#                        age_ranges = list('0-50', '50-100'),
#                        contact_matrix= list(matrix(c(1,0,0,1), nrow = 2),
#                                             matrix(c(1,0,0,1), nrow = 2)))
# 
#   initial_conditions(my_model) <- list(S0=c(0.6, 0.4),
#                                        E0=c(0, 0),
#                                        I0=c(0, 0),
#                                        R0=c(0, 0),
#                                        D0 = c(0,0))
# 
#   transmission_parameters(my_model) <- list(isolated_interventions = 0.4,
#                                             beta_isolated = 1, beta_not_isolated = 1,
#                                             kappa = 0.5, gamma = 0.5, mu = 0.01)
#   interventions(my_model) <- list(starts=0, stops=10, coverages=1)
#   expected_output <- data.frame('time'=rep(0:10, 12),
#                               'value'=c(rep(0.6, 3), rep(0.4, 3), rep(0, 30)))
# 
#   times = seq(0, 100, by = 1)
#   len_time = length(times)
#   res = run(my_model, times)
# 
#   # Test output is correct
#   expected_output_states <- data.frame('time'=rep(times, 10),
#                                        'value'=c(rep(0.6, len_time*1), rep(0.4, len_time*1), rep(0, len_time*8)))
# 
#   expected_output_changes <- data.frame('time'=rep(times, 4),
#                                         'value'=rep(0, len_time*4))
# 
#   expected_output_states$compartment = c(replicate(length(times)*2, "S"),
#                                          replicate(length(times)*2, "E"),
#                                          replicate(length(times)*2, "I"),
#                                          replicate(length(times)*2, "R"),
#                                          replicate(length(times)*2, "D"))
#   expected_output_states$age_range = unlist(rep(my_model@age_ranges, each=len_time))
#   expected_output_states <- expected_output_states %>%
#     dplyr::mutate(compartment=as.factor(compartment)) %>%
#     dplyr::mutate(compartment=forcats::fct_relevel(compartment, "S", "E", "I",
#                                                    "R", "D")) %>%
#     dplyr::mutate(age_range=as.factor(age_range)) %>%
#     dplyr::mutate(age_range=forcats::fct_relevel(age_range, my_model@age_ranges))
# 
#   expected_output_changes$age_range <- unlist(rep(my_model@age_ranges, each=len_time))
#   expected_output_changes <- expected_output_changes %>%
#     dplyr::mutate(age_range=as.factor(age_range)) %>%
#     dplyr::mutate(age_range=forcats::fct_relevel(age_range, my_model@age_ranges)) %>%
#     dplyr::mutate(compartment = c(replicate(length(times)*2, "Deaths"),
#                                   replicate(length(times)*2, "Incidence"))) %>%
#     dplyr::select("time", "value", "compartment", "age_range")
# 
#   expected_output = list("states" = expected_output_states,
#                          "changes" = expected_output_changes)
# 
#   expect_equal(run(object=my_model, times=times, t_intervention_1_2=20, t_intervention_2_3=50),
#                expected_output)
# 
#   # Test input errors
#   expect_error({
#     run(my_model, '0')
#     run(my_model, seq(0, 2, by = 1))})
# 
#   # Snapshot testing to check that outputs to the command line as expected
#   expect_snapshot_output(run(my_model, seq(0, 2, by = 1)))
#   
# })
