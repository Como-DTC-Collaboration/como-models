test_that("SEIRDAge gets instantiated", {
  my_model <- SEIRDAge(n_age_categories = 2,
                      age_ranges = list('0-50', '50-100'))

  # Test output is correct

  expect_equal(my_model@age_ranges,
               list('0-50', '50-100'))

  expect_equal(my_model@output_names,
               list('S', 'E', 'I', 'R', 'D', 'Incidence'))

  expect_equal(my_model@initial_condition_names,
               list('S0', 'E0', 'I0', 'R0', 'D0'))

  expect_equal(my_model@transmission_parameter_names,
               list('b', 'k', 'g', 'mu'))

  expect_length(my_model@initial_conditions, 5)

  expect_length(my_model@transmission_parameters, 4)

  expect_length(my_model@age_ranges, my_model@n_age_categories)

  expect_equal(my_model@n_age_categories, 2)
})

test_that("can retrieve SEIRDAge initial conditions", {
  my_model <- SEIRDAge(n_age_categories = 2)

  expect_equal(initial_conditions(my_model),
               vector(mode = "list", length = 5))
})

test_that("can retrieve SEIRDAge transmission parameters", {
  my_model <- SEIRDAge(n_age_categories = 2)

  expect_equal(transmission_parameters(my_model),
               vector(mode = "list", length = 4))
})

test_that("can set new initial conditions for the SEIRDAge", {
  my_model <- SEIRDAge(n_age_categories = 2)
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
  expect_error({
    initial_conditions(my_model) <- list(S0=c(0.4, 0, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),                                   
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0, 0))
  })

    expect_error({
    initial_conditions(my_model) <- list(S0=0,
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=0,
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=0,
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=0,
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         D0 = 0)
    })

    expect_error({
    initial_conditions(my_model) <- list(S0=c('0.4', 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, '0'),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, '0.15'),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c('0', 0),
                                         D0 = c(0, 0))
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.05, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, '0'))
    })

    expect_error(
    initial_conditions(my_model) <- list(S0=c(0.4, 0.4),
                                         E0=c(0, 0),
                                         I0=c(0.5, 0.15),
                                         R0=c(0, 0),
                                         D0 = c(0, 0))
    
    )
})

test_that("can set new transmission parameters for the SEIRDAge", {
  my_model <- SEIRDAge(n_age_categories = 2)
  transmission_parameters(my_model) <- list(b=1, k=0.5, g=0.5, mu = 0.01)

  # Test output is correct
  expected_transpar <- list('b'=1,
                            'k'=0.5,
                            'g'=0.5,
                            'mu' = 0.01)

  expect_equal(transmission_parameters(my_model),
               expected_transpar)
  
  # Multi-dimensional mu
  transmission_parameters(my_model) <- list(b=1, k=0.5, g=0.5, mu = c(0.01, 0.02))
  
  # Test output is correct
  expected_transpar <- list('b'=1,
                            'k'=0.5,
                            'g'=0.5,
                            'mu' = c(0.01, 0.02))
  
  expect_equal(transmission_parameters(my_model),
               expected_transpar)

  # Test input errors
  expect_error(
    transmission_parameters(my_model) <- list(b=c(1, 0), k=0.5, g=0.5, mu = 0.01)
  )
  expect_error(
    transmission_parameters(my_model) <- list(b=1, k=c(1, 0), g=0.5, mu = 0.01)
  )
  expect_error(
    transmission_parameters(my_model) <- list(b=1, k=0.5, g=0.5,
                                              mu = c(0.01, 0.01, 0.1))
  )
})

test_that("can run simulation for the SEIRDAge", {
  my_model <- SEIRDAge(n_age_categories = 2,
                      age_ranges = list('0-50', '50-100'), 
                      contact_matrix = matrix(c(1,0,0,1), nrow = 2))

  initial_conditions(my_model) <- list(S0=c(0.6, 0.4),
                                       E0=c(0, 0),
                                       I0=c(0, 0),
                                       R0=c(0, 0),
                                       D0 = c(0,0))

  transmission_parameters(my_model) <- list(b=1, k=0.5, g=0.5, mu = c(0.01, 0.01))

  expected_output_states <- data.frame('time'=rep(0:2, 10),
                                'value'=c(rep(0.6, 3), rep(0.4, 3), rep(0, 24)))
  
  expected_output_changes <- data.frame('time'=rep(0:2, 4),
                                'value'=rep(0, 12))
                                
  times = seq(0, 2, by = 1)
  expected_output_states$compartment = c(replicate(length(times)*2, "S"),
                           replicate(length(times)*2, "E"),
                           replicate(length(times)*2, "I"),
                           replicate(length(times)*2, "R"),
                           replicate(length(times)*2, "D"))
  expected_output_states$age_range = unlist(rep(my_model@age_ranges, each=3))
  expected_output_states <- expected_output_states %>% 
    dplyr::mutate(compartment=as.factor(compartment)) %>% 
    dplyr::mutate(compartment=forcats::fct_relevel(compartment, "S", "E", "I",
                                                   "R", "D")) %>% 
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
  
  # Snapshot testing to check that outputs to the command line as expected
  # expect_snapshot_output(run(my_model, seq(0, 2, by = 1)))

})

test_that("R0 calculation works for SEIRDAge", {
  my_model <- SEIRDAge(n_age_categories = 2,
                       age_ranges = list('0-50', '50-100'), 
                       contact_matrix = matrix(c(1,0,0,1), nrow = 2))
  
  initial_conditions(my_model) <- list(S0=c(0.6, 0.4),
                                       E0=c(0, 0),
                                       I0=c(0, 0),
                                       R0=c(0, 0),
                                       D0 = c(0,0))
  
  beta <- 1
  gamma <- 0.5
  mu <- 0.01
  transmission_parameters(my_model) <- list(b=beta, k=0.5, g=c(gamma, gamma), mu = c(mu, mu))
  
  # if all individuals in one compartment then reduces to SEIRD
  expect_true(abs(R0(my_model, c(1, 0)) - beta / (gamma + mu)) < 0.0001)
})

test_that("ode_model_structure works", {
  model <- SEIRDAge()
  g <- ode_structure_diagram(model)
  expect_s3_class(g, "html")
})
