test_that("age_model gets instantiated", {
  my_model <- new('age_model', name = 'my_model', n_age_categories = 2)

  # Test output is correct
  expect_equal(my_model@name,
               'my_model')

  expect_equal(my_model@output_names,
               list('S', 'E', 'I', 'R', 'Incidence'))

  expect_equal(my_model@parameter_names,
               list('S0', 'E0', 'I0', 'R0', 'b', 'k', 'g'))

  expect_length(my_model@parameters, 7)

  expect_equal(my_model@n_age_categories, 2)
})

test_that("can retrieve age_model parameters", {
  my_model <- new('age_model', name = 'my_model', n_age_categories = 2)

  expect_equal(get_parameters(my_model),
               vector(mode = "list", length = 7))
})

test_that("can set new parameters for the age_model", {
  my_model <- new('age_model', name = 'my_model', n_age_categories = 2)
  my_model <- set_parameters(my_model, c(0.4, 0.4), c(0, 0), c(0.05, 0.15),
                             c(0, 0), 1, 0.5, 0.5)

  # Test output is correct
  expected_parameters <- list('S0'=c(0.4, 0.4),
                             'E0'=c(0, 0),
                             'I0'=c(0.05, 0.15),
                             'R0'=c(0, 0),
                             'b'=1,
                             'k'=0.5,
                             'g'=0.5)

  expect_equal(get_parameters(my_model),
               expected_parameters)

  # Test input errors
  expect_error({
    my_model <- set_parameters(my_model, c(0.4, 0, 0.4), c(0, 0), c(0.5, 0.15),
                               c(0, 0), 1, 0.5, 0.5)
    my_model <- set_parameters(my_model, c(0.4, 0, 0.4), c(0, 0), c(0.05, 0.15),
                               c(0, 0), 1, 0.5, 0.5)
    my_model <- set_parameters(my_model, c(0.4, 0.4), c(0, 0, 0), c(0.05, 0.15),
                               c(0, 0), 1, 0.5, 0.5)
    my_model <- set_parameters(my_model, c(0.4, 0.4), c(0, 0), c(0.05, 0, 0.15),
                               c(0, 0), 1, 0.5, 0.5)
    my_model <- set_parameters(my_model, c(0.4, 0.4), c(0, 0), c(0.05, 0.15),
                               c(0, 0, 0), 1, 0.5, 0.5)
    my_model <- set_parameters(my_model, 0, c(0, 0), c(0.05, 0.15),
                               c(0, 0), 1, 0.5, 0.5)
    my_model <- set_parameters(my_model, c(0.4, 0.4), 0, c(0.05, 0.15),
                               c(0, 0), 1, 0.5, 0.5)
    my_model <- set_parameters(my_model, c(0.4, 0.4), c(0, 0), 0,
                               c(0, 0), 1, 0.5, 0.5)
    my_model <- set_parameters(my_model, c(0.4, 0.4), c(0, 0), c(0.05, 0.15),
                               0, 1, 0.5, 0.5)
    my_model <- set_parameters(my_model, c(0.4, 0.4), c(0, 0), c(0.05, 0.15),
                               c(0, 0), c(1, 0), 0.5, 0.5)
    my_model <- set_parameters(my_model, c(0.4, 0.4), c(0, 0), c(0.05, 0.15),
                               c(0, 0), 1, c(1, 0), 0.5)
    my_model <- set_parameters(my_model, c(0.4, 0.4), c(0, 0), c(0.05, 0.15),
                               c(0, 0), 1, 0.5, c(1, 0))
  })
})

test_that("can run simulation for the age_model", {
  my_model <- new('age_model', name = 'my_model', n_age_categories = 2)
  my_model <- set_parameters(my_model, c(0.6, 0.4), c(0, 0), c(0, 0),
                             c(0, 0), 1, 0.5, 0.5)

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
  expect_equal(simulate(my_model, seq(0, 2, by = 1)),
               expected_output)

  # Test input errors
  expect_error({
    simulate(my_model, '0')
    simulate(my_model, seq(0, 2, by = 1), is_plot = c(0, 0))})

  # Snapshot testing to check that plotting routines are called correctly.
  # case with plotting on
  expect_snapshot_output(simulate(my_model, seq(0, 2, by = 1)))

  # case with plotting off
  expect_snapshot_output(simulate(my_model, seq(0, 2, by = 1), FALSE))

})
