# Tests for functions in symptome_compartment_class.R on simulated test data
# --------------------------------------------------------------------------

# set up test data:
# initial population (in fraction)
S <- 0.9
E <- 0.1
I_asymptomatic <- 0
I_mild <- 0
I_severe <- 0
R <- 0
D_cumulative <- 0

# params
lam <- 1.0
gamma <- 0.5
omega <- 0.01
e2i <- list(i_mild = 0.35, i_severe = 0.05)
i2r <- list(i_asymptomatic = 0.9, i_mild = 0.5, i_severe = 0.05)
pdeath <- list(i_asymptomatic = 0.005, i_mild = 0.05, i_severe = 0.3)
# simulation period: 3000 time points (to relaxation)
t <- seq(0, 300, by = 1)

# test: validate model settings - ode simulation - plot, on test data
test_that("SEIaImIsR validate", {
  # 1. create the instance in class SEIaImIsR
  model <- new("SEIaImIsR", name = "model")
  # 2. set up parameters and initial population
  model <- set_init(object = model, S = S, E = E, I_asymptomatic = I_asymptomatic, I_mild = I_mild,
                    I_severe = I_severe, R = R, D_cumulative = D_cumulative,
                    lam = lam, gamma = gamma, omega = omega,
                    e2i = e2i, i2r = i2r, pdeath = pdeath)
  # 3. ode simulation and plot
  model <- ode_simulate(model, t)
  model <- plot_ode_output(model)

  # check the validity of the model class and settings
  expect_s4_class(model, "SEIaImIsR")
  expect_true(check_init(model))
  # view plot
  print(model@plot_output)
  # check @output
  expect_equal(colnames(model@output),
               c("time", "population_group", "fraction"))
  output_wide <- dcast(model@output, time~population_group)
  expect_equal(dim(output_wide),
               c(length(t), length(model@initial_population) + 1))
  expect_equal(colnames(output_wide),
               c("time", "S", "E", "I_asymptomatic", "I_mild", "I_severe", "R", "D_cumulative"))
  # check whether the sum of population at each simulation time point == 1
  expect_equal(rowSums(output_wide[, -which(names(output_wide) == "time")]),
                rep(1.0, length(t)))
  # check @plot_output
  expect_s3_class(model@plot_output, "ggplot")
})

# test input errors
test_that("model error input", {
  model <- new("SEIaImIsR", name = "model")
  expect_error({
    # initial population sum not equal to 1
    set_init(object = model, S = 0.1, E = E, I_asymptomatic = I_asymptomatic, I_mild = I_mild,
             I_severe = I_severe, R = R, D_cumulative = D_cumulative,
             lam = lam, gamma = gamma, omega = omega,
             e2i = e2i, i2r = i2r, pdeath = pdeath)
    # missing population group
    set_init(object = model, E = E, I_asymptomatic = I_asymptomatic, I_mild = I_mild,
             I_severe = I_severe, R = R, D_cumulative = D_cumulative,
             lam = lam, gamma = gamma, omega = omega,
             e2i = e2i, i2r = i2r, pdeath = pdeath)
    # missing parameter
    set_init(object = model, S = S, E = E, I_asymptomatic = I_asymptomatic, I_mild = I_mild,
             I_severe = I_severe, R = R, D_cumulative = D_cumulative,
             gamma = gamma, omega = omega,
             e2i = e2i, i2r = i2r, pdeath = pdeath)
    })
})
