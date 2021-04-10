# Tests for functions in symptome_compartment_class.R on simulated test data
# --------------------------------------------------------------------------

# set up test data:
# initial population (in fraction)
S <- 0.9
E <- 0.1
I <- 0
HA <- 0
R <- 0
CM <- 0

# params
lam <- 1.0
gamma <- 0.5
omega <- 0.01
ihr <- 0.1
nu <- list(i = 0.5, hsa = 0.05)
pdeath <- list(i = 0.05, hsa = 0.3)
# simulation period: 3000 time points (to relaxation)
t <- seq(0, 3000, by = 1)

# test: validate model settings - ode simulation - plot, on test data
test_that("model symptome", {
  # 1. create the object of class model_symptome
  model <- new("model_symptome", name = "model")
  # 2. set up parameters and initial population
  model <- set_init(object = model, S = S, E = E, I = I,
                    HA = HA, R = R, CM = CM,
                    lam = lam, gamma = gamma, omega = omega,
                    ihr = ihr, nu = nu, pdeath = pdeath)
  # 3. ode simulation and plot
  model <- ode_simulate(model, t)
  model <- plot_ode_output(model)

  # check the validity of the model class and settings
  expect_s4_class(model, "model_symptome")
  expect_true(check_init(model))
  # view plot
  print(model@plot_output)
  # check @output
  expect_equal(dim(model@output),
               c(length(t), length(model@initial_population) + 1))
  expect_equal(colnames(model@output),
               c("time", "S", "E", "I", "HA", "R", "CM"))
  # check whether the sum of population at each simulation time point == 1
  expect_equal(rowSums(model@output[, -which(names(model@output) == "time")]),
                rep(1.0, length(t)))
  # check @plot_output
  expect_s3_class(model@plot_output, "ggplot")
})

# test input errors
test_that("model error input", {
  model <- new("model_symptome", name = "model")
  expect_error({
    # initial population sum not equal to 1
    set_init(object = model, S = 0.1, E = E, I = I,
             HA = HA, R = R, CM = CM,
             lam = lam, gamma = gamma, omega = omega,
             ihr = ihr, nu = nu, pdeath = pdeath)
    # missing population group
    set_init(object = model, E = E, I = I,
             HA = HA, R = R, CM = CM,
             lam = lam, gamma = gamma, omega = omega,
             ihr = ihr, nu = nu, pdeath = pdeath)
    # missing parameter
    set_init(object = model, S = S, E = E, I = I,
             HA = HA, R = R, CM = CM,
             gamma = gamma, omega = omega,
             ihr = ihr, nu = nu, pdeath = pdeath)
    })
})
