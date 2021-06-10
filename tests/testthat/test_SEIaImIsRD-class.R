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
beta <- list(i_asymptomatic = 0.80, i_mild = 0.90, i_severe = 1.00)
kappa <- 0.50
omega <- 0.01
p_symptom <- list(i_mild = 0.35, i_severe = 0.05)
gamma <- list(i_asymptomatic = 0.9, i_mild = 0.50, i_severe = 0.05)
mu <- list(i_asymptomatic = 0.005, i_mild = 0.05, i_severe = 0.30)
# simulation period: 3000 time points (to relaxation)
t <- seq(0, 2000, by = 1)

# test: validate model settings - ode simulation - plot, on test data
test_that("SEIaImIsRD validate", {
  # 1. create the instance in class SEIaImIsRD
  model <- new("SEIaImIsRD")
  # 2. set up parameters and initial population
  model <- set_init(object = model, S = S, E = E, I_asymptomatic = I_asymptomatic, I_mild = I_mild,
                    I_severe = I_severe, R = R, D = D_cumulative,
                    beta = beta, kappa = kappa, omega = omega,
                    p_symptom = p_symptom, gamma = gamma, mu = mu)
  # 3. ode simulation and plot
  model <- ode_simulate(model, t)
  p <- plot_dataframe(model@output, x = "time", y = "fraction", c = "population_group")
  # 4. calculate basic reproduction number (R0)
  model <- R0_SEIaImIsRD(model)

  # check the validity of the model class and settings
  expect_s4_class(model, "SEIaImIsRD")
  expect_true(check_init(model))
  # print out R0 value
  print(paste0("R0 = ", model@R0))
  # view plot
  print(p)

  # check @output
  expect_equal(colnames(model@output),
               c("time", "population_group", "fraction"))
  output_wide <- dcast(model@output, time~population_group)
  expect_equal(dim(output_wide),
               c(length(t), length(model@initial_conditions) + 1))
  expect_equal(colnames(output_wide),
               c("time", "S", "E", "I_asymptomatic", "I_mild", "I_severe", "R", "D"))
  # check whether the sum of population at each simulation time point == 1
  expect_equal(rowSums(output_wide[, -which(names(output_wide) == "time")]),
                rep(1.0, length(t)))

})

# test input errors
test_that("model error input", {
  model <- SEIaImIsRD()
  expect_error({
    # initial population sum not equal to 1
    set_init(object = model, S = 0.1, E = E, I_asymptomatic = I_asymptomatic, I_mild = I_mild,
             I_severe = I_severe, R = R, D = D_cumulative,
             beta = beta, kappa = kappa, omega = omega,
             p_symptom = p_symptom, gamma = gamma, mu = mu)
    # missing population group (S)
    set_init(object = model, E = E, I_asymptomatic = I_asymptomatic, I_mild = I_mild,
             I_severe = I_severe, R = R, D = D_cumulative,
             beta = beta, kappa = kappa, omega = omega,
             p_symptom = p_symptom, gamma = gamma, mu = mu)
    # missing parameter (beta)
    set_init(object = model, S = S, E = E, I_asymptomatic = I_asymptomatic, I_mild = I_mild,
             I_severe = I_severe, R = R, D = D_cumulative,
             kappa = kappa, omega = omega,
             p_symptom = p_symptom, gamma = gamma, mu = mu)
    })
})
