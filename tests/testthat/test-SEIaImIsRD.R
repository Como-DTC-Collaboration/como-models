# Tests for functions in symptome_compartment_class.R on simulated test data
# --------------------------------------------------------------------------

# set up test data:
# initial population (in fraction)
S <- 0.99
E <- 0.01
I_asymptomatic <- 0
I_mild <- 0
I_severe <- 0
R <- 0
D <- 0

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
  model <- SEIaImIsRD()
  # 2. set up parameters and initial population
  transmission_parameters(model) <- list(beta = beta, kappa = kappa, omega = omega, p_symptom = p_symptom, gamma = gamma, mu = mu)
  initial_conditions(model) <- list(S = S, E = E, I_asymptomatic = I_asymptomatic, I_mild = I_mild, I_severe = I_severe, R = R, D = D)
  # 3. ode simulation and plot
  model <- run(model, t)
  p1 <- plot_dataframe(model@output$states, x = "time", y = "value", c = "compartment")
  p2 <- plot_dataframe(model@output$changes, x = "time", y = "value", c = "compartment")
  # 4. calculate basic reproduction number (R0)
  model <- R0(model)

  # check the validity of the model class and settings
  expect_s4_class(model, "SEIaImIsRD")
  # print out R0 value
  print(paste0("R0 = ", model@R0))
  # view plot
  print(p1)
  print(p2)

  # check @output
  expect_equal(colnames(model@output$states),
               c("time", "value", "compartment", "age_range"))
  output_wide <- dcast(model@output$states, time~compartment)
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
    initial_conditions(model) <- list(S = 0.1, E = E, I_asymptomatic = I_asymptomatic, I_mild = I_mild, I_severe = I_severe, R = R, D = D)
    # missing population group (S)
    initial_conditions(model) <- list(E = E, I_asymptomatic = I_asymptomatic, I_mild = I_mild, I_severe = I_severe, R = R, D = D)
    # missing parameter (beta)
    transmission_parameters(model) <- list(kappa = kappa, omega = omega, p_symptom = p_symptom, gamma = gamma, mu = mu)
    })
})
