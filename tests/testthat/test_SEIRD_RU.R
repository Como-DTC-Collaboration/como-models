test_that("SEIRD model is instantiated correctly", {
  my_model <- SEIRD_RU()
  
  expect_length(my_model@initial_conditions, 8)
  expect_length(my_model@initial_cases_deaths, 4)
  expect_length(my_model@transmission_parameters, 5)
  expect_length(my_model@contact_rates, 2)
})

test_that("Initial conditions can be set and retrieved", {
  my_model <- SEIRD_RU()
  initial_conditions(my_model) <- list(S_U0 = 0.699, E_U0 = 0, I_U0 = 0.001, R_U0 = 0,
                                       S_Y0 = 0.3, E_Y0 = 0, I_Y0 = 0, R_Y0 = 0)
  
  # Test output is correct
  expect_equal(initial_conditions(my_model),
               list("S_U0" = 0.699, "E_U0" = 0, "I_U0" = 0.001, "R_U0" = 0,
                    "S_Y0" = 0.3, "E_Y0" = 0, "I_Y0" = 0, "R_Y0" = 0))
  
  # Check error is raised when initial states are not doubles
  expect_error(
    initial_conditions(my_model) <- list(S_U0 = 0.699, E_U0 = 0,
                                         I_U0 = c(0.001,0.001), R_U0 = 0,
                                         S_Y0 = 0.3, E_Y0 = 0,
                                         I_Y0 = 0, R_Y0 = 0))
  expect_error(
    initial_conditions(my_model) <- list(S_U0="a", E_U0 = 0, I_U0 = 0.001, R_U0 = 0,
                                         S_Y0 = 0.3, E_Y0 = 0, I_Y0 = 0, R_Y0 = 0))
  
  # Check error is raised when sum of initial conditions is not 1
  expect_error(initial_conditions(my_model) <- list(S_U0 = 0.799, E_U0 = 0,
                                                    I_U0 = 0.001, R_U0 = 0,
                                                    S_Y0 = 0.300, E_Y0 = 0,
                                                    I_Y0 = 0, R_Y0 = 0))
})

test_that("Transmission parameters can be set and retrieved", {
  my_model <- SEIRD_RU()
  transmission_parameters(my_model) <- list(b=1, k=0.5, g=0.5, m=0.1, C=0.5)
  
  # Test output is correct
  expect_equal(transmission_parameters(my_model),
               list(b = 1, k = 0.5, g = 0.5, m = 0.1, C = 0.5))
  
  # Check error is raised when transmission parameters are not 1-dimensional
  expect_error(
    transmission_parameters(my_model) <- list(b=1, k=0.5, g=list(0.1, 0.1), m=0.1, C=0.5))
  expect_error(
    transmission_parameters(my_model) <- list(b=1, k=list(0.1, 0.1), g=0.5, m=0.1, C=0.5))
})

test_that("Contact rates and fraction rural can be set directly and retrieved", {
  # check directly inputting contact rates
  my_model <- SEIRD_RU()
  contact_rates(my_model) <- list(urban = 2,rural = 3)
  
  # Test output is correct
  expect_equal(contact_rates(my_model),
               list(urban = 2,rural = 3))
  
  #check directly inputting fraction rural
  my_model <- SEIRD_RU()
  country_demog(my_model) <- 0.7
  
  # Test output is correct
  expect_equal(fraction_rural(my_model), 0.7)
  
  #check for errors
  expect_error(country_demog(my_model) <- list(0.7,0.3))
  expect_error(country_demog(my_model) <- list(0.5,0.3,0.2))
})

test_that("SEIR model runs correctly", {
  my_model <- SEIRD_RU()
  initial_conditions(my_model) <- list(S_U0 = 0.699, E_U0 = 0, I_U0 = 0.001, R_U0 = 0,
                                       S_Y0 = 0.3, E_Y0 = 0, I_Y0 = 0, R_Y0 = 0)
  transmission_parameters(my_model) <- list(b=0, k=0, g=0, m=0, C=0.5)
  
  # load contact matrices
  load("./testing_data/contact_all_urban.rdata") 
  contact_all_urban <- contact_all
  names_urban <- names(contact_all_urban)
  load("./testing_data/contact_all_rural.rdata")
  contact_all_rural <- contact_all
  names_rural <- names(contact_all_rural)
  names_common <- intersect(names_urban,names_rural)
  
  #import all demographic data
  # data on percentage of the population that is rural
  load("./testing_data/percentrural_by_country.rdata")
  country_pop_rural <- x
  # age demographic breakdown into 5 year age categories
  load("./testing_data/agedemographics_by_country.rdata")
  country_pop_byage <- x
  # conversion table from 3 letter country codes to full names
  load("./testing_data/country_codetoname.rdata")
  code_to_country <- x
  
  # specify country
  country <- "TUN"
  if (!(country %in% names_common)) {
    stop(paste(country," is not a valid three-letter country code."))
  }
  # get fraction of population that is rural
  country_rural <- country_pop_rural$`2019`[country_pop_rural$`Country Code` == country]
  frac_rural <- country_rural/100 #turn percentage into fraction
  # get fraction of population in each 5-year age range
  # get full name of country
  country_fullname <- code_to_country$CountryName[code_to_country$CountryCode == country]
  # extract age demographic vector
  pop_byage_2019 <- country_pop_byage[(country_pop_byage$`Region, subregion, country or area *` == country_fullname & country_pop_byage$`Reference date (as of 1 July)` == 2019),9:29]
  pop_byage_2019 <- as.double(pop_byage_2019)
  # normalize to fractions of the total population
  pop_byage_2019 <- pop_byage_2019/sum(pop_byage_2019) 
  # must sum last five entries because contact matrices have an 80+ category instead of 100+
  pop_byage_2019 <- c(pop_byage_2019[1:15], sum(pop_byage_2019[16:20]))
  #expect error when setting contact matrices before demographic data
  expect_error(contact_rates(my_model) <- list(contact_all_urban[[country]],contact_all_rural[[country]]))
  country_demog(my_model) <- list(pop_byage_2019*(1-frac_rural),pop_byage_2019*frac_rural)
  contact_rates(my_model) <- list(contact_all_urban[[country]],contact_all_rural[[country]])
  
  # Check output shape
  t <- seq(0, 10, by = 0.1)
  out_df <- run(my_model, t)
  expect_identical(dim(out_df), as.integer(c(((10 - 0) / 0.1 + 1) * 12, 4)))
  
  # Check output value for rates equal 0s
  expected_data <- data.frame(
    S_U = 0.699, E_U = 0, I_U = 0.001, R_U = 0,
    Incidences_U = 0, Deaths_U = 0,
    S_Y = 0.3, E_Y = 0, I_Y= 0, R_Y = 0,
    Incidences_Y = 0, Deaths_Y = 0)
  out_df_temp <- dcast(out_df, time ~ compartment, value.var = "value")
  test_data <- out_df_temp[101, 2:13]
  row.names(test_data) <- NULL
  expect_identical(test_data, expected_data)
  
  # Check that sum of states is sufficiently close to one at all times
  transmission_parameters(my_model) <- list(b=0.9, k=0.2, g=0.01, m=0.1, C = 0.5)
  out_df <- run(my_model, seq(0, 10, by = 0.1))
  out_df_temp <- dcast(out_df, time ~ compartment, value.var = "value")
  out_df_temp$Deaths_U <- cumsum(out_df_temp$Deaths_U)
  out_df_temp$Deaths_Y <- cumsum(out_df_temp$Deaths_Y)
  test <- rowSums(out_df_temp[, c(2:5,8:11)]) + out_df_temp$Deaths_U + out_df_temp$Deaths_Y
  expected <- as.double(rep(1, 101))
  expect_equal(test,expected)
  
  # Test input errors
  expect_error(run(my_model, "a"))
})

test_that("Running model before setting parameters fails", {
  t <- seq(0, 10, by = 0.1)
  
  my_model <- SEIRD_RU()
  initial_conditions(my_model) <- list(S_U0 = 0.699, E_U0 = 0, I_U0 = 0.001, R_U0 = 0,
                                       S_Y0 = 0.3, E_Y0 = 0, I_Y0 = 0, R_Y0 = 0)
  expect_error(run(my_model, t))
  
  my_model <- SEIRD_RU()
  transmission_parameters(my_model) <- list(b=0.9, k=0.2, g=0.01, m=0.1, C=0.5)
  expect_error(run(my_model, t))
})

test_that("R0 works for SEIRD model", {
  my_model <- SEIRD_RU()
  initial_conditions(my_model) <- list(S_U0 = 0.69244, E_U0 = 0, I_U0 = 0.0001, R_U0 = 0,
                                       S_Y0 = 0.30746, E_Y0 = 0, I_Y0 = 0, R_Y0 = 0)
  b = 0.3 
  k = 0.2 
  g = 0.1
  m = 0.03 
  C = 1 
  transmission_parameters(my_model) <- list(b=b, k=k,
                                            g=g, m=m, C=C)
  # load contact matrices
  load("./testing_data/contact_all_urban.rdata") # ROWS are age of person, COLUMNS are age of contact
  contact_all_urban <- contact_all
  names_urban <- names(contact_all_urban)
  load("./testing_data/contact_all_rural.rdata")
  contact_all_rural <- contact_all
  names_rural <- names(contact_all_rural)
  names_common <- intersect(names_urban,names_rural)
  
  #import all demographic data
  # data on percentage of the population that is rural
  load("./testing_data/percentrural_by_country.rdata")
  country_pop_rural <- x
  # age demographic breakdown into 5 year age categories
  load("./testing_data/agedemographics_by_country.rdata")
  country_pop_byage <- x
  # conversion table from 3 letter country codes to full names
  load("./testing_data/country_codetoname.rdata")
  code_to_country <- x
  
  # specify country
  country <- "TUN"
  if (!(country %in% names_common)) {
    stop(paste(country," is not a valid three-letter country code."))
  }
  # get fraction of population that is rural
  country_rural <- country_pop_rural$`2019`[country_pop_rural$`Country Code` == country]
  frac_rural <- country_rural/100 #turn percentage into fraction
  # get fraction of population in each 5-year age range
  # get full name of country
  country_fullname <- code_to_country$CountryName[code_to_country$CountryCode == country]
  # extract age demographic vector
  pop_byage_2019 <- country_pop_byage[(country_pop_byage$`Region, subregion, country or area *` == country_fullname & country_pop_byage$`Reference date (as of 1 July)` == 2019),9:29]
  pop_byage_2019 <- as.double(pop_byage_2019)
  # normalize to fractions of the total population
  pop_byage_2019 <- pop_byage_2019/sum(pop_byage_2019) 
  # must sum last five entries because contact matrices have an 80+ category instead of 100+
  pop_byage_2019 <- c(pop_byage_2019[1:15], sum(pop_byage_2019[16:20]))
  #set demographic data
  country_demog(my_model) <- list(pop_byage_2019*(1-frac_rural),pop_byage_2019*frac_rural)
  # set contact matrices
  contact_rates(my_model) <- list(contact_all_urban[[country]],contact_all_rural[[country]])

  
  expect_equal(round(R0(my_model),digits = 5), 15.84567)
})

test_that("ode_model_structure works", {
  model <- SEIRD_RU()
  g <- ode_structure_diagram(model)
  expect_s3_class(g, "html")
})
