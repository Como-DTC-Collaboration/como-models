#' Returns transmission parameter values for gamma, mu, kappa and R0 given a specific
#' variant.
#' Parameters are returned in vector form for age-structured models, and
#' scalar form for non-age-structured models.
#' 
#' 1/kappa is the typical latent period, i.e. the period in which an individual
#' is infected but not infectious. The latent period for the base variant is
#' estimated to be 5.5 days [@Xin21]. The latent period for the Delta variant
#' is estimated to be around 3.7 days [@Li21]. The incubation period for the
#' Omicron variant is estimated to be 3 days [@CDC21], but no estimate for the
#' latent period is currently available. Because the incubation period of the
#' delta variant is estimated to be approximately 0.6 days longer than its
#' latent period [@Grant21], we assume the latent period for omicron to
#' be 2.4 days. We thus obtain values of kappa for each of the
#' variants: for base kappa = 1/5.5, for Delta kappa = 1/3.7, and for
#' omicron kappa = 1/2.4.
#' 
#' We assume that on average 0.66\% of individuals infected with the virus die
#' [@verity2020estimates]: this is termed an infection fatality ratio (IFR).
#' For age-structured models, we use age-specific IFR estimates
#' [@verity2020estimates].
#' Changing either gamma or mu affects both the rate at which
#' individuals recover or die and the proportions which recover or die.
#' As such, the implications of the individual parameter values are hard to
#' intuit without recourse to the other. Here, we choose a different
#' parameterisation that makes it simpler to set these parameters to
#' appropriate values:
#' 
#' gamma = zeta (1-IFR)
#' mu = zeta IFR
#' 
#' The proportion of infected individuals who go on to die due to infection
#' is given by the ratio of the rate of death to the overall rate out from the
#' infectious compartment, which is the definition of the IFR:
#' 
#' zeta = zeta IFR / (zeta IFR + zeta (1-IFR))
#'      = IFR
#' 
#' The average duration spent in the infectious compartment is given by:
#'   
#'     1 / (zeta IFR + zeta (1-IFR)) = 1 / zeta
#'     
#' The duration of the infectious period is again difficult to estimate and
#' likely varies over variants. We will assume this time period to be
#' 9 days, which is based on the base variant
#' [@MACINTYRE21]. So, we set zeta = 1/9.
#' 
#' The R0 of the base variant is estimated to be 2.4
#' [@ferguson2020report]. Estimates of R0 for the delta variant range between
#' 3.2 and 7, so we estimate it as 5 [@Liu21, @Meng21, @Burki21]. Estimating R0 for
#' Omicron is difficult with the many confounding variables like vaccine escape,
#' but a preliminary guess is 10 [@Burki21].
#' 
#' @param variant a string specifying the variant of SARS-CoV-2: base, delta
#' or omicron.
#' @param agestructured a boolean indicating whether or not the model is
#' age-structured
#' 
#' @return a vector or matrix for each parameter: kappa, gamma, mu and R0.
#' 
#'
covid_parameter_values <- function(variant = "base", agestructured = FALSE){
  # check if variant is valid
  if (!(variant %in% c("base","delta","omicron"))){
    return("ERROR: The specified variant is not valid. You must choose from base, delta and omicron.")
  }
  # check if agestructured is a boolean
  if (!(is.logical(agestructured))){
    return("ERROR: The argument agestructured must be a boolean (TRUE or FALSE).")
  }
  
  load("data/infection_fatality_ratio.rda")
  
  if (agestructured){
    # load ifr vector depending on age
    ifr <- infection_fatality_ratio$age_structured
    l <- 9
  }
  else{
    # load single scalar value
    ifr <- infection_fatality_ratio$overall
    l <- 1
  }
  
  # set zeta, which is the same for all variants
  zeta <- 1 / 9
  gamma <- zeta * ( 1- ifr)
  mu <- zeta * ifr
  
  if (variant=="base"){
    R0 <- 2.4
    kappa <- rep(1 / 5.5, l)
  }
  
  if (variant=="delta"){
    R0 <- 5
    kappa <- rep(1 / 3.7, l)
  }
  
  if (variant=="omicron"){
    R0 <- 10
    kappa <-rep(1 / 2.4, l)
  }
  
  final_list <- list(kappa = kappa, gamma = gamma, mu = mu, R0 = R0)
  return(final_list)
}








