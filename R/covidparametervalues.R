#' Returns parameter values for gamma, mu, kappa and R0 given a specific
#' variant.
#' Parameters are returned in vector form for age-structured models, and
#' scalar form for non-age-structured models.
#' 
#' Kappa is one over the latent period, i.e. the period in which an individual
#' is infected but not infectious. Although the latent period is difficult to
#' estimate, the incubation period, the time between infection and onset of
#' symptoms is much easier to estimate and therefor more readily available for
#' different variants. The latent period for the base variant is
#' estimated to be 5.5 days [@Xin21]. The latent period for the delta variant
#' is estimated to be around 3.7 days [@Li21]. The incubation period for the
#' omicron variant is estimated to be 3 days [@CDC21], but no estimate for the
#' latent period is current available. Because the incubation period of the
#' delta variant is estimated to be approximately 0.6 days longer than its
#' latent period [@Grant21], we will estimate the latent period for omicron to
#' be approximately 2 days. We thus obtain values of kappa for each of the
#' variants: for base $\kappa = 1/5.5$, for delta $\kappa = 1/3.7$, and for
#' omicron $\kappa = 1/2$.
#' 
#' We assume that on average 0.66\% of individuals infected with the virus die
#' [@verity2020estimates]: this is termed an infection fatality ratio (IFR).
#' For age-structured models, we use age-specific ifr estimates
#' [@verity2020estimates].
#' Changing either $\gamma$ or $\mu$ affects both the rate at which
#' individuals recover or die and the proportions which recover or die.
#' As such, the implications of the individual parameter values are hard to
#' intuit without recourse to the other. Here, we choose a different
#' parameterisation that makes it simpler to set these parameters to
#' appropriate values:
#' 
#' \begin{align}
#'     \gamma &= \zeta (1-\text{IFR})\\
#'     \mu &= \zeta \text{IFR}.
#' \end{align}
#' 
#' The proportion of infected individuals who go on to die due to infection
#' is given by the ratio of the rate of death to the overall rate out from the
#' infectious compartment, which is the definition of the IFR:
  
#' \begin{equation}
#'     \frac{\zeta \text{IFR}}{\zeta \text{IFR} + \zeta (1-\text{IFR})}
#'     = \text{IFR}.
#' \end{equation}
#' 
#' The average duration spent in the infectious compartment is given by:
#'   
#' \begin{equation}
#'     \frac{1}{\zeta \text{IFR} + \zeta (1-\text{IFR})} = \frac{1}{\zeta}.
#' \end{equation}

#' The duration of the infectious period is again difficult to estimate and
#' likely varies over variants. The level of infectiousness is also likely to
#' change over the infectious period. We will assume this time to be period to
#' last about 9 days, which is based on the base variant
#' [@MACINTYRE21].
#' As such, we set $\zeta = 1/9$.
#' 
#' The R0 of the base variant is estimated to be 2.4
#' [@ferguson2020report]. Estimates of R0 for the delta variant range between
#' 3.2 and 7, so we'll estimate it as 5 [@Liu21, @Meng21, @Burki21]. It is likely to soon for an
#' accurate estimate of R0 for omicron, especially with many confounding
#' variables like vaccine escape, etc., but a preliminary report puts it
#' around 10 [@Burki21].
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
  
  # load ifr data
  load("../data/infection_fatality_ratio.rda")
  
  if (agestructured){
    #load ifr vector depending on age
    ifr <- infection_fatality_ratio$age_structured$fatality_ratio
    l <- 9
  }
  else{
    #load single scalar value
    ifr <- infection_fatality_ratio$overall
    l <- 1
  }
  
  # set zeta, which is the same for all variants
  zeta <- 1/9
  gamma <- zeta * ( 1- ifr)
  mu <- zeta * ifr
  
  if (variant=="base"){
    R0 <- 2.4
    kappa <- rep(1/5.5,l)
  }
  
  if (variant=="delta"){
    R0 <- 5
    kappa <- rep(1/3.7,l)
  }
  
  if (variant=="omicron"){
    R0 <- 10
    kappa <-rep(1/2,l)
  }
  
  final_list <- list(kappa = kappa, gamma = gamma, mu = mu, R0 = R0)
  return(final_list)
}








