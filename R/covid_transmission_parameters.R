#' Returns transmission parameter values for gamma, mu, kappa and R0 given a specific
#' variant
#' 
#' Parameters are returned in vector form for age-structured models, and
#' scalar form for non-age-structured models.
#' 
#' 1/kappa is the typical latent period, i.e. the period in which an individual
#' is infected but not infectious. The latent period for the base variant is
#' estimated to be 5.5 days (Xin et al. 2021). The latent period for the Delta variant
#' is estimated to be around 3.7 days (Li et al. 2021). The incubation period for the
#' Omicron variant is estimated to be 3 days (Jansen 2021), but no estimate for the
#' latent period is currently available. Because the incubation period of the
#' delta variant is estimated to be approximately 0.6 days longer than its
#' latent period (Grant et al. 2021), we assume the latent period for omicron to
#' be 2.4 days. We thus obtain values of kappa for each of the
#' variants: for base kappa = 1/5.5, for Delta kappa = 1/3.7, and for
#' omicron kappa = 1/2.4.
#' 
#' We assume that on average 0.66\% of individuals infected with the virus die
#' (Verity et al. 2020): this is termed an infection fatality ratio (IFR).
#' For age-structured models, we use age-specific IFR estimates
#' (Verity et al. 2020).
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
#' (MacIntyre et al. 2021). So, we set zeta = 1/9.
#' 
#' The R0 of the base variant is assumed to be 2.4 in Report 9  (Ferguson et al. 2020).
#' Estimates of R0 for the delta variant range between
#' 3.2 and 7, so we estimate it as 5 (Liu et al. 2021; Meng et al. 2021; Burki 2021).
#' Estimating R0 for Omicron is difficult with the many confounding variables like vaccine escape,
#' but a preliminary guess is 10 (Burki 2021).
#' 
#' @source The following are a list of sources used for parameter estimates:
#'
#' Xin, Hualei, et al. "Estimating the latent period of coronavirus disease 2019
#' (COVID-19)." Clinical Infectious Diseases: an Official Publication of the
#' Infectious Diseases Society of America (2021).
#' 
#' Li, Baisheng, et al. "Viral infection and transmission in a large well-traced
#' outbreak caused by the Delta SARS-CoV-2 variant." MedRxiv (2021).
#' 
#' Jansen, Lauren. "Investigation of a SARS-CoV-2 B. 1.1. 529 (Omicron) Variant
#' Cluster—Nebraska, November–December 2021." MMWR. Morbidity and Mortality
#' Weekly Report 70 (2021).
#' 
#' Grant, Rebecca, et al. "Impact of SARS-CoV-2 Delta variant on incubation,
#' transmission settings and vaccine effectiveness: Results from a nationwide
#' case-control study in France." The Lancet Regional Health-Europe (2021): 100278.
#' 
#' Verity, Robert, et al. "Estimates of the severity of coronavirus disease 2019:
#' a model-based analysis." The Lancet infectious diseases 20.6 (2020): 669-677.
#' 
#' MacIntyre, C. Raina, Valentina Costantino, and Mallory Trent. "Modelling of
#' COVID-19 vaccination strategies and herd immunity, in scenarios of limited
#' and full vaccine supply in NSW, Australia." Vaccine (2021).
#' 
#' Ferguson, Neil, et al. "Report 9: Impact of non-pharmaceutical interventions
#' (NPIs) to reduce COVID19 mortality and healthcare demand." (2020).
#' 
#' Liu, Ying, and Joacim Rocklöv. "The reproductive number of the Delta variant
#' of SARS-CoV-2 is far higher compared to the ancestral SARS-CoV-2 virus."
#' Journal of travel medicine (2021).
#' 
#' Zhang, Meng, et al. "Transmission dynamics of an outbreak of the COVID-19
#' Delta variant B. 1.617. 2—Guangdong Province, China, May–June 2021." China
#' CDC Weekly 3.27 (2021): 584.
#' 
#' Burki, Talha Khan. "Omicron variant and booster COVID-19 vaccines." The Lancet
#' Respiratory Medicine (2021).
#' 
#' @param variant a string specifying the variant of SARS-CoV-2: base, delta
#' or omicron.
#' @param agestructured a boolean indicating whether or not the model is
#' age-structured
#' 
#' @return lists of numbers or (for gamma and mu for age-structured models) dataframes
#' representing transmission parameter values
#' 
covid_transmission_parameters <- function(variant = "base", is_age_structured = FALSE){
  # check if variant is valid
  if (!(variant %in% c("base","delta","omicron"))){
    return("ERROR: The specified variant is not valid. You must choose from base, delta and omicron.")
  }
  # check if agestructured is a boolean
  if (!(is.logical(is_age_structured))){
    return("ERROR: The argument agestructured must be a boolean (TRUE or FALSE).")
  }
  
  if (is_age_structured){
    # load ifr vector depending on age
    ifr <- infection_fatality_ratio$age_structured$fatality_ratio
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
    kappa <- 1 / 5.5
  }
  
  if (variant=="delta"){
    R0 <- 5
    kappa <- 1 / 3.7
  }
  
  if (variant=="omicron"){
    R0 <- 10
    kappa <- 1 / 2.4
  }
  
  if(is_age_structured) {
    base <- infection_fatality_ratio$age_structured %>% 
      dplyr::select(-.data$fatality_ratio)
    gamma <- base %>% 
      dplyr::mutate(gamma=gamma)
    mu <- base %>% 
      dplyr::mutate(mu=mu)
  }
  
  final_list <- list(R0 = R0, kappa = kappa, gamma = gamma, mu = mu)
  return(final_list)
}
