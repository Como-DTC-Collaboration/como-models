# script to compare the two versions of the urban vs. rural SEIRD models
# set wd to folder with this file in it
setwd("~/Documents/GitHub/DTC/como-models/R")
source("SEIR_rural_urban.R")
source("SEIR_rural_urban_2.R")
library(deSolve)
library(glue)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(readxl)
library(viridis)
library(matlib)

# load contact matrices
load("../Data/contact_all_urban.rdata")
contact_all_urban <- contact_all # ROWS are age of person, COLUMNS are age of contact
names_urban <- names(contact_all_urban)
load("../Data/contact_all_rural.rdata")
contact_all_rural <- contact_all
names_rural <- names(contact_all_rural)
names_common <- intersect(names_urban,names_rural)

#import all demographic data
# data on percentage of the population that is rural
country_pop_rural <- read_excel("../Data/Country_ruralpop_data.xlsx", sheet = "Data", skip = 3)
# conversion table from 3 letter country codes to full names
code_to_country <-  read_excel("../Data/Country_totalpop_data.xlsx", sheet = "Metadata - Countries")
# demographic breakdown into 5 year age categories
country_pop_byage <- read_excel("../Data/Populationper5yearagegroup.xlsx", sheet = "ESTIMATES", skip = 16)

# set parameters for both models, as equal as possible
# parameters they have in common
b = 0.15 # probability of infection
k = 0.33 # 1/(incubation period in days)
g = 0.1 # 1/(days between infection and recovery)
m = 0.03 # probability of death, cases-fatality ratio.
C_def = 0 # connectedness parameter

countries <- c("BDI","ETH","AFG","GIN","MLI","ROU","TUN","SAU","BRN","NLD")

store_R0 <- c()
store_phiY <- c()
store_R0_basic <- c()
store_comp <- c()

C_range <- seq(0,1,0.001)
n_C <- length(C_range)
R0_of_C <- c()
countries_store <- c()

for (n in 1:length(countries)){
  C <- C_def
  country = countries[n]
  print(country)
  # get fraction of population that is rural
  country_rural <- country_pop_rural$`2019`[country_pop_rural$`Country Code` == country]
  frac_rural <- country_rural/100 #turn percentage into fraction
  store_phiY[n] <- as.character(frac_rural)

  # get fraction of population in each 5-year age range
  # assume that these relative quantities are the same in urban and rural areas
  # probably not true but don't have data on age ranges otherwise
  country_fullname <- code_to_country$`TableName`[code_to_country$`Country Code` == country]
  print(country_fullname)
  pop_byage_2019 <- country_pop_byage[(country_pop_byage$`Region, subregion, country or area *` == country_fullname & country_pop_byage$`Reference date (as of 1 July)` == 2019),9:29]
  pop_byage_2019 <- as.double(pop_byage_2019)
  pop_byage_2019 <- pop_byage_2019/sum(pop_byage_2019) #normalized to fractions of population per age group
  # must sum last few entries because contact matrices have an 80+ category instead of 100+
  pop_byage_2019 <- c(pop_byage_2019[1:15], sum(pop_byage_2019[16:20]))

  # create model with NO migration, but with cross-community infection  
  model_nomig <- new("SEIR_rural_urban")

  # set initial conditions, same in both models
  start_infected_urban = 0
  start_infected_rural = 0
  S0U = 1-frac_rural - start_infected_urban
  E0U = 0
  I0U = start_infected_urban
  R0U = 0
  S0Y = frac_rural - start_infected_rural
  E0Y = 0
  I0Y = start_infected_rural
  R0Y = 0
  initial_conditions(model_nomig) <- list(S0U, E0U, I0U, R0U,
                                        S0Y, E0Y, I0Y, R0Y)

  # set contact matrices
  mat_urban = contact_all_urban[[country]]
  mat_rural = contact_all_rural[[country]]
  contact_matrices(model_nomig) <- list(mat_urban,mat_rural)

  #set demographic data
  pop_urban = pop_byage_2019*(1-frac_rural)
  pop_rural = pop_byage_2019*frac_rural
  country_demog(model_nomig) <- list(pop_urban,pop_rural)

  tend = 80
  time = seq(0, tend, by = 1)
  transmission_parameters(model_nomig) <- list(b, k, g, m, C)
  out_nomig <- run(model_nomig, time)

  #compute R0
  NU <- sum(rowSums(mat_urban) * (pop_urban/sum(pop_urban))) / 2
  print(NU)
  NY <- sum(rowSums(mat_rural) * (pop_rural/sum(pop_rural))) / 2
  print(NY)
  K <- ((1-frac_rural)* NU + frac_rural*NY)
  
  matF <- matrix(data = c(0, 0, b*S0U*(K*C + NU/(1-frac_rural)*(1-C)),              b*S0U*K*C,
                          0, 0,              b*S0Y*K*C,                b*S0Y*(K*C + NY/frac_rural*(1-C)),     
                          0, 0,                 0,                                   0,
                          0, 0,                 0,                                   0), 
              nrow = 4, ncol = 4, byrow = TRUE)
  matV <- matrix(data = c(k, 0, 0, 0,
                          0, k, 0, 0,
                          -k, 0, (g+m), 0,
                          0, -k, 0, (g+m)),
                 nrow = 4, ncol = 4, byrow = TRUE)
  
  store_R0[n] <- round(max(abs(eigen(matF %*% inv(matV))$values)),digits = 4)
  store_R0_basic[n] <- round(b*((1-frac_rural)* NU + frac_rural*NY)/(g+m),digits=4)
  if (store_R0[n] < store_R0_basic[n]){
    store_comp[n] <- c("\u2193")
  } else {
    store_comp[n] <- c("\u2191")
  } 
  
  n1 <- (n-1)*n_C + 1
  n2 <- n*n_C
  countries_store[n1:n2] <- rep(country,n_C)
  for (i in 1:n_C){
    C <- C_range[i]
    SQR <- ((1-frac_rural)*(K*C + NU/(1-frac_rural)*(1-C)))^2 + (frac_rural*(K*C + NY/frac_rural*(1-C)))^2 - 2*((1-frac_rural)*(K*C + NU/(1-frac_rural)*(1-C)))*(frac_rural*(K*C + NY/frac_rural*(1-C))) + 4*(K*C)^2*(frac_rural)*(1-frac_rural)
    Lplus <- 0.5*b*(1-frac_rural)/(g+m)*(K*C + NU/(1-frac_rural)*(1-C)) + 0.5*b*frac_rural/(g+m)*(K*C + NY/frac_rural*(1-C)) + b/(2*(g+m))*sqrt(SQR)
    Lmin <- 0.5*b*(1-frac_rural)/(g+m)*(K*C + NU/(1-frac_rural)*(1-C)) + 0.5*b*frac_rural/(g+m)*(K*C + NY/frac_rural*(1-C)) - b/(2*(g+m))*sqrt(SQR)
    n3 <- (n-1)*length(C_range) + i
    R0_of_C[n3] <- max(abs(Lplus),abs(Lmin))
  }
}


countries2 <- c("BDI \n 86.6%R","ETH \n 78.8%R","AFG \n 74.2%R","GIN \n 63.5%R","MLI \n 56.9%R","ROU \n 45.9%R","TUN \n 30.7%R","SAU \n 15.9%R","BRN \n 22.1%R","NLD \n 8.1%R")
df <- data.frame (country  = rep(countries2,2), model = c(rep("basic",length(countries)),rep("UvsR",length(countries))), R0 = c(store_R0_basic,store_R0), relative = c(rep("",length(countries)),store_comp))

ggplot(data=df, aes(x=country, y=R0, fill = model)) +
  geom_bar(stat="identity",position=position_dodge()) + 
  scale_x_discrete(limits=countries2) +
  geom_abline(slope=0, intercept=1,  col = "red") +
  geom_text(aes(label = R0), colour = "white", size = 2,
            vjust = 1.5, position = position_dodge(.9)) +
  geom_text(aes(label = relative), colour = "white", size = 2,
            vjust = 3.5, position = position_dodge(.9)) + 
  ggtitle("R0 of SEIRD and urban vs rural model for different countries")


df2 <- data.frame(C = rep(C_range,length(countries)), R0 = R0_of_C, country = countries_store)
df2$country <- factor(df2$country, levels = countries)
ggplot(data = df2, aes(x = C, y = R0, color = country)) +
  geom_line() + 
  ggtitle("R0 for different values of C, by country")



