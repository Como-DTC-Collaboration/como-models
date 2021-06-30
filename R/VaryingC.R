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
for (country in c("ETH","AFG",'TUN',"NLD")){
  # get fraction of population that is rural
  country_rural <- country_pop_rural$`2019`[country_pop_rural$`Country Code` == country]
  frac_rural <- country_rural/100 #turn percentage into fraction
  
  # get fraction of population in each 5-year age range
  # assume that these relative quantities are the same in urban and rural areas
  # probably not true but don't have data on age ranges otherwise
  country_fullname <- code_to_country$`TableName`[code_to_country$`Country Code` == country]
  pop_byage_2019 <- country_pop_byage[(country_pop_byage$`Region, subregion, country or area *` == country_fullname & country_pop_byage$`Reference date (as of 1 July)` == 2019),9:29]
  pop_byage_2019 <- as.double(pop_byage_2019)
  pop_byage_2019 <- pop_byage_2019/sum(pop_byage_2019) #normalized to fractions of population per age group
  # must sum last few entries because contact matrices have an 80+ category instead of 100+
  pop_byage_2019 <- c(pop_byage_2019[1:15], sum(pop_byage_2019[16:20]))
  
  # create model with NO migration, but with cross-community infection
  model_nomig <- new("SEIR_rural_urban")
  
  # set parameters for both models, as equal as possible
  # parameters they have in common
  b = 0.3 # probability of infection
  k = 0.2 # 1/(incubation period in days)
  g = 0.1# 1/(days between infection and recovery)
  m = 0.03 # probability of death, cases-fatality ratio.
  
  # set initial conditions, same in both models
  start_infected_urban = 0.01
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
  contact_matrices(model_nomig) <- list(contact_all_urban[[country]],contact_all_rural[[country]])
  
  #set demographic data
  country_demog(model_nomig) <- list(pop_byage_2019*(1-frac_rural),pop_byage_2019*frac_rural)
  
  # Explore role of C
  tend = 50
  time = seq(0, tend, by = 1)
  for (i in seq(0,1,0.05)) {
    C = i # connectedness parameter
    transmission_parameters(model_nomig) <- list(b, k, g, m, C)
    out_nomig <- run(model_nomig, time)
    temp_IU <- data.frame(Cvalue = C,country = country, t = time,compartment = "U",value = out_nomig$value[out_nomig$compartment == "I_U"])
    temp_IY <- data.frame(Cvalue = C,country = country, t = time,compartment = "Y",value = out_nomig$value[out_nomig$compartment == "I_Y"])
    if (i == 0 & country == "ETH"){
      result <- rbind(temp_IU,temp_IY)
    } else {
      result <- rbind(result,temp_IU)
      result <- rbind(result,temp_IY)
    }
  }
}
resultETH <- result[result$country == "ETH",]
resultAFG <- result[result$country == "AFG",]
resultTUN <- result[result$country == "TUN",]
resultNLD <- result[result$country == "NLD",]
CplotETH <- ggplot(resultETH, aes(x = t, y = value)) +
  geom_line(aes(color = as.factor(Cvalue), linetype = compartment), size = 1.5) +
  labs(x = "time", y = "fraction of the population",
       title = glue("Urban vs rural model for different levels of connectedness"),
       subtitle = "Ethiopia") +
  scale_color_viridis(discrete = TRUE)
CplotAFG <- ggplot(resultAFG, aes(x = t, y = value)) +
  geom_line(aes(color = as.factor(Cvalue), linetype = compartment), size = 1.5) +
  labs(x = "time", y = "fraction of the population",
       title = glue("Urban vs rural model for different levels of connectedness"),
       subtitle = paste("Afghanistan")) +
  scale_color_viridis(discrete = TRUE)
CplotTUN <- ggplot(resultTUN, aes(x = t, y = value)) +
  geom_line(aes(color = as.factor(Cvalue), linetype = compartment), size = 1.5) +
  labs(x = "time", y = "fraction of the population",
       title = glue("Urban vs rural model for different levels of connectedness"),
       subtitle = paste("Tunisia")) +
  scale_color_viridis(discrete = TRUE)
CplotNLD <- ggplot(resultNLD, aes(x = t, y = value)) +
  geom_line(aes(color = as.factor(Cvalue), linetype = compartment), size = 1.5) +
  labs(x = "time", y = "fraction of the population",
       title = glue("Urban vs rural model for different levels of connectedness"),
       subtitle = paste("Netherlands")) +
  scale_color_viridis(discrete = TRUE)

grid.arrange(CplotETH,CplotAFG,CplotTUN,CplotNLD,nrow = 2)



