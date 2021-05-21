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

# specify country and obtain population data
# NOTE: World bank data and contact matrix data use the same three-letter country codes!
country <- "TUN"
if (!(country %in% names_common)) {
  stop(paste(country," is not a valid three-letter country code."))
}

#import all demographic data
# data on percentage of the population that is rural
country_pop_rural <- read_excel("../Data/Country_ruralpop_data.xlsx", sheet = "Data", skip = 3)
# conversion table from 3 letter country codes to full names
code_to_country <-  read_excel("../Data/Country_totalpop_data.xlsx", sheet = "Metadata - Countries")
# demographic breakdown into 5 year age categories
country_pop_byage <- read_excel("../Data/Populationper5yearagegroup.xlsx", sheet = "ESTIMATES", skip = 16)

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

# compute average contact rate for urban an rural communities
# units: number of contacts/day
# contact_rate_urban = mean(rowSums(contact_all_urban[[country]]))
# contact_rate_rural = mean(rowSums(contact_all_rural[[country]]))
# scaling_urban_to_rural = contact_rate_rural/contact_rate_urban
#res_rural = matrix(0,174,2)
#for (i in 1:174) {
#  n = names_common[i]
#  rate_urban = mean(rowSums(contact_all_urban[[n]]))
#  rate_rural = mean(rowSums(contact_all_rural[[n]]))
#  res_rural[i,] = c(rate_urban,rate_rural)
#}

#res_rural <- data.frame("urban" = res_rural[,1], "rural" = res_rural[,2])
#row.names(res_rural) <- names_common

#ggplot(res_rural, aes(x=urban, y=rural)) + geom_point() +
#  coord_cartesian(xlim = c(7, 18), ylim = c(7, 18)) +
#  geom_segment(aes(x = 0, y = 0, xend = 20, yend = 20)) +
#  geom_text(label=names_common, hjust = -0.1, vjust = -0.1)

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

# simulate both models
tend = 50
time = seq(0, tend, by = 1)
for (i in seq(0,1,0.05)) {
  C = i # connectedness parameter
  transmission_parameters(model_nomig) <- list(b, k, g, m, C)
  out_nomig <- run(model_nomig, time)
  temp_IU <- data.frame(Cvalue = C,t = time,compartment = "U",value = out_nomig$value[out_nomig$compartment == "I_U"])
  temp_IY <- data.frame(Cvalue = C,t = time,compartment = "Y",value = out_nomig$value[out_nomig$compartment == "I_Y"])
  if (i == 0){
    result <- rbind(temp_IU,temp_IY)
  } else {
   result <- rbind(result,temp_IU)
   result <- rbind(result,temp_IY)
  }
}
Cplot <- ggplot(result, aes(x = t, y = value)) +
  geom_line(aes(color = as.factor(Cvalue), linetype = compartment), size = 1.5) +
  labs(x = "time", y = "fraction of the population",
       title = glue("Urban vs rural model for different levels of connectedness"),
       subtitle = paste(country_fullname,", ", (1-frac_rural)*100, "% urban (U), ", frac_rural*100, "% rural (Y)", sep = "")) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_viridis(discrete = TRUE)
print(Cplot)



# visualize results
# model without migration, but with intercommunity infection
#i <- sapply(out_nomig, is.factor)
#out_nomig[i] <- lapply(out_nomig[i], as.character)
#SEIR_df <- subset(out_nomig, compartment != "Incidences_U" & compartment != "Deaths_U" & compartment != "Incidences_Y" & compartment != "Deaths_Y")
#SEIR_df$compartment <- factor(SEIR_df$compartment, levels = c("S_U", "E_U", "I_U", "R_U", "S_Y", "E_Y", "I_Y", "R_Y"))
#col <- c("S_U" = "blue", "E_U" = "green", "I_U" = "yellow", "R_U" = "red", "S_Y" = "lightblue1", "E_Y" = "lightgreen", "I_Y" = "lightyellow", "R_Y" = "indianred")
#SEIRplot <- ggplot(SEIR_df, aes(x = time, y = value)) +
#  geom_line(aes(color = compartment), size = 1.5) +
#  labs(x = "time", y = "fraction of the population",
#       title = glue("SEIR model with urban and rural communities")) +
#  theme(legend.position = "bottom", legend.title = element_blank()) +
#  scale_color_manual(values = col)
#print(SEIRplot)

#case_df <- subset(out_nomig, compartment == "Incidences_U" | compartment == "Deaths_U" | compartment == "Incidences_Y" | compartment == "Deaths_Y")
#for (i in 1:tend) {
#  case_df$value[case_df$time==i & case_df$compartment == "Incidences_U"] <- sum(case_df$value[case_df$compartment == "Incidences_U" & case_df$time<(i+0.1) & case_df$time>(i-0.9)])
#  case_df$value[case_df$time==i & case_df$compartment == "Incidences_Y"] <- sum(case_df$value[case_df$compartment == "Incidences_Y" & case_df$time<(i+0.1) & case_df$time>(i-0.9)])
#  case_df$value[case_df$time==i & case_df$compartment == "Deaths_U"] <- sum(case_df$value[case_df$compartment == "Deaths_U" & case_df$time<(i+0.1) & case_df$time>(i-0.9)])
#  case_df$value[case_df$time==i & case_df$compartment == "Deaths_Y"] <- sum(case_df$value[case_df$compartment == "Deaths_Y" & case_df$time<(i+0.1) & case_df$time>(i-0.9)])
#}
#case_df <- case_df[(case_df$time %% 1 == 0),]
#case_df$compartment <- factor(case_df$compartment, levels = c("Incidences_U", "Deaths_U", "Incidences_Y", "Deaths_Y"))
#col <- c("Incidences_U" = "grey28", "Deaths_U" = "black", "Incidences_Y" = "lightgray", "Deaths_Y" = "darkgray")
#inc_plot <- ggplot(case_df, aes(x = time, y = value, fill = compartment)) +
#  geom_bar(stat="identity", position = position_dodge()) +
#  labs(x = "time", y = "fraction of the population per day",
#       title = glue("Incidences and deaths")) +
#  theme(legend.position = "bottom", legend.title = element_blank()) +
#  scale_color_manual(values = col)
#print(inc_plot)

#grid.arrange(SEIRplot,inc_plot,nrow = 1)



