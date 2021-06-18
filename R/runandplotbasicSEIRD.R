# script to compare the two versions of the urban vs. rural SEIRDD models
# set wd to folder with this file in it
setwd("~/Documents/GitHub/DTC/como-models/R")
source("SEIRD.R")
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
model_nomig <- new("SEIRD")

N_U <- sum(rowSums(contact_all_urban[[country]]) *
             pop_byage_2019*(1-frac_rural)) / 2
N_Y <- sum(rowSums(contact_all_rural[[country]]) *
             pop_byage_2019*frac_rural) / 2

# set parameters for both models, as equal as possible
# parameters they have in common
b = 0.3*((1-frac_rural) * N_U + frac_rural * N_Y) # probability of infection
k = 0.2 # 1/(incubation period in days)
g = 0.1# 1/(days between infection and recovery)
m = 0.03 # probability of death, cases-fatality ratio.

# set initial conditions, same in both models
start_infected = 0.01
S0 = 1 - start_infected
E0 = 0
I0 = start_infected
R0 = 0
initial_conditions(model_nomig) <- list("S0" = S0, "E0" = E0, "I0" = I0, "R0" = R0)

# Explore role of C
tend = 80
time = seq(0, tend, by = 1)
#for (i in seq(0,1,0.05)) {
#  C = i # connectedness parameter
transmission_parameters(model_nomig) <- list("beta" = b, "kappa" = k, "gamma" = g, "mu" = m)
out_nomig <- run(model_nomig, time)

# visualize results
# model without migration, but with intercommunity infection
i <- sapply(out_nomig, is.factor)
out_nomig[i] <- lapply(out_nomig[i], as.character)
SEIRD_df <- out_nomig$states
SEIRD_df$compartment <- factor(SEIRD_df$compartment, levels = c("S", "E", "I", "R"))
col <- c("S" = "blue", "E" = "green", "I" = "yellow", "R" = "red")
SEIRDplot <- ggplot(SEIRD_df, aes(x = time, y = value)) +
  geom_line(aes(color = compartment), size = 1.5) +
  labs(x = "time", y = "fraction of the population",
       title = glue("Basic SEIRDD model")) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values = col)
#print(SEIRDplot)

case_df <- out_nomig$changes
case_df$compartment <- factor(case_df$compartment, levels = c("Incidences", "Deaths"))
col <- c("Incidences" = "grey28", "Deaths" = "black")
inc_plot <- ggplot(case_df, aes(x = time, y = value, fill = compartment)) +
  geom_bar(stat="identity", position = position_dodge()) +
  labs(x = "time", y = "fraction of the population per day",
       title = glue("Incidences and deaths")) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values = col)
#print(inc_plot)

grid.arrange(SEIRDplot,inc_plot,nrow = 1)


