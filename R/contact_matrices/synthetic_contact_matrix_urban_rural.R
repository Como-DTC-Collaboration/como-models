# Investigate the values of country-specific age-structured synthetic contact matrices (rural vs urban)

library(dplyr)
library(ggplot2)
library(gridExtra)


# Load synthetic data from https://cmmid.github.io/topics/covid19/synthetic-contact-matrices.html
contacts_file <- "https://raw.githubusercontent.com/kieshaprem/synthetic-contact-matrices/master/generate_synthetic_matrices/output/syntheticcontactmatrices2020/synthetic_contacts_2020.csv"
contacts <- read.csv(contacts_file, header = T)
ages_grps <- contacts$age_contactor %>% unique()

# contacts$iso3c %>% unique()

# extract data of the target country
country = "GBR" # change to different countries
contacts1 = contacts[contacts$iso3c==country,]
contacts1$age_contactor <- factor(contacts1$age_contactor, levels=ages_grps)
contacts1$age_cotactee <- factor(contacts1$age_cotactee, levels=ages_grps)


# # normalize for each location-setting pair
# for(l in unique(contacts1$location_contact)){
#   for(s in unique(contacts1$setting)){
#     rows_select = contacts1$location_contact==l & contacts1$setting==s
#     contacts_sum = contacts1$mean_number_of_contacts[rows_select] %>% sum
#     contacts1[rows_select, "mean_number_of_contacts.norm.s.l"] = contacts1$mean_number_of_contacts[rows_select]/contacts_sum
#   }
# }



# plot in individual legends
xs <- split(contacts1,f = list(contacts1$setting, contacts1$location_contact))
grps <- names(xs)

p = list()
for(i in seq(length(grps))){
  p[[i]] <- ggplot(xs[[grps[i]]], aes(age_contactor, age_cotactee, fill=mean_number_of_contacts)) +
    geom_tile() +
    scale_fill_gradient(low="white", high="red") +
    geom_text(aes(label = round(mean_number_of_contacts,2))) +
    theme(axis.text.x = element_text(angle = 45), 
          legend.title  = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.3,"cm") ) +
    labs(title=grps[i])
}
#pdf(paste0(country, "synthetic_urban_rural_age_contacts_raw_values.pdf"),width = 20,height = 30)
do.call(grid.arrange, c(p, list(ncol=3)))
#dev.off()
  
  