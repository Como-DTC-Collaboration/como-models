library(tidyverse)
library(ggplot2)
library(gridExtra)

# Load Synthetic data adopted from https://cmmid.github.io/topics/covid19/synthetic-contact-matrices.html
contacts_file <- "https://raw.githubusercontent.com/kieshaprem/synthetic-contact-matrices/master/generate_synthetic_matrices/output/syntheticcontactmatrices2020/synthetic_contacts_2020.csv"
contacts <- read.csv(contacts_file, header = T)
ages_grps <- contacts$age_contactor %>% unique()


# extract GBR data as an example
contactsGBR = contacts[contacts$iso3c=="GBR",]
contactsGBR$age_contactor <- factor(contactsGBR$age_contactor, levels=ages_grps)
contactsGBR$age_cotactee <- factor(contactsGBR$age_cotactee, levels=ages_grps)
s
# normalize for each location-setting pair
for(l in unique(contactsGBR$location_contact)){
  for(s in unique(contactsGBR$setting)){
    rows_select = contactsGBR$location_contact==l & contactsGBR$setting==s
    contacts_sum = contactsGBR$mean_number_of_contacts[rows_select] %>% sum
    contactsGBR[rows_select, "mean_number_of_contacts.norm.s.l"] = contactsGBR$mean_number_of_contacts[rows_select]/contacts_sum
  }
}

p0 <- ggplot(contactsGBR, aes(age_contactor, age_cotactee, fill=mean_number_of_contacts.norm.s.l)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(~setting + location_contact)


# plot in individual legends
xs <- split(contactsGBR,f = list(contactsGBR$setting, contactsGBR$location_contact))
grps <- names(xs)
xs[["overall.all"]] %>% head

p = list()
for(i in seq(length(grps))){
  p[[i]] <- ggplot(xs[[grps[i]]], aes(age_contactor, age_cotactee, fill=mean_number_of_contacts.norm.s.l)) +
    geom_tile() +
    scale_fill_gradient(low="white", high="red") +
    theme(axis.text.x = element_text(angle = 45), 
          legend.title  = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.3,"cm") ) +
    labs(title=grps[i])
}
pdf("GBR.location.urban.rural.age_contacts.pdf",width = 20,height = 30)
do.call(grid.arrange, c(p, list(ncol=3)))
dev.off()
  