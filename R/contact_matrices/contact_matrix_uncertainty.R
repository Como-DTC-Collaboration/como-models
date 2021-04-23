# Script for studying uncertainty in contact matrices
library(socialmixr)
ages = c(0, 10, 20, 30, 40, 50, 60, 70)


# Generate 1000 bootstrap samples
data(polymod)
matrices = contact_matrix(polymod, n=1000, countries="United Kingdom", age.limits=ages)
matrices = matrices["matrices"][[1]]


# Look at the distribution of values at some selected positions
pos1_values = c()
pos2_values = c()
pos3_values = c()

for (i in 1:1000){
  pos1_values = c(pos1_values, matrices[[i]][[1]][1, 1])
  pos2_values = c(pos2_values, matrices[[i]][[1]][4, 4])
  pos3_values = c(pos3_values, matrices[[i]][[1]][1, 4])
}


# Plot the results as histograms
hist(pos1_values, main="United Kingdom, Age0--10 / Age0--10", xlab="Contact matrix value")
hist(pos2_values, main="United Kingdom, Age30--40 / Age30--40", xlab="Contact matrix value")
hist(pos3_values, main="United Kingdom, Age0--10 / Age30--40", xlab="Contact matrix value")



# Investigate the distribution between each age_group - contact_age_group pair of all bootstraps

library(ggplot2)
library(reshape2)

age.groups <- colnames(matrices[[1]]$matrix)
n.age.groups <- dim(matrices[[1]]$matrix)[1]
n.bootstrap <- 1000

# Reshape the values in matrices into a dataframe, with rows as bootstraps, columns as age_group - contact_age_group pairs  
matrices.reshape <- matrix(nrow=n.bootstrap, ncol=n.age.groups^2) %>% as.data.frame()
colnames(matrices.reshape) <- lapply(age.groups, function(i)paste0(i, "-contact_", age.groups)) %>% unlist


for(i in seq(n.bootstrap)){
  for(j in seq(n.age.groups)){
    matrices.reshape[i, ((j-1)*n.age.groups+1):(j*n.age.groups)] <- matrices[[i]]$matrix[j, ]
  }
}

matrices.reshape$bootstrap <- seq(n.bootstrap)

# Reshape into long format for ggplot
matrices.reshape.melt <- melt(matrices.reshape, id.vars = "bootstrap")
names(matrices.reshape.melt)[2] <- "age_group-contact_age_group"
matrices.reshape.melt$age.groups <- lapply(matrices.reshape.melt$`age_group-contact_age_group` %>% as.character(), function(i)strsplit(i,"-contact_")[[1]][1]) %>% unlist
matrices.reshape.melt$contact.age.groups <-lapply(matrices.reshape.melt$`age_group-contact_age_group` %>% as.character(), function(i)strsplit(i,"-contact_")[[1]][2]) %>% unlist


# ViolinPlot for visualizing the distribution of values of each age_group - contact_age_group pair
p <- ggplot(data = matrices.reshape.melt, aes(x = `age_group-contact_age_group`, y = value)) +
  geom_violin(aes(color=age.groups, fill=contact.age.groups)) +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p
