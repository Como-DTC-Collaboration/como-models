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
