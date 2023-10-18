#install.packages("patentsview")
library(tidyr)
library(dplyr)
library(igraph)
library(data.table)
library(patentsview) #this package is needed for retrieving the data
library(tidyr) #this package is needed for quickly subsetting the data
#install.packages("devtools")
#devtools::install_github("stasvlasov/nstandr")
library(nstandr)
#install.packages("networkD3")
library(networkD3)

company_data = read.csv("company_data.csv")
company_data[company_data == "n.a."] <- NA
company_data = company_data[1:68, ]
company_data <- company_data %>%
  filter(Company.name.Latin.alphabet != "AUTOMOTORES Y MAQUINARIA S.A.E.C.A.")
filtered_data = company_data %>% filter(!is.na(Operating.revenue..Turnover..th.USD.Last.avail..yr))
colnames(filtered_data)

filtered_data <- na.omit(filtered_data)
filtered_data$Number.of.live.publications <- as.numeric(filtered_data$Number.of.live.publications)
filtered_data$Operating.revenue..Turnover..th.USD.Last.avail..yr <- as.numeric(gsub(",", "", filtered_data$Operating.revenue..Turnover..th.USD.Last.avail..yr))
filtered_data$Number.of.publications <- as.numeric(gsub(",", "", filtered_data$Number.of.publications))
filtered_data$Number.of.employees.Last.avail..yr <- as.numeric(gsub(",", "", filtered_data$Number.of.employees.Last.avail..yr))
# Create the linear regression model
linear_model <- lm(formula = Number.of.live.publications ~ 
                     DegreeCentrality + 
                     BetweennessCentrality + 
                     ClosenessCentrality + 
                     EigenvectorCentrality + 
                     Operating.revenue..Turnover..th.USD.Last.avail..yr + 
                     Number.of.employees.Last.avail..yr, 
                   data = filtered_data)# Print the summary of the linear regression model
summary(linear_model)
