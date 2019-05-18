### This file will contain the main code.

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(knitr)
source("~/Documents/keys.R")

##API Call
repTable <- function(address1){
base_uri <- "https://www.googleapis.com/civicinfo/v2/"
resource <- "representatives?"

query_params <- list(key = google.key, address = address1)
response <- GET(paste0(base_uri, resource), query = query_params)

response_content <- content(response, "text")

parsed_data <- fromJSON(response_content)
offices <- parsed_data$offices
offices <- flatten(offices)
officials <- parsed_data$officials
officials <- flatten(officials)

input <- parsed_data$normalizedInput

state_input <- input$state

officials <- select(officials, name, party, emails, phones, urls, photoUrl)

office <- tidyr::unnest(offices, officialIndices)
colnames(office)[1] <- "office"

data <- mutate(officials, officialIndices = strtoi(row.names(officials)) - 1) %>% left_join(office)

#Selecting the columns to be displayed
final_data <- select(data, office, name, party, emails, phones, photoUrl)

#Renaming the columns of the data
colnames(final_data)[1] <- "Position"
colnames(final_data)[2] <- "Name"
colnames(final_data)[3] <- "Party"
colnames(final_data)[4] <- "Email"
colnames(final_data)[5] <- "Phone"
colnames(final_data)[6] <- "Photo"

data <- data %>% replace(.=="NULL", "")

final_data <- final_data %>% replace(is.na(final_data), "NULL")

final_data <- final_data %>% replace(.=="NULL", "-")

final_data <- final_data %>% replace(.=="NA", "-")

final_data$Name <- paste0("[", final_data$Name, "](", data$urls, ")")

final_data$Photo <- paste0("![alt text](", final_data$Photo, ")")

final_data <- final_data %>% replace(.=="![alt text]()", "-")

final_data <- final_data %>% replace(.=="NA", "-")

final_data <- final_data %>% replace(.=="![alt text](-)", "-")

return(final_data)
}
