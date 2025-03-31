# Retrieving the projects through an API 
# for the documentation see https://github.com/betterplace/betterplace_apidocs/blob/master/README.md

library(httr)
library(jsonlite)
citation("jsonlite")
library(dplyr)
library(lubridate)
library(tidyr)

#### API project Loop ####

all_projects <- list()
base_url <- "https://api.betterplace.org/de/api_v4/projects.json"
per_page <- 200 # maximum allowed per request

# Loop through the first 2 pages to access the total number of projects
for (page in 1:2) {
  # Make API request
  response <- GET(base_url, query = list(page = page, per_page = per_page))
  
  # Convert response to text and then to JSON
  project_text <- content(response, "text")
  projects_json <- fromJSON(project_text, flatten = TRUE)
  
  # Extract data and store in the list
  all_projects[[page]] <- projects_json$data
  
  # Print progress
  print(paste("Retireved page", page, "of 2"))
  
  # avoid overloading the server
  Sys.sleep(runif(1, 1, 2))  # Random sleep between 1 and 2 seconds
}


# Now find the total number of projects and loop through them
total_entries <- projects_json$total_entries # Total projects 62147, Stand 26.02.25 (10h30)
total_pages <- ceiling(total_entries / per_page) # Calculate total pages


# Loop through all pages
for (page in 261:total_pages) {
  # Make API request
  response <- GET(base_url, query = list(page = page, per_page = per_page))
  
  # Convert response to text and then to JSON
  project_text <- content(response, "text")
  projects_json <- fromJSON(project_text, flatten = TRUE)
  
  # Extract data and store in the list
  all_projects[[page]] <- projects_json$data
  
  # Print progress
  print(paste("Retireved page", page, "of", total_pages))
  
  # avoid overloading the server
  Sys.sleep(runif(1, 1, 2))  # Random sleep between 1 and 2 seconds
}

# after page 260 "Fehler: lexical error: invalid char in json text.<!DOCTYPE html><html><head>   <  (right here) ------^"
# --> redo the loop from 261 to 311 to retrieve all projects

# Combine data into a dataframe
projects_df <- bind_rows(all_projects)
projects_df <- projects_df %>%
  mutate(across(where(is.list), ~ sapply(., toString)))

# Save to CSV
write.csv(projects_df, "betterplace_projects_261_to_311.csv", row.names = FALSE)

#join the 2 dataframes
bp_1 <- read.csv("betterplace_projects_1_to_260.csv")
bp_2 <- read.csv("betterplace_projects_261_to_311.csv")
bp_projects <- rbind(bp_1,bp_2)
length(unique(bp_projects$id)) == nrow(bp_projects) # testing for duplicates
length(unique(bp_1$id)) == nrow(bp_1) # testing for duplicates
length(unique(bp_2$id)) == nrow(bp_2) # testing for duplicates
bp_1$id[unique(bp_1$id)]
summary(duplicated(bp_1))
summary(duplicated(bp_2))
summary(duplicated(bp_projects))
bp_projects2 <- distinct(bp_projects)
write.csv(bp_projects2, "betterplace_projects_unique.csv", row.names = FALSE)


