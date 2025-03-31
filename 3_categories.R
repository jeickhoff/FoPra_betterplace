# dataset of categories with their 400 most relevant projects
library(httr)
library(jsonlite)
library(dplyr)

results <- data.frame(category_id = integer(), project_id = integer(), stringsAsFactors = FALSE)

# Loop through category IDs from 1 to 84 (once for page 1,2,3,4 and 5)
for (category_id in 1:84) {
  # Construct URL
  url <- paste0("https://api.betterplace.org/de/api_v4/search?page=1&per_page=200&category_id=", category_id)
    # Make API request
  response <- GET(url)
  data <- content(response, as = "text")
    # Convert JSON to list
  json_data <- fromJSON(data)
      # Check if there are projects in the response
  if (!is.null(json_data$data) && length(json_data$data$id) > 0) {
    # Extract project IDs
    project_ids <- json_data$data$id
        # Create a temporary dataframe with project IDs and category ID
    temp_df <- data.frame(category_id = category_id, project_id = project_ids)
        # Append to results dataframe
    results <- bind_rows(results, temp_df)
  }
   print(paste("Processed category", category_id))
   Sys.sleep(runif(1, 1, 2))  # Random sleep between 1 and 2 seconds
}

table(results$category_id)
write.csv(results, "betterplace_API_200d_categories.csv", row.names = FALSE)

# merging with cateory names (manual transcript)
library(readxl)
manual_categories <- read_excel("manuelle_Kategoriesammlung.xlsx")
categories_200d <- read.csv("betterplace_API_200d_categories.csv")
category_df <- left_join(categories_200d, manual_categories, by = join_by(category_id))
table(category_df$category)
# deleting all irrelevant categories
category_df$relevant_categ_id <- category_df$category_id
category_df$relevant_categ_id[grepl("Weihnachten", category_df$category)] <- 0 # remove anything with Weihnachten
category_df$relevant_categ_id[category_df$category_id == 31|category_df$category_id == 37|category_df$category_id == 38] <- 0 # removing specific soccer clubs and cities
category_df$relevant_categ_id[grepl("Beliebteste", category_df$category)] <- 0 
category_df$relevant_categ_id[grepl("Lieblingsprojekte 2019", category_df$category)] <- 0 
category_df$relevant_categ_id[grepl("Aus Berlin", category_df$category)] <- 0  # alle geografischen Angaben (kann auch über city rausgefunden werden)
category_df$relevant_categ_id[grepl("Projekte in Leipzig", category_df$category)] <- 0 
category_df$relevant_categ_id[grepl("Baden-Württemberg", category_df$category)] <- 0 
category_df$relevant_categ_id[category_df$category_id == 57] <- 0 # removing Varel hilft - Matching Fonds der Stadt Varel
category_df$relevant_categ_id[grepl("Haltung zeigen", category_df$category)] <- 0 
category_df$relevant_categ_id[grepl("cuts - state funding", category_df$category)] <- 0 #keine Kategorie per se
category_df$relevant_categ_id[grepl("Selbstorganisation", category_df$category)] <- 0 #keine Kategorie per se
category_df$relevant_categ_id[grepl("NA", category_df$category)] <- 0 
category_df$relevant_categ_id[grepl("Giving Tuesday", category_df$category)] <- 0 
category_df$relevant_categ_id[grepl("Projekte ohne Kategorie", category_df$category)] <- 0 
category_df$relevant_categ_id[grepl("Seenotrettung", category_df$category)] <- 0 # seenotrettung is too small to be kept (8 projects in total)
category_df$relevant_categ_id[grepl("LGBTQIA+", category_df$category)] <- 0 # LGBTQIA+ is too small to be kept (67 projects in total, meaning that there are no more projects of this topic)
category_df$relevant_categ_id[grepl("Coronavirus Nothilfe", category_df$category)] <- 0  # Coronavirus Nothilfe is too specific

category_df <- category_df[category_df$relevant_categ_id != 0, ]

#combining similar categories
# combining every Obdachlosenhilfe (category 10)
category_df$category_id[grepl("Obdachlosenhilfe", category_df$category)] <- 10 
category_df$category[grepl("Obdachlosenhilfe", category_df$category)] <- "Obdachlosenhilfe"
# combining every Nothilfe (category 23, originally Nothilfe Syrien)
category_df$category_id[grepl("Nothilfe", category_df$category)] <- 23 
category_df$category[grepl("Nothilfe", category_df$category)] <- "Nothilfe"
category_df$category_id[category_df$relevant_categ_id == 55] <- 55

# Joining all types of Katastrophenschutz (7) (possibly identical to Nothilfe)
category_df$category_id[grepl("Erdbeben", category_df$category)] <- 7 # Erdbeben Türkei & Syrien
category_df$category[grepl("Erdbeben", category_df$category)] <- "Katastrophenschutz"
category_df$category_id[grepl("Hochwasserhilfe", category_df$category)] <- 7 # Hochwasserhilfe
category_df$category[grepl("Hochwasserhilfe", category_df$category)] <- "Katastrophenschutz"
category_df$category_id[grepl("Hunger", category_df$category)] <- 7 # Hunger in Ostafrika
category_df$category[grepl("Hunger", category_df$category)] <- "Katastrophenschutz"
category_df$category_id[grepl("Trinkwasser", category_df$category)] <- 7 # Trinkwasser
category_df$category[grepl("Trinkwasser", category_df$category)] <- "Katastrophenschutz"

# joining Integration und Geflüchtete (8)
category_df$category_id[grepl("Integration", category_df$category)] <- 8 
category_df$category[grepl("Integration", category_df$category)] <- "Geflüchtete"
# joining Tiere (1) und Haustiere
category_df$category_id[grepl("Haustiere", category_df$category)] <- 1 
category_df$category[grepl("Haustiere", category_df$category)] <- "Tiere"
# joining Umwelt und Klimaschutz (12)?
category_df$category_id[grepl("Umwelt", category_df$category)] <- 12 
category_df$category[grepl("Umwelt", category_df$category)] <- "Klimaschutz"
# joining Politische Bildung +10%, Politische Bildung and Bildung (3)
category_df$category_id[grepl("Bildung", category_df$category)] <- 3 
category_df$category[grepl("Bildung", category_df$category)] <- "Bildung"

table(category_df$category)

category_df <- category_df %>% rename(id = project_id)
category_df$number_of_projects <- NULL
category_df$relevant_categ_id <- NULL
table(duplicated(category_df)) #remove duplicates
category_df <- category_df[!duplicated(category_df), ]

write.csv(category_df, "betterplace_200d_categories.csv", row.names = FALSE)

bp_2024 <- read.csv("betterplace_2024_march_24th.csv")
category_lm <- inner_join(category_df, bp_2024, by = join_by(id))
write.csv(category_lm, "betterplace_with_categories_final_2403.csv", row.names = FALSE)

rm(category_df, category_lm, categories_200d)
rm(url, manual_categories, json_data, results, response, temp_df, data, project_ids, category_id)

# how many projects does the data set contain?
length(unique(bp_categories_2024$id))
