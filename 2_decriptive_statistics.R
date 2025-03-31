library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidytext)
bp <- read.csv("betterplace_projects_unique.csv")

#### Cleaning & deleting variables ####
# deleting irrelevant variables
summary(bp$tax_deductible) # true for every single project
bp$tax_deductible <- NULL
table(bp$negative_opinions_count) # zero for every single project
bp$negative_opinions_count <- NULL
table(bp$comments_count) # zero for every single project
bp$comments_count <- NULL
table(is.na(bp$around_distance)) # NA for all but 16 projects
bp$around_distance <- NULL
table(is.na(bp$profile_picture.fallback)) # NA for all but 680 projects
table(is.na(bp$matching_events)) # NA for all projects
bp$matching_events <- NULL 
bp$contact.picture.fallback <- NULL 
bp$carrier.picture.fallback <- NULL 
bp$contact.picture.links <- NULL 
bp$carrier.picture.links <- NULL 
bp$links <- NULL 
bp$contact.links <- NULL 
bp$carrier.links <- NULL 
bp$profile_picture.links <- NULL 
bp$profile_picture.fallback <- NULL 

bp$opinion_test <- ifelse(bp$positive_opinions_count == bp$donations_count, 1, 0)
table(bp$opinion_test) # the variable bp$positive_opinions_count is entirely identical to bp$donations_count
bp$opinion_test <- NULL
bp$positive_opinions_count <- NULL


# adapting date variables and adding one each that only contains the year
# what is the difference between updated_at and content_updated_at?
# completed at displays the DateTime of the moment the project was fully funded (100% progress_percentage).
# closed_at: DateTime when the project was closed by the project manager.

date_columns <- c("created_at","updated_at","content_updated_at","activated_at", "completed_at", "closed_at")
for (col in date_columns) {
  bp[[col]] <- substring(bp[[col]], 1, 10)  # Extract YYYY-MM-DD
  bp[[col]] <- as_date(bp[[col]]) #transform it into a date
  bp[[paste0(col, "_y")]] <- as.numeric(year(bp[[col]]))  # Extract only the year and transform to numeric
  }
rm(date_columns,col)
# checking result
class(bp$activated_at_y)
table(bp$created_at)

head(bp$description) # <br> <div> <strong> are often in the text. delete anything inside <>
bp$description <- gsub("<.*?>", " ", bp$description)
bp$description <- gsub("\\s+", " ", bp$description) # no unnecessary white spaces
bp$description <- gsub("&nbsp;", "", bp$description) # &nbsp;

head(bp$summary) # <br> <div> <strong> are often in the text. delete anything inside <>
bp$summary <- gsub("<.*?>", " ", bp$summary)
bp$summary <- gsub("\\s+", " ", bp$summary) # no unnecessary white spaces
bp$summary <- gsub("&nbsp;", "", bp$summary) # &nbsp;

#### Creating new variables ####

# create a new variable that subtracts start and finish date to find out, how long the project was online
bp$days_active <- ifelse(is.na(bp$closed_at),ymd(20250226)-bp$activated_at,bp$closed_at-bp$activated_at) # on the 26.02.25 I retrieved the dataset

# create a € variable
bp$donated_amount_in_euro <- bp$donated_amount_in_cents/100

# Germany vs not Germany
table(bp$country)
bp$germany <- ifelse(bp$country == "Deutschland", 1, 0) 
table(bp$germany) #43256
# longitude 38.9 is in syria, even though I have the filter germany = T applied
bp$germany[bp$latitude == 34.802075] #some of the projects are wrongfully marked as germany...
bp$country[bp$latitude == 34.802075]
bp$city[bp$latitude == 34.802075 & bp$carrier.country == "Deutschland" & bp$country == "Deutschland"]
bp$id[bp$latitude == 34.802075 & bp$carrier.country == "Deutschland" & bp$country == "Deutschland"] # id 148720, 73399 and 119418 are in Syria, but coded as Germany
bp$country[bp$latitude == 34.802075 & bp$carrier.country == "Deutschland" & bp$country == "Deutschland"] <- "Syrien"
bp$city[bp$latitude == 34.802075 & bp$carrier.country == "Deutschland" & bp$country == "Deutschland"] <- "Syrien"
bp$germany <- ifelse(bp$country == "Deutschland", 1, 0) 
table(bp$germany) #43253


# summary and description to lower case
bp$description <- tolower(bp$description) # all characters to lower case
bp$summary <- tolower(bp$summary)
bp$title <- tolower(bp$title)

# creating a variable that indicates if the organisation has multiple projects online (as a proxy for estabishment and outreach)
bp <- bp %>%
  group_by(carrier.id) %>%
  mutate(projects_per_carrier_count = n()) %>%
  ungroup()
table(bp$projects_per_carrier_count)

# save Zwischenstand
write.csv(bp, "betterplace_03_23.csv", row.names = FALSE) # all the above executed Stand 19.03. 
bp <- read.csv("betterplace_03_23.csv")

#### First descriptive overview ####
# extrafont::font_import() #loading fonts to match TNR
extrafont::loadfonts(device = "win")
windowsFonts(times = windowsFont("Times New Roman"))# set TNR as default
par(family = "times", font = 2, font.lab = 2, font.axis = 2)

sum(bp$donated_amount_in_cents[bp$year == 2024]) # amount donated to projects started in 2024 (not to be confused with donations in 2024)
sum(bp$donations_count[bp$year == 2024]) # individual donations to projects that started in 2024 (not to be confused with donations in 2024)
sum(bp$donated_amount_in_cents[bp$year == 2024]) / sum(bp$donations_count[bp$year == 2024]) #average donation amount (~126€)

# analysis of project over the years
library(ggplot2)
par(mar=c(4,5.5,4,5.5)+.1, family = "times", font = 2, font.lab = 2, font.axis = 2) # sets the width of the margins in the order: 'bottom', 'left', 'top', 'right'.
table(bp$activated_at_y)
yearly_published_projects <- barplot(table(bp$activated_at_y), xlab = "Jahr", ylab = "Anzahl neuer Projekte", 
        main = "Anzahl jährlich neu veröffentlichter Projekte auf betterplace.org",
       axis.lty = 1, cex.names = 0.8, cex.axis = 0.8, las = 1, col = "white", cex.main = 0.9, cex.lab = 0.7)

mean(diff(table(bp$activated_at_y[bp$activated_at_y != 2025]))) # durchschnittliche Veränderung von Jahr zu Jahr
barplot(diff(table(bp$activated_at_y[bp$activated_at_y != 2025])),
        xlab = "Jahr", ylab = "Veränderung im Vergleich zum Vorjahr", 
        ylim = c(-1000,1500), main = "Veränderung der jährlich neu veröffentlichten Projekte",
        las = 1, col = "white") # Veränderung der Anzeigezahlen jedes Jahr
# setzt 2025 den Wachstumstrend Trend fort?
table(bp$activated_at_y[bp$activated_at_y == 2024 & bp$activated_at <= ymd(20240226)])
table(bp$activated_at_y[bp$activated_at_y == 2025])

# create Subset for 2024
bp_2024 <- subset(bp, bp$activated_at_y == 2024) # subset of all donation requests activated in 2024
write.csv(bp_2024, "betterplace_2024.csv", row.names = FALSE) # subset 2024
bp_2024 <- read.csv("betterplace_2024.csv")
# from now on all analysis only of projects started in 2024

# IQR Method for outlier detection
qnt <- quantile(bp_2024$donated_amount_in_euro, probs=c(.25, .75), na.rm = T)
iqr <- IQR(bp_2024$donated_amount_in_euro, na.rm = T)
lower <- qnt[1] - 1.5*iqr
upper <- qnt[2] + 1.5*iqr
table(!is.na(bp_2024$donated_amount_in_euro) & (bp_2024$donated_amount_in_euro < lower | bp_2024$donated_amount_in_euro > upper))
table(bp_2024$donated_amount_in_euro[!is.na(bp_2024$donated_amount_in_euro) & (bp_2024$donated_amount_in_euro < lower | bp_2024$donated_amount_in_euro > upper)])
# 673 outliers
bp_2024$donated_amount_in_euro <- ifelse(bp_2024$donated_amount_in_euro > upper | bp_2024$donated_amount_in_euro < lower, NA, bp_2024$donated_amount_in_euro)
table(is.na(bp_2024$donated_amount_in_euro))

qnt <- quantile(bp_2024$progress_percentage, probs=c(.25, .75), na.rm = T)
iqr <- IQR(bp_2024$progress_percentage, na.rm = T)
lower <- qnt[1] - 1.5*iqr
upper <- qnt[2] + 1.5*iqr
table(!is.na(bp_2024$progress_percentage) & (bp_2024$progress_percentage < lower | bp_2024$progress_percentage > upper))
table(bp_2024$progress_percentage[!is.na(bp_2024$progress_percentage) & (bp_2024$progress_percentage < lower | bp_2024$progress_percentage > upper)])
# 4 outliers
bp_2024$progress_percentage <- ifelse(bp_2024$progress_percentage > upper | bp_2024$progress_percentage < lower, NA, bp_2024$progress_percentage)

qnt <- quantile(bp_2024$donations_count, probs=c(.25, .75), na.rm = T)
iqr <- IQR(bp_2024$donations_count, na.rm = T)
lower <- qnt[1] - 1.5*iqr
upper <- qnt[2] + 1.5*iqr
table(!is.na(bp_2024$donations_count) & (bp_2024$donations_count < lower | bp_2024$donations_count > upper))
table(bp_2024$donations_count[!is.na(bp_2024$donations_count) & (bp_2024$donations_count < lower | bp_2024$donations_count > upper)])
# 534 outliers
bp_2024$donations_count <- ifelse(bp_2024$donations_count > upper | bp_2024$donations_count < lower, NA, bp_2024$donations_count)
table(is.na(bp_2024$donations_count))
bp_2024 <- bp_2024[bp_2024$id != "146345",] # very clearly showed up as an outlier on the residuals plot etc
bp_2024 <- bp_2024[bp_2024$id != "138964",] # very clearly showed up as an outlier on the residuals plot etc

# 
# how much money does the average project generate? How is the distribution?
table(bp_2024$donated_amount_in_euro) # 952 projects (952/6538 = 14.56%) generated 0€ in donations
mean(bp_2024$donated_amount_in_euro) # durchschnittlich erreichte Spendensumme: 2939.89€
mean(bp_2024$donated_amount_in_euro[bp_2024$donated_amount_in_euro != 0]) # durchschnittlich erreichte Spendensumme ohne 0€ Spenden: 3440.92€
median(bp_2024$donated_amount_in_euro) # median erreichte Summe: 681.54€
median(bp_2024$donated_amount_in_euro[bp_2024$donated_amount_in_euro != 0]) # im Median erreichte Spendensumme ohne 0€ Spenden: 1000€
options(scipen = 999)
hist(bp_2024$donated_amount_in_euro)
plot(table(bp_2024$donated_amount_in_euro[bp_2024$donated_amount_in_euro != 0]), xlim = c(0,10000))
hist(bp_2024$donated_amount_in_euro[bp_2024$donated_amount_in_euro != 0])
boxplot(bp_2024$donated_amount_in_euro, horizontal = T, main = "Spendensumme pro Projekt (€)",
                              col = "white")
quantile(bp_2024$donated_amount_in_euro)

# how many donations does the average project have? How is the distribution?
table(bp_2024$donations_count)
bp_2024$title[bp_2024$donations_count == 1006]
plot(bp_2024$donations_count)
boxplot(bp_2024$donations_count, horizontal = T, main = "Spenden pro Projekt (Anzahl)",
                         col = "white") 
quantile(bp_2024$donations_count)

# What is the average progress percentage? How is the distribution?
mean(bp_2024$progress_percentage) # 33.59%
median(bp_2024$progress_percentage) # 19%
mean(bp_2024$progress_percentage[bp_2024$progress_percentage <= 100]) # 32.5%
table(bp_2024$progress_percentage) # 470 + 42 = 512 out of 6538 (7,83%) have fulfilled their goal (100% or more)
hist(bp_2024$progress_percentage[bp_2024$progress_percentage <= 100]) # 42 projects that have over-fulfilled their goal were excluded
box_progress_perc <- boxplot(bp_2024$progress_percentage, horizontal = T, main = "Finanzierungsfortschritt (%)",
                             col = "white") 
quantile(bp_2024$progress_percentage)

# what is the average donation amount?
bp_2024$average_donation_amount <- ifelse(bp_2024$donations_count == 0, 0,bp_2024$donated_amount_in_euro/bp_2024$donations_count)
mean(bp_2024$average_donation_amount)
median(bp_2024$average_donation_amount)

rm(upper, lower, iqr, qnt)

# For how those factors are geographically distributed see "map experiments".

#### Character count of "description" ####

bp_2024$description_nchar <- nchar(bp_2024$description)
summary(bp_2024$description_nchar)
bp_2024$description_nchar_ttpo2 <- (nchar(bp_2024$description))^2

#### Pronoun analysis ####
first_s <- c("ich", "meiner", "mir", "mich", "mein", "meine",  "meines", "meinem", "meinen")
second_s <- c("du", "deiner", "dir", "dich", "dein", "deine", "deines", "deinem", "deinen")
third_s <- c("er", "sie", "es", "seiner", "ihrer","ihr", "ihm", "ihn", "sein", "ihre", "sich", "seine", "ihres", "seines", "ihrem", "seinem", "ihren", "seinen") 
# sie sieht, Sie sehen und sie sehen are difficult to differentiate 
first_p <- c("wir", "unser", "uns", "unsere", "unseres", "unserer", "unserem", "unseren")
second_p <- c("euer","euch", "eure", "eures","eurem","euren") # "ihr" is exempt due to overlap with third person (s&p)
third_p <- c("sie", "ihrer", "sich", "ihnen", "ihr","ihre", "ihres", "ihrem", "ihren")
# huge overlap in third person singular and plural: sie, ihrer, ihr (3), sich, ihre, ihres, ihrem, ihren (only ihnen is just plural)
# therefore one combinded variable, since it indicates a similar positioning
third <- c("er", "sie", "es", "seiner", "ihrer", "ihm", "ihn", "sein", "ihre", "sich", "seine", "ihres", "seines", "ihrem", "seinem", "ihren", "seinen", "ihnen")
# "ihr" is exempt due to overlap with second person plural

# let's approach this as a dictionary 
pronoun_list <- list(
  first_person_s = first_s,
  second_person_s = second_s,
  first_person_p = first_p,
  second_person_p = second_p,
  third_person_s_and_p = third)
pronoun_df <- data.frame(pronoun_type = rep(names(pronoun_list), times = sapply(pronoun_list, length)),
  word = unlist(pronoun_list, use.names = FALSE))
for (type in names(pronoun_list)) {
  pronoun_df[[type]] <- ifelse(pronoun_df$word %in% pronoun_list[[type]], 1, 0)
}

bp_dictionary <- bp_2024 %>% unnest_tokens(word, description)
bp_dictionary <- inner_join(bp_dictionary, pronoun_df, by = "word") 
bp_2024_pronoun <- bp_dictionary %>% group_by(id) %>%  
  summarize(first_person_s = sum(first_person_s, na.rm = TRUE),
            second_person_s = sum(second_person_s, na.rm = TRUE),
            third_person_s_and_p = sum(third_person_s_and_p, na.rm = TRUE),
            first_person_p = sum(first_person_p, na.rm = TRUE),
            second_person_p = sum(second_person_p, na.rm = TRUE)) %>%  # sum scores per description
  ungroup()
bp_2024 <- left_join(bp_2024, bp_2024_pronoun, by = "id")

rm(bp_dictionary, pronoun_df, pronoun_list, bp_2024_pronoun, first_p, second_p, third_p, third_s, third, second_s, first_s, yearly_published_projects, type)


#### Sentiment analysis ####

# dictionary approach to sentiment analsis (SentiWS)
library(readr)
sentiWS_positive <- read_delim("Dictionaries/SentiWS_v2.0_Positive.txt", 
                               delim = "\t", escape_double = FALSE, 
                               col_names = FALSE, trim_ws = TRUE)
sentiWS_negative <- read_delim("Dictionaries/SentiWS_v2.0_Negative.txt", 
                               delim = "\t", escape_double = FALSE, 
                               col_names = FALSE, trim_ws = TRUE)
sentiWS <- rbind(sentiWS_negative,sentiWS_positive)

sentiWS$word_class <- sub(".*\\|", "", sentiWS$X1) # put the word class into a separate variable
sentiWS$X1 <- sub("\\|.*", "", sentiWS$X1) # delete the word class from X1
sentiWS$X3 <- ifelse(is.na(sentiWS$X3), sentiWS$X1, sentiWS$X3) 

sentiWS <- sentiWS %>% mutate(has_comma = grepl(",", X3))    # create a new variable that indicates if there are multiple words in X3
sentiWS <- sentiWS %>% bind_rows(filter(sentiWS, !has_comma) %>% mutate(X3 = X1)) %>% # duplicate rows with only one word in X3 and write X1's observation into it
  select(-has_comma)  # delete the new variable

sentiWS <- sentiWS %>% separate_rows(X3, sep = ",")
sentiWS <- sentiWS %>% rename(word = X3)
sentiWS <- sentiWS %>% rename(senti_score = X2)
sentiWS <- sentiWS %>% rename(basic_word_form = X1) # Grundform
sentiWS <- sentiWS[, c("word", "senti_score", "basic_word_form","word_class")] # reordering columns
sentiWS$word <- tolower(sentiWS$word) 
sentiWS$basic_word_form <- tolower(sentiWS$basic_word_form)

#now apply the cleaned dictionary to the data set
# overall sentiment score
bp_dictionary <- bp_2024 %>% unnest_tokens(word, description)
bp_dictionary <- inner_join(bp_dictionary, sentiWS, by = "word") 
bp_2024_sentiment <- bp_dictionary %>% group_by(id) %>%  
  summarize(sentiment_score = mean(senti_score, na.rm = TRUE)) %>%  # sum scores per description
  ungroup()
bp_2024 <- left_join(bp_2024, bp_2024_sentiment, by = "id")

# positive sentiment scores
sentiWS_positive <- subset(sentiWS,sentiWS$senti_score > 0)
bp_dictionary <- bp_2024 %>% unnest_tokens(word, description)
bp_dictionary <- inner_join(bp_dictionary, sentiWS_positive, by = "word") 
bp_2024_sentiment_pos <- bp_dictionary %>% group_by(id) %>%  
  summarize(sentiment_score_pos = mean(senti_score, na.rm = TRUE)) %>%  # sum scores per description
  ungroup()
bp_2024 <- left_join(bp_2024, bp_2024_sentiment_pos, by = "id")


# negative sentiment scores
sentiWS_negative <- subset(sentiWS,sentiWS$senti_score < 0)
bp_dictionary <- bp_2024 %>% unnest_tokens(word, description)
bp_dictionary <- inner_join(bp_dictionary, sentiWS_negative, by = "word") 
bp_2024_sentiment_neg <- bp_dictionary %>% group_by(id) %>%  
  summarize(sentiment_score_neg = mean(senti_score, na.rm = TRUE)) %>%  # sum scores per description
  ungroup()
bp_2024 <- left_join(bp_2024, bp_2024_sentiment_neg, by = "id")

# metrics of dictionary-based sentiment analysis
head(bp_2024$description)
bp_2024$description[bp_2024$id == "134465"]
bp_dictionary %>% count(id) %>% summarize(avg_repeats = mean(n))  # on average, the 
    # overall sentiment score is calculated on the basis of 15.24 words (median is 13)
    # positive sentiment score is calculated on the basis of 12.42 words (median is 10)
    # negative sentiment score is calculated on the basis of 3.94 words (median is 3)

# sentiment analysis with the sentiment as sum instead of mean
# overall sentiment score
bp_dictionary <- bp_2024 %>% unnest_tokens(word, description)
bp_dictionary <- inner_join(bp_dictionary, sentiWS, by = "word") 
bp_2024_sentiment <- bp_dictionary %>% group_by(id) %>%  
  summarize(sentiment_score_sum = sum(senti_score, na.rm = TRUE)) %>%  # sum scores per description
  ungroup()
bp_2024 <- left_join(bp_2024, bp_2024_sentiment, by = "id")

# positive sentiment scores
bp_dictionary <- bp_2024 %>% unnest_tokens(word, description)
bp_dictionary <- inner_join(bp_dictionary, sentiWS_positive, by = "word") 
bp_2024_sentiment_pos <- bp_dictionary %>% group_by(id) %>%  
  summarize(sentiment_score_pos_sum = sum(senti_score, na.rm = TRUE)) %>%  # sum scores per description
  ungroup()
bp_2024 <- left_join(bp_2024, bp_2024_sentiment_pos, by = "id")

# negative sentiment scores
bp_dictionary <- bp_2024 %>% unnest_tokens(word, description)
bp_dictionary <- inner_join(bp_dictionary, sentiWS_negative, by = "word") 
bp_2024_sentiment_neg <- bp_dictionary %>% group_by(id) %>%  
  summarize(sentiment_score_neg_sum = sum(senti_score, na.rm = TRUE)) %>%  # sum scores per description
  ungroup()
bp_2024 <- left_join(bp_2024, bp_2024_sentiment_neg, by = "id")
rm(sentiWS, sentiWS_negative, sentiWS_positive, bp_dictionary, bp_2024_sentiment, bp_2024_sentiment_neg, bp_2024_sentiment_pos)

#maybe try it with stemming words beforehand. Or different dictionary / different approach


# word cloud for the top words of 2024
library(quanteda)
descriptions <- bp_2024$description
descriptions_toks <- tokens(corpus(descriptions), remove_punct = TRUE, remove_numbers = TRUE,
                            remove_symbols = TRUE, remove_separators = TRUE)
descriptions_toks <- tokens_remove(descriptions_toks, c(stopwords("de"), "projekt", "projekte", "dass", "innen"), case_insensitive = TRUE)
descriptions_dfm <- dfm(descriptions_toks)

quanteda.textplots::textplot_wordcloud(descriptions_dfm, min_size = 1, min_count = 5)


col <- sapply(seq(0.2, 1, 0.1), function(x) adjustcolor("black", x))
quanteda.textplots::textplot_wordcloud(descriptions_dfm, adjust = 0.5, random_order = FALSE, 
                   color = col, rotation = F)
rm(descriptions, descriptions_dfm, descriptions_toks)


write.csv(bp_2024, "betterplace_2024_march_24th.csv", row.names = FALSE)
bp_2024 <- read.csv("betterplace_2024_march_24th.csv")

# text bearbeiten (stopwords wegen Pronomen noch nicht raus. Aber Punktuation)
# und vielleicht sogar nur wortstämme (falls es das auf DE gibt), damit ich nur mein dein etc codieren muss 
