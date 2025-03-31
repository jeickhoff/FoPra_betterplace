library(mapview)
citation("mapview")
bp_2024 <- read.csv("betterplace_2024_march_24th.csv")
bp_24_small <- bp_2024[, c("id", "longitude", "latitude", "activated_at_y", "title", "donated_amount_in_euro", "progress_percentage", "donor_count", "country", "germany", "sentiment_score_sum")]

# donation amounts regionally
# only 11 observations are above 100 000€; 33 observations are above 50 000€
bp_24_small_10tsd_e <- subset(bp_24_small,bp_24_small$donated_amount_in_euro <= 10000) # removes 314 observations
hist(bp_24_small_10tsd_e$donated_amount_in_euro) 
bp_24_small_5tsd_e <- subset(bp_24_small,bp_24_small$donated_amount_in_euro <= 5000) # removes 752 observations
bp_24_small_1tsd_e <- subset(bp_24_small,bp_24_small$donated_amount_in_euro <= 1000) # removes 2749 observations
mapview(bp_24_small_10tsd_e, xcol = "longitude", ycol = "latitude",zcol = "donated_amount_in_euro", legend = TRUE, layer.name = "Spendengelder in € (2024)", col.regions = rev(viridis::viridis(2593)), 
        alpha = 0, cex = 2, crs = 4326, grid = FALSE, type = "p")

# progress percentage regionally
bp_24_small_100_percent <- subset(bp_24_small,bp_24_small$progress_percentage <= 100)
mapview(bp_24_small_100_percent, xcol = "longitude", ycol = "latitude",zcol = "progress_percentage", legend = TRUE, layer.name = "Spendenfortschritt in % (2024)", col.regions = rev(viridis::viridis(100)), 
        alpha = 0, cex = 2, crs = 4326, grid = FALSE, type = "p")

# donor count regionally
bp_24_small_500_donors <- subset(bp_24_small,bp_24_small$donor_count <= 500)
bp_24_small_100_donors <- subset(bp_24_small,bp_24_small$donor_count <= 100)
mapview(bp_24_small_100_donors, xcol = "longitude", ycol = "latitude",zcol = "donor_count", legend = TRUE, layer.name = "Anzahl der Spender_innen (2024)", col.regions = rev(viridis::viridis(100)), 
        alpha = 0, cex = 2, crs = 4326, grid = FALSE, type = "p")

# seniment scores regionally
qnt <- quantile(bp_24_small$sentiment_score_sum, probs=c(.25, .75), na.rm = T)
iqr <- IQR(bp_24_small$sentiment_score_sum, na.rm = T)
lower <- qnt[1] - 1.5*iqr
upper <- qnt[2] + 1.5*iqr
table(!is.na(bp_24_small$sentiment_score_sum) & (bp_24_small$sentiment_score_sum < lower | bp_24_small$sentiment_score_sum > upper))
table(bp_24_small$sentiment_score_sum[!is.na(bp_24_small$sentiment_score_sum) & (bp_24_small$sentiment_score_sum < lower | bp_24_small$sentiment_score_sum > upper)])
bp_24_small$sentiment_score_sum <- ifelse(bp_24_small$sentiment_score_sum > upper | bp_24_small$sentiment_score_sum < lower, NA, bp_24_small$sentiment_score_sum)
table(is.na(bp_24_small$sentiment_score_sum))

mapview(bp_24_small, xcol = "longitude", ycol = "latitude",zcol = "sentiment_score_sum", legend = TRUE, layer.name = "Sentiment Scores (2024)", col.regions = rev(viridis::viridis(100)), 
        alpha = 0, cex = 2, crs = 4326, grid = FALSE, type = "p")

# combining all maps of 2024
  mapview(bp_24_small_500_donors, xcol = "longitude", ycol = "latitude",zcol = "donor_count", legend = TRUE, layer.name = "Anzahl der Spender_innen (2024)", col.regions = rev(viridis::viridis(100)), 
          alpha = 0, cex = 2, crs = 4326, grid = FALSE, type = "p") +
  mapview(bp_24_small_100_percent, xcol = "longitude", ycol = "latitude",zcol = "progress_percentage", legend = TRUE, layer.name = "Spendenfortschritt in % (2024)", col.regions = rev(viridis::viridis(100)), 
          alpha = 0, cex = 2, crs = 4326, grid = FALSE, type = "p") +
  mapview(bp_24_small_10tsd_e, xcol = "longitude", ycol = "latitude",zcol = "donated_amount_in_euro", legend = TRUE, layer.name = "Spendengelder in € (2024)", col.regions = rev(viridis::viridis(2593)), 
          alpha = 0, cex = 2, crs = 4326, grid = FALSE, type = "p") +
    mapview(bp_24_small_1tsd_e, xcol = "longitude", ycol = "latitude",zcol = "donated_amount_in_euro", legend = TRUE, layer.name = "Spendengelder in € (2024)", col.regions = rev(viridis::viridis(2593)), 
            alpha = 0, cex = 2, crs = 4326, grid = FALSE, type = "p")


# did the platform become more international over time?
bp <- read.csv("betterplace_03_19.csv")
bp_small <- bp[, c("id", "longitude", "latitude", "activated_at_y", "title")]
mapview(bp_small, xcol = "longitude", ycol = "latitude",zcol = "activated_at_y", legend = TRUE, layer.name = "Projekte über die Jahre", col.regions = rev(viridis::viridis(19)), 
        alpha = 0, cex = 2, crs = 4326, grid = FALSE, type = "p")

# how many foreign projects
table(bp_24_small$germany)
sort(table(bp_24_small$country)) # where are they exactly
sort(table(bp$country))

# the development of foreign and local projects over the years
library(ggplot2)
agg_data <- bp %>%
  group_by(activated_at_y, germany) %>%
  summarise(count = n(), .groups = "drop")
par(family = "times", font = 2, font.lab = 2, font.axis = 2)
ggplot(agg_data, aes(x = activated_at_y, y = count, fill = factor(germany, labels = c("Ausland", "Deutschland")), group = germany)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = c("skyblue", "tomato"), name = "Region") +
  labs(x = "Jahr", y = "Anzahl der Projekte", title = "Anzahl der deutschen und ausländischen Projekte pro Jahr") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(agg_data, aes(x = activated_at_y, y = count, fill = factor(germany, labels = c("Ausland", "Deutschland")), group = germany)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = c("skyblue", "tomato"), name = "Region") +
  labs(x = "Jahr", y = "Anzahl der Projekte", title = "Anzahl der deutschen und ausländischen Projekte pro Jahr") +
  theme_minimal() +  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "times", face = "bold"),  # Set font for axis labels
    axis.text.y = element_text(family = "times", face = "bold"),
    axis.title = element_text(family = "times", face = "bold"),
    plot.title = element_text(family = "times", face = "bold", size = 14),
    legend.text = element_text(family = "times", face = "bold"),
    legend.title = element_text(family = "times", face = "bold")
  )



# country-based count of projects
library(mapview)
library(countrycode)
# harmonizing german and english country names
world$country_de <- countrycode(world$name, origin = "country.name", destination = "cldr.short.de")
bp_24_small_foreign <- subset(bp_24_small, bp_24_small$germany != 1)
country_counts <- bp_24_small_foreign %>% 
  group_by(country) %>%
  summarise(project_count = n())
world_data <- left_join(world, country_counts, by = c("country_de" = "country"))
library(viridis)
mapview(world_data, zcol = "project_count",
        legend = TRUE, layer.name = "Projekte pro Land (2024)", col.regions = rev(viridis::viridis(10)), 
        crs = 4326, grid = FALSE, type = "p",
        alpha.regions = 0.7, na.color = "white")

