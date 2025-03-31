
bp_2024 <- read.csv("betterplace_2024_march_24th.csv")
library(stargazer)
library(lmtest)
# all variables that are somewhat useful to include:
  # country/ germany
  # activated_at (or days_active)
  # donated_amount_in_euro
  # donations_count
  # donor_count
  # newsletter_subscriptions_count
  # blog_post_count
  # progress_percentage
  # incomplete_need_count
  # completed_need_count
  # sentiment_score / sentiment_score_pos / sentiment_score_neg
  # description_nchar / description_nchar_ttpo2
  # projects_per_carrier_count
  # all the pronouns: first_person_s, second_person_s, third_person_s_and_p, first_person_p, second_person_p
summary(lm(donated_amount_in_euro ~ days_active, data = bp_2024))

bp_2024$activated_at <- ymd(bp_2024$activated_at)
class(bp_2024$activated_at)


#### General Regression Analysis #### 

# testing if Projects with bigger Organisations set lower donation goals
bp_2024$donation_goal_euro <- (bp_2024$open_amount_in_cents + bp_2024$donated_amount_in_cents) / 100
cor(bp_2024$donation_goal_euro, bp_2024$projects_per_carrier_count)

# which project has the most mentions of 2nd person plural pronouns?
summary(bp_2024$second_person_p)
bp_2024$description[bp_2024$second_person_p == 9]
summary(bp_2024$second_person_s)
bp_2024$description[bp_2024$second_person_p == 22]

options(scipen = 999)
# carefully consider to include or not include donations_count. If I include it, I basically only check for
# the effect of all factors on the amount per donations, not the total donations, and that is a lot less interesting

# include everything
lm_pp <- lm(progress_percentage ~ germany + days_active + projects_per_carrier_count +
           sentiment_score_pos + sentiment_score_sum + sentiment_score_neg + 
           description_nchar + first_person_s + second_person_s + third_person_s_and_p +
             first_person_p + second_person_p, data = bp_2024)
summary(lm_pp)


# testing OLS assumptions
plot(lm_pp, 1) # 1: linearity of the data. I would say is just met. The values are not evenly distributed around 0.
dwtest(lm_pp) # 1: testing for autocorrelation. is not met. DW = 1,62 with an insanely small p-value indicates a (weak) positive autocorrelation
plot(lm_pp, 1) # testing if residual errors have a mean value of 0. Met. Line is very close to lie flat on 0.
plot(lm_pp, 2) # 2+3: residuals do not follow a normal distribution -> violating the assumption of  random sampling of observations and the assumption of a conditional mean of 0
plot(lm_pp, 3) # 5: testing for homoscedasticity. residual errors have constant variance. Absolutely not met. residual points are not all equally spread out -> heteroscedastic
# -> maybe calculate log or sqrt of progress_percentage
plot(lm_pp, 5) # identifying highly influential observations: none that influence the regression result (2 point have been removed proir because of the test)
# 4: no multi-colinearity (or perfect colinearity) was tested by a corr-matrix finding no perfect colinearity
# to avoid endogeneity, blog posts and newsletter subscriptions were excluded. 
# The Error Term has Conditional Mean of Zero

# same for €
lm_da <- lm(donated_amount_in_euro ~ germany + days_active + projects_per_carrier_count +
              sentiment_score_pos + sentiment_score_sum + sentiment_score_neg + 
              description_nchar + first_person_s + second_person_s + third_person_s_and_p +
              first_person_p + second_person_p, data = bp_2024)
summary(lm_da)
# all in all a lot less conclusive.

# testing OLS assumptions
plot(lm_da, 1) # 1: linearity of the data. I would say is not met.
library(lmtest)
dwtest(lm_da) # 1: testing for autocorrelation. is not met. DW = 1,48 with an insanely small p-value indicates a positive autocorrelation
plot(lm_da, 1) # testing if residual errors have a mean value of 0. not really met.
plot(lm_da, 2) # 2+3: residuals do not follow a normal distribution -> violating the assumption of  random sampling of observations and the assumption of a conditional mean of 0
plot(lm_da, 3) # 5: testing for homoscedasticity. residual errors have constant variance. Absolutely not met. even though it is better than progress_percentage but still -> heteroscedastic
# -> maybe calculate log or sqrt of progress_percentage
plot(lm_da, 5) # identifying highly influential observations (Cook's distance)

lm_dc <- lm(donations_count ~ germany + days_active + sentiment_score_pos_sum + 
             sentiment_score_pos + sentiment_score + sentiment_score_neg_sum + sentiment_score_neg + 
             description_nchar + projects_per_carrier_count + 
             first_person_s + second_person_s + third_person_s_and_p + first_person_p + 
             second_person_p, data = bp_2024)
summary(lm_dc)

# export as stargazer
stargazer(lm_pp,lm_da, summary = TRUE, type = "html", covariate.labels = c("Achsenabschnitt","Deutschland", "Tage Online", "Projekte Pro Träger","Positiver Sentiment-Score (Durchschnitt)", "Sentiment-Score (Summe)","Negativer Sentiment-Score (Durchschnitt)","Textlänge (Zeichen)", "1. Person Sing.", "2. Person Sing.", "3. Person Sing. u. Plural", "1. Person Plural", "2. Person Plural"),
          notes = "eigene Berechnung; Darstellung mit stargazer", title = "Ergebnisse OLS Regression", style = "aer",single.row = TRUE,
          dep.var.labels = c("Finanzierungsfortschritt","Spendensumme"),
          notes.align = "l", intercept.top = TRUE, intercept.bottom = FALSE, align = TRUE, out = "reg_progress_perc_general_stargazer1.doc",
          summary.stat = c("n","mean","sd","min","max"))

citation("stargazer")

# description of all variables
lm_desc <- lm(donated_amount_in_euro ~ progress_percentage + germany + days_active + projects_per_carrier_count +
                sentiment_score_pos + sentiment_score_sum + sentiment_score_neg + 
                description_nchar + first_person_s + second_person_s + third_person_s_and_p +
                first_person_p + second_person_p, data = bp_2024)
summary(bp_2024$donated_amount_in_euro)
model_inc_all <- model.matrix(lm_desc)
stargazer(model_inc_all, summary = TRUE, type = "html", covariate.labels = c("Achsenabschnitt","Finanzierungsfortschritt","Deutschland", "Tage Online", "Projekte Pro Träger", "Positiver Sentiment-Score (Durchschnitt)", "Sentiment-Score (Summe)", "Negativer Sentiment-Score (Durchschnitt)","Textlänge", "1. Person Sing.", "2. Person Sing.", "3. Person Sing. u. Plural", "1. Person Plural", "2. Person Plural"),
          notes.align = "l", align = TRUE, title = "Variablenübersicht", out = "reg_summary_variables.doc",
          summary.stat = c("n","mean", "median","min","max"))

#### Category Regressions ####
bp_categories_2024 <- read.csv("betterplace_with_categories_final_2403.csv")
table(bp_categories_2024$category)

bp_bildung_2024 <- subset(bp_categories_2024, bp_categories_2024$category == "Bildung")
bp_klimaschutz_2024 <- subset(bp_categories_2024, bp_categories_2024$category == "Klimaschutz")
bp_tiere_2024 <- subset(bp_categories_2024, bp_categories_2024$category == "Tiere")
bp_frauen_2024 <- subset(bp_categories_2024, bp_categories_2024$category == "Frauen")
bp_sport_2024 <- subset(bp_categories_2024, bp_categories_2024$category == "Sport")
bp_geflüchtete_2024 <- subset(bp_categories_2024, bp_categories_2024$category == "Geflüchtete")
bp_obdachlosenhilfe_2024 <- subset(bp_categories_2024, bp_categories_2024$category == "Obdachlosenhilfe")
bp_menschenrechte_2024 <- subset(bp_categories_2024, bp_categories_2024$category == "Menschenrechte")
bp_kinder_2024 <- subset(bp_categories_2024, bp_categories_2024$category == "Kinder und Jugend")
bp_katstrophenschutz_2024 <- subset(bp_categories_2024, bp_categories_2024$category == "Katastrophenschutz")
bp_entwcklungsarbeit_2024 <- subset(bp_categories_2024, bp_categories_2024$category == "Entwicklungsarbeit")

lm <- lm(progress_percentage ~ germany + days_active + sentiment_score_pos_sum + 
           sentiment_score_pos + sentiment_score_sum + sentiment_score_neg_sum + sentiment_score_neg + 
           description_nchar + projects_per_carrier_count + 
           first_person_s + second_person_s + third_person_s_and_p + first_person_p + 
           second_person_p, data = bp_bildung_2024)
summary(lm)

# Durbin-Watson Test for autocorrelation (value between 0 and 4 - 2 meaning no autocorrelation -> 1.99 slight positive autocorrelation)
library(lmtest)
dwtest(lm)

# notes
# for category bildung nothing is significant
# for category klimaschutz only the projects_per_carrier_count is significant
# bp_tiere_2024 days_active and projects_per_carrier_count are significant
# bp_frauen_2024 projects_per_carrier_count significant
# bp_sport_2024 description_nchar
# bp_geflüchtete_2024 description_nchar, projects_per_carrier_count and second_person_p
# bp_obdachlosenhilfe_2024 projects_per_carrier_count significant
# bp_menschenrechte_2024 nothing significant
# bp_kinder_2024 nothing significant
# bp_katstrophenschutz_2024 nothing significant
# bp_entwcklungsarbeit_2024 nothing significant



# comparing quartiles of project categories
par(mar=c(7,5,4,1)+.1, family = "times", font = 2, font.lab = 2, font.axis = 2) # sets the width of the margins in the order: 'bottom', 'left', 'top', 'right'.
boxplot(donated_amount_in_euro ~ category, col = "white", main = "Spendensumme nach Projektkategorie",
        cex.axis = 0.8, xlab = "", ylab = "Spendensumme pro Projekt (€)", las = 2,
        data = bp_categories_2024)
dev.off()

boxplot(progress_percentage ~ category, col = "white", main = "Finanzierungsfortschritt nach Projektkategorie",
        cex.axis = 0.7, xlab = "Kategorie", ylab = "Fortschritt pro Projekt (%)",
        data = bp_categories_2024)

# checking the actual numbers from the boxplot
quantile(bp_categories_2024$donated_amount_in_euro[bp_categories_2024$category == "Obdachlosenhilfe"], na.rm = T)
quantile(bp_categories_2024$donated_amount_in_euro[bp_categories_2024$category == "Senior_innen"], na.rm = T)
quantile(bp_categories_2024$donated_amount_in_euro[bp_categories_2024$category == "Geflüchtete"], na.rm = T)
quantile(bp_categories_2024$donated_amount_in_euro[bp_categories_2024$category == "Frauen"], na.rm = T)
quantile(bp_categories_2024$donated_amount_in_euro[bp_categories_2024$category == "Kinder und Jugend"], na.rm = T)
quantile(bp_categories_2024$donated_amount_in_euro[bp_categories_2024$category == "Sport"], na.rm = T)


# transforming the donations into log() would improve the assumption of normality and get
# rid of the right-skewness. However the aov results do not change significantly and all the
# values that are 0 are being left out. Which is not great. 

par(mfrow = c(1, 2)) # combine plots
# histogram
hist(aov_cat$residuals)
# QQ-plot
library(car)
qqPlot(aov_cat$residuals,
       id = FALSE) # id = FALSE to remove point identification
par(mfrow = c(1, 1))

# category co-occurence matrix
category_matrix <- bp_categories_2024 %>%
  distinct(id, category) %>%       
  mutate(value = 1) %>%                    
  pivot_wider(names_from = category,      
              values_from = value,        
              values_fill = 0)           
category_matrix_num <- as.matrix(category_matrix[, -1])
co_occurrence_matrix <- t(category_matrix_num) %*% category_matrix_num # multiply matrix with the transposed matrix
rm(co_occurrence_matrix, category_matrix, category_matrix_num)



#### Categories as Dummy Variables: Regression ####
library(fastDummies)
bp_categories_2024$category[bp_categories_2024$category == "Kinder und Jugend"] <- "Kinder"
table(bp_categories_2024$category)
bp_categories_2024 <- dummy_cols(bp_categories_2024, select_columns = "category")
table(bp_categories_2024$category) # choose a reference category
bp_categories_2024 <- select(bp_categories_2024, -category_Bildung) # remove reference category

lm <- lm(progress_percentage ~ germany + days_active + sentiment_score_pos_sum + 
           sentiment_score_pos + sentiment_score + sentiment_score_neg_sum + sentiment_score_neg + 
           description_nchar + projects_per_carrier_count + 
           first_person_s + second_person_s + third_person_s_and_p + first_person_p + second_person_p +
           category_Frauen + category_Geflüchtete + category_Gesundheit + category_Kinder + 
           category_Klimaschutz + category_Obdachlosenhilfe + category_Sport + category_Tiere, data = bp_categories_2024)
summary(lm)

lm <- lm(donated_amount_in_euro ~ germany + days_active + sentiment_score_pos_sum + 
           sentiment_score_pos + sentiment_score + sentiment_score_neg_sum + sentiment_score_neg + 
           description_nchar + projects_per_carrier_count + 
           first_person_s + second_person_s + third_person_s_and_p + first_person_p + second_person_p +
           category_Frauen + category_Geflüchtete + category_Gesundheit + category_Kinder + 
           category_Klimaschutz + category_Obdachlosenhilfe + category_Sport + category_Tiere, data = bp_categories_2024)
summary(lm)


# testing OLS assumptions
plot(lm, 1) # 1: linearity of the data. I would say is met.
library(lmtest)
dwtest(lm) # 1: testing for autocorrelation. assumtion is met. DW = 1.9489 with an p-value of 0,099 -> no autocorrelation
plot(lm, 1) # testing if residual errors have a mean value of 0. I would say is met.
plot(lm, 2) # 2+3: residuals do not follow a normal distribution -> violating the assumption of  random sampling of observations and the assumption of a conditional mean of 0 (but better that without dummies)
plot(lm, 3) # 5: testing for homoscedasticity. residual errors have constant variance. not met. even though it is better than progress_percentage & donated_amount_€ but still -> heteroscedastic
plot(lm, 5) # identifying highly influential observations (Cook's distance)



