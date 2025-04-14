install.packages("readr")
library(readr)
df<-read_tsv("risks_tests_20.tsv")

#verwijderen degene die 'NA' bij MOCA hebben staan
df_zonder_MOCA_NA <- df[!is.na(df$MOCA), ]



#verschil berekenen tussen beide cognitieve testen
df_zonder_MOCA_NA$verschil_cogn_test<- df_zonder_MOCA_NA$`MMSE`- df_zonder_MOCA_NA$`MOCA`

#verschil berekenen tussen data van afname cognitieve test
<<<<<<< HEAD
df_zonder_MOCA_NA$verschil_dag_testafn <- df_zonder_MOCA_NA$`MOCA_date`- df_zonder_MOCA_NA$`MMSE_date`
=======
df_zonder_MOCA_NA$verschil_dag_testafn <- df_zonder_MOCA_NA$`MMSE_date`- df_zonder_MOCA_NA$`MOCA_date`
>>>>>>> 368c69e (final)

#verschil van dagen naar jaren zetten
df_zonder_MOCA_NA$verschil_jaar_testafn <- (df_zonder_MOCA_NA$`verschil_dag_testafn`/(365))

#numeriek maken aangezien R er dagen van maaktte
df_zonder_MOCA_NA$verschil_jaar_testafn <- as.numeric(df_zonder_MOCA_NA$verschil_jaar_testafn)

#daling cognitieve test per jaar
df_zonder_MOCA_NA$decrease_by_year <- (df_zonder_MOCA_NA$verschil_cogn_test/ (df_zonder_MOCA_NA$verschil_jaar_testafn))


#extra kolom zodat je mmse date minus birthyear kan doen
library(dplyr)

df_zonder_MOCA_NA <- df_zonder_MOCA_NA %>%
  mutate(Year_MMSE = as.numeric(format(as.Date(MMSE_date), "%Y")),
         age_at_MMSE = Year_MMSE - as.numeric(birthyear))


#onderscheidt maken man en vrouw
table (df_zonder_MOCA_NA$gender)

# Maak aparte datasets voor mannen en vrouwen
df_men <- df_zonder_MOCA_NA[df_zonder_MOCA_NA$gender == "m", ]
df_women <- df_zonder_MOCA_NA[df_zonder_MOCA_NA$gender == "f", ]


# tabel maken
install.packages("gridExtra")
library(gridExtra)
library(dplyr)
library(gt)

#variabeles 
n_men <- nrow(df_men)
n_women <- nrow(df_women)

rokers_m <- nrow(df_men[df_men$smoking == 1, ])
rokers_f <- nrow(df_women[df_women$smoking == 1, ])
breuk_rokers_m <- paste(rokers_m, "/", n_men, " (", 
                        round((rokers_m/n_men*100), 2), "%)", 
                        sep = "")
breuk_rokers_f <-paste(rokers_f,"/", n_women, " (", 
                       round((rokers_f/n_women*100), 2), "%)", 
                       sep = "")

sight_m <- nrow(df_men[df_men$sight == 1, ])
sight_f <- nrow(df_women[df_women$sight == 1, ])
breuk_sight_m <- paste(sight_m, "/", n_men, " (", 
                       round((sight_m/n_men*100), 2), "%)", 
                       sep = "")
breuk_sight_f <-paste(sight_f,"/", n_women, " (", 
                      round((sight_f/n_women*100), 2), "%)", 
                      sep = "")

hearing_loss_m <- nrow(df_men[df_men$hearing_loss == 1, ])
hearing_loss_f <- nrow(df_women[df_women$hearing_loss == 1, ])
breuk_hearing_loss_m <- paste(hearing_loss_m, "/", n_men, " (", 
                              round((hearing_loss_m/n_men*100), 2), "%)", 
                              sep = "")
breuk_hearing_loss_f <-paste(hearing_loss_f,"/", n_women, " (", 
                             round((hearing_loss_f/n_women*100), 2), "%)", 
                             sep = "")

cholesterol_m <- nrow(df_men[df_men$cholesterol == 1, ])
cholesterol_f <- nrow(df_women[df_women$cholesterol == 1, ])
breuk_cholesterol_m <- paste(cholesterol_m, "/", n_men, " (", 
                             round((cholesterol_m/n_men*100), 2), "%)", 
                             sep = "")
breuk_cholesterol_f <-paste(cholesterol_f,"/", n_women, " (", 
                            round((cholesterol_f/n_women*100), 2), "%)", 
                            sep = "")

education_m <- nrow(df_men[df_men$education == -1, ])
education_f <- nrow(df_women[df_women$education == -1, ])
breuk_education_m <- paste(education_m, "/", n_men, " (", 
                           round((education_m/n_men*100), 2), "%)", 
                           sep = "")
breuk_education_f <-paste(education_f,"/", n_women, " (", 
                          round((education_f/n_women*100), 2), "%)", 
                          sep = "")

social_contact_m <- nrow(df_men[df_men$social_contact == -1, ])
social_contact_f <- nrow(df_women[df_women$social_contact == -1, ])
breuk_social_contact_m <- paste(social_contact_m, "/", n_men, " (", 
                                round((social_contact_m/n_men*100), 2), "%)", 
                                sep = "")
breuk_social_contact_f <-paste(social_contact_f,"/", n_women, " (", 
                               round((social_contact_f/n_women*100), 2), "%)", 
                               sep = "")

depression_m <- nrow(df_men[df_men$depression == 1, ])
depression_f <- nrow(df_women[df_women$depression == 1, ])
breuk_depression_m <- paste(depression_m, "/", n_men, " (", 
                            round((depression_m/n_men*100), 2), "%)", 
                            sep = "")
breuk_depression_f <-paste(depression_f,"/", n_women, " (", 
                           round((depression_f/n_women*100), 2), "%)", 
                           sep = "")

brain_injury_m <- nrow(df_men[df_men$brain_injury == 1, ])
brain_injury_f <- nrow(df_women[df_women$brain_injury == 1, ])
breuk_brain_injury_m <- paste(brain_injury_m, "/", n_men, " (", 
                              round((brain_injury_m/n_men*100), 2), "%)", 
                              sep = "")
breuk_brain_injury_f <-paste(brain_injury_f,"/", n_women, " (", 
                             round((brain_injury_f/n_women*100), 2), "%)", 
                             sep = "")

exercise_m <- nrow(df_men[df_men$exercise == -1, ])
exercise_f <- nrow(df_women[df_women$exercise== -1, ])
breuk_exercise_m <- paste(exercise_m, "/", n_men, " (", 
                          round((exercise_m/n_men*100), 2), "%)", 
                          sep = "")
breuk_exercise_f <-paste(exercise_f,"/", n_women, " (", 
                         round((exercise_f/n_women*100), 2), "%)", 
                         sep = "")

diabetes_m <- nrow(df_men[df_men$diabetes == 1, ])
diabetes_f <- nrow(df_women[df_women$diabetes== 1, ])
breuk_diabetes_m <- paste(diabetes_m, "/", n_men, " (", 
                          round((diabetes_m/n_men*100), 2), "%)", 
                          sep = "")
breuk_diabetes_f <-paste(diabetes_f,"/", n_women, " (", 
                         round((diabetes_f/n_women*100), 2), "%)", 
                         sep = "")

blood_pressure_m <- nrow(df_men[df_men$blood_pressure == 1, ])
blood_pressure_f <- nrow(df_women[df_women$blood_pressure== 1, ])
breuk_blood_pressure_m <- paste(blood_pressure_m, "/", n_men, " (", 
                                round((blood_pressure_m/n_men*100), 2), "%)", 
                                sep = "")
breuk_blood_pressure_f <-paste(blood_pressure_f,"/", n_women, " (", 
                               round((blood_pressure_f/n_women*100), 2), "%)", 
                               sep = "")

obese_m <- nrow(df_men[df_men$obese == 1, ])
obese_f <- nrow(df_women[df_women$obese== 1, ])
breuk_obese_m <- paste(obese_m, "/", n_men, " (", 
                       round((obese_m/n_men*100), 2), "%)", 
                       sep = "")
breuk_obese_f <-paste(obese_f,"/", n_women, " (", 
                      round((obese_f/n_women*100), 2), "%)", 
                      sep = "")

res_m <- nrow(df_men[df_men$residence == "city", ])
res_f <-nrow(df_women[df_women$residence == "city", ])
breuk_res_m <- paste("city: ", res_m, "/", n_men, " (", 
                     round((res_m/n_men*100), 2), "%)",
                     sep = "")
breuk_res_f <- paste("city:", res_f, "/", n_women, " (", 
                     round((res_f/n_women*100), 2), "%)", 
                     sep = "")

<<<<<<< HEAD
drank_m <- nrow(df_men[df_men$alcohol_units %in% c("10-15_u/w","15-20_u/w",">20_u/w"), ])
drank_f <-nrow(df_women[df_women$alcohol_units %in% c("10-15_u/w","15-20_u/w",">20_u/w"), ])
breuk_drank_m <- paste(drank_m, "/", n_men, " (", 
                     round((res_m/n_men*100), 2), "%)",
                     sep = "")
breuk_drank_f <- paste(drank_f, "/", n_women, " (", 
                     round((res_f/n_women*100), 2), "%)", 
=======
drank_m <- nrow(df_men[df_men$alcohol_units %in% c(">20_u/w"), ])
drank_f <-nrow(df_women[df_women$alcohol_units %in% c(">20_u/w"), ])
breuk_drank_m <- paste(drank_m, "/", n_men, " (", 
                     round((drank_m/n_men*100), 2), "%)",
                     sep = "")
breuk_drank_f <- paste(drank_f, "/", n_women, " (", 
                     round((drank_f/n_women*100), 2), "%)", 
>>>>>>> 368c69e (final)
                     sep = "")

#gemiddeldes
#formule: CI=μ±Z×(σ/sqrt(n)

Z <- 1.96

<<<<<<< HEAD
=======

average_age_men_MOCA <- mean(df_men$age_at_MOCA, na.rm = TRUE)
>>>>>>> 368c69e (final)
std_dev_men_MOCA <- sd(df_men$age_at_MOCA)
ci_men <- paste(round(average_age_men_MOCA, 2), " (", 
                round(average_age_men_MOCA - Z * (std_dev_men_MOCA / sqrt(n_men)), 2), "-", 
                round(average_age_men_MOCA + Z * (std_dev_men_MOCA / sqrt(n_men)), 2), ")", 
                sep = "")


<<<<<<< HEAD
=======
average_age_women_MOCA <- mean(df_women$age_at_MOCA, na.rm = TRUE)
>>>>>>> 368c69e (final)
std_dev_women_MOCA <- sd(df_women$age_at_MOCA)
ci_women <- paste(round(average_age_women_MOCA, 2), " (", 
                  round(average_age_women_MOCA - Z * (std_dev_women_MOCA / sqrt(n_women)), 2), "-", 
                  round(average_age_women_MOCA + Z * (std_dev_women_MOCA / sqrt(n_women)), 2), ")", 
                  sep = "")

<<<<<<< HEAD

=======
average_MMSE_men <- mean(df_men$MMSE, na.rm = TRUE)
>>>>>>> 368c69e (final)
std_dev_MMSE_men <- sd(df_men$MMSE)
ci_men_MMSE <- paste(round(average_MMSE_men, 2), " (", 
                     round(average_MMSE_men - Z * (std_dev_MMSE_men/sqrt(n_men)), 2), "-", 
                     round(average_MMSE_men + Z * (std_dev_MMSE_men/sqrt(n_men)), 2), ")", 
                     sep="")


<<<<<<< HEAD
=======
average_MMSE_women <- mean(df_women$MMSE, na.rm = TRUE)
>>>>>>> 368c69e (final)
std_dev_MMSE_women <- sd(df_women$MMSE,na.rm = TRUE )
ci_women_MMSE <- paste(round(average_MMSE_women, 2), " (", 
                       round(average_MMSE_women - Z * (std_dev_MMSE_women/sqrt(n_women)), 2), "-", 
                       round(average_MMSE_women + Z * (std_dev_MMSE_women/sqrt(n_women)), 2), ")", 
                       sep="")


<<<<<<< HEAD
=======
average_MOCA_men <- mean(df_men$MOCA, na.rm = TRUE)
>>>>>>> 368c69e (final)
std_dev_MOCA_men <- sd(df_men$MOCA)
ci_men_MOCA <- paste(round(average_MOCA_men, 2), " (", 
                       round(average_MOCA_men - Z * (std_dev_MOCA_men/sqrt(n_men)), 2), "-", 
                       round(average_MOCA_men + Z * (std_dev_MOCA_men/sqrt(n_men)), 2), ")", 
                       sep="")


<<<<<<< HEAD
std_dev_MOCA_women <- sd(df_women$MOCA,na.rm = TRUE )
=======
average_MOCA_women <- mean(df_women$MOCA, na.rm = TRUE)
std_dev_MOCA_women <- sd(df_women$MOCA, na.rm = TRUE)
>>>>>>> 368c69e (final)
ci_women_MOCA <- paste(round(average_MOCA_women, 2), " (", 
                       round(average_MOCA_women - Z * (std_dev_MOCA_women/sqrt(n_women)), 2), "-", 
                       round(average_MOCA_women + Z * (std_dev_MOCA_women/sqrt(n_women)), 2), ")", 
                       sep="")


<<<<<<< HEAD
average_age_men_MMSE <- mean(df_men$age_at_MMSE)
=======
average_age_men_MMSE <- mean(df_men$age_at_MMSE, na.rm = TRUE)
>>>>>>> 368c69e (final)
std_dev_men_MMSE <- sd(df_men$age_at_MMSE)
ci_men_age_MMSE <- paste(round(average_age_men_MMSE, 2), " (", 
                         round(average_age_men_MMSE - Z * (std_dev_men_MMSE / sqrt(n_men)), 2), "-", 
                         round(average_age_men_MMSE + Z * (std_dev_men_MMSE / sqrt(n_men)), 2), ")", 
                         sep = "")


average_age_women_MMSE <- mean(df_women$age_at_MMSE, na.rm = TRUE)
std_dev_women_MMSE <- sd(df_women$age_at_MMSE,na.rm = TRUE)
ci_women_age_MMSE <- paste(round(average_age_women_MMSE, 2), " (", 
                         round(average_age_women_MMSE - Z * (std_dev_women_MMSE / sqrt(n_women)), 2), "-", 
                         round(average_age_women_MMSE + Z * (std_dev_women_MMSE / sqrt(n_women)), 2), ")", 
                         sep = "")

average_decrease_m <- round(mean(df_men$decrease_by_year), 5)
average_decrease_f <- round(mean(df_women$decrease_by_year, na.rm = TRUE), 5)
                          

#hierbij ook nog kolom invoegen zodat we 'total cohort' beschrijving krijgen
# Gehele cohort
df_total <- df_zonder_MOCA_NA
n_total <- nrow(df_total)

rokers_total <- nrow(df_total[df_total$smoking == 1, ])
breuk_rokers_total <- paste(rokers_total, "/", n_total, " (", 
                            round((rokers_total/n_total*100), 2), "%)", 
                            sep = "")

sight_total <- nrow(df_total[df_total$sight == 1, ])
breuk_sight_total <- paste(sight_total, "/", n_total, " (", 
                           round((sight_total/n_total*100), 2), "%)", 
                           sep = "")

hearing_loss_total <- nrow(df_total[df_total$hearing_loss == 1, ])
breuk_hearing_loss_total <- paste(hearing_loss_total, "/", n_total, " (", 
                                  round((hearing_loss_total/n_total*100), 2), "%)", 
                                  sep = "")

cholesterol_total <- nrow(df_total[df_total$cholesterol == 1, ])
breuk_cholesterol_total <- paste(cholesterol_total, "/", n_total, " (", 
                                 round((cholesterol_total/n_total*100), 2), "%)", 
                                 sep = "")

education_total <- nrow(df_total[df_total$education == -1, ])
breuk_education_total <- paste(education_total, "/", n_total, " (", 
                               round((education_total/n_total*100), 2), "%)", 
                               sep = "")

social_contact_total <- nrow(df_total[df_total$social_contact == -1, ])
breuk_social_contact_total <- paste(social_contact_total, "/", n_total, " (", 
                                    round((social_contact_total/n_total*100), 2), "%)", 
                                    sep = "")

depression_total <- nrow(df_total[df_total$depression == 1, ])
breuk_depression_total <- paste(depression_total, "/", n_total, " (", 
                                round((depression_total/n_total*100), 2), "%)", 
                                sep = "")

brain_injury_total <- nrow(df_total[df_total$brain_injury == 1, ])
breuk_brain_injury_total <- paste(brain_injury_total, "/", n_total, " (", 
                                  round((brain_injury_total/n_total*100), 2), "%)", 
                                  sep = "")

exercise_total <- nrow(df_total[df_total$exercise == -1, ])
breuk_exercise_total <- paste(exercise_total, "/", n_total, " (", 
                              round((exercise_total/n_total*100), 2), "%)", 
                              sep = "")

diabetes_total <- nrow(df_total[df_total$diabetes == 1, ])
breuk_diabetes_total <- paste(diabetes_total, "/", n_total, " (", 
                              round((diabetes_total/n_total*100), 2), "%)", 
                              sep = "")

blood_pressure_total <- nrow(df_total[df_total$blood_pressure == 1, ])
breuk_blood_pressure_total <- paste(blood_pressure_total, "/", n_total, " (", 
                                    round((blood_pressure_total/n_total*100), 2), "%)", 
                                    sep = "")

obese_total <- nrow(df_total[df_total$obese == 1, ])
breuk_obese_total <- paste(obese_total, "/", n_total, " (", 
                           round((obese_total/n_total*100), 2), "%)", 
                           sep = "")

res_total <- nrow(df_total[df_total$residence == "city", ])
breuk_res_total <- paste("city: ", res_total, "/", n_total, " (", 
                         round((res_total/n_total*100), 2), "%)", 
                         sep = "")

<<<<<<< HEAD
drank_total <- nrow(df_total[df_total$alcohol_units %in% c("10-15_u/w","15-20_u/w",">20_u/w"), ])
=======
drank_total <- nrow(df_total[df_total$alcohol_units %in% c(">20_u/w"), ])
>>>>>>> 368c69e (final)
breuk_drank_total <- paste(drank_total, "/", n_total, " (", 
                           round((drank_total/n_total*100), 2), "%)", 
                           sep = "")


# gemiddeles vor de extra kolom
df_total <- df_total %>%
  mutate(Year_MMSE = as.numeric(format(as.Date(MMSE_date), "%Y")),
         age_at_MMSE = Year_MMSE - as.numeric(birthyear))

average_age_total_MMSE <- mean(df_total$age_at_MMSE, na.rm = TRUE)
std_dev_total_MMSE <- sd(df_total$age_at_MMSE, na.rm = TRUE)
ci_total_age_MMSE <- paste(round(average_age_total_MMSE, 2), " (", 
                           round(average_age_total_MMSE - Z * (std_dev_total_MMSE / sqrt(n_total)), 2), "-", 
                           round(average_age_total_MMSE + Z * (std_dev_total_MMSE / sqrt(n_total)), 2), ")", 
                           sep = "")

df_total <- df_total %>%
  mutate(Year_MOCA = as.numeric(format(as.Date(MOCA_date), "%Y")),
         age_at_MOCA = Year_MOCA - as.numeric(birthyear))

average_age_total_MOCA <- mean(df_total$age_at_MOCA, na.rm = TRUE)
std_dev_total_MOCA <- sd(df_total$age_at_MOCA, na.rm = TRUE)
ci_total_age_MOCA <- paste(round(average_age_total_MOCA, 2), " (", 
                           round(average_age_total_MOCA - Z * (std_dev_total_MOCA / sqrt(n_total)), 2), "-", 
                           round(average_age_total_MOCA + Z * (std_dev_total_MOCA / sqrt(n_total)), 2), ")", 
                           sep = "")


average_MMSE_score <- mean(df_total$MMSE, na.rm = TRUE)
average_MMSE_score <- round(mean(df_total$MMSE, na.rm = TRUE), 2)
std_dev_MMSE <- sd(df_total$MMSE, na.rm = TRUE)
n_MMSE <- sum(!is.na(df_total$MMSE))  
ci_MMSE <- paste(round(average_MMSE_score, 2), " (", 
                 round(average_MMSE_score - Z * (std_dev_MMSE / sqrt(n_MMSE)), 2), "-", 
                 round(average_MMSE_score + Z * (std_dev_MMSE / sqrt(n_MMSE)), 2), ")",
                 sep = "")


average_MOCA_score <- mean(df_total$MOCA, na.rm = TRUE)
average_MOCA_score <- round(mean(df_total$MOCA, na.rm = TRUE), 2)
std_dev_MOCA <- sd(df_total$MOCA, na.rm = TRUE)
n_MOCA <- sum(!is.na(df_total$MOCA))  
ci_MOCA <- paste(round(average_MOCA_score, 2), " (", 
                 round(average_MOCA_score - Z * (std_dev_MOCA / sqrt(n_MOCA)), 2), "-", 
                 round(average_MOCA_score + Z * (std_dev_MOCA / sqrt(n_MOCA)), 2), ")",
                 sep = "")


df_total <- df_total %>%
<<<<<<< HEAD
  mutate(decrease_by_year = (average_MMSE_score - average_MOCA_score) / (Year_MOCA - Year_MMSE))
=======
  mutate(decrease_by_year = (average_MMSE_score - average_MOCA_score) / (Year_MMSE - Year_MOCA))
>>>>>>> 368c69e (final)

average_decrease_total <- round(mean(df_total$decrease_by_year, na.rm = TRUE), 5)


install.packages("tidyverse")
library(tidyverse)
install.packages("gt")
library(gt)
installed.packages()["tibble", ]
library("tibble", )
  
                  
tabel_data <- tibble(
  Variable = c("Number of individuals","Average age at MMSE", "Average MMSE score", "Average age at MoCA", "Average MoCA score","Cognitive decline per year", "Smoking", "Vision loss", 'Hearing loss', "High LDL cholesterol", "Low education", "Social isolation", "Depression", "Brain injury", "Physical inactivity", "Diabetes", "Hypertension", "Obesity", "Air pollution", "Excessive alcohol consumption"),
  Men = c(n_men,ci_men_age_MMSE,ci_men_MMSE,ci_men,ci_men_MOCA,average_decrease_m,breuk_rokers_m, breuk_sight_m,breuk_hearing_loss_m, breuk_cholesterol_m, breuk_education_m, breuk_social_contact_m, breuk_depression_m, breuk_brain_injury_m,breuk_exercise_m, breuk_diabetes_m,breuk_blood_pressure_m,breuk_obese_m, breuk_res_m,breuk_drank_m   ),
  Women = c(n_women,ci_women_age_MMSE,ci_women_MMSE,ci_women,ci_women_MOCA, average_decrease_f,breuk_rokers_f, breuk_sight_f, breuk_hearing_loss_f,breuk_cholesterol_f, breuk_education_f, breuk_social_contact_f, breuk_depression_f, breuk_brain_injury_f, breuk_exercise_f, breuk_diabetes_f,breuk_blood_pressure_f, breuk_obese_f, breuk_res_f,breuk_drank_f),
  Total = c(n_total,ci_total_age_MMSE,ci_MMSE,ci_total_age_MOCA,ci_MOCA, average_decrease_total,breuk_rokers_total, breuk_sight_total, breuk_hearing_loss_total,breuk_cholesterol_total, breuk_education_total, breuk_social_contact_total, breuk_depression_total, breuk_brain_injury_total, breuk_exercise_total, breuk_diabetes_total,breuk_blood_pressure_total, breuk_obese_total, breuk_res_total,breuk_drank_total)
)

library(gt)
library(dplyr)

tabel_data |> 
  gt() |> 
  tab_header(
    title = "Healthy elderly cohort description"
  ) |> 
  tab_spanner(
    label = "Gender",
    columns = c(Men, Women)
  ) |> 
  tab_style(
    style = cell_text(color = "purple"),
<<<<<<< HEAD
    locations = cells_body(columns = Variable, rows = Variable == "Vision loss")
=======
    locations = cells_body(columns = Variable, rows = Variable == "Physical inactivity")
>>>>>>> 368c69e (final)
  ) |> 
  tab_style(
    style = cell_text(color = "gold"),
    locations = cells_body(columns = Variable, rows = Variable == "Hearing loss")
  ) |> 
  tab_style(
    style = cell_text(color = "red"),
    locations = cells_body(columns = Variable, rows = Variable == "High LDL cholesterol")
  ) |> 
  tab_style(
    style = cell_text(color = "blue"),
    locations = cells_body(columns = Variable, rows = Variable == "Hypertension")
  ) |> 
  tab_style(
    style = cell_text(color = "pink"),
    locations = cells_body(columns = Variable, rows = Variable == "Air pollution")
  ) |> 
  tab_style(
    style = cell_text(color = "green"),
<<<<<<< HEAD
    locations = cells_body(columns = Variable, rows = Variable == "Excessive alcohol consumption")
  ) |> 
  tab_style(
    style = cell_text(color = "purple"),
    locations = cells_body(columns = Total, rows = Variable == "Vision loss")
=======
    locations = cells_body(columns = Variable, rows = Variable == "Obesity")
  ) |> 
  tab_style(
    style = cell_text(color = "purple"),
    locations = cells_body(columns = Total, rows = Variable == "Physical inactivity")
>>>>>>> 368c69e (final)
  ) |> 
  tab_style(
    style = cell_text(color = "gold"),
    locations = cells_body(columns = Total, rows = Variable == "Hearing loss")
  ) |> 
  tab_style(
    style = cell_text(color = "red"),
    locations = cells_body(columns = Total, rows = Variable == "High LDL cholesterol")
  ) |> 
  tab_style(
    style = cell_text(color = "blue"),
    locations = cells_body(columns = Total, rows = Variable == "Hypertension")
  ) |> 
  tab_style(
    style = cell_text(color = "pink"),
    locations = cells_body(columns = Total, rows = Variable == "Air pollution")
  ) |> 
  tab_style(
    style = cell_text(color = "green"),
<<<<<<< HEAD
    locations = cells_body(columns = Total, rows = Variable == "Excessive alcohol consumption")
=======
    locations = cells_body(columns = Total, rows = Variable == "Obesity")
>>>>>>> 368c69e (final)
  )


#significantie ttesten
install.packages("exactRankTests")
library(exactRankTests)
install.packages("coin")
library(coin)
install.packages("survival")
library(survival)
t.test(decrease_by_year ~ gender, data = df_zonder_MOCA_NA, alternative = "two.sided", var.equal = TRUE)

<<<<<<< HEAD

=======
library(car)

leveneTest(decrease_by_year ~ gender, data = df_zonder_MOCA_NA)
>>>>>>> 368c69e (final)


#multiple regression
numeric_columns <- sapply(df_zonder_MOCA_NA, is.numeric)
numeric_data <- df_zonder_MOCA_NA[, numeric_columns]


linear_r_model = lm(formula = decrease_by_year ~ gender + smoking + sight + hearing_loss + cholesterol + 
                      education + social_contact + depression +
                      brain_injury + exercise + diabetes +
                      blood_pressure + obese + age_at_MMSE + age_at_MOCA , 
                    data = df_zonder_MOCA_NA)
coefs <- coef(linear_r_model)

vcov_matrix <- vcov(linear_r_model)
std_errors <- sqrt(diag(vcov_matrix))

model_summary <- summary(linear_r_model)

coefs <- model_summary$coefficients[, 1] 
std_errors <- model_summary$coefficients[, 2] 
t_stats <- model_summary$coefficients[, 3]  
p_values <- model_summary$coefficients[, 4] 
<<<<<<< HEAD

=======
f_statistic <- model_summary$fstatistic[1]
f_p_value <- pf(f_statistic, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)

print(f_statistic)
print(f_p_value)
>>>>>>> 368c69e (final)

coefs_tibble <- tibble(
  Variable = c("Intercept", "Gender", "Smoking","Vision loss", "Hearing loss", "High LDL cholesterol", "Low education", "Social isolation", "Depression", "Brain injury", "Physical inactivity", "Diabetes", "Hypertension", "Obesity", "Age at MMSE", "Age at MoCA"),
  Coefficient = round(coefs,4),
  Std_Error = round(std_errors, 4),
  p_value = round(p_values,4)
)


<<<<<<< HEAD
=======

#print te tabel
>>>>>>> 368c69e (final)
library(gt)
library(dplyr)

coefs_tibble |> 
  gt() |> 
  tab_header(
    title = "test"
  )


<<<<<<< HEAD
=======
#hier terug multiple regression (hoge waarden verwijderd)
numeric_columns2 <- sapply(df_zonder_MOCA_NA, is.numeric)
numeric_data2 <- df_zonder_MOCA_NA[, numeric_columns2]


linear_r_model2 = lm(formula = decrease_by_year ~ gender + sight + hearing_loss +
                      education + depression + 
                      brain_injury + exercise + diabetes +
                      blood_pressure + age_at_MMSE + age_at_MOCA , 
                    data = df_zonder_MOCA_NA)
coefs2 <- coef(linear_r_model2)

vcov_matrix2 <- vcov(linear_r_model2)
std_errors2 <- sqrt(diag(vcov_matrix2))

model_summary2 <- summary(linear_r_model2)

coefs <- model_summary2$coefficients[, 1] 
std_errors <- model_summary2$coefficients[, 2] 
t_stats <- model_summary2$coefficients[, 3]  
p_values <- model_summary2$coefficients[, 4] 
f_statistic2 <- model_summary2$fstatistic[1]
f_p_value2 <- pf(f_statistic2, model_summary2$fstatistic[2], model_summary2$fstatistic[3], lower.tail = FALSE)

print(f_statistic2)
print(f_p_value2)

coefs_tibble2 <- tibble(
  Variable = c("Intercept", "Gender","Vision loss", "Hearing loss", "Low education", "Depression", "Brain injury", "Physical inactivity", "Diabetes", "Hypertension","Age at MMSE", "Age at MoCA"),
  Coefficient = round(coefs,4),
  Std_Error = round(std_errors, 4),
  p_value = round(p_values,4)
)



#print te tabel
library(gt)
library(dplyr)

coefs_tibble2 |> 
  gt() |> 
  tab_header(
    title = "test"
  ) 



#figuur gemaakt voor in onze ppt
tabel_data_1 <- tibble(
  Variable = c("Number of individuals","Average age at MMSE", "Average MMSE score", "Average age at MoCA", "Average MoCA score","Cognitive decline per year", 'Hearing loss', "High LDL cholesterol", "Physical inactivity", "Hypertension", "Obesity"),
  Men = c(n_men,ci_men_age_MMSE,ci_men_MMSE,ci_men, ci_men_MOCA,average_decrease_m,breuk_hearing_loss_m, breuk_cholesterol_m, breuk_exercise_m, breuk_blood_pressure_m, breuk_obese_m),
  Women = c(n_women,ci_women_age_MMSE,ci_women_MMSE,ci_women,ci_women_MOCA, average_decrease_f, breuk_hearing_loss_f,breuk_cholesterol_f, breuk_exercise_f,breuk_blood_pressure_f, breuk_obese_f),
  Total = c(n_total,ci_total_age_MMSE,ci_MMSE,ci_total_age_MOCA,ci_MOCA, average_decrease_total,breuk_hearing_loss_total,breuk_cholesterol_total, breuk_exercise_total,breuk_blood_pressure_total, breuk_obese_total)
)

tabel_data_1 |> 
  gt() |> 
  tab_header(
    title = "Cohort description"
  ) |> 
  tab_spanner(
    label = "Gender",
    columns = c(Men, Women)
  )
>>>>>>> 368c69e (final)

