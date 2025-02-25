install.packages("readr")
library(readr)
df<-read_tsv("risks_tests_20.tsv")

#verwijderen degene die 'NA' bij MOCA hebben staan
df_zonder_MOCA_NA <- df[!is.na(df$MOCA), ]



#verschil berekenen tussen beide cognitieve testen
df_zonder_MOCA_NA$verschil_cogn_test<- df_zonder_MOCA_NA$`MMSE`- df_zonder_MOCA_NA$`MOCA`

#verschil berekenen tussen data van afname cognitieve test
df_zonder_MOCA_NA$verschil_dag_testafn <- df_zonder_MOCA_NA$`MOCA_date`- df_zonder_MOCA_NA$`MMSE_date`

#verschil van dagen naar jaren zetten
df_zonder_MOCA_NA$verschil_jaar_testafn <- (df_zonder_MOCA_NA$`verschil_dag_testafn`/(365))

#numeriek maken aangezien R er dagen van maaktte
df_zonder_MOCA_NA$verschil_jaar_testafn <- as.numeric(df_zonder_MOCA_NA$verschil_jaar_testafn)

#daling cognitieve test per jaar
df_zonder_MOCA_NA$decrease_by_year <- (df_zonder_MOCA_NA$verschil_cogn_test/ (df_zonder_MOCA_NA$verschil_jaar_testafn))



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
breuk_rokers_m <- paste(rokers_m, "/", n_men)
breuk_rokers_f <-paste(rokers_f,"/", n_women)

sight_m <- nrow(df_men[df_men$sight == 1, ])
sight_f <- nrow(df_women[df_women$sight == 1, ])
breuk_sight_m <- paste(sight_m, "/", n_men)
breuk_sight_f <-paste(sight_f,"/", n_women)

hearing_loss_m <- nrow(df_men[df_men$hearing_loss == 1, ])
hearing_loss_f <- nrow(df_women[df_women$hearing_loss == 1, ])
breuk_hearing_loss_m <- paste(hearing_loss_m, "/", n_men)
breuk_hearing_loss_f <-paste(hearing_loss_f,"/", n_women)

cholesterol_m <- nrow(df_men[df_men$cholesterol == 1, ])
cholesterol_f <- nrow(df_women[df_women$cholesterol == 1, ])
breuk_cholesterol_m <- paste(cholesterol_m, "/", n_men)
breuk_cholesterol_f <-paste(cholesterol_f,"/", n_women)

education_m <- nrow(df_men[df_men$education == 1, ])
education_f <- nrow(df_women[df_women$education == 1, ])
breuk_education_m <- paste(education_m, "/", n_men)
breuk_education_f <-paste(education_f,"/", n_women)

social_contact_m <- nrow(df_men[df_men$social_contact == 1, ])
social_contact_f <- nrow(df_women[df_women$social_contact == 1, ])
breuk_social_contact_m <- paste(social_contact_m, "/", n_men)
breuk_social_contact_f <-paste(social_contact_f,"/", n_women)

depression_m <- nrow(df_men[df_men$depression == 1, ])
depression_f <- nrow(df_women[df_women$depression == 1, ])
breuk_depression_m <- paste(depression_m, "/", n_men)
breuk_depression_f <-paste(depression_f,"/", n_women)

brain_injury_m <- nrow(df_men[df_men$brain_injury == 1, ])
brain_injury_f <- nrow(df_women[df_women$brain_injury == 1, ])
breuk_brain_injury_m <- paste(brain_injury_m, "/", n_men)
breuk_brain_injury_f <-paste(brain_injury_f,"/", n_women)

exercise_m <- nrow(df_men[df_men$exercise == 1, ])
exercise_f <- nrow(df_women[df_women$exercise== 1, ])
breuk_exercise_m <- paste(exercise_m, "/", n_men)
breuk_exercise_f <-paste(exercise_f,"/", n_women)

diabetes_m <- nrow(df_men[df_men$diabetes == 1, ])
diabetes_f <- nrow(df_women[df_women$diabetes== 1, ])
breuk_diabetes_m <- paste(diabetes_m, "/", n_men)
breuk_diabetes_f <-paste(diabetes_f,"/", n_women)

blood_pressure_m <- nrow(df_men[df_men$blood_pressure == 1, ])
blood_pressure_f <- nrow(df_women[df_women$blood_pressure== 1, ])
breuk_blood_pressure_m <- paste(blood_pressure_m, "/", n_men)
breuk_blood_pressure_f <-paste(blood_pressure_f,"/", n_women)

obese_m <- nrow(df_men[df_men$obese == 1, ])
obese_f <- nrow(df_women[df_women$obese== 1, ])
breuk_obese_m <- paste(obese_m, "/", n_men)
breuk_obese_f <-paste(obese_f,"/", n_women)



#gemiddeldes
average_age_men_MOCA <- round(mean(df_men$age_at_MOCA), 2)
average_age_women_MOCA <- round(mean(df_women$age_at_MOCA), 2)

average_MMSE_men <- round(mean(df_men$MMSE), 2)
average_MOCA_men <- round(mean(df_men$MOCA), 2)

average_MMSE_women <- round(mean(df_women$MMSE), 2)
average_MOCA_women <- round(mean(df_women$MOCA), 2)  

average_decrease_m <- round(mean(df_men$decrease_by_year), 5)
average_decrease_f <- round(mean(df_women$decrease_by_year), 5)
                          
                          
tabel_data <- tibble(
  Variable = c("Number of individuals","Average MMSE score", "Average age (at MoCA)", "Average MoCA score","Decrease by year", "Smoking", "Sight", 'Hearing loss', "Cholesterol", "Education", "Social contact", "Depression", "Brain injury", "Exercise", "Diabetes", "Blood pressure", "Obese"),
  Men = c(n_men,average_MMSE_men, average_age_men_MOCA,average_MOCA_men,average_decrease_m, breuk_sight_m,breuk_hearing_loss_m, breuk_cholesterol_m, breuk_education_m, breuk_social_contact_m, breuk_depression_m, breuk_brain_injury_m,breuk_exercise_m, breuk_diabetes_m,breuk_blood_pressure_m,breuk_obese_m    ),
  Women = c(n_women,average_MMSE_women, average_age_women_MOCA,average_MOCA_women, average_decrease_f,breuk_rokers_f, breuk_sight_f, breuk_hearing_loss_f,breuk_cholesterol_f, breuk_education_f, breuk_social_contact_f, breuk_depression_f, breuk_brain_injury_f, breuk_exercise_f, breuk_diabetes_f,breuk_blood_pressure_f, breuk_obese_f  )
)


tabel_data |> 
  gt() |> 
  tab_header(
    title = "Healthy elderly cohort description"
  ) |> 
  tab_spanner(
    label = "Gender",
    columns = c(Men, Women)
  )

