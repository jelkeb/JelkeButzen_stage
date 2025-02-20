library(readxl)
library(tidyverse)
data_helel_cohort <- read_excel("Documents/data_helel_cohort.xlsx")
View(data_helel_cohort)

data_helel_cohort |>
  ggplot(aes(x=gender, y=`age at MOCA 3`)) + 
  geom_boxplot() +
  labs(
      title="Average age for taking the MOCA test",
      subtitle="Subdivided by gender"
  )

x <- data_helel_cohort |>
  gather(key="test",value="waardes",`MMSE 1`,MOCA)

View(x)

x |>
  ggplot(aes(x=test, y=waardes))+
  geom_boxplot()+
  labs(
    title="Average score of various cognitive tests"
  )


data_helel_cohort$verschil <- data_helel_cohort$`MMSE 1`- data_helel_cohort$MOCA


data_helel_cohort |>
  ggplot(aes(x=gender, y=verschil))+
  geom_boxplot()+
  labs(
    title="Substraction of both cognitive test",
    subtitle = "Subdivided by gender"
  )

data_helel_cohort$cognitive_by_year <- (data_helel_cohort$verschil/(25-15))
view(data_helel_cohort)

data_helel_cohort |>
  select(-...10)

view(data_helel_cohort)  
