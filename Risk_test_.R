install.packages("readr")
library(readr)
df<-read_tsv("risks_tests_20.tsv")


#verschil berekenen tussen beide cognitieve testen
df$verschil_cogn_test<- df$`MMSE`- df$`MOCA`



#verschil berekenen tussen data van afname cognitieve test
df$verschil_dag_testafn <- df$`MOCA_date`- df$`MMSE_date`

#verschil van dagen naar jaren zetten
df$verschil_jaar_testafn <- (df$`verschil_dag_testafn`/(365))

#numeriek maken aangezien R er dagen van maaktte
df$verschil_jaar_testafn <- as.numeric(df$verschil_jaar_testafn)



#daling cognitieve test per jaar
df$decrease_by_year <- (df$verschil_cogn_test/ (df$verschil_jaar_testafn))

library(tidyverse)


#grafiek maken: cognitieve achteruitgang linken met BP (ja/nee)
df_selected <- df |>
  filter(`MOCA`<26, `verschil_cogn_test`>=3)

df_selected |> 
  ggplot(aes(x = factor(blood_pressure), y = decrease_by_year)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("-1" = "low BP", "1" = "high BP")) +
  labs(
  title="Cognitive decline per year",
  subtitle = "Comparison Between Low and High Blood Pressure"
)


#vergelijken op cholesterol
df_selected |> 
  ggplot(aes(x = factor(cholesterol), y = decrease_by_year)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("-1" = "low cholesterol", "1" = "high cholesterol")) +
  labs(
    title="Cognitive decline per year",
    subtitle = "Comparison Between Low and High cholesterol"
  )

#alle boxplotten tesamen om te kijken welke RF eruit springt
df_tesamen<- df|> 
  select(hearing_loss, cholesterol, education, depression, social_contact, 
         brain_injury, sight, exercise, diabetes, smoking, blood_pressure, 
         obese, decrease_by_year) |>
  gather(key = "risicofactoren", value = "waarde", -decrease_by_year) |> 
  filter(waarde == 1)


ggplot(df_tesamen, aes(x = risicofactoren, y = decrease_by_year)) +
  geom_boxplot() + 
  labs(
    title = "Influence of RF on cognetive tests",
       x = "Risk factors",
       y = "Decrease by year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#GRAFIEK MAKEN
library(ggplot2)

ggplot(df, aes(x = decrease_by_year)) +
  geom_histogram(color = "black", fill= "grey", binwidth=0.1)+
  geom_line(data = df[df$blood_pressure == 1, ], 
            aes(y = ..count..), stat = "bin", color = "purple", size = 1)+
  geom_line(data = df[df$cholesterol == 1, ], 
            aes(y = ..count..), stat = "bin", color = "yellow", size = 1)+
  geom_line(data = df[df$obese == 1, ], 
            aes(y = ..count..), stat = "bin", color = "red", size = 1)+
  geom_line(data = df[df$sight == 1, ], #1 of -1??
            aes(y = ..count..), stat = "bin", color = "blue", size = 1)+
  geom_line(data = df[df$hearing_loss == 1, ], 
            aes(y = ..count..), stat = "bin", color = "pink", size = 1)+
  geom_line(data = df[df$education == -1, ], #-1 want willen degene met lage eduation
            aes(y = ..count..), stat = "bin", color = "green", size = 1)+
  labs(
    x = "Average cognitive deterioration per year",
    y = "Number of people ", 
    color = "blood pressure"
  )


#nu selecteren op mensen met score lager dan 26 en verschil >3: 
library(ggplot2)

ggplot(df_selected, aes(x = decrease_by_year)) +
  geom_histogram(color = "black", fill= "grey", binwidth=0.1, na.rm = TRUE)+
  geom_line(data = df_selected[df_selected$hearing_loss == 1, ], 
            aes(y = ..count..), stat = "bin", color = "pink", size = 1)+
  geom_line(data = df_selected[df_selected$cholesterol == 1, ], 
            aes(y = ..count..), stat = "bin", color = "yellow", size = 1)+
  geom_line(data = df_selected[df_selected$sight == 1, ], #1 of -1??????
            aes(y = ..count..), stat = "bin", color = "blue", size = 1)+
  labs(
    x = "Average cognitive deterioration per year",
    y = "Number of people ", 
  )
