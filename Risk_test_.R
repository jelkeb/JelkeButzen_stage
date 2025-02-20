install.packages("readr")
library(readr)
df<-read_tsv("Documents/stage/risks_tests.tsv")


#verschil berekenen tussen beide cognitieve testen
df$verschil_cogn_test<- df$`MMSE 1.y`- df$`MOCA 3 (per 21/01)`



#verschil berekenen tussen data van afname cognitieve test
df$verschil_dag_testafn <- df$`MOCA date`- df$`MMSE date`

#verschil van dagen naar jaren zetten
df$verschil_jaar_testafn <- (df$`verschil_dag_testafn`/(365))

#numeriek maken aangezien R er dagen van maaktte
df$verschil_jaar_testafn <- as.numeric(df$verschil_jaar_testafn)



#daling cognitieve test per jaar
df$decrease_by_year <- (df$verschil_cogn_test/ (df$verschil_jaar_testafn))



#grafiek maken: cognitieve achteruitgang linken met BP (ja/nee)
df_selected <- df |>
  filter(`MOCA 3 (per 21/01)`<26, verschil_cogn_test>=3)

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
df_selected |> 
  select(hearing_loss, cholesterol, education, depression, social_contact, 
         brain_injury, sight, exercise, diabetes, smoking, blood_pressure, 
         obese) |>
  gather(key = "risicofactoren", value = "waarde") |> 
  filter(waarde == 1)

df_selected |> 

  
  
df |> 
  ggplot(aes(x=decrease_by_year,y))
  geom_histogram()


