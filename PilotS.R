library(readxl)
TS_feb <- read_excel("TS feb.xlsx", col_types = c("text", 
                                                  "text", "text", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "text", 
                                                  "numeric"))
View(TS_feb)

library(ggplot2)
library(stringi)

install.packages("tidyr")
library(tidyr)

install.packages("dplyr")
library(dplyr)



# grafiek maken concentratie: 
ggplot(TS_feb %>% filter(!is.na(`Concentratie (pg/µL)`)), aes(x = factor(`code (D373)`), y = `Concentratie (pg/µL)`, color = type)) +
  geom_line(aes(group=type), size = 1)+
  scale_color_manual(values = c("Long (immediate)" = "blue", "Short (immediate)" = "red", "Short (frozen)"="purple","Long (frozen)"="green")) +
  labs(x = " ", y = "Concentration (pg/µL)", 
       title = "Comparison of concentrations between long and short tear fluid strips ") +
        subtitle = "Depending on their  "
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  scale_x_discrete(drop = FALSE)

"-> boxplot: concentraties op y as; 4 boxplotten naasteen 
  functie : facet_grid ofxel facet_wrap --> legendes samen en wel nog 2 delen
      variabele is frozen en no frozen"
  
  
#significante test (long en short van meteen knippen en extractie)
TS_filtered_conc <- TS_feb %>% filter(!is.na(`Concentratie (pg/µL)`))
TS_wide_conc <- TS_filtered_conc %>%
  select(`code (D373)`, `Concentratie (pg/µL)`, type) %>%
  pivot_wider(names_from = type, values_from = `Concentratie (pg/µL)`)
TS_wide_conc <- TS_wide_conc %>% filter(!is.na(long) & !is.na(short))
wilcox.test(TS_wide_conc$short, TS_wide_conc$long, paired = TRUE, alternative = "two.sided")

#significante test (long en long)
TS_filtered_LL <- TS_feb %>% 
  filter(!is.na(`Concentratie (pg/µL)`) & type %in% c("long", "VL"))
wilcox.test(`Concentratie (pg/µL)` ~ type, data = TS_filtered_LL, exact = FALSE)

#significante test (short en short)
TS_filtered_SS <- TS_feb %>% 
  filter(!is.na(`Concentratie (pg/µL)`) & type %in% c("short", "VS"))
wilcox.test(`Concentratie (pg/µL)` ~ type, data = TS_filtered_SS, exact = FALSE)






#grafiek voor MW
ggplot(TS_feb %>% filter(!is.na(`MW (g/mol)`)), aes(x = factor(`code (D373)`), y = `MW (g/mol)`, color = type)) +
  geom_line(aes(group = type), size = 1) +
  scale_color_manual(values = c("long" = "blue", "short" = "red", "VS"="purple", "VL"="green")) +
  labs(x = " ", y = "Molecular weight (g/mol)", title = "Comparison of M.W. between long and short tear fluid strips") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_x_discrete(drop = FALSE)

TS_filtered_MW <- TS_feb %>% filter(!is.na(`MW (g/mol)`))
TS_wide_MW <- TS_filtered_MW %>%
  select(`code (D373)`, `MW (g/mol)`, type) %>%
  pivot_wider(names_from = type, values_from = `MW (g/mol)`)
TS_wide_MW <- TS_wide_MW %>% filter(!is.na(long) & !is.na(short))
wilcox.test(TS_wide_MW$short, TS_wide_MW$long, paired = TRUE, alternative = "two.sided")








# grafiek voor RIne
ggplot(TS_feb %>% filter(!is.na(`RINe`)), aes(x = factor(`code (D373)`), y = `RINe`, color = type)) +
  geom_line(aes(group = type), size = 1) +
  scale_color_manual(values = c("long" = "blue", "short" = "red", "VS"="purple", "VL"="green")) +
  labs(x = " ", y = "RIN", title = "Comparison of RIN between long and short tear fluid strips") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_x_discrete(drop = FALSE)

#test
TS_filtered_R <- TS_feb %>% filter(!is.na(`RINe`))
TS_wide_R <- TS_filtered_R %>%
  select(`code (D373)`, `RINe`, type) %>%
  pivot_wider(names_from = type, values_from = `RINe`)
TS_wide_R <- TS_wide_R %>% filter(!is.na(long) & !is.na(short))
wilcox.test(TS_wide_R$short, TS_wide_R$long, paired = TRUE, alternative = "two.sided")

#significante test (long en long)
TS_filtered_LL_R <- TS_feb %>% 
  filter(!is.na(`RINe`) & type %in% c("long", "VL"))
wilcox.test(`RINe` ~ type, data = TS_filtered_LL_R, exact = FALSE)

#significante test (short en short)
TS_filtered_SS_R <- TS_feb %>% 
  filter(!is.na(`RINe`) & type %in% c("short", "VS"))
wilcox.test(`RINe` ~ type, data = TS_filtered_SS_R, exact = FALSE)
