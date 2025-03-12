library(readxl)
TS_feb <- read_excel("TS feb.xlsx", col_types = c("text", 
                                                  "text", "text", "text", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "text", 
                                                  "numeric"))
View(TS_feb)

install.packages("ggtext")
library(ggtext)

# boxplot concentraties
ggplot(TS_feb %>% filter(!is.na(`Concentration (pg/µL)`)), 
       aes(x = interaction(type, mani), 
           y = `Concentration (pg/µL)`)) +
  geom_boxplot(width = 1, position = position_dodge(width = 0.5)) + 
  labs(x = " ", 
            y = "Concentration (pg/µL)", ) +
  theme(axis.text.x = element_blank())+
  theme_minimal() +
  theme(axis.text.x = element_markdown(size = 14),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 16))

#significante test uitvoeren --> L en L
concentration_LL <- TS_feb %>%
  filter(type == "Long", !is.na(`Concentration (pg/µL)`)) %>%
  select(type, mani, `Concentration (pg/µL)`)
wilcox.test(`Concentration (pg/µL)` ~ mani, data = concentration_LL)

#significante test uitvoeren --> S en S
concentration_SS <- TS_feb %>%
  filter(type == "Short", !is.na(`Concentration (pg/µL)`)) %>%
  select(type, mani, `Concentration (pg/µL)`)
wilcox.test(`Concentration (pg/µL)` ~ mani, data = concentration_SS)









# boxplot RINe
ggplot(TS_feb %>% filter(!is.na(`RINe`)), 
       aes(x = interaction(type, mani), 
           y = `RINe`)) +
  geom_boxplot(width = 1, position = position_dodge(width = 0.5)) + 
  labs(x = " ", 
       y = "RIN", ) +
  theme(axis.text.x = element_blank())+
  theme_minimal()+
  theme(axis.text.x = element_markdown(size = 14),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 16))

#significante test uitvoeren --> L en L
RIN_LL <- TS_feb %>%
  filter(type == "Long", !is.na(`RINe`)) %>%
  select(type, mani, `RINe`)
wilcox.test(`RINe` ~ mani, data = RIN_LL)

#significante test uitvoeren --> S en S
RIN_SS <- TS_feb %>%
  filter(type == "Short", !is.na(`RINe`)) %>%
  select(type, mani, `RINe`)
wilcox.test(`RINe` ~ mani, data = RIN_SS)








# boxplot MW
ggplot(TS_feb %>% filter(!is.na(`MW (g/mol)`)), 
       aes(x = interaction(type, mani), 
           y = `MW (g/mol)`)) +
  geom_boxplot(width = 1, position = position_dodge(width = 0.5)) + 
  labs(x = " ", 
       y = "MW (g/mol)", ) +
  theme(axis.text.x = element_blank())+
  theme_minimal()

#significante test uitvoeren --> L en L
MW_LL <- TS_feb %>%
  filter(type == "Long", !is.na(`MW (g/mol)`)) %>%
  select(type, mani, `RINe`)
wilcox.test(`MW (g/mol)` ~ mani, data = MW_LL)

#significante test uitvoeren --> S en S
MW_SS <- TS_feb %>%
  filter(type == "Short", !is.na(`MW (g/mol)`)) %>%
  select(type, mani, `RINe`)
wilcox.test(`MW (g/mol)` ~ mani, data = MW_SS)




