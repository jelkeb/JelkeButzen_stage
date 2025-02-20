install.packages("tidyverse")

library(tidyverse)

install.packages(
  c("arrow", "babynames", "curl", "duckdb", "gapminder", 
    "ggrepel", "ggridges", "ggthemes", "hexbin", "janitor", "Lahman", 
    "leaflet", "maps", "nycflights13", "openxlsx", "palmerpenguins", 
    "repurrrsive", "tidymodels", "writexl")
)
install.packages("ggrepel")

1+2
2+5
10+30

library(palmerpenguins)
library(ggthemes)

palmerpenguins::penguins

print(penguins) 

glimpse(penguins)

#om overzicht vd tabel te krijgen
view(penguins)

#hier uitleg om grafiek te maken; mapping om assen te benoemen
ggplot(data = penguins)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)

#geom is om geometrical object te gebruiken om data te representeren
#point gebruiken om scatterplot te maken 
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()
#zie in console that there are 2 penguins: missing body mass or flipper lenght

#grafiek mooier maken via color toevoegen
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()

#lijnen toevoegen via lm
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method=lm)

#1 duidelijke lijn,, dus gaan color verplaatsen 
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping=aes(color = species)) +
  geom_smooth(method=lm)

#verschillende species een andere vorm geven
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping=aes(color = species, shape=species)) +
  geom_smooth(method=lm)

#grafiek vervolledigen met titels, via labs functie
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping=aes(color = species, shape=species)) +
  geom_smooth(method=lm)+
  labs(
    title="Body mass and flipper length",
    subtitle="Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x="Flipper length (mm)", y = "Body mass (g)",
    color="Species", shape = "Species"
  ) +
    scale_color_colorblind()

#histogram maken
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)

#boxplot maken
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()

#andere grafieken kijk step 1: data viualization

#meer variabelen, niet enkel kleur, maar ook island type
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))

#onderverdelen per island, gebruik facet_wrap
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)
ggsave(filename="penguins.png")
#plot bewaren onder: ggsave(filename="...")


1/200*30

#vector maken via c
primers <- c(2,3,5)

primers*2

seq(1,10)


#varibles verplaatsen via relocate() --> puntje 3
#group_by() en summerize()
#slice() om data van elkaar te scheiden 

#tidy data is data organiseren in goede manier


billboard

#pivot longer, cols is colums, 
billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank"
  )

#gebruiken om NA te verwijderen, is toch info waar we niet veel mee zijn
billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  )

#mutate gebruiken om nieuwe colom te introduceren
billboard_longer <- billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week)
  )
#om te printen/ te bekijken
billboard_longer

view(billboard_longer)

