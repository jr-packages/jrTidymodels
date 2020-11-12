library("tidyverse")
data(penguins, package = "palmerpenguins")
penguins = penguins %>%
  drop_na() %>%
  select(-island, -year)

save(penguins, file = "data/penguins.RData",
     compress = "xz",
     version = 2)
