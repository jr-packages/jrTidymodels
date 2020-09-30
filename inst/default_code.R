library("dplyr")
data(Default, package = "ISLR")
#nolint start
Default = tibble::as_tibble(Default) %>%
  mutate(default_numeric = as.numeric(default) - 1) %>%
  relocate(default_numeric, .after = default)
save(Default, file = "data/Default.RData")
#nolint end
