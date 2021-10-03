---
title: "2021 olympics"
author: "Esben Lykke Skovgaard"
date: "25/9/2021"
output: 
  prettydoc::html_pretty:
    keep_md: true
  theme: architect
editor_options:
  chunk_output_type: console
---




Import data

```r
medals <- read_excel(here("data", "medals.xlsx")) %>% 
  clean_names() %>% 
  mutate(team_noc = factor(team_noc),
         across(where(is.character), as.numeric))

athletes <- 
  read_excel(here("data", "athletes.xlsx")) %>% 
  clean_names()

coaches <- 
  read_excel(here("data", "coaches.xlsx")) %>% 
  clean_names()

gender <- 
  read_excel(here("data", "entriesgender.xlsx")) %>% 
  clean_names()

teams <- 
  read_excel(here("data", "teams.xlsx")) %>% 
  clean_names()

pop_country <- 
  read_csv(here("data", "population_by_country_2020.csv")) %>% 
  clean_names()

athlete_events <- 
  read_csv(here("data", "athlete_events.csv")) %>% 
  clean_names()

noc_regions <- 
  read_csv(here("data", "noc_regions.csv")) %>% 
  clean_names()

tokyoo_2021 <- 
  read_csv(here("data", "Tokyo 2021 dataset.csv")) %>% 
  clean_names()
```

Medals awarded per nation
# ```{r}
# medals %>%
#   mutate(
#     team_noc = fct_reorder(team_noc, total)
#   ) %>%
#   slice_max(team_noc, n = 15) %>% 
#   ggplot(aes(team_noc, total)) +
#   geom_segment(aes(x = 0, xend = total, yend = total, color = if_else(team_noc == "Japan", "orange", "grey50"))) + #TODO fix plot
#   geom_point(aes(team_noc, total)) +
#   coord_flip()
# ```