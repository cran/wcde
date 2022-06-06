## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("wcde")

## ----eval=FALSE---------------------------------------------------------------
#  library(devtools)
#  install_github("guyabel/wcde", ref = "main")

## ---- messages = FALSE, message=FALSE-----------------------------------------
library(wcde)
# download education specific tfr data
get_wcde(indicator = "etfr",
         country_name = c("Brazil", "Albania"))

# download education specific survivorship rates
get_wcde(indicator = "eassr",
         country_name = c("Niger", "Korea"))

## -----------------------------------------------------------------------------
find_indicator(x = "tfr")

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(tidyverse)
get_wcde(indicator = "e0",
         country_name = c("Japan", "Australia")) %>%
  filter(period == "2015-2020")

get_wcde(indicator = "sexratio",
         country_name = c("China", "South Korea")) %>%
  filter(year == 2020)

## -----------------------------------------------------------------------------
wic_indicators %>%
  filter(past) %>%
  select(1:2)

## ---- messages = FALSE, message=FALSE-----------------------------------------
get_wcde(indicator = "sexratio",
         country_name = c("China", "South Korea")) %>%
  filter(year == 2020,
         age == "All")

## ---- messages = FALSE, message=FALSE-----------------------------------------
get_wcde(indicator = "tfr",
         country_name = c("U.A.E", "Espania", "Österreich"))

## ---- messages = FALSE, message=FALSE-----------------------------------------
get_wcde(indicator = "etfr", country_code = c(44, 100))

## -----------------------------------------------------------------------------
wic_locations

## ---- messages = FALSE, message=FALSE-----------------------------------------
get_wcde(indicator = "growth",
         country_name = c("India", "China"),
         scenario = c(1:3, 21, 22)) %>%
  filter(period == "2095-2100")

## ---- messages = FALSE, message=FALSE-----------------------------------------
get_wcde(indicator = "tfr",
         country_name = c("Kenya", "Nigeria", "Algeria"),
         scenario = 1:3,
         include_scenario_names = TRUE) %>%
  filter(period == "2045-2050")

## -----------------------------------------------------------------------------
wic_scenarios

## ---- messages = FALSE, message=FALSE-----------------------------------------
get_wcde(indicator = "mage")

## ---- messages = FALSE, message=FALSE-----------------------------------------
mi <- tibble(ind = c("odr", "nirate", "ggapedu25")) %>%
  mutate(d = map(.x = ind, .f = ~get_wcde(indicator = .x)))
mi

mi %>%
  filter(ind == "odr") %>%
  select(-ind) %>%
  unnest(cols = d)

mi %>%
  filter(ind == "nirate") %>%
  select(-ind) %>%
  unnest(cols = d)

mi %>%
  filter(ind == "ggapedu25") %>%
  select(-ind) %>%
  unnest(cols = d)

## -----------------------------------------------------------------------------
get_wcde(indicator = "pop", country_name = "India")

## -----------------------------------------------------------------------------
get_wcde(indicator = "pop", country_code = 900, pop_edu = "four")

## -----------------------------------------------------------------------------
get_wcde(indicator = "pop", country_code = 900, pop_edu = "six", pop_sex = "both")

## -----------------------------------------------------------------------------
w <- get_wcde(indicator = "pop", country_code = 900,
              pop_age = "all", pop_sex = "both", pop_edu = "four")
w

w <- w %>%
  mutate(pop_pm = ifelse(test = sex == "Male", yes = -pop, no = pop),
         pop_pm = pop_pm/1e3)
w

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(lemon)

w %>%
  filter(year == 2020) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = fct_rev(education))) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "black") +
  scale_x_symmetric(labels = abs) +
  scale_fill_manual(values = wic_col4, name = "Education") +
  labs(x = "Population (millions)", y = "Age") +
  theme_bw()


## -----------------------------------------------------------------------------
w <- w %>%
  mutate(pop_max = ifelse(sex == "Male", -max(pop/1e3), max(pop/1e3)))

w %>%
  filter(year == 2020) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = fct_rev(education))) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "black") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = wic_col4, name = "Education") +
  labs(x = "Population (millions)", y = "Age") +
  facet_wrap(facets = "sex", scales = "free_x", strip.position = "bottom") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))

## ---- echo=FALSE, eval=FALSE--------------------------------------------------
#  library(gganimate)
#  
#  g <- ggplot(data = w,
#         mapping = aes(x = pop_pm, y = age, fill = fct_rev(education))) +
#    geom_col() +
#    geom_vline(xintercept = 0, colour = "black") +
#    scale_x_continuous(labels = abs, expand = c(0, 0)) +
#    scale_fill_manual(values = wic_col4, name = "Education") +
#    facet_wrap(facets = "sex", scales = "free_x", strip.position = "bottom") +
#    geom_blank(mapping = aes(x = pop_max * 1.1)) +
#    theme(panel.spacing.x = unit(0, "pt"),
#          strip.placement = "outside",
#          strip.background = element_rect(fill = "transparent"),
#          strip.text.x = element_text(margin = margin(b = 0, t = 0))) +
#    transition_time(time = year) +
#    labs(x = "Population (millions)", y = "Age",
#         title = 'SSP2 World Population {round(frame_time)}')
#  
#  animate(g, width = 672, height = 520, units = "px", res = 100,
#          renderer = gifski_renderer())
#  
#  anim_save(filename = "../man/figures/world4_ssp2.gif")

## ---- eval =FALSE-------------------------------------------------------------
#  library(gganimate)
#  
#  ggplot(data = w,
#         mapping = aes(x = pop_pm, y = age, fill = fct_rev(education))) +
#    geom_col() +
#    geom_vline(xintercept = 0, colour = "black") +
#    scale_x_continuous(labels = abs, expand = c(0, 0)) +
#    scale_fill_manual(values = wic_col4, name = "Education") +
#    facet_wrap(facets = "sex", scales = "free_x", strip.position = "bottom") +
#    geom_blank(mapping = aes(x = pop_max * 1.1)) +
#    theme(panel.spacing.x = unit(0, "pt"),
#          strip.placement = "outside",
#          strip.background = element_rect(fill = "transparent"),
#          strip.text.x = element_text(margin = margin(b = 0, t = 0))) +
#    transition_time(time = year) +
#    labs(x = "Population (millions)", y = "Age",
#         title = 'SSP2 World Population {round(frame_time)}')

