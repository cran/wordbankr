## ----include=FALSE------------------------------------------------------------
library(wordbankr)
library(dplyr)
library(ggplot2)
knitr::opts_chunk$set(message = FALSE, warning = FALSE, cache = FALSE)
theme_set(theme_minimal())

con <- connect_to_wordbank()
can_connect <- !is.null(con)
knitr::opts_chunk$set(eval = can_connect)

## -----------------------------------------------------------------------------
get_administration_data(language = "English (American)", form = "WS")
get_administration_data()

## -----------------------------------------------------------------------------
get_item_data(language = "Italian", form = "WG")
get_item_data()

## -----------------------------------------------------------------------------
get_instrument_data(
  language = "English (American)",
  form = "WS",
  items = c("item_26", "item_46")
)

## ----fig.width=6, fig.height=4------------------------------------------------
items <- get_item_data(language = "English (American)", form = "WS")
if (!is.null(items)) {
  animals <- items %>% filter(category == "animals")
}

## -----------------------------------------------------------------------------
if (!is.null(animals)) {
  animal_data <- get_instrument_data(language = "English (American)",
                                     form = "WS",
                                     items = animals$item_id,
                                     administration_info = TRUE,
                                     item_info = TRUE)
}

## ----fig.width=6, fig.height=4------------------------------------------------
if (!is.null(animal_data)) {
  animal_summary <- animal_data %>%
    group_by(age, data_id) %>%
    summarise(num_animals = sum(produces, na.rm = TRUE)) %>%
    group_by(age) %>%
    summarise(median_num_animals = median(num_animals, na.rm = TRUE))
  
  ggplot(animal_summary, aes(x = age, y = median_num_animals)) +
    geom_point() +
    labs(x = "Age (months)", y = "Median animal words producing")
}

## -----------------------------------------------------------------------------
get_instruments()

## -----------------------------------------------------------------------------
get_datasets(form = "WG")
get_datasets(language = "Spanish (Mexican)", admin_data = TRUE)

## -----------------------------------------------------------------------------
fit_aoa(animal_data)
fit_aoa(animal_data, method = "glmrob", proportion = 1/3)

## -----------------------------------------------------------------------------
get_crossling_items()

## ----eval=FALSE---------------------------------------------------------------
#  get_crossling_data(uni_lemmas = c("hat", "nose")) %>%
#    select(language, uni_lemma, item_definition, age, n_children, comprehension,
#           production, comprehension_sd, production_sd) %>%
#    arrange(uni_lemma)

