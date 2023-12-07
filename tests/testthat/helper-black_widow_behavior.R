
black_widow_behavior <- readr::read_csv(
  file           = "Black_Widow_Behavior.csv",
  na             = c(".", ""),
  show_col_types = FALSE
  ) |>
dplyr::mutate(
  dplyr::across(dplyr::contains("Date"), ~ as.Date(x = ., format = "%m/%d/%y")),
  dplyr::across(dplyr::contains("Mass"), as.numeric),
  Habitat = as.factor(Habitat),
  Site    = as.factor(Site)
  ) |>
dplyr::filter(!is.na(ID))

black_widow_behavior_desc <- "Western black widow spider characteristics and behaviors in the field and the laboratory"

black_widow_behavior_edited <- black_widow_behavior
