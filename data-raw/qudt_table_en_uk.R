## code to prepare `qudt_table_en_uk` dataset goes here

qudt_table_en_uk <- capeml::qudt_table[capeml::qudt_table$lang == "en", c("name", "unit", "label")]

usethis::use_data(qudt_table_en_uk, overwrite = TRUE)
