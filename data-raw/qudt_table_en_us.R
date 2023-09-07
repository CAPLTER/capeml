## code to prepare `qudt_table_en_us` dataset goes here

qudt_table_en_us <- capeml::qudt_table[capeml::qudt_table$lang == "en-us", c("name", "unit", "label")]

usethis::use_data(qudt_table_en_us, overwrite = TRUE)
