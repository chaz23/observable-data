# A script to download the english title for vaggas, pannasas etc. --------

library(dplyr)
library(tidyr)
library(stringr)
library(httr)

nikayas <- c("dn", "mn", "sn", "an")

title_path_prefix <- "https://raw.githubusercontent.com/suttacentral/sc-data/master/sc_bilara_data/translation/en/sujato/name/sutta/"
title_path_suffix <- "-name_translation-en-sujato.json"

title_path <- paste0(title_path_prefix, 
                     nikayas, 
                     title_path_suffix)

title_metadata <- lapply(lapply(title_path, function (x) x), 
                         function (x) GET(x))

title_metadata_content <- lapply(title_metadata, function (x) {
  content(x, as = "parsed", type = "application/json")
})

title_df <- lapply(title_metadata_content, function (x) {
  x %>% 
    as_tibble() %>% 
    pivot_longer(cols = everything(), values_to = "title")
}) %>% 
  do.call("bind_rows", .) %>% 
  mutate(name = str_remove_all(name, "^.*:")) %>% 
  mutate(name = str_remove_all(name, "^[0-9]+?\\."))

save(title_df, file = "./what-does-the-buddha-talk-about/data/en-titles.Rda")