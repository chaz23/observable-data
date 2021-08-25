# A script to download the blurbs for all suttas, vaggas, etc. ------------

library(dplyr)
library(tidyr)
library(stringr)
library(httr)

nikayas <- c("dn", "mn", "sn", "an")

blurbs_path_prefix <- "https://raw.githubusercontent.com/suttacentral/sc-data/master/sc_bilara_data/root/en/blurb/"
blurbs_path_suffix <- "-blurbs_root-en.json"

blurbs_path <- paste0(blurbs_path_prefix, 
                      nikayas, 
                      blurbs_path_suffix)

blurbs_metadata <- lapply(lapply(blurbs_path, function (x) x), 
                          function (x) GET(x))

blurbs_metadata_content <- lapply(blurbs_metadata, function (x) {
  content(x, as = "parsed", type = "application/json")
})

blurbs_df <- lapply(blurbs_metadata_content, function (x) {
  x %>% 
    as_tibble() %>% 
    pivot_longer(cols = everything(), values_to = "blurb")
}) %>% 
  do.call("bind_rows", .) %>% 
  mutate(name = str_remove_all(name, "^.*:"))

save(blurbs_df, file = "./what-does-the-buddha-talk-about/data/blurbs.Rda")
