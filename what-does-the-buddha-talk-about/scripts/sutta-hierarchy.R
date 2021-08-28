# Create a dataset that describes the hierarchy of the suttas. ------------

# -------------------------------------------------------------------------

# Sutta division scheme:
# ----------------------

# DN:
# Divided into 3 chapters (vaggas).
# 34 suttas => 3 vaggas.

# MN:
# Divided into 3 groups of 50 suttas (pannasas). [1-50, 51-100, 101-152]
# Each group is divided into 5 chapters (vaggas).
# 152 suttas => 3 pannasas -> 15 vaggas.

# SN:
# Divided into 5 books (vaggas) consisting of 56 samyuttas.
# [1-11, 12-21, 22-34, 35-44, 45-56]
# (Number of suttas varies based on how texts are counted.)
# suttas => 5 vaggas => 56 samyuttas

# AN:
# Divided into 11 books (nipatas) with varying number of vaggas in each.


# Output:
# -------

# The output table will group the suttas as BOOK => CHAPTER => SUTTAS.
# For the DN, book is null.
# For the MN, "book" is used to represent a chapter of ~50 suttas.

# DN: book (NULL)    => chapter (vagga)    => suttas
# MN: book (pannasa) => chapter (vagga)    => suttas
# SN: book (vagga)   => chapter (samyutta) => suttas
# AN: book (nipata)  => chapter (vagga)    => suttas

# Blurbs:
# -------

# Blurbs are available for:

# DN: chapter, sutta
# MN: chapter, sutta
# SN: book, sutta
# AN: sutta

# Github content location:
# ------------------------

# Sutta names:
# https://github.com/suttacentral/sc-data/tree/master/sc_bilara_data/translation/en/sujato/name/sutta

# Sutta hierarchy objects:
# https://github.com/suttacentral/sc-data/tree/master/structure/tree/sutta

# MISSING NAMES
# Missing from "https://github.com/suttacentral/sc-data/blob/master/sc_bilara_data/translation/en/sujato/name/sutta/sn-name_translation-en-sujato.json" and other nikayas.
# an6-ragapeyyala
# an10-ragapeyyala
# sn12-satthusuttadi
# sn12-sikkhasuttadipeyyalaekadasaka
# sn13-abhisamayavagga

# -------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(httr)
library(jsonlite)

load("./what-does-the-buddha-talk-about/data/blurbs.Rda")
load("./what-does-the-buddha-talk-about/data/en-titles.Rda")
load("./what-does-the-buddha-talk-about/data/sutta_word_lengths.Rda")

# Create tables encoding the hierarchies in the nikayas. ------------------

nikayas <- c("dn", "mn", "sn", "an")

hierarchy_path_prefix <- "https://raw.githubusercontent.com/suttacentral/sc-data/master/structure/tree/sutta/"

hierarchy_path_suffix <- "-tree.json"

hierarchy_path <- paste0(hierarchy_path_prefix, 
                         nikayas, 
                         hierarchy_path_suffix)

hierarchy_metadata <- lapply(lapply(hierarchy_path, function (x) x), 
                             function (x) GET(x))

hierarchy_metadata_content <- lapply(hierarchy_metadata, function (x) {
  content(x, as = "parsed", type = "application/json")
})


dn_hierarchy <- hierarchy_metadata_content[[1]] %>%
  as_tibble() %>% 
  mutate(chapter = map_chr(dn, names),
         sutta = map(dn, lapply, FUN = function (x) unlist(x))) %>% 
  select(-dn) %>% 
  unnest(cols = sutta) %>% 
  unnest(cols = sutta) %>% 
  left_join(blurbs_df, by = c("chapter" = "name")) %>% 
  rename(chapterBlurb = blurb) %>% 
  left_join(blurbs_df, by = c("sutta" = "name")) %>% 
  rename(suttaBlurb = blurb)


mn_hierarchy <- hierarchy_metadata_content[[2]] %>%
  as_tibble() %>% 
  mutate(book = map_chr(mn, names),
         sutta = map(mn, lapply, FUN = function (x) unlist(x))) %>% 
  select(-mn) %>% 
  unnest(cols = sutta) %>% 
  mutate(chapter = map(sutta, names)) %>% 
  unnest(cols = c(sutta, chapter)) %>% 
  mutate(chapter = str_remove_all(chapter, "[0-9]")) %>% 
  # left_join(blurbs_df, by = c("book" = "name")) %>%
  # rename(bookBlurb = blurb) %>%
  left_join(blurbs_df, by = c("chapter" = "name")) %>% 
  rename(chapterBlurb = blurb) %>% 
  left_join(blurbs_df, by = c("sutta" = "name")) %>% 
  rename(suttaBlurb = blurb)


sn_hierarchy <- hierarchy_metadata_content[[3]] %>%
  as_tibble() %>% 
  mutate(book = map_chr(sn, names),
         sutta = map(sn, lapply, FUN = function (x) unlist(x))) %>% 
  select(-sn) %>% 
  unnest(cols = sutta) %>% 
  mutate(chapter = map(sutta, names)) %>% 
  unnest(cols = c(sutta, chapter)) %>% 
  mutate(chapter = str_remove_all(chapter, "^.*\\."),
         chapter = str_remove_all(chapter, "[0-9]+$")) %>% 
  left_join(blurbs_df, by = c("book" = "name")) %>%
  rename(chapterBlurb = blurb) %>%
  left_join(blurbs_df, by = c("sutta" = "name")) %>% 
  rename(suttaBlurb = blurb)


an_hierarchy <- hierarchy_metadata_content[[4]] %>%
  as_tibble() %>% 
  mutate(book = map_chr(an, names),
         sutta = map(an, lapply, FUN = function (x) unlist(x))) %>% 
  select(-an) %>% 
  unnest(cols = sutta) %>% 
  mutate(chapter = map(sutta, names)) %>% 
  unnest(cols = c(sutta, chapter)) %>% 
  unnest(cols = sutta) %>% 
  mutate(chapter = str_remove_all(chapter, "[0-9]+$"),
         chapter = str_remove_all(chapter, "^.*\\.")) %>%
  left_join(blurbs_df, by = c("sutta" = "name")) %>% 
  rename(suttaBlurb = blurb)


hierarchy_list <- list(dn = dn_hierarchy,
                       mn = mn_hierarchy,
                       sn = sn_hierarchy,
                       an = an_hierarchy)

sutta_hierarchy <- do.call("bind_rows", hierarchy_list) %>% 
  mutate(nikaya = str_extract(sutta, "^[a-z][a-z]")) %>% 
  left_join(sutta_word_lengths, by = c("sutta" = "discourse")) %>% 
  rename(suttaWordCount = n)

# Discrepancies in names means word lengths haven't come through. 
sutta_hierarchy <- sutta_hierarchy %>% 
  mutate(sutta_temp = str_remove_all(sutta, "-.*$")) %>% 
  left_join(sutta_word_lengths, by = c("sutta_temp" = "discourse")) %>% 
  mutate(suttaWordCount = case_when(is.na(suttaWordCount) ~ n,
                           TRUE ~ suttaWordCount),
         suttaWordCount = replace_na(suttaWordCount, 100)) %>% 
  filter(sutta != "an4.106") %>% 
  select(-sutta_temp, -n)

# Calculate word counts for chapters.
sutta_hierarchy <- sutta_hierarchy %>% 
  group_by(chapter) %>% 
  mutate(chapterWordCount = sum(suttaWordCount))


# write_json(list(data = sutta_hierarchy),
           # path = "./what-does-the-buddha-talk-about/data/sutta_hierarchy.json")
