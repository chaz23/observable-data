# A script to calculate (approximate) sutta word lengths. -----------------

library(dplyr)
library(tidytext)

load("./what-does-the-buddha-talk-about/data/sutta_data.Rda")

sutta_word_lengths <- sutta_data %>% 
  unnest_tokens(word, segment_text) %>% 
  count(discourse)

save(sutta_word_lengths, file = "./what-does-the-buddha-talk-about/data/sutta_word_lengths.Rda")