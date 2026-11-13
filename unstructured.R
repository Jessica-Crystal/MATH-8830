#=========================================
# UNSTRUCTURED DATA
#=========================================

library(tidyverse)
library(tidytext)
library(textdata)
library(janeaustenr)

books <- austen_books()|> group_by(book)|>mutate(line = row_number())|> ungroup()
#str_detection : detect anything you need to have from the text
words <- books |> filter(
  str_detect(text, "[A-Za-z]")
) |> unnest_tokens(word, text)


content_words <- words |> anti_join(stop_words, by="word")

content_words|> count(word, sort = TRUE)|> slice_head(n = 10)
