library(readxl)
library(stringr)
library(music)
library(dplyr)
library(knitr)
library(tibble)
combine_wordsFr <- function(words) {
  words <- combine_words(words, and = " et ", oxford_comma = F)
  return(words)
}


all <- T
chord <- buildChord("C4", "diminished7th")
if(all){chord <- str_remove_all(chord, "[:digit:]")}
chord




