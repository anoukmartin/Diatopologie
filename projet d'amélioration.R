
# On identifie les notes qui sont dans ces accords 

notes$tire$main_droite <- clav$plan$main_droite %>%
  filter(soufflet == "T") %>%  
  filter(note %in% accord$notes) %>%
  mutate(note2 = paste0(note, octave))
notes$pousse$main_droite <- clav$plan$main_droite %>%
  filter(soufflet == "P") %>%  
  filter(note %in% accord$notes) %>%
  mutate(note2 = paste0(note, octave))

notes$tire$main_gauche <- clav$plan$main_gauche %>%
  filter(soufflet == "T") %>%  
  filter(note == accord$root) 
notes$pousse$main_gauche <- clav$plan$main_gauche %>%
  filter(soufflet == "P") %>%  
  filter(note == accord$root) 


# une fonction qui identifie les accords qui ont du sens 

list_chords <- function(notesdata){
  notesdata <- notesdata %>%
    mutate(freq = note2freq(note2)) %>%
    group_by(note) %>%
    arrange(freq) 
  listnotes <- data.frame(note2 = get_notes_between_with_octaves(note1 = notesdata$note2[1], note2 = notesdata$note2[nrow(notesdata)]))
  listnotes <- listnotes %>% 
    mutate(note = str_remove_all(note2, "[:digit:]")) %>%
    filter(note %in% notesdata$note)
  
  
  listaccords <- lapply(1:(nrow(listnotes)-length(unique(notesdata$note))+1), function(i){
    A <- listnotes[i:(i+length(unique(notesdata$note))-1), ]
    A$n <- i
    return(A)
  })
  listaccords <- bind_rows(listaccords) %>%
    group_by(n)
  return(listaccords)
}

## On cherche les accords dans les notes du clavier
listaccords <- list_chords(notes$tire$main_droite) %>%
  group_by(n) %>%
  filter(all(note2 %in% notes$tire$main_droite$note2)) 
listaccords



