#Identify characters in transcripts

#TO DO: MAKE SURE EXPORTS ARE IN PROPER FORMAT

## Start session ----

rm(list = ls())

library(stringr)
library(rebus)
library(magrittr)
library(tidyverse)

## Load patterns from previous script ---- 

pattern.character.names  <- read_rds("interim_output/regex_pattern_identify_characters_transcripts.RDS")

    #alternative.pattern <- read_rds("interim_output/regex_alternative_pattern_identify_characters_transcripts.RDS")


## Load episode list

episode.list.df <- read_rds("interim_output/episode_list_transcripts.RDS")


#TO DO: use str_View to manually check matches in transcripst

if(!"transcripts_matched" %in% list.files("interim_output/")){

  dir.create("interim_output/transcripts_matched")
  
}
    
# try with loop

  range.to.plot <- c(47)

  obj. <- list()
  
  for(e in range.to.plot){
    
    if(is.na(episode.list.df[["transcript"]][e])){
      
      next()
      
    }
    
    obj.[[e]] <-  str_view_all(episode.list.df[e,"transcript"], pattern.character.names) 
    
    file. <- file(str_c("interim_output/transcripts_matched/", e, ".html") )
    
    sink(file. )
    
    print(obj.[[e]], type="html")  
    
    sink()
    close(file.)
    
  }
  
# Detect characters
  
episode.list.df %>%
  select(transcript) %>%
  purrr::transpose() %>%
  unlist %>%
  str_which( rebus::or(stringr::regex(pattern = "ROOMAEMON", ignore_case = T)) )
  
  
