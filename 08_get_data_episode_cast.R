#08_get_data_episode_cast

# The aim is to get data for casts to know if a character appeared in a given episode. 

# Prepare session ----


rm(list = ls())

library(rebus)
library(xml2)
library(stringr)
library(rvest)
library(magrittr)
library(tidyverse)


# Season list of links

## Manually insert number of seasons!

current_num_seasons <- 7

season_links <- str_c("http://www.imdb.com/title/tt0944947/episodes?season=", 
      1:7, 
      "&ref_=tt_eps_sn_",
      1:7)


# Get raw data 

## Get episode links

all_seasons_cast_ls <- list()


for(s_num in seq_along(season_links)){
 
  s_link <- season_links[s_num]

if(!str_c("data/web/season_", s_num, "_full_cast_names_clean.RDS") %in% list.files("data/web/", full.names = T)){ 

season_html <- read_html(s_link)


    ### Very annoying: web page seems to keep changing it's structure.

links_season_html <- try(season_html %>% xml_child(2) %>% xml_child(3) %>% xml_child(1) %>% xml_child(7) %>% xml_child(4) %>% xml_child(1) %>% xml_child(1) %>% xml_child(2) %>%
  xml_child(3) %>% xml_child(4), silent = TRUE) 

if(class(links_season_html) == "try-error"){
  
  links_season_html <- season_html %>% xml_child(2) %>% xml_child(3) %>% xml_child(1) %>% xml_child(7) %>% xml_child(4) %>% xml_child(1) %>% xml_child(1) %>% xml_child(2) %>%
    xml_child(4) %>% xml_child(4) 

  }

n_episodes_season <- length(links_season_html %>% xml_children())

episode_link_season_ls <- list()

for (i in 1:n_episodes_season){
  
  episode_link_season_ls[[i]] <- links_season_html %>% xml_child(i) %>% xml_child(1) %>% xml_child(1) %>% xml_attr("href")
  
}

rm(i)


# full cast page

full_cast_link_season_vec <- episode_link_season_ls %>% unlist()

full_cast_link_season_vec %<>% 
  str_match(pattern = "tt\\d{7}") %>% 
  .[,1]

full_cast_link_season_vec <- str_c("http://www.imdb.com/title/", 
                                   full_cast_link_season_vec, 
                                   "/fullcredits?ref_=tt_cl_sm") 

season_1_full_cast_ls <- list()

for(u in full_cast_link_season_vec){
  
  season_1_full_cast_ls[[u]] <- read_html(u)
  
}

rm(u)


# To do: 

season_full_cast_names_ls <- season_1_full_cast_ls %>%
    map(function(x){
      
      xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(x, 
                                                                                                2), 3), 1), 7), 4), 1), 1), 3), 6) %>% html_table
      })

season_full_cast_names_ls %>%
  write_rds(str_c("data/web/season_",s_num,"_full_cast_names_ls.RDS"))
  

   ## Clean list with all cast

season_full_cast_names_ls %<>%
  map(function(x){
    
      x %>% 
      select(-X1, -X3)

  })

  ### create vector with episode id

season_cast_length_episodes <- season_full_cast_names_ls %>% 
  map_int(function(x){x %>% nrow})

names(season_cast_length_episodes) <- NULL

season_full_cast_names_df <- season_full_cast_names_ls %>%
  bind_rows()

vector_episode_number <- rep(1:length(season_cast_length_episodes), season_cast_length_episodes)

season_full_cast_names_df %<>% 
  mutate(episode = vector_episode_number)
  
    ### get only cols for character and episode

season_full_cast_names_df %<>%
  select(X4, episode) %>%
  rename(character_cast = X4)

    ### remove empty rows

season_full_cast_names_df %<>% 
  filter(!str_detect(character_cast, pattern = rebus::or("^$", "Rest of cast listed alphabetically", "#\\d") ) )

    ### clean character names

season_full_cast_names_df %<>%
  mutate(character_cast = str_replace(character_cast, 
                                      pattern = "'[[:print:]]*' ",
                                      replacement = "") )

season_full_cast_names_df %<>%
  mutate(character_cast = str_replace(character_cast, 
                                      pattern = " \n  \n  \n  \\([[:print:]]*\\)", 
                                      replacement = ""))

season_full_cast_names_df %>%
  write_rds(str_c("data/web/season_", s_num, "_full_cast_names_clean.RDS"))

}else{
  
  season_full_cast_names_df <- read_rds(str_c("data/web/season_", s_num, "_full_cast_names_clean.RDS"))
  
}

  all_seasons_cast_ls[[s_num]] <- season_full_cast_names_df
  

} # end for loop over seven seasons  

all_seasons_cast_df <- all_seasons_cast_ls %>% 
  bind_rows(.id = "season")  

all_seasons_cast_df %>% 
  write_rds("data/all_seasons_cast_imdb_df.RDS")

all_seasons_cast_df %>% 
  write_csv("data/all_seasons_cast_imdb_df.csv")

