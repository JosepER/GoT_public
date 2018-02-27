
#12_import_api_anapioficeandfire_data

rm(list = ls())

#devtools::install_github("MangoTheCat/GoTr")

library(assertthat)
library(GoTr)
library(rebus)
library(rvest)
library(stringr)
library(magrittr)
library(tidyverse)

# Import data from wikipedia characters ----

char_all_map <- read_rds("interim_output/characters.all.map.06.RDS")

# Import data from transcript characters ----

char_transcripts <- read_rds("counts/transcripts/counts_by_episode_long_with_imdb_10.RDS") %$%
  character.names.wikipedia %>% unique()

# Import data from API ----

if(!"api_anapioficeandfire.RDS" %in% list.files("data/web/")){
list_character_info_api <- list()
  
test_error <- F

for(i in 1:2200){
  
  if(i %in% 1509:1511){
    next()
  }

api_info <- try(got_api(type = "characters", id = i))

test_error <- is.error(api_info)

if(test_error == F){
  
  list_character_info_api[[i]] <- api_info
  
}

}

list_character_info_api %>% 
  write_rds("data/web/api_anapioficeandfire.RDS")

}else{
  
 list_character_info_api <- read_rds("data/web/api_anapioficeandfire.RDS") #replace with old (had longer length!)
  
}



# Import data directly from the webpage ----

if(!"api_anapioficeandfire_directly_web.RDS" %in% list.files("data/web/")){

list_info_api_directly_webpage <- list()

for(l in 1:2200){
  
 result_ <- try(read_html(str_c("https://www.anapioficeandfire.com/api/characters/", l)) %>% html_text )
 
 if(assertthat::is.error(result_)){
   
   list_info_api_directly_webpage[[l]] <- "error"
   
 }else{
   
   list_info_api_directly_webpage[[l]] <- result_
   
 }
  

}

list_info_api_directly_webpage %>% 
  write_rds("data/web/api_anapioficeandfire_directly_web.RDS")

}else{
  
list_info_api_directly_webpage <- read_rds("data/web/api_anapioficeandfire_directly_web.RDS")
  
}


list_info_api_directly_webpage_names <- list_info_api_directly_webpage %>% 
  map(function(x){x %>% str_match(pattern = ",\"name\":\"" %R% 
                                    capture("[[:print:]][^\"]+") %R% 
                                    "\",")}) 

list_info_api_directly_webpage_names %<>% 
  map_chr(function(x){ x[,2]   })


# Clean data ----
 
## empty elements in list?

appears_in_tv_series_index <- list_character_info_api %>% 
  map("tvSeries") %>% 
  map(unlist) %>% 
  map(function(x){str_c(x, collapse = "_")}) %>% 
  unlist %>%
  str_detect(pattern = "^Season") %>%
  which()

#There's a problem: the 'tvSeries' column is not reliable. It leaves many tv characters out (e.g. 1620 Robb Stark) 

# Delete Null elements from the API list ----

list_character_info_api_null <- list_character_info_api %>% 
  map(is.null) %>% 
  unlist 

list_character_info_api <- list_character_info_api[!list_character_info_api_null]

rm(list_character_info_api_null)


#names_in_info_api <- list_character_info_api_in_series %>%
 # map_chr("name")

names_in_info_api <- list_character_info_api %>%
  map("name") %>% 
  unlist


# Change names ---- 

names_in_info_api[11] <- "Waif" 

names_in_info_api[585] <- "Greatjon Umber"

names_in_info_api[586] <- "Smalljon Umber"

names_in_info_api[1090] <- "Walder Rivers"

names_in_info_api[1610] <- "Lem Lemoncloak"

names_in_info_api[747] <- "Merry Frey"

names_in_info_api[1932] <- "Selyse Baratheon"

names(list_character_info_api) <- names_in_info_api


# Create a data_frame with API information ----

character_info_api <- data_frame(char_name = names_in_info_api)

    ## TV series
    
## Note: not all characters that appeared in TV series are labelled in this 
## variable (e.g. Robb Stark appears in TV series but has empty 'tvSeries' variable in the API).
## However, we need this variable because some characters appear repeated and we need to retreive
## the version of it that appears in TV series.

tvseries_vector <- list_character_info_api %>%
  map(function(x){x$"tvSeries" %>% 
      unlist %>% 
      str_c(collapse = "_") %>% 
      str_detect(pattern = "^[[:print:]]+$") } ) %>%
  unlist()

    ## Gender

gender_vector <- list_character_info_api %>% map("gender") 

#gender_vector[list_character_info_api_null] <- NA_character_

gender_vector %<>% unlist

    ## Culture

culture_vector <- list_character_info_api %>% map("culture") 

#culture_vector[list_character_info_api_null] <- NA_character_

culture_vector %<>% unlist

culture_vector <- if_else(culture_vector == "",
                          true = NA_character_,
                          false = culture_vector)

    ## Born (year)
  ### Note: firt year or approximation

born_vector <- list_character_info_api %>% map("born") 

#born_vector[list_character_info_api_null] <- NA_character_

born_vector_year <- born_vector %>% 
  str_match(pattern = "(\\d{3}) AC") %>%
  .[,2] %>% as.numeric

    ## Born (place)

born_vector_place <- born_vector %>% 
  str_match(pattern =  rebus::or("at ", "At ") %R% capture("[[:print:]]+$") ) %>%
  .[,2]

born_vector_place[born_vector_place == "Unknown"] <- NA_character_

    ## Born (dummy / we have or don't have info)

born_vector_dummy <- born_vector %>% 
  str_detect(pattern = "^$") %>%
  !.

    ## Died (dummy)

died_vector_dummy <- list_character_info_api %>% 
  map("died")

died_vector_dummy %<>% 
  unlist %>% 
  str_detect(pattern = "^$") %>%
  !.

    ## Titles (number of titles)

titles_vector <- list_character_info_api %>%
  map("titles") 

titles_vector_count <- titles_vector %>% 
  map(function(x){str_count(x, pattern = "^[[:print:]]+$") %>% sum}) %>%
  unlist()

    ## Aliases (number of aliases)

aliases_vector <- list_character_info_api %>%
  map("aliases")

aliases_vector_count <- aliases_vector %>%
  map(function(x){str_count(x, pattern = "^[[:print:]]+$") %>% sum}) %>%
  unlist()



  ## Father (dummy)
    ###Note: I'm not sure this information is complete. E.g. Sansa Stark and Ramsay Snow have no father in the list?  
    ### Decision: don't use this information    


#father_vector_dummy <- list_character_info_api_in_series %>%
 # map("father")


  ## Mother (dummy)

###Note: I'm not sure this information is complete. 
### Decision: don't use this information   

#list_character_info_api_in_series %>%
 #  map("mother")

  ## Spouse (dummy)

spouse_vector_dummy <- list_character_info_api %>%
  map("spouse") 

#spouse_vector_dummy[list_character_info_api_null] <- NA_character_

spouse_vector_dummy %<>% 
  unlist() %>%
  str_detect(pattern = "^[[:print:]]+$")

  ## Allegiances (dataframe with allegiances and number of these)

allegiances_vector <- list_character_info_api %>%
  map("allegiances") 

#allegiances_vector[list_character_info_api_null] <- NA_character_

allegiances_vector %<>%
  map(function(x){x %>% str_match_all(pattern = "api/houses/" %R% capture("\\d+$")) %>% unlist }) 

allegiances_vector_logical_temp <- allegiances_vector %>%
  map_dbl(length)  == 0

allegiances_vector %<>% 
  map(function(x){x %>% str_extract_all("^\\d+$") %>% unlist %>% str_c(collapse = "_")}) 

allegiances_vector[allegiances_vector_logical_temp] <- "None"

allegiances_vector %<>% unlist

allegiances_count <- if_else(allegiances_vector != "None",
                             true = str_count(allegiances_vector, pattern = "_") + 1,
                             false = 0)
  
allegiances_df <- data_frame(allegiances_count, allegiances_vector) %>%
  mutate(allegiances_vector = if_else(allegiances_vector == "None",
                                      true = NA_character_,
                                      false = allegiances_vector)) %>%
  separate(allegiances_vector, into = c("allegiances_1", "allegiances_2", 
                                        "allegiances_3", "allegiances_4"), 
           fill = "right") 
  
rm(allegiances_vector, allegiances_vector_logical_temp,
   allegiances_count)


  ## Books (number)

books_vector_number <- list_character_info_api %>%
  map("books") %>% 
  map(function(x) {x %>% unlist}) %>%
  map(length) %>%
  unlist()


  ## Url
  ### use it as unique identifier to merge manually matched API characters.

url_vector <- list_character_info_api %>%
  map_chr("url")
  
## Test before merging all data

test_objects <- list(gender_vector, culture_vector, born_vector_year, born_vector_place, 
                  born_vector_dummy, titles_vector_count, aliases_vector_count, 
                  spouse_vector_dummy, url_vector)

test_ <- test_objects %>%
  map_lgl(is.vector) %>%
  prod

if(test_ != 1){
  
  stop("Failed test: some of the objects merged in 'Merge all data' subsection are not vectors")

}

test_2 <- test_objects %>%
  map_dbl(length) %>%
  sd

if(test_2 != 0){
  
  stop("Failed test: not all the objects merged in 'Merge all data' subsection have the same length.")
  
}

rm(test_, test_2, test_objects)


 ## Merge all data ---

character_info_api %<>%
  bind_cols(gender = gender_vector, 
            culture = culture_vector,
            born_year = born_vector_year,
            born_place = born_vector_place,
            born_dummy = born_vector_dummy,
            titles_count = titles_vector_count,
            aliases_count = aliases_vector_count,
            spouse_dummy = spouse_vector_dummy,
            allegiances_df %>% select(allegiances_count),
            tvseries_dummy = tvseries_vector,
            url_vector = url_vector) 

rm(gender_vector, culture_vector, born_vector_year, born_vector_place,
   born_vector_dummy, titles_vector_count, aliases_vector_count, 
   spouse_vector_dummy, allegiances_df, tvseries_vector, url_vector)

rm(died_vector_dummy, books_vector_number, born_vector, titles_vector)

# Rename characters in api list ----

character_info_api %<>%
  mutate(char_name = toupper(char_name))

## Manually inputed table ----

reviewed_char_df <- tibble::tribble(~char_name,     ~char_name_replace,
                                         "WALDER",          "HODOR",
                                        "MORDANE",        "SEPTA MORDANE",
                                         "UNELLA",          "SEPTA UNELLA",
                                "AEMON TARGARYEN",                "AEMON",
                                       "AERYS II",      "AERYS TARGARYEN",
                                   "ASHA GREYJOY",         "YARA GREYJOY",
                                  "BRANDON STARK",           "BRAN STARK",
                                 "EDDISON TOLLET",      "EDDISON TOLLETT",
                                 "GALBART GLOVER",               "delete",
                                   "IMRY FLORENT",               "delete",
                                  "MAEGE MORMONT",               "delete",
                                "MELESSA FLORENT",        "MELESSA TARLY",
                                 "OLENNA REDWYNE",        "OLENNA TYRELL",
                                         "QHORIN",      "QHORIN HALFHAND",
                                    "RAMSAY SNOW",        "RAMSAY BOLTON",
                                   "ROBERT ARRYN",          "ROBIN ARRYN",
                             "ROBERT I BARATHEON",     "ROBERT BARATHEON",
                                    "ROSLIN FREY",         "ROSLIN TULLY",
                                     "WALDA FREY",         "WALDA BOLTON",
                                  "WALDER RIVERS",  "BLACK WALDER RIVERS",
                                 "BRYNDEN RIVERS",        "delete", 
                       "CHELLA DAUGHTER OF CHEYK",               "delete",
                                    "DAEMON SAND",               "delete",
                                         "DAGMER",      "DAGMER CLEFTJAW",
                                          "DROGO",           "KHAL DROGO",
                                "HUMFREY CLIFTON",               "delete",
                                      "HUNNIMORE",               "delete",
                                   "JEYNE WATERS",               "delete",
                                        "JON POX",               "delete",
                                      "JON VANCE",               "delete",
                              "JYNESSA BLACKMONT",               "delete",
                                     "KIRBY PIMM",               "delete",
                                          "LAYNA",               "delete",
                                          "LEWYS",               "delete",
                                          "LOMYS",               "delete",
                                    "LOREZA SAND",               "delete",
                                           "LUKE",               "delete",
                                            "LUM",               "delete",
                                         "MAERIE",               "delete",
                                         "MAERIE",               "delete",
                                       "MALLIARD",               "delete",
                                         "MALLOR",               "delete",
                                  "MARTYN RIVERS",               "delete",
                                        "MATRICE",               "delete",
                                          "MELLY",               "delete",
                                        "MEZZARA",               "delete",
                                         "MIKKEN",               "delete",
                                         "MOLLOS",               "delete",
                                 "MORGAN MARTELL",               "delete",
                                       "MUSHROOM",               "delete",
                                        "MYSARIA",               "delete",
                                         "NORREN",               "delete",
                                           "NUTE",               "delete",
                                          "NYMOS",               "delete",
                                   "OMER FLORENT",               "delete",
                                   "ORO TENDYRIS",               "delete",
                                            "PIA",               "delete",
                                   "POUL PEMFORD",               "delete",
                                        "PUCKENS",               "delete",
                                        "PUDDING",               "delete",
                                 "QARRO VOLENTIN",               "delete",
                                          "QEZZA",               "delete",
                                          "QUILL",               "delete",
                                    "RAGNOR PYKE",               "delete",
                                           "RALF",         "RALF KENNING",
                                 "RICHARD FARROW",               "delete",
                       "RONEL RIVERS",               "delete",
                       "RUS",               "delete",
                       "RYON ALLYRION",               "delete",
                       "SAWWOOD",               "delete",
                       "SQUINT",               "delete",
                       "STEEELSKIN",               "delete",
                       "STONE THUMBS",               "delete",
                       "SYMEON STAR-EYES",               "delete",
                       "TARLE",               "delete",
                       "TERRO",               "delete",
                       "THEOBALD",               "delete",
                       "TIM STONE",               "delete",
                       "TIMETT, SON OF TIMETT",               "delete",
                       "TUMCO LHO",               "delete",
                       "TURNIP",               "delete",
                       "VAL",               "delete",
                       "VYMAN",               "delete",
                       "WILLUM",               "delete",
                       "KARL", "KARL TANNER",
                        "LOMMY", "LOMMY GREENHANDS",
                       "MAGGY" ,"MAGGY FROG",
                       "MORO", "KHAL MORO",
                       "NAN", "OLD NAN",
                       "OBERYN NYMEROS MARTELL", "OBERYN MARTELL",
                       "OTHOR", "delete",
                       "THE TICKLER", "TICKLER",
                       "TORMUND", "TORMUND GIANTSBANE",
                       "WALTON","STEELSHANKS WALTON"                       )  

## Note: the list of all characters looks very strange. Some characters are repeated several times. 
## I'll first subset those that appear in tvseries. Then I'll check which characters in transcripts
## are not in the API dataset with tvseries_dummy == T. Then I'll add these characters (hopefully they
## won't be repeated).

## Merge inputed table with api data_frame ----

character_info_api %<>%
  left_join(reviewed_char_df, by = "char_name")

rm(reviewed_char_df)

## Delete manually reviewed characters that were found not to appear in transcripts

character_info_api %<>%
  filter(is.na(char_name_replace) | char_name_replace != "delete" )

## Replace character names with reviewed version

character_info_api %<>%
  mutate(char_name = if_else(!is.na(char_name_replace),
                             true = char_name_replace,
                             false = char_name)) %>%
  select(-char_name_replace)

# Subset API elements tvSeries or manually matched ----
## use only those that API elements that appear in tvSeries OR were manually matched

## import table with manually matched API characters ----

manually_matched <- tibble::tribble(~name,  ~url_,
  "Alys Karstark",    "https://anapioficeandfire.com/api/characters/96",
  "Donnel Hill",  "https://anapioficeandfire.com/api/characters/1334",
  "Ebrose",  "https://anapioficeandfire.com/api/characters/1354",
  "Euron Greyjoy",   "https://anapioficeandfire.com/api/characters/385",
  "Gared",   "https://anapioficeandfire.com/api/characters/400",
  "Hugh",   "https://anapioficeandfire.com/api/characters/508",
  "Lollys Stokeworth",   "https://anapioficeandfire.com/api/characters/638",
  "Melara Hetherspoon",   "https://anapioficeandfire.com/api/characters/739",
  "Ralf Kenning",   "https://anapioficeandfire.com/api/characters/846",
  "Varly",  "https://anapioficeandfire.com/api/characters/2068",
  "Will",  "https://anapioficeandfire.com/api/characters/1109",
  "Wyman Manderly",  "https://anapioficeandfire.com/api/characters/1124",
  "Jaremy Rykker",   "https://anapioficeandfire.com/api/characters/536",
  "Leo Lefford",   "https://anapioficeandfire.com/api/characters/622",
  "Tomard",  "https://anapioficeandfire.com/api/characters/2020",
  "Cohollo",  "https://anapioficeandfire.com/api/characters/1278",
  "Merianne Frey",   "https://anapioficeandfire.com/api/characters/747",
  "Selyse Baratheon", "https://anapioficeandfire.com/api/characters/1935")

  ## Filter API dataframe ----
## keep only characters that appear on 


character_info_api_in_series_df <- character_info_api %>%
  filter(tvseries_dummy == T | url_vector %in% manually_matched$url_)


# Compare API character names with list of all characters ----

char_all_vector <- char_all_map %$%
  character.names.wikipedia %>% 
  unique

api_series_char_vector <- character_info_api_in_series_df %$%
  char_name %>%
  toupper()

## Check which characters from 'all characters' list are not in the API

char_all_vector[!char_all_vector %in% api_series_char_vector]

#try(got_api(type = "characters", query = list(name = "VARYS")))


## Check that all character names from API can be matched to our main list of characters

test <- (api_series_char_vector[!api_series_char_vector %in% char_all_vector] ) %>% length()

if(test != 0){
  
  stop("Failed test: there were characters from the API that couldn't be matched 
to those in the list of all wikipedia + transcripts characters. The list from API characters should be reviewed again.")
  
}

test_repeated <- character_info_api_in_series_df %>%
  count(char_name) %>%
  filter(n > 1) %>%
  nrow()

if(test_repeated != 0){
  
  stop("Failed test: there are repeated names in the API information file.")
  
}

rm(test_repeated, test)



# missing characters ----
## try to find characters that appear in transcripts but not in API subset of seriesTV 

character_info_api_in_series_df$char_name %in% char_all_map$character.names.wikipedia

missing_characters <- char_transcripts[!char_transcripts %in% character_info_api_in_series_df$char_name]

## check all names in API and alliases

missing_characters_detected_list <- list()

detect_names <- NA_integer_

detect_aliases <- NA_integer_


for(i in missing_characters){
  
  ### compute name for regex
  
  name_ <- str_c("(?i)^", i, "$(?-i)")
  
  ### detect and match in all api names
  
  detect_names <- names_in_info_api %>%
    str_detect(pattern = name_) %>%
    sum()
  
  if(detect_names > 0){
  
  missing_characters_detected_list[[i]][[1]] <- names_in_info_api %>%
    str_which(pattern = name_) %>%
    str_c(collapse = "_")
  
  }else if(detect_names == 0){
    
    missing_characters_detected_list[[i]][[1]] <- NA_character_
    
  }
    
  ### detect and match in all api aliases 
  
  detect_aliases <- aliases_vector %>%
    map(function(x){x %>% 
        str_detect(pattern = name_)} ) %>%
    unlist %>% 
    sum()
  
  if(detect_aliases > 0){
  
  missing_characters_detected_list[[i]][[2]] <- aliases_vector %>%
    map_lgl(function(x){x %>% str_detect(pattern = name_) %>% sum() > 0} ) %>%
    which %>%
    str_c(collapse = "_")
    
  }else if(detect_aliases == 0){
    
    missing_characters_detected_list[[i]][[2]] <- NA_character_
    
  }
  
}


missing_characters_detected_df <- missing_characters_detected_list %>% 
  unlist %>% 
  matrix(ncol = 2, nrow = length(missing_characters_detected_list), byrow = T) %>%
  as_data_frame()

missing_characters_detected_df <- bind_cols(name = names(missing_characters_detected_list),
           missing_characters_detected_df)

names(missing_characters_detected_df) <- c("name", "all_names_api", "all_aliases_vector")



# Export data
       
character_info_api_in_series_df %>% 
  write_rds("data/api_anapioficeandfire_clean_12.RDS")
