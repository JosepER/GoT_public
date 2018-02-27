
# Prepare session

rm(list = ls())

library(RCurl)
library(stringr)
library(rebus)
library(magrittr)
library(tidyverse)


# Import data ----

all_seasons_cast_imdb_df <- read_rds("data/all_seasons_cast_imdb_df.RDS")



# Modify names ----

all_seasons_cast_imdb_df %<>%
  mutate(character_cast = character_cast %>%
           toupper, 
         character_cast = if_else(character_cast != "LORD OF BONES",
                                  true = str_replace(character_cast, 
                                                     pattern = rebus::or("'[[:print:]]*' ", "GRAND MAESTER ",
                                                                         "LORD ", "^MAESTER ", "^YOUNG ",
                                                                         "^ARCHMAESTER "),
                                                     replacement = ""),
                                  false = character_cast),
         character_cast = str_replace(character_cast, 
                                      pattern = "THE WAIF", replacement = "WAIF") )

# Rows to delete from IMDB cast dataframe ----

rows_to_delete <- c("ANARA", "ASH", "AYA", "BARATHEON ARCHER",
                    "BARATHEON GENERAL", "BARATHEON GUARD",
                    "BARATHEON OFFICER", "BARATHEON SOLDIER", "BATHHOUSE BOY",
                    "BATHHOUSE PROSTITUTE", "BEAR ISLAND MAESTER", "BEGGAR WOMAN",
                    "BLACK ARMORED WHITE WALKER", "BLACKMONT", "BOAT COMMANDER",
                    "BOLTON GENERAL", "BOLTON GUARD", "BOLTON OFFICER", "BOLTON SOLDIER",
                    "BRAAVOSI", "BRAAVOSI CAPTAIN", "BRAAVOSI MADAM",
                    "BRAAVOSI THEATRE SERVER", "BRAAVOSI THEATRE SOUND ARTIST",
                    "BROTHEL CUSTOMER", "BROTHEL GUARD", "BROTHERHOOD MEMBER",
                    "BURNING LANNISTER", "CAPTAIN\'S DAUGHTER",
                    "CAPTAIN OF THE ARCHERS", "CAPTAIN OF THE BOLTON ARCHERS", "CAPTURED WIGHT",
                    "CERSEI BODY DOUBLE", "CITADEL MAESTER", "CRASTER\'S DAUGHTER",
                    "CRASTER\'S WIFE", "CRASTER\'S YOUNGER WIFE",
                    "CROSSROADS INN PATRON", "DESPONDENT MAN",
                    "DOSH KHALEEN HIGH PRIESTESS", "DOTHRAKI", "DOTHRAKI CRONE", "DOTHRAKI MAN HAVING SEX",
                    "DOTHRAKI RIDER", "DOTHRAKI WOMAN HAVING SEX",
                    "DRAGONSTONE WAITER", "DROWNED PRIEST", "DRUMMER", "DRUNK PATRON",
                    "DYING MAN", "ELDER MEEREEN SLAVE", "EON HUNTER",
                    "EYRIE GUARD", "FACELESS MAN", "FAITH MILITANT", "FARMER\'S DAUGHTER",
                    "FARMER HAMLET", "FIELD WORKER", "FIGHTER", "FIRST MATE",
                    "FOREIGN MERCHANT", "FREY GUARD", "FREY GUARDSMAN",
                    "FREY LORD", "FREY WEDDING GUEST", "FRUIT VENDOR", "GALBART GLOVER",
                    "GLOVER GENERAL", "GOATHERD", "GOATHERD\'S SON",
                    "GOLDCLOAK", "HANDMAID", "HEALTOR TROOP", "HOG FARMER",
                    "INN WAITRESS", "INNKEEPER", "INNKEEPER\'S DAUGHTER", "IRONBORN",
                    "IRONBORN ABUSING A VOLANTENE WHORE", "IRONBORN AT BROTHEL",
                    "IRONBORN AT BROTHEL /  ", "7", "IRONBORN IN SKIFF",
                    "JAQEN\'S DISGUISE", "JON SNOW SOLDIER", "KARSTARK LEAD ARCHER",
                    "KARSTARK LOOKOUT", "KARSTARK SOLDIER", "KING\'S GUARD",
                    "KING\'S LANDING BAKER", "KING\'S LANDING BOASTER",
                    "KING\'S LANDING DRUNKARD", "KING\'S LANDING HANDMAIDEN",
                    "KING\'S LANDING NOBLEMAN", "KING\'S LANDING NOBLEMAN (2016)",
                    "KING\'S LANDING TAILOR", "KING\'S LANDING URCHIN",
                    "KING\'S LANDING WHORE", "KING BALON GREYJOY DWARF",
                    "KING JOFFREY BARATHEON DWARF", "KING RENLY BARATHEON DWARF",
                    "KING ROBB STARK DWARF", "KING STANNIS BARATHEON DWARF", "KINGSLAND TAVERNER",
                    "KNIGHT", "KNIGHT OF HOUSE FREY",
                    "KNIGHT OF HOUSE LYNDERLY", "LADY KITTY FREY", "LANNISTER ARCHER",
                    "LANNISTER CAPTAIN", "LANNISTER GENERAL", "LANNISTER GUARD",
                    "LANNISTER LORD", "LANNISTER MAP PAINTER", "LANNISTER MESSENGER",
                    "LANNISTER SCOUT", "LANNISTER SOLDIER", "LANNISTER SPEARMAN",
                    "LANNISTER TORTURER", "LEAD DORNISH GUARD", "LEAD KINGSGUARD",
                    "LITTLE BIRD", "LITTLEFINGER\'S CREW",
                    "LORDSPORT DOCKHAND", "MALE PROSTITUTE", "MALLISTER SUPPORTER", "MAN",
                    "MANSERVANT", "MARGAERY\'S HANDMAIDEN", "MARGAERY TYRELL MUMMER",
                    "MARKET TRADER", "MASTER TORTURER", "MEEREEN GUARD",
                    "MEEREEN SLAVE", "MEEREEN SLAVE MASTER",
                    "MEEREENESE HOMELESS MOTHER", "MERCHANT CAPTAIN", "MERCHANT IN TAVERN",
                    "MOLE\'S TOWN BROTHEL PATRON", "MOLE\'S TOWN MADAM",
                    "MOLE\'S TOWN WHORE", "MORGAN\'S FRIEND", "MORMONT SOLDIER", "MUSICIAN",
                    "NAN", "NIGHT\'S WATCH", "NIGHT\'S WATCH ARCHER",
                    "NIGHT\'S WATCH DESERTER", "NIGHT\'S WATCH MESSENGER",
                    "NIGHT\'S WATCH OFFICER", "NIGHT\'S WATCHMAN", "NIGHTS WATCH", "NOBLE LADY",
                    "NOBLE MAN", "NOBLEMAN", "NORTHERN BANNERMAN",
                    "NORTHERN LORD", "NORTHMAN ARCHER", "NORTHMAN RIDER",
                    "NORTHMAN RIOTER", "OLD MAN", "OLD WOMAN", "OLD WOMAN PRISONER",
                    "ORPHAN KID", "PEASANT", "PENTOSHI SERVANT", "PIT ANNOUNCER",
                    "PIT FIGHTER", "POISONED FREY", "PRISONER", "PROTESTOR", "RHAEGO",
                    "QARTHEEN WOMAN", "RED KEEP STABLEBOY", "REGINALD LANNISTER",
                    "RIVERLANDS TRAVELLER", "RIVERRUN NOBLEMAN", "ROYAL STEWARD",
                    "SAILOR", "SECOND SON", "SEPTON", "SERVING GIRL",
                    "SICK MAESTER", "SILENCE CREW", "SINGING LANNISTER SOLDIER", "SLAVER",
                    "SOLDIER", "SOLDIER TOM", "SON OF THE HARPY", "SORCERER",
                    "SPARRING BOY", "STANNIS CREW", "STANNIS SOLDIER",
                    "STARK BANNERMAN", "STARK GUARD", "STARK MESSENGER",
                    "STARK SOLDIER", "STONE MAN", "STREET RED PRIESTESS MEEREEN",
                    "STREET RED PRIESTESS VOLANTIS", "STRONG FIGHTER", "TAVERN MAID",
                    "THE BEAR", "THE CRONE", "THE MAIDEN", "THE MOTHER",
                    "THE SMITH", "THE STRANGER /  ", "3", "TOMMEN\'S ATTENDANT",
                    "TOMMEN\'S MANSERVANT", "TORTURED PRISONER", "TORTURED SLAVE",
                    "TOURNEY HERALD", "TULLY BANNERMAN", "TULLY SOLDIER",
                    "TULLY TROOP", "TYRELL BANNERMAN", "TYRELL GUARD", "TYRELL LADY",
                    "TYRELL SERVANT", "TYRELL SOLDIER", "TYRELL WEDDING GUEST",
                    "UMBER SOLDIER", "UNSULLIED", "VALE LORD", "VALYRIAN SLAVE",
                    "VOLANTENE WHORE", "VOLANTIS FIGHTER", "WAIF\'S DISGUISE",
                    "WAITRESS", "WARLOCK", "WEDDING BAND", "WEDDING GUEST",
                    "WESTEROSI TRADER", "WHITE WALKER", "WHORE", "WIGHT",
                    "WIGHT WILDLING GIRL", "WILDING GLADIATOR", "WILDLING",
                    "WILDLING ARCHER", "WILDLING RIOTER", "WINE MERCHANT",
                    "WINTER TOWN MAN", "WINTERFELL BEEKEEPER", "WINTERFELL LOCAL",
                    "WINTERFELL SHEPHERD", "WINTERFELL SPY", "WOMAN IN KING\'S LANDING",
                    "WOODCUTTER", "WOUNDED LANNISTER", "YUNKAI\'I WHORE",
                    "YUNKAI CITIZEN", "THE STRANGER /  \n            THE MOTHER OF DRAGONS",
                    "IRONBORN AT BROTHEL /  \n            TULLY GUARD",
                    "THEON'S MASTER OF HOUNDS", "THENN WARRIOR", "THE WARRIOR")


all_seasons_cast_imdb_df %<>%
  filter(!character_cast %in% rows_to_delete)

# Rows to rename ----

rows_to_rename <- tibble::tribble(
  ~character_cast,               ~new_name,
  "BLACK JACK BULWER",           "JACK BULWER",
  "BLACK LORREN",                "LORREN",
  "COLEN OF GREENPOOLS",                 "COLEN",
  "DONGO THE GIANT",                 "DONGO",
  "HUGH OF THE VALE",                  "HUGH",
  "MAG THE MIGHTY",  "MAG MAR TUN DOH WEG",
  "MAG MAR TUN DOH WEG\n", "MAG MAR TUN DOH WEG",
  "MAGGY",            "MAGGY FROG",
  "NED",          "EDDARD STARK",
  "NED STARK",          "EDDARD STARK",
  "RAMSAY SNOW",         "RAMSAY BOLTON",
  "RATTLESHIRT",         "LORD OF BONES",
  "ROSLIN FREY",          "ROSLIN TULLY",
  "THE NIGHT KING",            "NIGHT KING",
  "THE TICKLER",               "TICKLER",
  "WUN WUN",   "WUN WEG WUN DAR WUN",
  "BABY SAM",  "LITTLE SAM",
  "BROTHER RAY", "RAY"
)


all_seasons_cast_imdb_df %<>%
  left_join(rows_to_rename, by = "character_cast") %>%
  mutate(character_cast = if_else(!is.na(new_name),
                                  true = new_name,
                                  false = character_cast)) %>%
  select(-new_name)

# Delete repeated rows
    ## Apparently they used different cast to represent 'little sam' in some episodes.

all_seasons_cast_imdb_df %<>% unique

# Export clean data


all_seasons_cast_imdb_df %>%
  write_rds("interim_output/all_seasons_cast_imdb_df_09.RDS")