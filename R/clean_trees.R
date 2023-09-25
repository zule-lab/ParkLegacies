clean_trees <- function(trees_raw, field_cc){
  
# Park Classes ------------------------------------------------------------

  trees_raw <- trees_raw %>%
    mutate(ParkClass = case_when(Park == "Bois-de-Liesse" ~ "nature",
                                 Park == "Bois-de-Saraguay" ~ "nature",
                                 Park == "Cap-Saint-Jacques" ~ "nature", 
                                 Park == "Île-Bizard" ~ "nature",
                                 Park == "L'Anse-à-l'Orme" ~ "nature", 
                                 Park == "Pointe-aux-Prairies" ~ "nature",
                                 .default = "city"))

    
# DBH ---------------------------------------------------------------------


  # excel reformatted range data (1-3 as 03-Jan and 3-5 as 05-Mar)
  
  # replace plus signs with spaces 
  trees_raw$DBH <- gsub("+", " ", trees_raw$DBH, fixed = T)
  
  # split multiple stem DBH into separate columns 
  ind_dbh <- as.data.frame(str_split_fixed(trees_raw$DBH, " ", 20)) %>%
    mutate_at(vars(starts_with("V")), replace_string) %>% # replace 1-3 and 3-5 DBH values with randomly sampled values w/in that range
    mutate_all(as.numeric) %>% # convert to numeric 
    mutate(NoDBH = rowSums(!is.na(.))) %>% # count number of stems (number of columns w a value)
    mutate_all(funs(ifelse(is.na(.), 0, .))) %>% # substitute NAs w zeroes
    mutate(SumDBH = rowSums(across(starts_with("V"))), # sum all DBH together
           DBHCalc = round(SumDBH/NoDBH, 1)) %>% # calculate mean DBH 
    select(DBHCalc)
  
  trees_dbh <- cbind(trees_raw, ind_dbh)
  
  

# Species Codes -----------------------------------------------------------

  # create species codes
  trees_code <- trees_dbh %>%
    mutate(SpCode = toupper(paste(str_sub(trees_dbh$Genus, start = 1, end = 2), str_sub(trees_dbh$Species, start = 1, end = 2), sep = "")))
  # create unique code for dead trees as they don't have genus/species names
  trees_code$SpCode[trees_code$CommonName == "Dead"] <- 'DEAD'
  # create unique code for unknown species as they don't have genus/species names
  trees_code$SpCode[trees_code$CommonName == "Unknown"] <- 'UNK'
  # replace sugar maple code since it is the same as silver maple
  trees_code$SpCode[trees_code$CommonName == "Sugar Maple"] <- 'ACSC' 
  
  
  
  

# Park/Canopy Labels ------------------------------------------------------

  trees_label <- trees_code %>%
    inner_join(field_cc, by = "PlotID") %>%
    select(-c(Comments, Name))
    
  
  
  return(trees_label)
  
}





replace_string <- function(g) {
  sapply(g, function(a) {
    case_match(a,
               "03-Jan" ~ as.character(runif(1, 1, 3)),
               "1-3" ~ as.character(runif(1, 1, 3)),
               "05-Mar" ~ as.character(runif(1, 3, 5)),
               "3-5" ~ as.character(runif(1, 3, 5)),
               .default = a
    )
  })
}



