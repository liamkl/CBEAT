library(xml2)
library(tidyverse)
library(stringr)

get_node <- function(item){
  bioguideId <- item$bioguideId %>% unlist()
  last_name <- item$lastName %>% unlist() %>% str_to_title()
  first_name <- item$firstName %>% unlist() %>% str_to_title()
  party <- item$party %>% unlist() %>% str_to_upper()
  state <- item$state %>% unlist()
  row_data <- data.frame(bioguideId = bioguideId, 
                          last_name = last_name, 
                          first_name = first_name, 
                          party = party, state = state)
  return(row_data)
}

get_sponsor_edges <- function(sponsorID, cosponsor_list){
  sponsor_edges <- data.frame(from = cosponsor_list$bioguideId,
                              to = sponsorID)
  sponsor_edges$weight <- sapply(row(sponsor_edges)[, 1], function(x) return(1 / x))
  sponsor_edges$type <- "sponsor"
  return(sponsor_edges)
}

# Fn to do reverse paste (for apply purposes)
paste_dir <- function(end, beginning) return(paste0(beginning, end))
data_dir <- paste0(getwd(), "/data/BILLSTATUS-")
con_bill_type <- c("113-hjres", "113-hr", "113-s", "113-sjres",
                   "114-hjres", "114-hr", "114-s", "114-sjres",
                   "115-hjres", "115-hr", "115-s", "115-sjres")
folders <- sapply(con_bill_type, paste_dir, data_dir, USE.NAMES = FALSE)
file_list <- list.files(folders, full.names = TRUE)

# Create empty dataframes
nodes <- data.frame()
edges <- data.frame()
# 
chamber_id_dict <- c("House" = 1, "Senate" = 2)

# Break file list into chunks just for easier running/debugging
test_file <- file_list[280]
file_list_1 <- file_list[1:2500]
file_list_2 <- file_list[2501:5000]
file_list_3 <- file_list[5001:7500]
file_list_4 <- file_list[7501:10000]
file_list_5 <- file_list[10001:12500]
file_list_6 <- file_list[12501:15000]
file_list_7 <- file_list[15001:17500]
file_list_8 <- file_list[17501:20000]
file_list_9 <- file_list[20001:22500]
file_list_10 <- file_list[22501:25780]


for(cosponsor in bill_data$bill$cosponsors){
  print(cosponsor$bioguideId)
  print(cosponsor$sponsorshipDate)
  print(cosponsor$isOriginalCosponsor)
  get_node(cosponsor) %>% print()
}


for(file_name in test_file){
  bill_data <- read_xml(test_file) %>% as_list()
  bill_sponsor <- get_node(bill_data$bill$sponsors$item)
  bill_cosponsors <- data.frame()
  for(cosponsor in bill_data$bill$cosponsors){
    cosponsor_node <- get_node(cosponsor)
    bill_cosponsors <- rbind(bill_cosponsors, cosponsor_node)
  }
  # bill_cosponsors is in order of sponsorship
  bill_edges <- get_sponsor_edges(bill_sponsor$bioguideId, bill_cosponsors)
  
}


chamber_label <- bill_data$bill$originChamber %>% unlist(use.names = FALSE)
chamber_id <- chamber_id_dict[chamber_label]

chamber_label <- bill_data$bill$originChamber %>% unlist(use.names = FALSE)
chamber_id <- chamber_id_dict[chamber_label]
