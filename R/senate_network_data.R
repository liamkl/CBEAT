# Imports
library(xml2)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)
library(readr)

# Functions
get_edge_weights <- function(cosponsor_list){
  # Function to calculate edge weight 
  # Edge weights are a function of both when
  # sponsorship was added 
  # As well as how many people sponsored so far
  ###############################################
  # Get unique sponsorship dates
  sponsor_dates <- data.frame(date = unique(cosponsor_list$sponsor_date))
  # Get count for each date
  sponsor_dates$count <- sapply(sponsor_dates$date, 
                                function(x) unlist(count(filter(cosponsor_list, sponsor_date <= x))))
  # Calculate weight for given date and counts
  sponsor_dates$weights <- exp((1 - seq(1, nrow(sponsor_dates))) / 10) / sponsor_dates$count
  # Merge weights into cosponsor_list
  cosponsor_list <- left_join(cosponsor_list, sponsor_dates, by = c("sponsor_date" = "date"))
  
  return(cosponsor_list$weights)
}

get_sponsor_edges <- function(sponsorID, cosponsor_list){
  # From/to columns
  sponsor_edges <- data.frame(from = cosponsor_list$bioguideId,
                              to = sponsorID)
  sponsor_edges$weight <- get_edge_weights(cosponsor_list)
  # Add edge type
  # Take this out for now - 11/26/2017 LL
  # sponsor_edges$type <- "sponsor"
  return(sponsor_edges)
}

get_node <- function(item, chamber){
  chamber_id <- chamber_id_dict[chamber]
  bioguideId <- item$bioguideId %>% unlist()
  last_name <- item$lastName %>% unlist() %>% str_to_title()
  first_name <- item$firstName %>% unlist() %>% str_to_title()
  party <- item$party %>% unlist() %>% str_to_upper()
  state <- item$state %>% unlist()
  row_data <- data.frame(bioguideId = bioguideId, 
                         last_name = last_name, 
                         first_name = ifelse(nchar(first_name) == 0, "", first_name), 
                         party = party, state = state,
                         chamber = chamber, chamber_id = chamber_id)
  return(row_data)
}

create_network_data <- function(files){
  # Create empty dataframes
  nodes <- data.frame()
  edges <- data.frame()
  for(file_name in files){
    # Read in bill
    bill_data <- read_xml(file_name) %>% as_list()
    chamber_label <- bill_data$bill$originChamber %>% unlist(use.names = FALSE)
    congressno <- bill_data$bill$congress %>% unlist(use.names = FALSE)
    bill_type <- bill_data$bill$billType %>% unlist(use.names = FALSE)
    billno <- bill_data$bill$billNumber %>% unlist(use.names = FALSE)
    # Get sponsor node information
    # After checking to ensure it's not empty
    if(!is_empty(bill_data$bill$sponsors$item)){
      bill_sponsor <- get_node(bill_data$bill$sponsors$item, chamber_label)
    }
    # Add to node list
    nodes <- rbind(nodes, bill_sponsor)
    # Check to see if any cosponsors -- if not, move to next bill
    if(is_empty(bill_data$bill$cosponsors)) next
    bill_cosponsors <- data.frame()
    
    for(cosponsor in bill_data$bill$cosponsors){
      if(is_empty(cosponsor)) next 
      cosponsor_node <- get_node(cosponsor, chamber_label)
      cosponsor_node$sponsor_date <- cosponsor$sponsorshipDate %>% unlist() %>% as.Date()
      bill_cosponsors <- rbind(bill_cosponsors, cosponsor_node)
    }
    # bill_cosponsors is in order of sponsorship
    bill_edges <- get_sponsor_edges(bill_sponsor$bioguideId, bill_cosponsors)
    bill_edges <- bill_edges %>%
      mutate(congress = congressno,
             bill_type = bill_type,
             bill_no = billno)
    nodes <- rbind(nodes, select(bill_cosponsors, -sponsor_date))
    nodes <- unique(nodes)
    edges <- rbind(edges, bill_edges)
  }
  output <- list()
  output$nodes <- nodes
  output$edges <- edges
  return(output)
}

# Fn to do reverse paste (for apply purposes)
paste_dir <- function(end, beginning) return(paste0(beginning, end))
data_dir <- paste0(getwd(), "/data/BILLSTATUS-")
con_bill_type <- c("113-s", "113-sjres",
                   "114-s", "114-sjres",
                   "115-s", "115-sjres")
folders <- sapply(con_bill_type, paste_dir, data_dir, USE.NAMES = FALSE)
# Break file list into chunks just for easier running/debugging
file_list_1 <- list.files(folders[1], full.names = TRUE)
file_list_2 <- list.files(folders[2], full.names = TRUE)
file_list_3 <- list.files(folders[3], full.names = TRUE)
file_list_4 <- list.files(folders[4], full.names = TRUE)
file_list_5 <- list.files(folders[5], full.names = TRUE)
file_list_6 <- list.files(folders[6], full.names = TRUE)

# "dictionary" for chamber IDs
chamber_id_dict <- c("House" = 1, "Senate" = 2)

t1 <- Sys.time()
batch_1 <- create_network_data(file_list_1)
difftime(Sys.time(), t1) %>% print()

t1 <- Sys.time()
batch_2 <- create_network_data(file_list_2)
difftime(Sys.time(), t1) %>% print()

t1 <- Sys.time()
batch_3 <- create_network_data(file_list_3)
difftime(Sys.time(), t1) %>% print()

t1 <- Sys.time()
batch_4 <- create_network_data(file_list_4)
difftime(Sys.time(), t1) %>% print()

t1 <- Sys.time()
batch_5 <- create_network_data(file_list_5)
difftime(Sys.time(), t1) %>% print()

t1 <- Sys.time()
batch_6 <- create_network_data(file_list_6)
difftime(Sys.time(), t1) %>% print()

nodes <- data.frame()
edges <- data.frame()

nodes <- rbind(nodes,
               batch_1$nodes,
               batch_2$nodes,
               batch_3$nodes,
               batch_4$nodes,
               batch_5$nodes,
               batch_6$nodes
               )
nodes <- unique(nodes)

edges <- rbind(edges,
               batch_1$edges,
               batch_2$edges,
               batch_3$edges,
               batch_4$edges,
               batch_5$edges,
               batch_6$edges
               )

# Get info on 113th/114th/115th Congresses for each node
nodes$in_113 <- sapply(nodes$bioguideId, 
                       function(x) sum(x %in% edges$from[edges$congress == 113] | x %in% edges$edges$from[edges$congress == 113]))
nodes$in_114 <- sapply(nodes$bioguideId, 
                       function(x) sum(x %in% edges$from[edges$congress == 114] | x %in% edges$edges$from[edges$congress == 114]))
nodes$in_115 <- sapply(nodes$bioguideId, 
                        function(x) sum(x %in% edges$from[edges$congress == 115] | x %in% edges$edges$from[edges$congress == 115]))

write_csv(nodes, paste0(getwd(), "/data/output_data/nodes.csv"))
write_csv(edges, paste0(getwd(), "/data/output_data/edges.csv"))
