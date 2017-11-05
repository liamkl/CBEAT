library(xml2)
library(tidyverse)
library(stringr)

# Fn to do reverse paste (for apply purposes)
paste_dir <- function(end, beginning) return(paste0(beginning, end))
data_dir <- paste0(getwd(), "/data/BILLSTATUS-")
con_bill_type <- c("113-hjres", "113-hr", "113-s", "113-sjres",
                   "114-hjres", "114-hr", "114-s", "114-sjres",
                   "115-hjres", "115-hr", "115-s", "115-sjres")
folders <- sapply(con_bill_type, paste_dir, data_dir, USE.NAMES = FALSE)
file_list <- list.files(folders, full.names = TRUE)

# Read in member and policy area lists
members <- read_csv("./data/member_list.csv")
policy_areas <- read_csv("./data/policy_areas.csv")
# Create empty dataframe with row for every member and 
# column for every policy area
m_pa_matrix <- data.frame(matrix(nrow = nrow(members),
                                 ncol = nrow(policy_areas)+1))
colnames(m_pa_matrix) <- c("bioguideId", policy_areas$policy_area)

# Break file list into chunks just for easier running/debugging
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

# Fill with ID numbers and 0 for each policy area
m_pa_matrix[, 2:ncol(m_pa_matrix)] <- 0
m_pa_matrix$bioguideId <- members$bioguideId
orphan_pas <- c()

# Loop to fill the Member-Policy Area matrix
for(file_name in file_list_10){
  # Read in bill data from XML, convert to list for easier access
  bill_data <- read_xml(file_name) %>% as_list()
  # Extract sponsor
  sponsor <- bill_data$bill$sponsors$item$bioguideId %>% unlist()
  # Extract policy area
  policy_area <- bill_data$bill$subjects$billSubjects$policyArea %>% unlist()
  if(is.null(policy_area)) next
  # Some policy areas have some kind of extra specifier which comes after a comma
  # For instance: "Civil Rights and Liberties, Minority Issues"
  # If exists, cut off the end and just keep the first part
  if(grepl("[[:punct:]]", policy_area)){
    policy_area <- str_split(policy_area, ", ", n = 2) %>% unlist() %>% .[1]
  }
  # Even though the policy list from Congress' website is supposed to 
  # be exhaustive, I came up against at least one which was not included
  # called "Private Legislation". If the policy area is not in my existing list
  # I move to next iteration and make note of it
  if(!(policy_area %in% policy_areas$policy_area)){
    orphan_pas <- c(orphan_pas, policy_area)
    next
  }
  # Increment policy area for that sponsor
  m_pa_matrix[m_pa_matrix$bioguideId == sponsor, policy_area] <- 1 + m_pa_matrix[m_pa_matrix$bioguideId == sponsor, policy_area] 
}
# Save matrix after each iteration just in case
saved_matrix <- m_pa_matrix
# Write to disk
write_csv(saved_matrix, "./data/output_data/member_policy_area_matrix.csv")
