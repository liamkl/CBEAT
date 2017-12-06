library(rvest)
library(xml2)
library(stringr)

save_path <- paste0(getwd(), "/data/member_list.csv")
if(!file.exists(save_path)){

page_url <- "https://www.congress.gov/help/field-values/member-bioguide-ids"
# Get table with list of members and their IDs
members <- html_node(read_html(page_url), "table.std.full") %>% 
            html_table(header = TRUE, fill = TRUE)
# Get rid of NAs which were inserted every other row for some reason
members <- members %>% filter(!is.na(Member))

# Create new empty columns
members$last <- NA
members$first <- NA
members$party <- NA
members$state_name <- NA

# Parse member field into 
# Last name, first name, party, state
for (i in 1:nrow(members)){
  # Last one here is a unique case, for Joe Lieberman
  # who was both a democrat and independent
  members$party[i] <- str_match(members$Member[i], 
                                "Independent Democrat|Republican|Democratic|Independent")
  split_one <- str_split(members$Member[i], members$party[i]) %>% unlist()
  members$state_name[i] <- split_one[2] %>% gsub("[[:punct:]]", "", .) %>% str_trim()
  split_two <- str_split_fixed(split_one[1], ", ", n = 2)
  members$last[i] <- split_two[1]
  members$first[i] <- str_replace(split_two[2], " \\($", "")
}

# Drop original "Member" field
members <- select(members, -Member)
# Rename member ID column to match XML
colnames(members)[1] <- "bioguideId"
# Load state data
data(state)
# Add missing non-state names/abbreviations
state_names <- c(state.name, "Puerto Rico", "Guam", "Virgin Islands", "American Samoa", "District of Columbia", "Northern Mariana Islands")
state_abbs <- c(state.abb, "PR", "GU", "VI", "AS", "DC", "MP")
# Fill in state abbreviations
members <- mutate(members, state = state_abbs[match(state_name, state_names)]) 

# Write to disk
save_path <- paste0(getwd(), "/data/member_list.csv")
write_csv(members, save_path)}
