# Imports
library(tidyverse)
library(stringr)
library(rvest)
library(xml2)
#####

# Functions
get_bill_list <- function(congress_no, bill_type){
  base_url <- "https://www.gpo.gov/fdsys/bulkdata/BILLSTATUS/"
  # concatenate full url
  page_url <- paste0(base_url, as.character(congress_no), "/", bill_type)
  # Get table with all bills
  tab <- html_node(read_html(page_url), "table.styles3") %>% 
          html_table(header = TRUE, fill = TRUE)
  # There are two header rows 
  # We want the second one
  # Rename columns with first row data
  # then delete first row
  colnames(tab) <- tab[1, ]
  tab <- tab[-1, ]
  # Just in case
  tab <- filter(tab, grepl("xml", Name))
  return(tab)
}

get_bill_data <- function(bill, congress_no, bill_type) {
  base_url <-  "https://www.gpo.gov/fdsys/bulkdata/BILLSTATUS/"
  # Concatenate full url
  page_url <- paste0(base_url, as.character(congress_no), "/", bill_type, "/", bill)
  #print(page_url) # For debugging purposes
  # Read in XML and return
  dat <- read_xml(page_url)
  return(dat)
}

export_bill_xml <- function(bill, base_path){
  # Extract bill details from string for function call
  congress_no <- str_extract(bill, "(?<=-)\\d+")
  bill_type <- str_extract(bill, "(?<=\\d{3})\\D+")
  bill_no <- str_extract(bill, "(?<=\\d{3}\\w{0,100})\\d+")
  # Get bill data
  xml_to_save <- get_bill_data(bill, congress_no, bill_type)
  # Build full path to save data
  full_path <- paste0(base_path, "BILLSTATUS-", congress_no, "-", bill_type, 
                      "/", "BILLSTATUS-", congress_no, bill_type, bill_no, ".xml")
  write_xml(xml_to_save, full_path)
}
#####

# Types of interest
# These all have "force of law"
bill_types <- c("hr", "s", "hjres", "sjres")

# Set base directory to save the data
data_dir <- paste0(getwd(), "/data/")

# Loop through bill types, then get and save data for each bill.
# I downloaded zips of 113th and 114th Congress bills, but this could
# easily be extended to pull those as well by adding another for-loop
# to loop through Congresses.
#
# Takes a LONG time so be careful. 
# May want to not loop and do it in chunks instead.
for (bill_type in bill_types){
  bill_list <- get_bill_list(115, bill_type)
  sapply(bill_list$Name, export_bill_xml, data_dir)
}

sapply(bill_list$Name[1323:nrow(bill_list)], export_bill_xml, data_dir)
nrow(bill_list)
