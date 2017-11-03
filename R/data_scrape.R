library(tidyverse)
library(stringr)
library(httr)
library(rvest)
library(xml2)
library(jsonlite)

get_bill_list <- function(congress_no, bill_type){
  base_url <- "https://www.gpo.gov/fdsys/bulkdata/BILLSTATUS/"
  page_url <- paste0(base_url, as.character(congress_no), "/", bill_type)
  # Get table with all bills
  tab <- html_node(read_html(page_url), "table.styles3") %>% 
    html_table(header = TRUE, fill = TRUE)
  colnames(tab) <- tab[1, ]
  tab <- tab[-1, ]
  tab <- filter(tab, grepl("xml", Name))
  return(tab)
}

get_bill_data <- function(congress_no, type, name) {
  base_url <-  "https://www.gpo.gov/fdsys/bulkdata/BILLSTATUS/"
  page_url <- paste0(base_url, as.character(congress_no), "/", type, "/", name)
  print(page_url)
  dat <- read_xml(page_url)
  return(dat)
}

# Types of interest
# These all have "force of law"
bill_types <- c("hr", "s", "hjres", "sjres")
# Congresses for which the data exists
congress_nos <- c(113, 114, 115)
hr_bills1 <- get_bill_list(114, "hr")
testbill <- get_bill_data(114, "hr", hr_bills1$Name[10])
testbill_list <- as_list(testbill)
testbill_list$bill$cosponsors
text <- testbill_list$bill$summaries$billSummaries$item$text
cosponsors <- xml_find_all(testbill, ".//cosponsors")
cosponor_list <- cosponsors %>% xml_children() %>% as_list()

file_list <- list.files("./data/BILLSTATUS-113-s")
test_bill <- read_xml(paste0("./data/BILLSTATUS-113-s/", file_list[20]))
listed <- test_bill %>% as_list
