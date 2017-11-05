library(tidyverse)
library(distances)

mpam <- read.csv("./data/output_data/member_policy_area_matrix.csv")
# Filter out all rows that are entirely 0
mpam <- filter(mpam, rowSums(mpam[2:33]) != 0)
# Compute distances
dists <- distances(mpam, id_variable = "bioguideId")
nns <- nearest_neighbor_search(dists, k = 5)


test <- nns[2, 1]
test_match <- mpam[test, 1]
members %>% filter(bioguideId == names(test))
members %>% filter(bioguideId == test_match)

mpam %>% filter(bioguideId %in% c("A000374", "M001149")) %>% View
