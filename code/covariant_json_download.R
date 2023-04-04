library(jsonlite)
library(tidyverse)
url <- "https://raw.githubusercontent.com/hodcroftlab/covariants/master/cluster_tables/EUClusters_data.json"
json_data <- fromJSON(url, simplifyDataFrame = TRUE)
us <- as_tibble(json_data$countries$US) %>% mutate(country = "United States") #%>% select(country, week, total_sequences, contains("23A"))
ca <- as_tibble(json_data$countries$Canada) %>% mutate(country = "Canada") #%>% select(country, week, total_sequences, contains("23A"))
at <- as_tibble(json_data$countries$Austria) %>% mutate(country = "Austria") #%>% select(country, week, total_sequences, contains("23A"))
fr <- as_tibble(json_data$countries$France) %>% mutate(country = "France") #%>% select(country, week, total_sequences, contains("23A"))
de <- as_tibble(json_data$countries$Germany) %>% mutate(country = "Germany") #%>% select(country, week, total_sequences, contains("23A"))
uk <- as_tibble(json_data$countries$`United Kingdom`) %>% mutate(country = "United Kingdom") #%>% select(country, week, total_sequences, contains("23A"))

xbb <- bind_rows(us, ca, at, fr, de, uk)

xbb <- xbb %>%
  select(country, week, total_sequences, contains("23A")) 

names(xbb) <- c("country", "date", "total_sequences", "xbb_sequences")

xbb <- xbb %>%
  mutate(
    xbb_prop = xbb_sequences / total_sequences
  )
