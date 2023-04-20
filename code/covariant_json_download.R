library(jsonlite)
library(tidyverse)
url <- "https://raw.githubusercontent.com/hodcroftlab/covariants/master/cluster_tables/EUClusters_data.json"
json_data <- fromJSON(url, simplifyDataFrame = FALSE)
us <- as_tibble(json_data$countries$US) %>% mutate(country = "United States") #%>% select(country, week, total_sequences, contains("23A"))
ca <- as_tibble(json_data$countries$Canada) %>% mutate(country = "Canada") #%>% select(country, week, total_sequences, contains("23A"))
at <- as_tibble(json_data$countries$Austria) %>% mutate(country = "Austria") #%>% select(country, week, total_sequences, contains("23A"))
fr <- as_tibble(json_data$countries$France) %>% mutate(country = "France") #%>% select(country, week, total_sequences, contains("23A"))
de <- as_tibble(json_data$countries$Germany) %>% mutate(country = "Germany") #%>% select(country, week, total_sequences, contains("23A"))
uk <- as_tibble(json_data$countries$`United Kingdom`) %>% mutate(country = "United Kingdom") #%>% select(country, week, total_sequences, contains("23A"))
sg <- as_tibble(json_data$countries$Singapore) %>% mutate(country = "Singapore") #%>% select(country, week, total_sequences, contains("23A"))
ind <- as_tibble(json_data$countries$India) %>% mutate(country = "India") #%>% select(country, week, total_sequences, contains("23A"))

xbb <- bind_rows(us, ca, at, fr, de, uk)

xbb <- xbb %>%
  select(country, week, total_sequences, contains("23A")) 

names(xbb) <- c("country", "date", "total_sequences", "xbb_sequences")

xbb <- xbb %>%
  mutate(
    xbb_prop = xbb_sequences / total_sequences
  )

country_data <- map_df(json_data$countries, as_tibble)
json_countries <- names(json_data$countries)

get_country_data <- function(l, c) {
  
  d <- as_tibble(l)
  d <- d %>%
    mutate(
      country = c
    )
  
}

country_data <- map2_df(json_data$countries, json_countries, get_country_data)

xbb_count <- country_data %>%
  group_by(country) %>%
  summarise(count = sum(`23A (Omicron)`))

xbb_count %>%
  filter(!is.na(count) & count > 100) %>%
  ggplot(aes(x = fct_reorder(country, count), y = count)) +
    geom_col() + 
  coord_flip()
