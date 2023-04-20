## code to plot epidemic curve based on first few months of COVID-19 pandemic

require(tidyverse)
require(here)

ca_dat <- vax_data %>%
  filter(location == "Canada")

ca_dat_2020 <- ca_dat %>%
  filter(date <= as.Date("2020-12-31")) %>%
  select(date, new_cases_smoothed)

## add time before 2020-01-03
dates <- tibble(
  date = seq(as.Date("2019-11-01"), as.Date("2020-01-02"), 1)
  )

## add some early signals
ca_dat_2020 <- bind_rows(dates, ca_dat_2020) %>%
  arrange(date) %>%
  mutate(
    signals = case_when(
      date == as.Date("2020-01-14") ~ 20,
      date == as.Date("2020-01-21") ~ 25,
      date == as.Date("2020-01-23") ~ 45,
      date == as.Date("2020-01-27") ~ 67,
      date == as.Date("2020-01-29") ~ 32,
      TRUE ~ 0
    )
  ) %>%
  replace_na(list(new_cases_smoothed = 0, signals = 0))

## set end date for plot
end_date <- as.Date("2020-07-01")

## create plot
g <- ca_dat_2020 %>%
  filter(date < end_date) %>%
  ggplot() +
  geom_area(aes(x = date, y = new_cases_smoothed), fill = "#7A9FBF", col = "#7A9FBF", alpha = 0.5) +
  geom_col(aes(x = date, y = signals), fill = "#7A9FBF", col = "#7A9FBF", alpha = 0.5, width = 0.1) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

pre_pandemic_start <- as.Date("2019-11-01")
pre_pandemic_end <- as.Date("2020-03-10")
pre_pandemic_mid <- pre_pandemic_start + (pre_pandemic_end - pre_pandemic_start) / 2
pandemic_start <- pre_pandemic_end + 5
pandemic_end <- end_date
pandemic_mid <- pandemic_start + (pandemic_end - pandemic_start) / 2

y_position <- -60

## add labels
g +
  geom_segment(aes(y = y_position, yend = y_position, x = pre_pandemic_start, xend = pre_pandemic_end),
            col = "gray50", alpha = 0.5, linewidth = 2, lineend = "round") +
  geom_text(aes(y = y_position - 30, x = pre_pandemic_mid, label = "Pre-pandemic"), check_overlap = TRUE) +
  geom_segment(aes(y = y_position, yend = y_position, x = pandemic_start, xend = pandemic_end),
               col = "gray50", alpha = 0.5, linewidth = 2, lineend = "round") +
  geom_text(aes(y = y_position - 30, x = pandemic_mid, label = "Pandemic phase"), check_overlap = TRUE)

ggsave(here("plots", "pandemic_epicurve.png"), device = "png", dpi = 300, width = 19, height = 5)
