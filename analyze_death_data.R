library(tidyverse)
library(ggpubr)

library(readxl)
deaths_2016_2021 <- read_excel("deaths_2016_2021.xlsx", 
                               col_names = FALSE, skip = 4)

library(lubridate)

cols <- c("year_kw", 
          "kw", 
          "overall", 
          "0_64_overall", 
          "65_more_overall",
          "men_overall",
          "0_64_men_overall",
          "65_more_men_overall",
          "women_overall",
          "0_64_women_overall",
          "65_more_women_overall")

names(deaths_2016_2021) <- cols

deaths_2016_2021 <- deaths_2016_2021 %>%
  filter(!is.na(overall)) %>%
  select(-kw) %>%
  mutate(year = str_sub(year_kw, 1, 4)) %>%
  mutate(kw = str_sub(year_kw, 5, 6)) %>%
  mutate(year_group = if_else(year == 2021, "2021", if_else(year == 2020, "2020", "2016-2019")))

covid_data <- read_delim("CovidFaelle_Timeline.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)

covid_death_data <- covid_data %>%
  select(date = Time, deaths = AnzahlTotTaeglich) %>%
  mutate(date = dmy_hms(date)) %>%
  mutate(year = format(date, "%Y")) %>%
  mutate(week_number = format(date, "%V"))

# Durchschnittliche Todesfälle 2021, 2021, 2016-2019
avg_overall <- deaths_2016_2021 %>%
  group_by(year_group) %>%
  summarise(avg_deaths = mean(overall))

# Comparison of deaths by week number
avg_week_overall <- deaths_2016_2021 %>%
  group_by(year_group, kw) %>%
  summarise(avg_week_deaths = mean(overall)) %>%
  ungroup()

a <- ggplot(avg_week_overall) +
  geom_line(mapping = aes(x = as.numeric(kw), 
                          y = avg_week_deaths, 
                          group = factor(year_group),
                          colour = factor(year_group))) +
  ylab("Anzahl der Sterbefälle") +
  xlab("Kalenderwoche") + 
  labs(color='Jahre') 

# Comparison of covid deaths by week number of 2020 and 2021
avg_week_covid_deaths <- covid_death_data %>%
  group_by(year, week_number) %>%
  summarise(avg_covid_deaths = mean(deaths)) %>%
  ungroup() %>%
  filter(year < "2022")

b <- ggplot(avg_week_covid_deaths) +
  geom_line(mapping = aes(x = as.numeric(week_number), 
                          y = avg_covid_deaths, 
                          group = factor(year),
                          colour = factor(year))) +
  ylab("Anzahl der COVID-19 Sterbefälle") +
  xlab("Kalenderwoche") + 
  labs(color='Jahre') 



ggarrange(a, b, ncol = 1, nrow = 2, align = "h", labels = c("Gesamt Sterbefälle", "COVID-19 Sterbefälle"))





