library(dplyr)
library(readr)
library(ggplot2)

### cases <- read_csv("../../google_charts_data/line_chart_african_countries/line_chart_cases.csv")
### deaths <- read_csv("../../google_charts_data/line_chart_african_countries/line_chart_deaths.csv")

data <- read_csv("../../african_latest_data.csv")

data$dateRep <- as.Date(data$dateRep, format="%d/%m/%Y")
data$popData2018 <- data$popData2018 / 1000000
data$cases_per_million <- data$cumulative_cases / data$popData2018
data$deaths_per_million <- data$cumulative_deaths / data$popData2018

data_graphic <- data[, c("dateRep", "countriesAndTerritories", "cases_per_million", "cumulative_cases", "cases", "deaths_per_million", "cumulative_deaths", "deaths")]

## remove france

data_graphic <- subset(data_graphic, countriesAndTerritories != "France")

## add moving average

## 2 sided => backwards and forwards, 1 only backward
ma <- function(x, n = 3){ifelse(x > 3, stats::filter(x, rep(1 / n, n), sides = 1), 0)}

## use the moving average to determine when we start the curve (when moving average is > 3 like in the El Pais graphics).

data_graphic <- data_graphic %>% 
    group_by(countriesAndTerritories) %>%
    arrange(dateRep) %>%
    mutate(mov_avg_new = ma(cases)) %>%
    mutate(mov_avg_cum_cases = ma(cumulative_cases))

## group and remove rows prior to 3 cases, but leave if there are fewer than 3 after the initial date------

## get_min_date <- function(){min(x[x$cases >= 3]$dateRep)}

test <- data_graphic %>%
    group_by(countriesAndTerritories) %>%
    arrange(dateRep) %>%
    ## find date when ma >= 3, and remove all prior to this date------
    mutate(cases_3 = mov_avg_cum_cases >= 3)

test2 <- test %>%
    group_by(countriesAndTerritories) %>%
    arrange(dateRep) %>%
    filter(cases_3 == TRUE) %>%
    mutate(first_date = min(dateRep))

## map from country to first date----
test2 <- unique(test2[, names(test2) %in% c("countriesAndTerritories", "first_date")])

## first date is good, but need to apply this date to data_graphic (not test2 because this gets rid of obs less then 3 post min date).

data_graphic_with_min <- merge(data_graphic, test2, by = "countriesAndTerritories")

data_graphic_with_min <- subset(data_graphic_with_min, dateRep >= first_date)

### Last, need to create days since MA cases > 3-----
data_graphic_with_min$const <- 1
data_graphic_with_min <- data_graphic_with_min %>%
    group_by(countriesAndTerritories) %>%
    arrange(dateRep) %>%
    mutate(days_since_ma_3 = cumsum(const))





####### Graphics -----------------------

## cases graphic
## ggplot(data_graphic_with_min, aes(days_since_ma_3, cumulative_cases, colour = "#0066CC")) +
##  geom_line(data_graphic_with_min$cumulative_cases, colour = 'grey80', show.legend = FALSE) +
##   geom_line(alpha = 0.7, show.legend = FALSE, colour = "#0066CC") +
##   #scale_colour_manual(values = country_colors) +
##   scale_y_log10() +
##     facet_wrap(~countriesAndTerritories)

new_graphic_ggplot <- data_graphic_with_min
new_graphic_ggplot$countriesAndTerritories <- gsub("Democratic_Republic_of_the_Congo", "Dem_Rep_of_the_Congo", new_graphic_ggplot$countriesAndTerritories)
new_graphic_ggplot$y <- new_graphic_ggplot$countriesAndTerritories

ggplot(new_graphic_ggplot, aes(x = days_since_ma_3, y = cumulative_cases)) +
  geom_line(data = transform(new_graphic_ggplot, countriesAndTerritories = NULL), aes(group = y), colour = 'grey80')+
    geom_line(aes(group = countriesAndTerritories), colour = '#0066CC', show.legend = FALSE) +
      scale_y_log10() +
    facet_wrap(~countriesAndTerritories) + #, ncol = 10) +
    labs(x = "Days Since an Average of 3 Cumulative Cases", y = "Cumulative Cases (in log)") 

ggsave("./cumulative_cases.pdf")
ggsave("./cumulative_cases.png")
## saving as Saving 15 x 8.67 in image is the right one


## deaths graphic

ggplot(new_graphic_ggplot, aes(x = days_since_ma_3, y = cumulative_deaths)) +
  geom_line(data = transform(new_graphic_ggplot, countriesAndTerritories = NULL), aes(group = y), colour = 'grey80')+
    geom_line(aes(group = countriesAndTerritories), colour = '#0066CC', show.legend = FALSE) +
      scale_y_log10() +
    facet_wrap(~countriesAndTerritories) + #, ncol = 10) +
    labs(x = "Days Since an Average of 3 Cumulative Cases", y = "Cumulative Deaths (in log)") 

ggsave("./cumulative_deaths.pdf")
ggsave("./cumulative_deaths.png")
## saving as Saving 15 x 8.67 in image is the right one


## cases per million graphic


## deaths per million graphic
