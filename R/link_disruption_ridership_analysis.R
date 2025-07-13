---
  title: "R Notebook"
output: html_notebook
---

  This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code.

```{r Initial set up, error=TRUE, message=FALSE, warning=FALSE, include=FALSE}
#unquote these if you haven't install the packages
install.packages("tidyverse")
install.packages('DBI')
install.packages('odbc')
install.packages("rstudioapi")
install.packages("janitor")
install.packages("gridExtra")

library(DBI) #this package needs to be installed first.
library(odbc)
library(tidyverse)
library(rstudioapi)
library(janitor)
library(ggplot2)
library(gridExtra)


#set working directory to current file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#set up T-Bird connection (require VPN)
server <- "kcitazrsqlprp01.database.windows.net"
database = "tbird_dw"

con <- DBI::dbConnect(odbc::odbc(),
                      Driver="ODBC Driver 17 for SQL Server",
                      Server = server, Database = database,
                      Authentication = "ActiveDirectoryIntegrated")

```

```{r Load transfer stops data, error=TRUE, message=FALSE, warning=FALSE}
transfer_stops_og <- read_csv("feasible_transfer_stops_within_600ft_of_station.csv")

#check data
#head(transfer_stops_og)
#summary(transfer_stops_og)
#colnames(transfer_stops_og)
```

```{r Tidy transfer stops data, error=TRUE, message=FALSE, warning=FALSE, include=FALSE}
#split route_list into multiple rows, while keeping other columns unchanged. Each subpart of the route_list is associated with its original stop_num, station_stop_num, and station_name. Then replace RapidRide letters with RapidRide route numbers, and and replace 21E with 21. Then, remove all leading and trailing spaces around values in the route column. Lastly, convert stop_num values from numbers to strings

transfer_routes_stops_tidy <- transfer_stops_og %>% separate_rows(route_list, sep = ", ") %>%
  rename(route = route_list) %>% mutate(route = recode(route, "C" = "673", "D" = "674", "E" = "675", "H" = "678", "21E" = "21")) %>% mutate(route = str_trim(route)) %>% mutate(stop_num = as.character(stop_num))

#unique(transfer_routes_stops_tidy$route)
```

```{r Define SQL Input parameters, error=TRUE, message=FALSE, warning=FALSE, include=FALSE}
#list of alternative stops less than 600ft from Link stations
alternative_stops <- unique(transfer_routes_stops_tidy$stop_num)

#list of alternative routes to Link disrupted segment
alternative_routes <- unique(transfer_routes_stops_tidy$route)

#dates for analysis
baseline_date_start <- "2025-01-06"
baseline_date_end <- "2025-04-13"
disruption_period_start <- "2025-04-14"
disruption_period_end <- "2025-04-23"

#define SQL query for the stop level data (Link disruption impacted routes and stops)
sql_query <- paste0(
  "SELECT [OPERATION_DATE],
  CASE
      WHEN OPERATION_DATE BETWEEN '", baseline_date_start, "' AND '", baseline_date_end, "' THEN 'pre_disruption_period'
      WHEN OPERATION_DATE BETWEEN '", disruption_period_start, "' AND '", disruption_period_end, "' THEN 'disruption_period'
    END AS period,
    [SERVICE_RTE_LIST] AS route,
    [STOP_ID] AS stop_num,
    [STOP_NM] AS stop_name,
    [ACTUAL_ARRIVAL_SECS_AFTER_MIDNT],
          SUBSTRING([ACTUAL_ARRIVAL_SECS_AFTER_MIDNT], 1, 2) AS hour,
          SUBSTRING([ACTUAL_ARRIVAL_SECS_AFTER_MIDNT], 3, 2) AS min,
          SUBSTRING([ACTUAL_ARRIVAL_SECS_AFTER_MIDNT], 5, 2) AS sec,
    [PSNGR_BOARDINGS] AS ons,
    [PSNGR_ALIGHTINGS] AS offs,
    [DEP_PSNGR_LOAD] AS departing_load,
    [SCHED_DAY_TYPE_CODED_NUM] AS day_type
  FROM [DP].[STOP_ACTIVITY_GRANULAR]
  WHERE (OPERATION_DATE BETWEEN '", baseline_date_start, "' AND '", baseline_date_end, "' OR OPERATION_DATE BETWEEN '", disruption_period_start, "' AND '", disruption_period_end, "')
  AND STOP_ID IN (", paste(alternative_stops, collapse= ", "), ")
  AND SERVICE_RTE_LIST IN (", paste(alternative_routes, collapse= ", "), ")
  ORDER BY OPERATION_DATE")

print(sql_query)

#---------------------
#define SQL query for the trip level data (ridership from rest of the system for difference in difference analysis)
sql_query_trip_else <- paste0(
  "SELECT [OPERATION_DATE],
  CASE
      WHEN OPERATION_DATE BETWEEN '", baseline_date_start, "' AND '", baseline_date_end, "' THEN 'pre_disruption_period'
      WHEN OPERATION_DATE BETWEEN '", disruption_period_start, "' AND '", disruption_period_end, "' THEN 'disruption_period'
    END AS period,
    [SERVICE_RTE_NUM] AS route,
    [TRIP_ID] AS trip_id,
    [ACTUAL_START_TIME_MNTS_AFTER_MIDNT] AS arrival_time_min,
    [PSNGR_BOARDINGS] AS ons,
    [PSNGR_ALIGHTINGS] AS offs,
    [MAX_PSNGR_LOAD] AS max_passenger_load,
    [SCHED_DAY_TYPE_CODED_NUM] AS day_type
  FROM [DP].[TRIP_DETAIL]
  WHERE (OPERATION_DATE BETWEEN '", baseline_date_start, "' AND '", baseline_date_end, "' OR OPERATION_DATE BETWEEN '", disruption_period_start, "' AND '", disruption_period_end, "')
  AND SERVICE_RTE_NUM NOT IN (118, 119, 208, 204, 224, 630, 631, 635, 773, 775, 901, 903, 906, 907, 914, 915, 917, 930, ", paste(alternative_routes, collapse= ", "), ")
  ORDER BY OPERATION_DATE")
print(sql_query_trip_else)

#---------------------
#define SQL query for affected routes data ()
sql_query_trip_affected <- paste0(
  "SELECT [OPERATION_DATE],
  CASE
      WHEN OPERATION_DATE BETWEEN '", baseline_date_start, "' AND '", baseline_date_end, "' THEN 'pre_disruption_period'
      WHEN OPERATION_DATE BETWEEN '", disruption_period_start, "' AND '", disruption_period_end, "' THEN 'disruption_period'
    END AS period,
    [SERVICE_RTE_NUM] AS route,
    [TRIP_ID] AS trip_id,
    [ACTUAL_START_TIME_MNTS_AFTER_MIDNT] AS arrival_time_min,
    [PSNGR_BOARDINGS] AS ons,
    [PSNGR_ALIGHTINGS] AS offs,
    [MAX_PSNGR_LOAD] AS max_passenger_load,
    [SCHED_DAY_TYPE_CODED_NUM] AS day_type
  FROM [DP].[TRIP_DETAIL]
  WHERE (OPERATION_DATE BETWEEN '", baseline_date_start, "' AND '", baseline_date_end, "' OR OPERATION_DATE BETWEEN '", disruption_period_start, "' AND '", disruption_period_end, "')
  AND SERVICE_RTE_NUM IN (", paste(alternative_routes, collapse= ", "), ")
  ORDER BY OPERATION_DATE")

```

```{r Load T-Bird data for analysis, error=TRUE, message=FALSE, warning=FALSE, include=FALSE}
#execute query on above connected T-bird database
tbird_data <- dbGetQuery(con, sql_query)
tbird_data_rest <- dbGetQuery(con, sql_query_trip_else)
tbird_data_trip_affected <- dbGetQuery(con, sql_query_trip_affected)

## Stop_level data cleanup
#clean the stop_level data in tbird_data dataframe
tbird_data <- tbird_data %>%
  mutate(hour = as.numeric(hour),
         min = as.numeric(min),
         sec = as.numeric(sec),
         arrival_time_min = (hour*60) + min + (sec/60),
         time_period = case_when(arrival_time_min >= 300 & arrival_time_min < 540 ~ 'AM Peak',
                                 arrival_time_min >= 540 & arrival_time_min < 900 ~ 'Midday',
                                 arrival_time_min >= 900 & arrival_time_min < 1140 ~ 'PM Peak',
                                 arrival_time_min >= 1140 & arrival_time_min < 1320 ~ 'Evening',
                                 TRUE ~ 'Night')) %>%
  mutate(day_type = recode(as.character(day_type), "0" = "Weekday", "1" = "Saturday", "2" = "Sunday")) %>%
  #select(-ACTUAL_ARRIVAL_SECS_AFTER_MIDNT, -arrival_time_min) %>%
  janitor::clean_names()

#average across the days for baseline period and period with disruption
tbird_data_avg <- tbird_data %>%
  group_by(period, route, stop_num, stop_name, hour, time_period, day_type ) %>%
  summarise(across(ons:offs:departing_load, ~ round(mean(.)), .names = "avg_{.col}"))

#--------------------------
## Trip_level data cleanup
#clean the trip_level data in tbird_data_rest dataframe
tbird_data_rest <- tbird_data_rest %>%
  mutate(hour = floor(arrival_time_min/60),
         time_period = case_when(arrival_time_min >= 300 & arrival_time_min < 540 ~ 'AM Peak',
                                 arrival_time_min >= 540 & arrival_time_min < 900 ~ 'Midday',
                                 arrival_time_min >= 900 & arrival_time_min < 1140 ~ 'PM Peak',
                                 arrival_time_min >= 1140 & arrival_time_min < 1320 ~ 'Evening',
                                 TRUE ~ 'Night')) %>%
  mutate(day_type = recode(as.character(day_type), "0" = "Weekday", "1" = "Saturday", "2" = "Sunday")) %>%
  filter(day_type != "6") %>%
  select(-arrival_time_min) %>%
  janitor::clean_names()

#clean trip-level data for AFFECTED routes
tbird_data_trip_affected <- tbird_data_trip_affected %>%
  mutate(
    hour = floor(arrival_time_min / 60),
    time_period = case_when(
      arrival_time_min >= 300 & arrival_time_min < 540 ~ 'AM Peak',
      arrival_time_min >= 540 & arrival_time_min < 900 ~ 'Midday',
      arrival_time_min >= 900 & arrival_time_min < 1140 ~ 'PM Peak',
      arrival_time_min >= 1140 & arrival_time_min < 1320 ~ 'Evening',
      TRUE ~ 'Night'
    ),
    day_type = recode(as.character(day_type),
                      "0" = "Weekday", "1" = "Saturday", "2" = "Sunday")
  ) %>%
  filter(day_type != "6") %>%
  select(-arrival_time_min) %>%
  janitor::clean_names()

# Add route group tags and combine both datasets
tbird_data_trip_affected <- tbird_data_trip_affected %>%
  mutate(route_group = "affected")

tbird_data_rest <- tbird_data_rest %>%
  mutate(route_group = "rest_of_network")

# Combine for route-level difference-in-difference analysis
tbird_data_combined <- bind_rows(tbird_data_trip_affected, tbird_data_rest)

#average across the days for baseline period and period with disruption
tbird_data_rest_avg <- tbird_data_rest %>%
  group_by(period, route, hour, time_period, day_type) %>%
  summarise(across(ons:offs:max_passenger_load, ~ round(mean(.)), .names = "avg_{.col}"))

#export data into csv
#write.csv(tbird_data_avg, file = "tbird_data_avg.csv", row.names = FALSE)
```

The T-Bird data comes from [DP].[STOP_ACTIVITY_GRANULAR] database. In its current form, it includes the total of ons, offs, and departing loads per trip, by day, by route and by stop number.

For the top-line daily ridership comparisons, the data comes from [DP].[TRIP_DETAIL] because it is cleaner and more summarized up.


## Total Daily Ridership Change
First, let's take a look at the high-level change, do we see a significant difference in overall daily ridership across all applicable routes and stops for Weekdays, Saturday and Sunday?

```{r Set up data for daily ridership change analysis, echo=FALSE, error=TRUE, message=FALSE, warning=FALSE}

#summarize up from hourly to daily ridership data
ridership_by_day <- tbird_data_avg %>%
  group_by(period, day_type) %>%
  summarise(across(avg_ons:avg_offs:avg_departing_load, sum, .names = "sum_{.col}"))

#reshape data into wide form to enable easy calculation of difference
ridership_diff_by_day <- ridership_by_day %>%
  pivot_wider(names_from = period,
              values_from = c(sum_avg_ons, sum_avg_offs, sum_avg_departing_load),
              values_fill = NA)

#calculate difference between pre_disruption_period and disruption_period
ridership_diff_by_day <- ridership_diff_by_day %>%
  mutate(diff_avg_ons = sum_avg_ons_disruption_period - sum_avg_ons_pre_disruption_period,
         diff_avg_offs = sum_avg_offs_disruption_period - sum_avg_offs_pre_disruption_period,
         diff_avg_departing_load = sum_avg_departing_load_disruption_period - sum_avg_departing_load_pre_disruption_period)


#melt data into long format for easier plotting
long_ridership_diff_by_day <- ridership_diff_by_day %>%
  select(day_type, diff_avg_ons, diff_avg_offs, diff_avg_departing_load) %>%
  pivot_longer(
    cols = starts_with("diff_"),
    names_to = "metric",
    values_to = "difference"
  )

#order data for ease of graphing
long_ridership_diff_by_day$day_type <- factor(long_ridership_diff_by_day$day_type, levels = c("Weekday", "Saturday", "Sunday"))

long_ridership_diff_by_day$metric <- factor(long_ridership_diff_by_day$metric, levels = c("diff_avg_ons", "diff_avg_offs", "diff_avg_departing_load"))

# Create a bar graph
daily_ridership_change_graph <- ggplot(long_ridership_diff_by_day, aes(x = day_type, y = difference, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c(
      "diff_avg_ons" = "#006848",
      "diff_avg_offs" = "#FDB71A",
      "diff_avg_departing_load" = "#0072BC"),
    labels = c(
      "diff_avg_ons" = "Change in average boardings",
      "diff_avg_offs" = "Change in average alighting",
      "diff_avg_departing_load" = "Change in average departing load")
    ) +
  geom_text(aes(label = difference),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  labs(
    title = "Daily Ridership Change Before and During Disruption Periods, across all stops",
    x = "Day Type",
    y = "Difference",
    fill = "Metric"
  ) +
  theme_minimal()

daily_ridership_change_graph
```

Then, the following graph helps puts the difference in boarding and alighting in perspective - how does that compare to the change in the rest of the KCM network for the same pre-disruption and disruption periods?
```{r Set up data for difference in difference analysis, echo=FALSE, message=FALSE, warning=FALSE}

#summarize up from hourly to daily ridership data
ridership_rest_by_day <- tbird_data_rest_avg %>%
  group_by(period, day_type) %>%
  summarise(across(avg_ons:avg_offs, sum, .names = "sum_{.col}"))

#reshape data into wide form to enable easy calculation of difference
ridership_diff_rest_by_day <- ridership_rest_by_day %>%
  pivot_wider(names_from = period,
              values_from = c(sum_avg_ons, sum_avg_offs),
              values_fill = NA)

#calculate difference between pre_disruption_period and disruption_period
ridership_diff_rest_by_day <- ridership_diff_rest_by_day %>%
  mutate(diff_avg_ons = sum_avg_ons_disruption_period - sum_avg_ons_pre_disruption_period,
         diff_avg_offs = sum_avg_offs_disruption_period - sum_avg_offs_pre_disruption_period)



#melt data into long format for easier plotting
long_ridership_diff_rest_by_day <- ridership_diff_rest_by_day %>%
  select(day_type, diff_avg_ons, diff_avg_offs) %>%
  pivot_longer(
    cols = starts_with("diff_"),
    names_to = "metric",
    values_to = "difference"
  )

#join table of differences
compare_diff_by_day <- long_ridership_diff_rest_by_day %>%
  bind_rows(long_ridership_diff_by_day, .id = "id") %>%
   mutate(id = recode(as.character(id), "1" = "rest_of_network", "2" = "routes_alternative_to_link"),
          metric = recode(metric, "diff_avg_ons" = "avg ons", "diff_avg_offs" = "avg offs")) %>%
  filter(metric != "diff_avg_departing_load")

#order data for ease of graphing
compare_diff_by_day$day_type <- factor(compare_diff_by_day$day_type, levels = c("Weekday", "Saturday", "Sunday"))
compare_diff_by_day$metric <- factor(compare_diff_by_day$metric, levels = c("avg ons", "avg offs"))
compare_diff_by_day$id <- factor(compare_diff_by_day$id, levels = c("routes_alternative_to_link", "rest_of_network"))


# Create a bar graph
compare_diff_by_day_graph <- ggplot(compare_diff_by_day, aes(x = metric, y = difference, fill = id)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~day_type) +
  geom_text(aes(label = difference),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(
    values = c(
      "rest_of_network" = "#0072BC",
      "routes_alternative_to_link" = "#F57F29"),
    labels = c(
      "rest_of_network" = "Routes in the rest of the network",
      "routes_alternative_to_link" = "Routes that are alternative to link")
    ) +
  labs(
    title = "Daily Ridership Change Before and During Disruption Periods",
    subtitle = "Compare alternative routes to Link and routes in the rest of the KCM network",
    x = "Day Type",
    y = "Difference (during disruption - before disruption)",
    fill = "Metric"
  ) +
  theme_minimal()

compare_diff_by_day_graph
```
