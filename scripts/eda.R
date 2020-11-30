# setup
library(readr)
library(tidyverse)
library(leaflet)
library(highcharter)

df_donors <- read_csv("data/Donors.csv")
df_donations <- read_csv("data/Donations.csv")
#df_projects <- read_csv("data/Projects.csv")
#df_resources <- read_csv("data/Resources.csv")
df_schools <- read_csv("data/Schools.csv")
#df_teachers <- read_csv("data/Teachers.csv")

df_mapdata_states <- get_data_from_map(download_map_data("countries/us/us-all"))
df_mapdata_counties <- get_data_from_map(download_map_data("countries/us/us-all-all"))

df_state_mapping = df_mapdata_states %>%
  transmute(state_code = `hc-a2`, state_name = name) %>%
  distinct(state_code, state_name)

df_county_mapping = df_mapdata_counties %>%
  transmute(county_code = `hc-key`, county_name = name) %>%
  distinct(county_code, county_name)

# donors' master table
df_donations_summary = df_donations %>%
  group_by(`Donor ID`) %>%
  arrange(`Donation Received Date`) %>%
  mutate(time_between_donations = round(as.numeric(`Donation Received Date`-lag(`Donation Received Date`), units = 'days'))) %>%
  summarise(num_projects = n_distinct(`Project ID`), 
            num_donations = n(), 
            sum_donations = sum(`Donation Amount`),
            first_donation = min(`Donation Received Date`),
            last_donation = max(`Donation Received Date`),
            mean_donation_gap = mean(time_between_donations, na.rm = TRUE),
            max_donation_gap = max(time_between_donations, na.rm = TRUE)) %>%
  mutate(repeat_donations = if_else(num_donations > 1, 1, 0), 
         repeat_projects = if_else(num_projects > 1, 1, 0),
         average_donation = sum_donations/num_donations) %>%
  left_join(df_donors)

# distribution comparison
df_donations_dist = df_donations_summary %>%
  group_by(num_projects) %>%
  summarise(num_donors = n(), sum_donations = sum(sum_donations)) %>%
  mutate(average_donation = sum_donations/num_donors)

df_donations_dist %>%
  hchart('column', hcaes(x = num_projects, y = num_donors)) %>%
  hc_navigator(enabled = TRUE, xAxis = list(labels = list(enabled = FALSE)))

df_donations_dist %>%
  hchart('column', hcaes(x = num_projects, y = average_donation)) %>%
  hc_navigator(enabled = TRUE, xAxis = list(labels = list(enabled = FALSE)))

df_donations_dist %>%
  filter(num_projects< 10) %>%
  hchart('column', hcaes(x = num_projects, y = num_donors)) %>%
  hc_navigator(enabled = TRUE, xAxis = list(labels = list(enabled = FALSE)))

df_donations_dist %>%
  filter(num_projects< 10) %>%
  hchart('column', hcaes(x = num_projects, y = average_donation)) %>%
  hc_navigator(enabled = TRUE, xAxis = list(labels = list(enabled = FALSE)))

# state comparison
df_state_summary = df_donations_summary %>%
  group_by(`Donor State`) %>%
  summarise(prop_rep_donations = 100*sum(repeat_donations)/n(),
            prop_rep_projects = 100*sum(repeat_projects)/n()) %>%
  inner_join(df_state_mapping, by = c('Donor State' = 'state_name')) %>%
  drop_na()
    
hcmap("countries/us/us-all", data = df_state_summary, value = "prop_rep_projects",
      joinBy = c("hc-a2", "state_code"), name = "Proportion Donating Multiple Projects",
      borderColor = "transparent") %>%
  hc_tooltip(valueDecimals = 1, valueSuffix = "%") %>%
  hc_colorAxis(stops = color_stops(6)) %>%
  #hc_colorAxis(dataClasses = color_classes(seq(min(df_state_summary$prop_rep_projects), max(df_state_summary$prop_rep_projects), length.out = 6))) %>% 
hc_legend(layout = "vertical", align = "right",
          floating = TRUE, valueDecimals = 0, valueSuffix = "%") 
  

# time series
df_monthly_summary = df_donations_summary %>%
  mutate(first_month = as.Date(floor_date(first_donation, "month"))) %>%
  #mutate(first_month =  format(as.Date(first_donation), "%Y-%m")) %>%
  group_by(first_month) %>%
  summarise(prop_rep_donations = 100*sum(repeat_donations)/n(),
            prop_rep_projects = 100*sum(repeat_projects)/n()) %>%
  ungroup() %>%
  gather(variable, value, -first_month)

df_monthly_summary %>%
  hchart("line", hcaes(x = first_month, y = value, group = variable)) %>%
  hc_navigator(enabled = TRUE) %>%
  hc_rangeSelector(enabled = TRUE) %>%
  hc_tooltip(shared = TRUE, valueDecimals = 1, valueSuffix = "%")
# sort dates + percentages format


# state/time heatmap (+ animation?)
df_monthly_state_summary = df_donations_summary %>%
  mutate(first_month = as.Date(floor_date(first_donation, "month"))) %>%
  group_by(first_month, `Donor State`) %>%
  summarise(prop_rep_donations = 100*sum(repeat_donations)/n(),
            prop_rep_projects = 100*sum(repeat_projects)/n()) %>%
  ungroup()
  #gather(variable, value, -first_month, -`Donor State`) %>%
  #mutate(donor_state = `Donor State`)

df_monthly_state_summary %>%
  hchart("heatmap", hcaes(y = first_month, x = `Donor State`, value = prop_rep_projects)) %>%
  hc_colorAxis(stops = color_stops(10)) %>%
  hc_tooltip(shared = TRUE, valueDecimals = 1, valueSuffix = "%") %>%
  hc_yAxis(showLastLabel = FALSE, showFirstLabel = FALSE)





  
# time between donations - mean 
df_donations_summary %>%
  mutate(mean_donation_gap = round(mean_donation_gap)) %>%
  group_by(mean_donation_gap) %>%
  summarise(num_donors = n()) %>%
  drop_na() %>%
  hchart("line", hcaes(x = mean_donation_gap, y = num_donors)) %>%
  hc_navigator(enabled = TRUE, xAxis = list(labels = list(enabled = FALSE)))

# time between donations - max 
df_donations_summary %>%
  mutate(max_donation_gap = round(max_donation_gap)) %>%
  group_by(max_donation_gap) %>%
  summarise(num_donors = n()) %>%
  drop_na() %>%
  hchart("line", hcaes(x = max_donation_gap, y = num_donors)) %>%
  hc_navigator(enabled = TRUE, xAxis = list(labels = list(enabled = FALSE)))
  
# time between donations - map  
df_state_time = df_donations_summary %>%
  group_by(`Donor State`) %>%
  summarise(mean_donation_gap = mean(mean_donation_gap, na.rm = TRUE)) %>%
  inner_join(df_state_mapping, by = c('Donor State' = 'state_name')) %>%
  drop_na()
  
hcmap("countries/us/us-all", data = df_state_time, value = "mean_donation_gap",
      joinBy = c("hc-a2", "state_code"), name = "Mean time between donations",
      borderColor = "transparent") %>%
hc_tooltip(valueDecimals = 1) %>%
hc_colorAxis(stops = color_stops(6)) %>%
hc_legend(layout = "vertical", align = "right",
          floating = TRUE, valueDecimals = 0)  
  
  
  
  

# schools map
# map setup

df_schools_joined <- df_schools %>% 
  left_join(df_state_mapping, by = c('School State' = 'state_name')) %>%
  left_join(df_county_mapping, by = c('School County' = 'county_name'))

# maps 1
df_schools_num = df_schools_joined %>%
  group_by(state_code) %>%
  summarise(num_schools = n())

hcmap("countries/us/us-all", data = df_schools_num, value = "num_schools",
      joinBy = c("hc-key", "state_code"), name = "Number of Schools",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1)

# maps 2
df_schools_free_lunch = df_schools_joined %>%
  group_by(state_code) %>%
  summarise(free_lunch_mean = mean(`School Percentage Free Lunch`, na.rm = TRUE))

hcmap("countries/us/us-all", data = df_schools_free_lunch, value = "free_lunch_mean",
      joinBy = c("hc-a2", "state_code"), name = "Mean Free Lunch",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1)
  
# maps 3
df_schools_num_county = df_schools_joined %>%
  group_by(county_code) %>%
  summarise(num_schools = n()) %>%
  ungroup() %>%
  drop_na() %>%
  right_join(df_county_mapping) %>%
  replace_na(list(num_schools = 0))

hcmap("countries/us/us-all-all", data = df_schools_num_county, value = "num_schools",
           joinBy = c("hc-key", "county_code"), name = "Number of Schools",
           borderColor = "transparent") %>%
  #hc_colorAxis() %>%
  hc_colorAxis(dataClasses = color_classes(c(1, 10, 100, 1000, max(df_schools_num_county$num_schools)))) %>%
  hc_title(text = "Where the schools at!?") %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0) 

# maps 4
df_schools_free_lunch_county = df_schools_joined %>%
  group_by(county_code) %>%
  summarise(free_lunch_mean = mean(`School Percentage Free Lunch`, na.rm = TRUE)) %>%
  drop_na()

hc = hcmap("countries/us/us-all-all", data = df_schools_free_lunch_county, value = "free_lunch_mean",
      joinBy = c("hc-a2", "county_code"), name = "Mean Free Lunch",
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(seq(0, 100, by = 20))) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") 










