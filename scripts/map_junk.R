data(unemployment)
df_mapdata_dist <- get_data_from_map(download_map_data("countries/us/us-all-all"))
yo = download_map_data("countries/us/us-all-all")


highchart(type = "map") %>%
  hc_chart(backgroundColor = "#161C20") %>% 
  hc_add_series(mapData = hi, showInLegend = FALSE, nullColor = "#424242",
                borderWidth = 0) %>%
  hc_add_series(data = unemployment, type = "", name = "Unemployment", value = "value", joinBy = c("hc-key", "code"),
                borderColor = "transparent") %>%
  hc
  
data("USArrests", package = "datasets")
data("usgeojson")

USArrests <- mutate(USArrests, state = rownames(USArrests))
highchart() %>%
  hc_title(text = "Violent Crime Rates by US State") %>%
  hc_subtitle(text = "Source: USArrests data") %>%
  hc_add_series_map(usgeojson, USArrests, name = "Murder arrests (per 100,000)",
                    value = "Murder", joinBy = c("woename", "state"),
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.postalcode}')) %>%
  hc_colorAxis(stops = color_stops()) %>%
  hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
  hc_mapNavigation(enabled = TRUE)



highchart() %>%
  hc_add_series_map(usgeojson, df_state_summary, value = "prop_rep_projects",
                    joinBy = c("woename", "name"), name = "Proportion Donating Multiple Projects",
                    borderColor = "transparent") %>%
  hc_tooltip(valueDecimals = 1, valueSuffix = "%") %>%
  hc_colorAxis(stops = color_stops(6)) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") %>%
  hc_title(text = "Proportion of Donors Donating to Multiple Projects by State")


df_st











hcmap(yo, data = unemployment,
      name = "Unemployment", value = "value", joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 10, by = 2), 50))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") 


df_mapdata_hc <- get_data_from_map(download_map_data("countries/us/us-all"))


data_fake <- df_mapdata_hc %>% 
  select(code = `hc-a2`) %>% 
  mutate(value = 1e5 * abs(rt(nrow(.), df = 10)))

glimpse(data_fake)

hcmap("countries/us/us-all", data = data_fake, value = "value",
      joinBy = c("hc-a2", "code"), name = "Fake data",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2, valuePrefix = "$", valueSuffix = " USD")) 


sf_fifty <- sf::st_as_sf(fifty_states, coords = c("long", "lat")) %>% 
  group_by(id) %>% 
  summarize(do_union=FALSE) %>%
  st_cast("POLYGON") %>% 
  ungroup()


leaflet(sf_fifty) %>% addPolygons()

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)

# map_id creates the aesthetic mapping to the state name column in your data
p <- ggplot(crimes, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = Assault), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

p


states_gg <- map_data('state')

states_gg %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "white")

states <- 
  geojson_read( 
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
    , what = "sp"
  )

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  states$name, states$density
) %>% lapply(htmltools::HTML)

leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
            position = "bottomright")