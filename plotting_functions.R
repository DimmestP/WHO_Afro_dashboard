## Functions to plot the graphs shown in the WHO AFRO Dashboard

# function to scale y-axis if asked
scale_y_function <- function(name=ggplot2::waiver(),
                             scale_type="Linear",
                             ...) {
  if (scale_type == "Linear") {
    return(ggplot2::scale_y_continuous(name,...))
  } else {
    return(ggplot2::scale_y_log10(name,...))
  }
}

africa_map_plot <- function(input,country_click_reactive,africa){
  # set border line colour
  border_colour <- tibble(country = africa$location,colour = "#03F")
  
  if(sum(country_click_reactive()$clicked) > 1){
    border_colour$colour[border_colour$country %in% (country_click_reactive() %>% filter(clicked) %>% pull(country))] <- brewer.pal(n = length(country_click_reactive() %>% filter(clicked) %>% pull(country)), name = "Dark2")
  }
  
  # draw base map of Africa 
  africa_map <- leaflet(africa) %>%
    addTiles() %>%
    setView(lat=1.261,lng=15,zoom=3) %>%
    setMaxBounds(lat1 = 40, lng1=-30, lat2 = -50, lng2=60) 
  
  # Fill country colours with selected variable
  
  if(input$map_fill_colour == "Total cases") {
    # colour scheme to fill map with
    breaks <- classIntervals(africa@data$total_cases, n = 8, style = "jenks", na.rm=T)$brks
    breaks <- c(0,breaks)
    breaks[2]<-1
    pal <- colorBin(palette = "Blues", domain = NULL, bins = breaks, na.color = "#000000")
    africa_map <- africa_map %>%
      addPolygons(
        stroke = country_click_reactive()$clicked,
        color = border_colour$colour, 
        smoothFactor = 0.3,
        fillOpacity = 0.5,
        fillColor = ~pal(total_cases),
        layerId = ~location,
        group = "Cases",
        label = ~paste0(NAME, ": ", formatC(total_cases, big.mark = ",")))  %>%
      addLegend(
        pal = pal, 
        values = ~total_cases, 
        opacity = 1.0,
        na.label = "Non WHO Afro country",
        position = "bottomleft",
        group = "Cases",
        title = "Total Cases")
    }
  else if(input$map_fill_colour == "Total deaths"){
    # colour scheme to fill map with
    breaks <- classIntervals(africa@data$total_deaths, n = 8, style = "jenks", na.rm=T)$brks
    breaks <- c(0,breaks)
    breaks[2]<-1
    pal <- colorBin(palette = "Reds", domain = NULL, bins = breaks, na.color = "#000000")
    
    
    africa_map <- africa_map %>%
      addPolygons(
        stroke = country_click_reactive()$clicked,
        color = border_colour$colour,
        smoothFactor = 0.3,
        fillOpacity = 0.5,
        fillColor = ~pal(total_deaths),
        layerId = ~location,
        group = "Deaths",
        label = ~paste0(NAME, ": ", formatC(total_deaths, big.mark = ","))) %>%
      addLegend(
        pal = pal, 
        values = ~total_deaths, 
        opacity = 1.0,
        na.label = "Non WHO Afro country",
        position = "bottomleft",
        group = "Deaths",
        title = "Total Deaths")}
  else if(input$map_fill_colour == "Total deaths per 10k pop"){
    # colour scheme to fill map with
    breaks <- classIntervals(africa@data$DeathsperPop, n = 8, style = "jenks", na.rm=T)$brks
    breaks <- c(0,breaks)
    breaks[2]<-0.001
    pal <- colorBin(palette = "Reds", domain = NULL, bins = breaks, na.color = "#000000")
    
    
    africa_map <- africa_map %>%
      addPolygons(
        stroke = country_click_reactive()$clicked,
        color = border_colour$colour,
        smoothFactor = 0.3,
        fillOpacity = 0.5,
        fillColor = ~pal(DeathsperPop),
        layerId = ~location,
        group = "Deaths",
        label = ~paste0(NAME, ": ", formatC(DeathsperPop, big.mark = ","))) %>%
      addLegend(
        pal = pal, 
        values = ~DeathsperPop, 
        opacity = 1.0,
        na.label = "Non WHO Afro country",
        position = "bottomleft",
        group = "Deaths",
        title = "Total Deaths Per\n 10k Pop")}
  
  else if(input$map_fill_colour == "Total cases per 10k pop"){
    # colour scheme to fill map with
    breaks <- classIntervals(africa@data$CaseperPop, n = 8, style = "jenks", na.rm=T)$brks
    breaks <- c(0,breaks)
    breaks[2]<-0.001
    pal <- colorBin(palette = "Blues", domain = NULL, bins = breaks, na.color = "#000000")
    
    
    africa_map <- africa_map %>%
      addPolygons(
        stroke = country_click_reactive()$clicked,
        color = border_colour$colour,
        smoothFactor = 0.3,
        fillOpacity = 0.5,
        fillColor = ~pal(CaseperPop),
        layerId = ~location,
        group = "Cases",
        label = ~paste0(NAME, ": ", formatC(CaseperPop, big.mark = ","))) %>%
      addLegend(
        pal = pal, 
        values = ~CaseperPop, 
        opacity = 1.0,
        na.label = "Non WHO Afro country",
        position = "bottomleft",
        group = "Cases",
        title = "Total Cases Per 10k Pop")}
  else if(input$map_fill_colour == "Cases doubling time"){
    # colour scheme to fill map with
    # colour scheme to fill map with
    breaks <- classIntervals(africa@data$Dt_cases[africa@data$Dt_cases>0], n = 7, style = "jenks", na.rm=T)$brks
    breaks <- c(-1,breaks)
    breaks[2]<-0.00001
    palBlue <- brewer.pal(7, name = "Blues")
    palBlue[7] <- "#999999"
    pal <- colorBin(palette = palBlue, domain = NULL, bins = breaks, na.color = "#000000",reverse = TRUE)
    
    africa_map <- africa_map %>%
      addPolygons(
        stroke = country_click_reactive()$clicked,
        color = border_colour$colour,
        smoothFactor = 0.3,
        fillOpacity = 0.5,
        fillColor = ~pal(Dt_cases),
        layerId = ~location,
        group = "Cases",
        label = ~paste0(NAME, ": ", formatC(Dt_cases, big.mark = ","))) %>%
      addLegend(
        colors = c(rev(brewer.pal(6, name = "Blues")),"#999999","#000000"), 
        labels = c(paste0("0-",breaks[3]),
                   paste0(breaks[3],"-",breaks[4]),
                   paste0(breaks[4],"-",breaks[5]),
                   paste0(breaks[5],"-",breaks[6]),
                   paste0(breaks[6],"-",breaks[7]),
                   paste0(breaks[7],"-",breaks[8]),
                   "Unavailable",
                   "Non WHO Afro country"), 
        opacity = 0.5,
        na.label = "Non WHO Afro country",
        position = "bottomleft",
        title = "Cases Doubling Time")}
  else if(input$map_fill_colour == "Death doubling time"){
    # colour scheme to fill map with
    breaks <- classIntervals(africa@data$Dt_deaths[africa@data$Dt_deaths>0], n = 7, style = "jenks", na.rm=T)$brks
    breaks <- c(-1,breaks)
    breaks[2]<-0.00001
    palRed <- brewer.pal(7, name = "Reds")
    palRed[7] <- "#999999"
    pal <- colorBin(palette = palRed, domain = NULL, bins = breaks, na.color = "#000000",reverse = TRUE)
    
    africa_map <- africa_map %>%
      addPolygons(
        stroke = country_click_reactive()$clicked,
        color = border_colour$colour,
        smoothFactor = 0.3,
        fillOpacity = 0.5,
        fillColor = ~pal(Dt_deaths),
        layerId = ~location,
        group = "Deaths",
        label = ~paste0(NAME, ": ", formatC(Dt_deaths, big.mark = ","))) %>%
      addLegend(
        colors = c(rev(brewer.pal(6, name = "Reds")),"#999999","#000000"), 
        labels = c(paste0("0-",breaks[3]),
                   paste0(breaks[3],"-",breaks[4]),
                   paste0(breaks[4],"-",breaks[5]),
                   paste0(breaks[5],"-",breaks[6]),
                   paste0(breaks[6],"-",breaks[7]),
                   paste0(breaks[7],"-",breaks[8]),
                   "Unavailable",
                   "Non WHO Afro country"),
        opacity = 0.5,
        na.label = "Non WHO Afro country",
        position = "bottomleft",
        title = "Deaths Doubling Time")}
  
  return(africa_map)
}

# function to extract all the relevant summary information for the text below the country specific graphs
country_summary_text_function <- function(i, WHO_latest_day_cases_and_deaths_simulated, WHO_cases_and_deaths_doubling_time) {
  
  if(i == "Côte d’Ivoire") i = "Cote d'Ivoire"
  
  currrent_country_latest_cases <- WHO_latest_day_cases_and_deaths_simulated %>%
    filter(country == i) %>%
    pull(last_day_case_obs)
  
  currrent_country_latest_cases_rank <- WHO_latest_day_cases_and_deaths_simulated %>%
    arrange(desc(last_day_case_obs)) %>%
    mutate(rank = 1:length(WHO_latest_day_cases_and_deaths_simulated$country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  currrent_country_latest_deaths <- WHO_latest_day_cases_and_deaths_simulated %>%
    filter(country == i) %>%
    pull(last_day_deaths_obs)
  
  currrent_country_latest_deaths_rank <- WHO_latest_day_cases_and_deaths_simulated %>%
    arrange(desc(last_day_deaths_obs)) %>%
    mutate(rank = 1:length(WHO_latest_day_cases_and_deaths_simulated$country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  currrent_country_latest_cases_per_pop <- WHO_latest_day_cases_and_deaths_simulated %>%
    filter(country == i) %>%
    pull(last_day_case_obs_per_10k)
  
  currrent_country_latest_cases_per_pop_rank <- WHO_latest_day_cases_and_deaths_simulated %>%
    arrange(desc(last_day_case_obs_per_10k)) %>%
    mutate(rank = 1:length(WHO_latest_day_cases_and_deaths_simulated$country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  currrent_country_latest_deaths_per_pop <- WHO_latest_day_cases_and_deaths_simulated %>%
    filter(country == i) %>%
    pull(last_day_deaths_obs_per_10k)
  
  currrent_country_latest_deaths_per_pop_rank <- WHO_latest_day_cases_and_deaths_simulated %>%
    arrange(desc(last_day_deaths_obs_per_10k)) %>%
    mutate(rank = 1:length(WHO_latest_day_cases_and_deaths_simulated$country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  current_country_cases_dt <- WHO_cases_and_deaths_doubling_time  %>%
    filter(country == i) %>%
    pull(cases_doubling_time)
  
  current_country_cases_dt_rank <- WHO_cases_and_deaths_doubling_time %>%
    filter(cases_doubling_time > -1) %>%
    ungroup() %>%
    arrange(cases_doubling_time) %>%
    mutate(rank = 1:length(country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  current_country_deaths_dt <- WHO_cases_and_deaths_doubling_time %>%
    filter(country == i) %>%
    pull(deaths_doubling_time)
  
  current_country_deaths_dt_rank <- WHO_cases_and_deaths_doubling_time %>%
    filter(deaths_doubling_time > -1) %>%
    ungroup() %>%
    arrange(deaths_doubling_time) %>%
    mutate(rank = 1:length(country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  if(current_country_cases_dt %in% c(Inf,-1,NA) | current_country_deaths_dt %in% c(Inf,-1,NA)){
    
    if(current_country_cases_dt %in% c(Inf,-1,NA) & current_country_deaths_dt %in% c(Inf,-1,NA)){
      if(current_country_cases_dt %in% c(Inf,NA)) sentence_3 = " The doubling time of reported cases cannot be calculated as no new cases have been reported in last 7 days."
      else sentence_3 = " The doubling time of reported cases cannot be calculated as some cases have been redacted in last 7 days."
      if(current_country_deaths_dt %in% c(Inf,NA)) sentence_4 = " The doubling time of reported deaths cannot be calculated as no new deaths have been reported in last 7 days."
      else sentence_4 = " The doubling time of reported deaths cannot be calculated as some deaths have been redacted in last 7 days."
    }
    else if(current_country_cases_dt %in% c(Inf,-1,NA)){
      if(current_country_cases_dt %in% c(Inf,NA)) sentence_3 = " The doubling time of reported cases cannot be calculated as no new cases have been reported in last 7 days."
      else sentence_3 = " The doubling time of reported cases cannot be calculated as some cases have been redacted in last 7 days."
      sentence_4 = paste0(" The doubling time over the last 7 days for deaths is ", current_country_deaths_dt, " days (", toOrdinal(current_country_deaths_dt_rank),").")
    }
    else{
      if(current_country_deaths_dt %in% c(Inf,NA)) sentence_4 = " The doubling time of reported deaths cannot be calculated as no new deaths have been reported in last 7 days."
      else sentence_4 = " The doubling time of reported deaths cannot be calculated as some deaths have been redacted in last 7 days."
      sentence_3 = paste0(" The doubling time over the last 7 days for cases is ", current_country_cases_dt, " days (", toOrdinal(current_country_cases_dt_rank), ").")
    }
    
  }
  else {
    sentence_3 = paste0(" Doubling times over the last 7 days for cases and deaths are ", current_country_cases_dt, " days (", toOrdinal(current_country_cases_dt_rank), ") and ", current_country_deaths_dt, " days (", toOrdinal(current_country_deaths_dt_rank),").")
    sentence_4 = NULL
  }
  tibble(
    sentence_1 = paste0(i, " has ", currrent_country_latest_cases, " reported case(s) (", toOrdinal(currrent_country_latest_cases_rank), " in the region) and ", currrent_country_latest_deaths, " reported death(s) (", toOrdinal(currrent_country_latest_deaths_rank), ")."),
    sentence_2 = paste0(" Cumulative counts per 10,000 population for reported cases and deaths are ", signif(currrent_country_latest_cases_per_pop,2), " (", toOrdinal(currrent_country_latest_cases_per_pop_rank), ") and ", signif(currrent_country_latest_deaths_per_pop,2), " (", toOrdinal(currrent_country_latest_deaths_per_pop_rank), ") respectively."),
    sentence_3 = sentence_3,
    sentence_4 = sentence_4
  )
}
