#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(DT)
library(leaflet)
library(tidyverse)
library(classInt)
library(cowplot)
library(lubridate)
library(RColorBrewer)
library(toOrdinal)

date = dmy("18-06-2020")

load('./WHO_report_analysis_2020-06-18.RData')
source("./plotting_functions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$date_surveillance <- renderText({
        paste0("Updated : ", format(date,"%d %b %Y"))})
    
    output$date_seq <- renderText({
        paste0("Updated : ", format(date,"%d %b %Y"))})
    
    # Values for the WHO AFRO region total cases and deaths
    output$total_cases <- renderText({
        paste0("WHO Afro Region Cases : ",WHO_cases_and_deaths %>% filter(date == max(date)) %>% pull(cum_cases) %>% sum())})
    
    output$total_deaths <- renderText({
        paste0("WHO Afro Region Deaths : ",WHO_cases_and_deaths %>% filter(date == max(date)) %>% pull(cum_deaths) %>% sum())})
    
    
    
    # vector to hold all country click events
    country_click_history <- "NA"
    
    
    # tibble to hold whether a country has been selected
    country_click_reactive <- reactiveVal( tibble(country = africa$location, clicked = FALSE))
    
    
    output$AFRO_map <- renderLeaflet({
        # if country on map is clicked, change state
        
        if(!is.null(input$AFRO_map_shape_click$id)){
            if(input$AFRO_map_shape_click$id == country_click_history) {
                assign("country_click_history", "NA",inherits = TRUE)}
            else{
                assign("country_click_history", input$AFRO_map_shape_click$id,inherits = TRUE)
                country_click_reactive_updated <- country_click_reactive()
                select_country <- (country_click_reactive_updated$country == input$AFRO_map_shape_click$id)
                select_country[is.na(select_country)] <- FALSE
                country_click_reactive_updated$clicked[select_country] <- !country_click_reactive_updated$clicked[select_country]
                country_click_reactive(country_click_reactive_updated)}}
        # ensure at least one country is selected
        if(sum(country_click_reactive()$clicked) == 0){
            country_click_reactive_updated <- country_click_reactive()
            country_click_reactive_updated$clicked[country_click_reactive_updated$country == sample(country_click_reactive_updated %>% filter(!is.na(country)) %>% pull(country),1)] <- TRUE
            country_click_reactive(country_click_reactive_updated)}
        
        # plot map of africa with chosen colour scheme
        africa_map_plot(input,country_click_reactive,africa)}) 
    
    output$country_plot <- renderPlot({
        # align chosen y-axis values with column names in WHO_cases_and_deaths tibble
        chosen_cases <- paste0("cum_cases",input$y_axis_value)
        chosen_deaths <- paste0("cum_deaths",input$y_axis_value)
        
        # rename côte d'ivoire
        renamed_country_click_reactive <- country_click_reactive()
        renamed_country <- renamed_country_click_reactive$country
            renamed_country[renamed_country == "Côte d’Ivoire"] = "Cote d'Ivoire"
        renamed_country_click_reactive$country <- renamed_country
        
        current_country_data <- WHO_cases_and_deaths %>% 
            transmute(
                country,
                date,
                y_axis_cases = WHO_cases_and_deaths %>% pull(chosen_cases),
                y_axis_deaths = WHO_cases_and_deaths %>% pull(chosen_deaths),
                current_country = (country == renamed_country_click_reactive %>% filter(clicked) %>% pull(country)))
        
        # align colours with map border colours
        country_colour_order <-  africa$location[africa$location %in% (renamed_country_click_reactive %>% filter(clicked) %>% pull(country))]
        
        # remove zeros if in log10 scale
        if(input$y_axis_scale == "Log10"){
            current_country_data_cases <- current_country_data %>%
                filter(y_axis_cases > 0)
            current_country_data_deaths <- current_country_data %>%
                filter(y_axis_deaths > 0)}
        else{
            current_country_data_cases <- current_country_data
            current_country_data_deaths <- current_country_data}
        
        # scale graph y-axes according to current country's maximum value
        current_country_max_cases <- current_country_data %>% 
            filter(current_country) %>%
            pull(y_axis_cases) %>%
            max()
        
        current_country_max_deaths <- current_country_data %>% 
            filter(current_country) %>%
            pull(y_axis_deaths) %>%
            max()
        
        cases_max <- ifelse(input$y_axis_value == "",40,0.1)
        
        death_max <- ifelse(input$y_axis_value == "",20,0.1)
        
        cases_min <- ifelse(input$y_axis_value == "",1,0.0001)
        
        death_min <- ifelse(input$y_axis_value == "",1,0.0001)
        
        if(sum(country_click_reactive()$clicked) > 1){
            cases_plot <- ggplot() +
                geom_line(data=current_country_data_cases %>% filter(current_country),
                          aes(x=date,y=y_axis_cases,colour=country),
                          size = 0.8, show.legend = FALSE) +
                theme_classic(base_size = 20) + 
                theme(panel.grid.major.y = element_line(colour = "lightgrey",size=0.02),
                      axis.title.x = element_blank(),
                      plot.title = element_text(hjust = 0.5)) +
                labs(title="Cases",y="Cumulative") +
                scale_y_function(scale_type=input$y_axis_scale) + 
                scale_color_manual(values = brewer.pal(sum(country_click_reactive()$clicked), name = "Dark2"), breaks = country_colour_order) +
                scale_x_date(date_labels = "%b %d",breaks=c(today-84,today-63,today-42,today-21,today)) +
                coord_cartesian(ylim = c(ifelse(input$y_axis_scale == "Log10",cases_min,0),ifelse(current_country_max_cases > cases_max, current_country_max_cases,cases_max)))
            
            deaths_plot <- ggplot() +
                geom_line(data=current_country_data_deaths %>% filter(current_country),
                          aes(x=date,y=y_axis_deaths,colour=country),
                          size = 0.8, show.legend = TRUE) +
                theme_classic(base_size = 20) +
                theme(panel.grid.major.y = element_line(colour = "lightgrey",size=0.02),
                      plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
                labs(title = "Deaths", y="Cumulative",x="Date") +
                scale_y_function(scale_type=input$y_axis_scale) + 
                scale_color_manual(values = brewer.pal(sum(country_click_reactive()$clicked), name = "Dark2"), breaks = country_colour_order) +
                       scale_x_date(date_labels = "%b %d",breaks=c(today-84,today-63,today-42,today-21,today)) +
                coord_cartesian(ylim = c(ifelse(input$y_axis_scale == "Log10",death_min,0),ifelse(current_country_max_deaths > death_max, current_country_max_deaths,death_max)))}
        else{
            cases_plot <- ggplot() +
                geom_line(data=current_country_data_cases %>% filter(!current_country),
                          aes(x=date,y=y_axis_cases,group=country),
                          colour = "lightgray") +
                geom_line(data=current_country_data_cases %>% filter(current_country),
                          aes(x=date,y=y_axis_cases,group=NULL),
                          colour="black",size = 0.8) +
                theme_classic(base_size = 20) + 
                theme(panel.grid.major.y = element_line(colour = "lightgrey",size=0.02),
                      axis.title.x = element_blank(),
                    plot.title = element_text(hjust = 0.5)) +
                labs(title="Cases",y="Cumulative") +
                scale_y_function(scale_type=input$y_axis_scale) + 
                scale_x_date(date_labels = "%b %d",breaks=c(today-84,today-63,today-42,today-21,today)) +
                coord_cartesian(ylim = c(ifelse(input$y_axis_scale == "Log10",cases_min,0),ifelse(current_country_max_cases > cases_max, current_country_max_cases,cases_max)))
        
            deaths_plot <- ggplot() +
                geom_line(data=current_country_data_deaths %>% filter(!current_country),
                          aes(x=date,y=y_axis_deaths,group=country),
                          colour = "lightgray") +
                geom_line(data=current_country_data_deaths %>% filter(current_country),
                          aes(x=date,y=y_axis_deaths,group=NULL),
                          colour="black",size = 0.8) +
                theme_classic(base_size = 20) +
                theme(panel.grid.major.y = element_line(colour = "lightgrey",size=0.02),
                      plot.title = element_text(hjust = 0.5)) +
                labs(title = "Deaths", y="Cumulative",x="Date") +
                scale_y_function(scale_type=input$y_axis_scale) + 
                scale_x_date(date_labels = "%b %d",breaks=c(today-84,today-63,today-42,today-21,today)) +
                coord_cartesian(ylim = c(ifelse(input$y_axis_scale == "Log10",death_min,0),ifelse(current_country_max_deaths > death_max, current_country_max_deaths,death_max)))}
        plot_grid(cases_plot,
                  deaths_plot,
                  nrow = 2,
                  align = "hv")})
    
    output$cases_cumulative_table = DT::renderDataTable(server = FALSE, {
        DT::datatable(WHO_latest_day_cases_and_deaths_simulated %>%
            transmute(Country = country,
                      `Cum. reported cases` = last_day_case_obs,
                      `95%CI lower` = last_day_case_ci_low,
                      `95%CI upper` = last_day_case_ci_high), 
        rownames = FALSE, 
        options = list(dom = 'Bfrtip',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                       "iDisplayLength" = 15, 
                       "aLengthMenu"= c(15, 30, 50, 100)), 
        extensions = 'Buttons') })
    
    output$deaths_cumulative_table = DT::renderDataTable({
        DT::datatable(WHO_latest_day_cases_and_deaths_simulated %>%
            transmute(Country = country,
                      `Cum. reported cases` = last_day_deaths_obs,
                      `95%CI lower` = last_day_deaths_ci_low,
                      `95%CI upper` = last_day_deaths_ci_high), 
            rownames = FALSE,  
            options = list(dom = 'Bfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           "iDisplayLength" = 15, 
                           "aLengthMenu"= c(15, 30, 50, 100)), 
            extensions = 'Buttons') })
    
    output$deaths_doubling_table = DT::renderDataTable({
        DT::datatable(WHO_cases_and_deaths_doubling_time %>%
            ungroup() %>%
            transmute(Country = country,
                      `Doubling time (days)` = deaths_doubling_time,
                      `95%CI lower` = deaths_ci_low,
                      `95%CI upper` = deaths_ci_upp), 
            rownames = FALSE,  
            options = list(dom = 'Bfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           "iDisplayLength" = 15, 
                           "aLengthMenu"= c(15, 30, 50, 100)), 
            extensions = 'Buttons')})
    
    output$cases_doubling_table = DT::renderDataTable({
        DT::datatable(WHO_cases_and_deaths_doubling_time %>%
            ungroup() %>%
            transmute(Country = country,
                      `Doubling time (days)` = cases_doubling_time,
                      `95%CI lower` = cases_ci_low,
                      `95%CI upper` = cases_ci_upp), 
            rownames = FALSE,  
            options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           "iDisplayLength" = 15, 
                           "aLengthMenu"= c(15, 30, 50, 100)), 
            extensions = 'Buttons')})
    
    output$cases_time_series_table <- DT::renderDataTable({
        DT::datatable(
            WHO_cases_and_deaths %>%
                transmute(Date = date, 
                         Country = country,
                         "Cumulative Cases" = cum_cases,
                         "New Cases" = cases,
                         "Cumulative Cases Per 10k" = signif(cum_cases_per_10k,3)), 
            rownames = FALSE,  
            options = list(dom = 'Bfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           "iDisplayLength" = 15, 
                           "aLengthMenu"= c(15, 30, 50, 100)), 
            extensions = 'Buttons')})
    
    output$deaths_time_series_table <- DT::renderDataTable({
        DT::datatable(
            WHO_cases_and_deaths %>%
                transmute(Date = date, 
                         Country = country,
                         "Cumulative Deaths" = cum_deaths,
                         "New Deaths" = deaths,
                         "Cumulative Deaths Per 10k" = signif(cum_deaths_per_10k,3)), 
            rownames = FALSE,  
            options = list(dom = 'Bfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           "iDisplayLength" = 15, 
                           "aLengthMenu"= c(15, 30, 50, 100)), 
            extensions = 'Buttons')})
    
    output$seq_plot <- renderPlot({
        ggplot(AF, aes(betterDates, fill=str_wrap(country,20))) +
            theme_bw(base_size = 20) +
            scale_x_date(date_labels = "%d/%m") +
            geom_bar(width=1,colour="black") +
            ylab('SARS-CoV-2 genomes on GISAID') +
            xlab('Sample collection date') +
            scale_y_continuous(breaks=seq(0, 8, by = 2)) + 
            scale_fill_brewer(palette = "BrBG") + 
            labs(fill = 'Country') +
            theme(panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.position = "bottom",
                  legend.key.size = unit(0.7, "cm"))})
    
    output$seq_text_overview <- renderText({
        paste0("As of ", format(date,"%d %b %Y"),", ", length(AF$Collection.date), " SARS-CoV-2 genomes collected and sequenced in Africa have been submitted to
               GISAID.")})
    
    output$seq_map <- renderLeaflet({
        breaks <- classIntervals(africa@data$sequence, n = 4, style='jenks', na.rm=T)$brks
        pal <- colorBin(palette = "Oranges", domain = NULL, bins = breaks, na.color = "#000000")
        
        leaflet(africa) %>%
            addTiles() %>%
            setView(lat=1.261,lng=15,zoom=3) %>%
            setMaxBounds(lat1 = 40, lng1=-30, lat2 = -50, lng2=60) %>%
            addPolygons(
                stroke = FALSE,
                smoothFactor = 0.3,
                fillOpacity = 0.5,
                fillColor = ~pal(sequence),
                label = ~paste0(SOVEREIGNT, ": ", formatC(sequence, big.mark = ","))) %>%
            addLegend(
                pal = pal, 
                values = ~sequence, 
                opacity = 1.0,
                na.label = "Non WHO Afro country",
                position = "bottomleft",
                title = "Sequences Collected")})
    
    output$date_data_case <- renderText({
        paste0(" (accessed 2400 ", format(date,"%d %b %Y"),").")})
    
    output$date_data_deaths <- renderText({
        paste0(" (accessed 2400 ", format(date,"%d %b %Y"),").")})
    
    # Create country specific summary text
    
    output$country_summary_text <- renderText({
        
        if(sum(country_click_reactive()$clicked) == 1 ){
            country_summary_text <- country_summary_text_function(country_click_reactive() %>% filter(clicked) %>% pull(country), WHO_latest_day_cases_and_deaths_simulated, WHO_cases_and_deaths_doubling_time)
            
            return(paste0(country_summary_text$sentence_1,
                       country_summary_text$sentence_2,
                       country_summary_text$sentence_3,
                       country_summary_text$sentence_4))}})})
