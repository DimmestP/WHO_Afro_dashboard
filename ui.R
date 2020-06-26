#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)
library(leaflet)

sidebar <- dashboardSidebar(
    img(
        src= "./tiba_logo.png", 
        alt="TIBA Logo", 
        height="110em", 
        width="206em"),
    p(),
    sidebarMenu(menuItem("Map", tabName = "Map")),
    selectInput("map_fill_colour","Map fill colour :",c("Total cases","Total deaths","Total cases per 10k pop","Total deaths per 10k pop","Cases doubling time","Death doubling time")),
    radioButtons("y_axis_value","Plot y-axis :",c("Total" = "","Per 10k pop" = "_per_10k"), inline = TRUE),
    radioButtons("y_axis_scale","Plot y-axis scale :",c("Linear","Log10"), inline = TRUE),
    sidebarMenu(
        menuItem("Data Tables", tabName = "tables"),
        menuItem("About", tabName = "about")),
    width = "15em")
    

body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "Map",
            fluidRow(
                box(
                    h2("COVID-19 situation report\n for the WHO Africa Region", style="color:Orange"),
                    h3("Produced by the TIBA COVID-19 Pandemic Response Unit", style="color:DarkRed"),
                    h3(textOutput("date_surveillance"),
                    width = 12),
                    width = 5),
                box(
                    h2("Instructions"),
                    h3("Click on the map to select a country and plot its COVID-19 survellience data."),
                    h3("Multiple countries can be selected for direct comparisons."),
                    h3("Use the sidebar settings to adjust the plots."),
                    width = 7)),
            fluidRow(
                box(
                    leafletOutput(
                        "AFRO_map",
                        height = "60em")),
                box(
                    plotOutput(
                        "country_plot",
                        height = "51em"))),
            fluidRow(
                box(h2(textOutput("total_cases"),style="color:SteelBlue"),
                    h2(textOutput("total_deaths"), style="color:Tomato"),
                    width = 4),
                box(h3(textOutput("country_summary_text")),
                    width = 8))),
        
        tabItem(
            tabName = "tables",
            tabBox(
                title = "Country Case Data",
                id = "tabset1", height = "250px",
                tabPanel("Latest Cumulative Counts", DT::dataTableOutput("cases_cumulative_table")),
                tabPanel("Doubling Times", DT::dataTableOutput("cases_doubling_table")),
                tabPanel("Time Series", DT::dataTableOutput("cases_time_series_table"))),
            tabBox(
                title = "Country Death Data",
                id = "tabset1", height = "250px",
                tabPanel("Latest Cumulative Counts", DT::dataTableOutput("deaths_cumulative_table")),
                tabPanel("Doubling Times", DT::dataTableOutput("deaths_doubling_table")),
                tabPanel("Time Series", DT::dataTableOutput("deaths_time_series_table")))),
       tabItem(
            tabName = "about",
            fluidPage(
                withMathJax(),
                box(
                    title = h2("Methods"),
                    width = 12,
                    h4('Doubling times are calculated over the prior 7 days using method described by E. Vynnycky & R. White (2010) An Introduction to Infectious Disease Modelling, page 74,'),
                    "$$Dt = (T_2 - T_1)\\frac{ln(2)}{ln\\Big(\\frac{N_2}{N_1}\\Big)},$$",
                    h4(' \\(N_1\\) and \\(N_1\\) are the cumulative counts of cases/deaths at times \\(T_1\\) and \\(T_2\\), respectively.'),
                    h4("Confidence intervals are calculated using bootstrapping of a simulated dataset with Poisson error structure, using method published here: ",tags$a("https://doi.org/10.1101/2020.02.05.20020750)",href="https://doi.org/10.1101/2020.02.05.20020750)"))),
                box(
                    title = h2("Caveats"), 
                    h4("Case count data are affected by any changes in testing strategy or testing effort over time and/or any variation in testing strategy or testing effort between regions."),
                    h4("Case count data are likely a substantial under-representation of the true number of COVID-19 infections."),
                    h4("Death data are considered more reliable but may lag behind case data by as much as 3 weeks."),
                    h4("Data are provided by WHO Afro but were smoothed where necessary (e.g. if a decrease in cumulative count was reported) for doubling time calculations.)"),
                    width = 12),
                box(
                    title = h2("Data"),
                    h4("Case counts for countries in WHO Africa Region from WHO Coronavirus disease (COVID-2019) situation reports", 
                            tags$a("https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports",href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports"),
                            textOutput("date_data_case")), 
                    h4("Death counts for countries in WHO African Region from WHO Coronavirus disease (COVID-2019) situation reports",
                            tags$a("https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports",href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports"),
                            textOutput("date_data_deaths")),
                    h4("Population counts from the World Bank (2018)",
                                tags$a("https://data.worldbank.org/indicator/SP.POP.TOTL?locations=ZG",href="https://data.worldbank.org/indicator/SP.POP.TOTL?locations=ZG"),
                                " (accessed 0900 30/03/2020)"),
                    width = 12),
                box(
                    title = h2("Credits"),
                    h4("This site and data visualisation was prepared by members of the Unversity of Edinburgh's ",tags$a("Epigroup",href = "http://www.epigroup.biology.ed.ac.uk/"),". Issues with / comments about the website should be directed to ", a("s.j.haynes@sms.ed.ac.uk"),", questions related to the COVID-19 epidemic in Africa should be directed to Prof Mark Woolhouse, ", a("mark.woolhouse@ed.ac.uk"),"."),
                    h4("This application was built using the R Shiny package."),
                    width = 12)))))

dashboardPage(
    dashboardHeader(disable = TRUE),
    sidebar,
    body)
