# WHO Afro dashboard

This repo contains all of the source code for deploying the TIBA COVID-19 online dashboard for african countries under the WHO Africa Region office. The actual dashboard is available at https://dimmestp.shinyapps.io/who_africa_app/.

The dashboard is created using the shinydashboard library in R. The main two files are the ui.R and server.R files. the ui.R file contains the skeleton code for what is included in the dashboard, options for how the user can interact with the data and the structure of the web page itself. server.R decides what should be presented in each slot defined in the ui.R file according the possible user inputs and availible data. Some of the static elements of the webpage are held in the www/ folder. Finally some of the logic required to create the graphs called in the server.R file are held separately in the plotting_function.R file.
