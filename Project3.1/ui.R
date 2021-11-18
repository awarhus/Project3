library(shiny)
library(readr)
library(tidyverse)

setwd("C:\\Users\\awarhus_piusxi\\Desktop\\ST558\\Shiny Apps\\Project3")

disciplines <- read_csv(".\\discipline_incidents_certified_2019-20.csv")
shinyUI(navbarPage(
    
    
    title = "Notable Discipline Incidents in Wisconsin Schools 2019-2020",
    tabsetPanel(
        
        tabPanel(
            title="About",
            mainPanel(
                h3("App Description"),
                "This app explores behavior incident data from",
                "schools in Wisconsin during the 2019-2020 school year.", 
                "It can be broken down by type of school, location and type of incident.",
                h3("Data Explanation"),
                "The data is from Wisconsin's DPI Wisedash website. ",
                "The data can be found at",
                a(href = "https://dpi.wi.gov/wisedash/download-files",
                  "the Wisconsin DPI website"),
                
                h3("Tab Explanation"),
                "I will describe the tabs here"
            )
        ),
        tabPanel(
            title = "Data Exploration",
            sidebarPanel(
                h3("Choose the type of graph"),
                radioButtons(
                    inputId = "plotType",
                    label = "Plot Type",
                    choices= c("Barchart", "Lollipop"),
                    selected = "Barchart",
                    inline = TRUE
                ),
                radioButtons( inputId = "var", 
                              label = "Which variable would you like to graph?",
                              choices = c("Agency Type","Grade", "District"),
                              selected = "Agency Type"
                ),
                conditionalPanel(
                    condition ="input.var == 'Agency Type'",
                    radioButtons(inputId = "typeOfSchool",
                                 label = strong("Choose the School Type"),
                                 choices = c("Public School","School District",
                                             "Non District Charter Schools","Public Schools-Multidistrict Charters"),
                                 selected = "Public School")
                ),
                conditionalPanel(
                    condition ="input.var == 'Grade'",
                    radioButtons(inputId = "gradeGroup",
                                 label = strong("Choose the Grade"),
                                 choices = c("[All]","Elementary School","High School",
                                             "Combined Elementary/Secondary School","Middle School"),
                                 selected = "High School")
                    
                ),
                conditionalPanel(
                    condition ="input.var == 'District'",
                    selectInput(inputId = "district",
                                label = strong("Choose the District"),
                                choices = unique(disciplines$DISTRICT_NAME),
                                selected = "Ashwaubenon")
                ),
                h3("Choose the type of summary"),
                radioButtons(
                    inputId = "summ",
                    label = "Summary Type",
                    choices= c("Five Number Summary", "Just the Mean"),
                    selected = "Five Number Summary",
                    inline = TRUE
                ),
                radioButtons(inputId = "schoolSumm",
                             label = strong("Choose the School Type"),
                             choices = c("All","None Assigned ","Public School","School District",
                                         "Non District Charter Schools","Public Schools-Multidistrict Charters"),
                             selected = "All")
            ),
            mainPanel(
                conditionalPanel(
                    condition = "input.plotType == 'Barchart'",
                    plotOutput("barchart")
                ),
                conditionalPanel(
                    condition = "input.plotType == 'Lollipop'",
                    plotOutput("lollipop")
                ),
                conditionalPanel(
                    condition = "input.summ == 'Five Number Summary'",
                    dataTableOutput("fivenumber")
                ),
                conditionalPanel(
                    condition = "input.summ == 'Just the Mean'",
                    dataTableOutput("mean")
                ))), #closes the data exploration tab
        tabPanel(
            title="Data",
            sidebarPanel(
                h3("Filter the data based on the following categories (note: some 
                                                                combinations will yield no data)"),
                radioButtons(inputId = "typeOfSchool",
                             label = strong("Choose the School Type"),
                             choices = c("Public School","School District",
                                         "Non District Charter Schools","Public Schools-Multidistrict Charters"),
                             selected = "Public School"),
                radioButtons(inputId = "gradeGroup",
                             label = strong("Choose the Grade"),
                             choices = c("[All]","Elementary School","High School",
                                         "Combined Elementary/Secondary School","Middle School"),
                             selected = "High School"),
                selectInput(inputId = "district",
                            label = strong("Choose the District"),
                            choices = unique(disciplines$DISTRICT_NAME),
                            selected = "Ashwaubenon"),
                selectInput(
                    inputId = "columns",
                    label = "Select which columns you'd like to see.",
                    choices = colnames(disciplines),
                    selected = colnames(disciplines),
                    multiple = TRUE
                ),
                sidebarPanel(downloadButton("downloadData", "Download"))
            ),
            mainPanel(
                dataTableOutput("dataTab")
            )
        ) #closes the data tab
    ) #tabsetpanel 
)) #shinyUI and navbar


