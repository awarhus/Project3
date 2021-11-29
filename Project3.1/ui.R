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
                    choices= c("Barchart", "Boxplot"),
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
                    condition = "input.plotType == 'Boxplot'",
                    plotOutput("boxplot")
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
        ), #closes the data tab
        navbarMenu(
            title="Modeling",
            tabPanel(
                title = "Modeling Info",
                mainPanel(fluidPage(
                    h4("Logistic Regression"),
                    "Logistic regression uses predictor variables to model the log odds of",
                    "the response event occurring. It is easily interpretable, and highly",
                    "efficient. However, logistic regression is limited by the number of",
                    "observations (n) compared to the number of variables (m).",
                    "If n<m, logistic regression shouldn't be used.",
                    br(),
                    br(),
                    h4("Classification Trees"),
                    "Classification trees recursively split data into different regions based",
                    "on the most dominant class. They are highly interpretable.",
                    "Splits are made to reduce training error, but they are based of off a",
                    "greedy algorithm. Thismeans that the split is based on what is best now",
                    "in terms of error reduction, but not necessarily what is best in the future.",
                    "This means that the final model is not necessarily the best overall model.",
                    br(),
                    br(),
                    h4("Random Forests"),
                    "For classification, random forests use bootstrap samples and create",
                    "trees on each. Majority rule is used for prediction.",
                    "One benefit of random forests is that the bootstrap samples create",
                    "subsets of the data, which prevents strong predictors from dominating",
                    "the begining of all trees. This makes them more likely to be independent",
                    "of each other. However, due to the complexity of the random forest process",
                    "these models are good for prediction but do not have the interpretability",
                    "of simpler models.",
                    br(),
                    br(),
                    br()
                ))
            )
        )
            
       
    ) #tabsetpanel 
)) #shinyUI and navbar


