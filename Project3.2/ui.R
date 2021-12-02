library(shiny)
library(readr)
library(tidyverse)


disciplines <- read_csv(".\\disciplines_final_2.csv")

shinyUI(navbarPage(
  
  
  title = "Notable Discipline Incidents in Wisconsin Schools 2019-2020",
  tabsetPanel(
    #About tab
    tabPanel(
      title="About",
      mainPanel(
        h3("App Description"),
        img(
          src = "DPIimage.PNG", 
          height = '100px', 
          width = '300px'
        ),
        br(),
        br(),
        "This app explores behavior incident data from",
        "schools in Wisconsin during the 2019-2020 school year.", 
        "It can be broken down by type of school, location and type of incident.",
        h3("Data Explanation"),
        "The data is from Wisconsin's DPI Wisedash website. ",
        "The data can be found at",
        a(href = "https://dpi.wi.gov/wisedash/download-files",
          "the Wisconsin DPI website"),
        
        h3("Tab Explanation"),
        "Each tab provides a different way to explore the data.",
        br(),
        "The Data Exploration tab will do some general exploratory data analysis",
        "and provide graphs and tables based on options from the user.",
        br(),
        "The Modeling tab contains 3 sub tabs. The first sub-tab, Modeling Info",
        "describes the three different models that will be fit. The second sub-tab,",
        "Model Fitting will allow the user to choose parameters with which to fit",
        "models to the data and report the fit statistics. The third sub-tab,
                Prediction. Based on the chosen parameters from the Model Fitting tab and",
        "will report a prediction for the user from their chosen model."
        
      )
    ),
    #Data Exploration tab
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
        h3("Choose the type of summary"),
        radioButtons(
          inputId = "summ",
          label = "Summary Type",
          choices= c("Median", "Just the Mean"),
          selected = "Median",
          inline = TRUE
        ),
        radioButtons( inputId = "var", 
                      label = "Which variable would you like to explore?",
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
          condition = "input.summ == 'Median'",
          DT::dataTableOutput("fivenumber")
        ),
        conditionalPanel(
          condition = "input.summ == 'Just the Mean'",
          DT::dataTableOutput("mean")
        ))), 
    
    #Data tab
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
      ), 
      mainPanel(
        dataTableOutput("dataTab"),
        downloadButton("downloadData", "Download")
      )
    ),
    
    #Modeling tab
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
          "trees on each. Majority rule is used for prediction in the classification",
          "case and the average of predictions",
          uiOutput("avgFormula"),
          "in the regression case.",
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
      ), 
      
      #Model Fitting Tab
      tabPanel(
        title = "Model Fitting",
        sidebarPanel(
          h3("Train-Test Split"), #Choose proportion of data for train/test split
          selectInput(
            inputId = "proportions",
            label = "Proportion of Data to use for testing set. The
                        proportion of data used for the training set will be 
                        1-the number you choose for the testing set.",
            choices = c(0.1,0.2,0.3,0.4,0.5),
            selected = 0.5),
          h3("Options for Models"),
          selectInput( #choose mtry
            inputId = "mtry", 
            label = "Select values for mtry:", 
            choices = 1:10,
            selected = c(1,2),
            multiple = TRUE),
          selectInput( #Choose # of folds for CV
            inputId = "cv",
            label = "Number of folds for cross validation:",
            choices = c(5,10),
            selected = 5
          ),
          selectInput( #choose cp
            inputId = "cp",
            label = "Choose the cp:",
            choices = c(0,0.001,0.002, 0.003,0.004, 0.005,0.006,0.007,0.008, 0.009, 0.01)
          ),
          h3("Logistic Regression Parameters"),#choose parameters for logistic regression
          selectInput(
            inputId = "regVars",
            label = "Variables to Include:",
            choices = c("AGENCY_TYPE",
                        "BEHAVIOR_TYPE",
                        "CESA",
                        "COUNTY",
                        "GRADE_GROUP",
                        "CHARTER_IND",
                        "DISTRICT_NAME",
                        "SCHOOL_NAME",
                        "INCIDENTS_COUNT"),
            selected = c("AGENCY_TYPE",
                         "BEHAVIOR_TYPE",
                         "CESA",
                         "COUNTY",
                         "GRADE_GROUP",
                         "CHARTER_IND",
                         "DISTRICT_NAME",
                         "SCHOOL_NAME",
                         "INCIDENTS_COUNT"),
            multiple = TRUE
          ),
          h3("Tree Parameters"), #choose parameters for classification tree 
          selectInput(
            inputId = "treeVars",
            label = "Variables to Include:",
            choices = c("AGENCY_TYPE",
                        "CESA",
                        "COUNTY",
                        "GRADE_GROUP",
                        "CHARTER_IND",
                        "DISTRICT_NAME",
                        "SCHOOL_NAME",
                        "INCIDENTS_COUNT"),
            selected = c("AGENCY_TYPE",
                         "CESA",
                         "COUNTY",
                         "GRADE_GROUP",
                         "CHARTER_IND",
                         "DISTRICT_NAME",
                         "SCHOOL_NAME",
                         "INCIDENTS_COUNT"),
            multiple = TRUE
          ),
          h3("Random Forest Parameters"), #choose parameters for random forest 
          selectInput(
            inputId = "rfVars",
            label = "Variables to Include:",
            choices = c("AGENCY_TYPE",
                        "CESA",
                        "COUNTY",
                        "GRADE_GROUP",
                        "CHARTER_IND",
                        "DISTRICT_NAME",
                        "SCHOOL_NAME",
                        "INCIDENTS_COUNT"),
            selected = c("AGENCY_TYPE",
                         "CESA",
                         "COUNTY",
                         "GRADE_GROUP",
                         "CHARTER_IND",
                         "DISTRICT_NAME",
                         "SCHOOL_NAME",
                         "INCIDENTS_COUNT"),
            multiple = TRUE
          )
        ), 
        mainPanel( #Output the test statistics from the logistic regression and have a start button to fit the models.
          actionButton(
            inputId = "start",
            label = "Fit Models!"
          ),
          h2("Training Results"),
          h3("Logistic Regression Training Results"),
          DT::dataTableOutput("logistic"),
          h3("Classification Tree Training Results"),
          DT::dataTableOutput("classTree"),
          h3("Random Forest Training Results"),
          DT::dataTableOutput("randForest"),
          h2("Testing Results"),
          h3("Logistic Regressing Testing Results"),
          DT::dataTableOutput("logTest"),
          h3("Classification Tree Testing Results"),
          DT::dataTableOutput("classTest"),
          h3("Random Forest Testing Results"),
          DT::dataTableOutput("rfTest2")
        )
      ),
      tabPanel(
        title = "Prediction",
        sidebarPanel(
          radioButtons(
            inputId = "modelType",
            label = "Choose a Model",
            choiceNames = c(
              "Logistic Regression", 
              "Classification Tree", 
              "Random Forest"
            ),
            choiceValues = c("logReg", "tree", "randFor"),
            selected = "logReg"
          )
        ),
          mainPanel(
            h3("Predicted type of School"),
              DT::dataTableOutput("pred")
            )
        ) 
      ) 
    ))
)


