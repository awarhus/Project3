#Install needed packages
library(shiny)
library(dplyr)
library(stringr)
library(DT)
library(caret)

#Read in the data
disciplines <- read_csv(".\\disciplines_final_2.csv")

shinyServer(function(input, output, session) {
  
  #Data Exploration tab-Render the barchart based on user input
  output$barchart <- renderPlot({
    vars <- input$var
    if(vars == "Agency Type"){
      type<-input$typeOfSchool
      filt<-str_detect(disciplines$AGENCY_TYPE, type)
      disciplines1<-filter(disciplines, filt)
      g <- ggplot(disciplines1, aes(x=BEHAVIOR_TYPE)) + 
        stat_count() + 
        scale_y_continuous("Count")
      g}
    else if (vars =="Grade"){
      type<-input$gradeGroup
      filt<-str_detect(disciplines$GRADE_GROUP,type)
      disciplines1<-filter(disciplines, filt)
      g <- ggplot(disciplines1, aes(x=BEHAVIOR_TYPE)) + 
        stat_count() + 
        scale_y_continuous("Count")
      g}
    else if(vars =="District"){
      type<-input$district
      filt<-str_detect(disciplines$DISTRICT_NAME,type)
      disciplines1<-filter(disciplines, filt)
      g <- ggplot(disciplines1, aes(x=BEHAVIOR_TYPE)) + 
        stat_count() + 
        scale_y_continuous("Count")
      g
    }
  })
  
  #Data Exploration tab-render the boxplot based on user input
  output$boxplot <- renderPlot({
    vars <- input$var
    type<-input$typeOfSchool
    if (vars == "Agency Type"){
      filt<-str_detect(disciplines$AGENCY_TYPE, type)
      disciplines1<-filter(disciplines, filt)
      counts<-as.numeric(disciplines1$INCIDENTS_COUNT)
      ggplot(disciplines1, aes(x=BEHAVIOR_TYPE, y=counts)) +
        geom_boxplot()
    }
    else if (vars == "Grade"){
      type<-input$gradeGroup
      filt<-str_detect(disciplines$GRADE_GROUP,type)
      disciplines1<-filter(disciplines, filt)
      counts<-as.numeric(disciplines1$INCIDENTS_COUNT)
      ggplot(disciplines1, aes(x=BEHAVIOR_TYPE),y=counts) +
        geom_boxplot()
    }
    else if (vars =="District"){
      type<-input$district
      filt<-str_detect(disciplines$DISTRICT_NAME,type)
      disciplines1<-filter(disciplines, filt)
      counts<-as.numeric(disciplines1$INCIDENTS_COUNT)
      ggplot(disciplines1, aes(x=BEHAVIOR_TYPE, y=counts)) +
        geom_boxplot()
    }
  })
  
  #Data Exploration tab-render the data table based on user input
  output$fivenumber <- renderDataTable({
    vars <- input$var
    if(vars == "Agency Type"){
      type<-input$typeOfSchool
      filt<-str_detect(disciplines$AGENCY_TYPE, type)
      disciplines1<-filter(disciplines, filt)
      disciplines1$counts<-as.numeric(disciplines1$INCIDENTS_COUNT)
      fiver<-disciplines1 %>% select(AGENCY_TYPE,GRADE_GROUP, 
                                     DISTRICT_NAME,BEHAVIOR_TYPE, counts) %>%
        group_by(BEHAVIOR_TYPE) %>% na.omit() %>% summarize(median(counts))
      fiver
    }
    else if (vars == "Grade"){
      type<-input$gradeGroup
      filt<-str_detect(disciplines$GRADE_GROUP,type)
      disciplines1<-filter(disciplines, filt)
      disciplines1$counts<-as.numeric(disciplines1$INCIDENTS_COUNT)
      fiver<-disciplines1 %>% select(AGENCY_TYPE,GRADE_GROUP, 
                                     DISTRICT_NAME,BEHAVIOR_TYPE, counts) %>%
        group_by(BEHAVIOR_TYPE) %>% na.omit() %>% summarize(median(counts))
      fiver
    }
    else if (vars =="District"){
      type<-input$district
      filt<-str_detect(disciplines$DISTRICT_NAME,type)
      disciplines1<-filter(disciplines, filt)
      disciplines1$counts<-as.numeric(disciplines1$INCIDENTS_COUNT)
      fiver<-disciplines1 %>% select(AGENCY_TYPE,GRADE_GROUP, 
                                     DISTRICT_NAME,BEHAVIOR_TYPE, counts) %>%
        group_by(BEHAVIOR_TYPE) %>% na.omit() %>% summarize(median(counts))
      fiver
    }
  })
  
  #Data Exploration tab-render the data table based on user input
  output$mean <- renderDataTable({
    vars <- input$var
    if(vars == "Agency Type"){
      type<-input$typeOfSchool
      filt<-str_detect(disciplines$AGENCY_TYPE, type)
      disciplines1<-filter(disciplines, filt)
      disciplines1$counts<-as.numeric(disciplines1$INCIDENTS_COUNT)
      fiver<-disciplines1 %>% select(AGENCY_TYPE,GRADE_GROUP, 
                                     DISTRICT_NAME,BEHAVIOR_TYPE, counts) %>%
        group_by(BEHAVIOR_TYPE) %>% na.omit() %>% summarize(mean(counts))
      fiver
    }
    else if (vars == "Grade"){
      type<-input$gradeGroup
      filt<-str_detect(disciplines$GRADE_GROUP,type)
      disciplines1<-filter(disciplines, filt)
      disciplines1$counts<-as.numeric(disciplines1$INCIDENTS_COUNT)
      fiver<-disciplines1 %>% select(AGENCY_TYPE,GRADE_GROUP, 
                                     DISTRICT_NAME,BEHAVIOR_TYPE, counts) %>%
        group_by(BEHAVIOR_TYPE) %>% na.omit() %>% summarize(mean(counts))
      fiver
    }
    else if (vars =="District"){
      type<-input$district
      filt<-str_detect(disciplines$DISTRICT_NAME,type)
      disciplines1<-filter(disciplines, filt)
      disciplines1$counts<-as.numeric(disciplines1$INCIDENTS_COUNT)
      fiver<-disciplines1 %>% select(AGENCY_TYPE,GRADE_GROUP, 
                                     DISTRICT_NAME,BEHAVIOR_TYPE, counts) %>%
        group_by(BEHAVIOR_TYPE) %>% na.omit() %>% summarize(mean(counts))
      fiver
    }
  })
  
  #Data tab-filter and display data based on user input 
  output$dataTab <- renderDataTable({
    school<-input$typeOfSchool
    grade<-input$gradeGroup
    district<-input$district
    columns<-input$columns
    schoolFilt<-str_detect(disciplines$AGENCY_TYPE, school)
    disciplines1<-filter(disciplines, schoolFilt)
    gradeFilt<-str_detect(disciplines1$GRADE_GROUP, grade)
    disciplines2<-filter(disciplines1, gradeFilt)
    districtFilt<-str_detect(disciplines2$DISTRICT_NAME, district)
    disciplines3<-filter(disciplines2, districtFilt)
    disciplines4<-select(disciplines3,columns)
    disciplines4
  })
  
  #Download the filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(disciplines %>% 
                  filter(dataTab()), file)
    }
  )
  
  #Modeling tab-add formula in mathJax
  output$avgFormula <- renderUI({
    withMathJax(
      helpText(
        "$$\\frac{1}{B}\\sum{f_b (x')} $$"
      )
    )
  })
  
  #Logistic Regression for Model Fitting Tab
  logReg<-eventReactive(input$start,{
    set.seed(8758)
    props <- as.numeric(input$proportions)
    cv<-as.numeric(input$cv)
    vars<-unlist(input$regVars)
    split<- createDataPartition(y = disciplines$CHARTER_IND, 
                                p = props, list = FALSE)
    train <- disciplines[-split, ]
    test <- disciplines[split, ]
    train0<-train(CHARTER_IND~.,
                  data=train[,c("CHARTER_IND",vars)],
                  method="glm",
                  family="binomial",
                  preProcess=c("center","scale"),
                  trControl=trainControl(method="cv",number=cv))
    
  })
  output$logistic<-renderDataTable({
    data.frame(logReg()$results)
  })
  logRegTest<-eventReactive(input$start,{
    set.seed(8758)
    props <- as.numeric(input$proportions)
    cv<-as.numeric(input$cv)
    vars<-unlist(input$regVars)
    split<- createDataPartition(y = disciplines$CHARTER_IND, 
                                p = props, list = FALSE)
    train <- disciplines[-split, ]
    test <- disciplines[split, ]
    train0<-train(CHARTER_IND~.,
                  data=train[,c("CHARTER_IND",vars)],
                  method="glm",
                  family="binomial",
                  preProcess=c("center","scale"),
                  trControl=trainControl(method="cv",number=cv))
    confusionMatrix(data=test$CHARTER_IND,
                    reference=predict(train0,newdata=test))
    
  })
  output$logTest<-renderDataTable({
    data.frame(logRegTest()$results)
  })
  
  #Classification Tree for Model Fitting Tab
  tree<-eventReactive(input$start,{
    set.seed(234)
    cv<-input$cv
    cp<-input$cp
    vars<-unlist(input$treeVars)
    props <- input$proportions
    split <- createDataPartition(y = disciplines$CHARTER_IND, 
                                 p = props, list = FALSE)
    train <- disciplines[-split, ]
    test <- disciplines[split, ]
    ctrl <- trainControl(method="repeatedcv",number=cv, repeats = 3)
    treeFit <- train(BEHAVIOUR_TYPE ~ .,
                     data = train[,c("CHARTER_IND",vars)], 
                     method = "rpart", trControl = ctrl, 
                     preProcess = c("center","scale"),
                     cp = cp)
    return(treeFit)
  })
  output$classTree<-renderDataTable({
    data.frame(tree()$results)
  })
  treeTest<-eventReactive(input$start,{
    set.seed(234)
    cv<-input$cv
    cp<-input$cp
    vars<-unlist(input$treeVars)
    props <- input$proportions
    split <- createDataPartition(y = disciplines$CHARTER_IND, 
                                 p = props, list = FALSE)
    train <- disciplines[-split, ]
    test <- disciplines[split, ]
    ctrl <- trainControl(method="repeatedcv",number=cv, repeats = 3)
    treeFit <- train(BEHAVIOUR_TYPE ~ .,
                     data = train[,c("CHARTER_IND",vars)], 
                     method = "rpart", trControl = ctrl, 
                     preProcess = c("center","scale"),
                     cp = cp)
    confusionMatrix(data=test$CHARTER_IND,
                    reference=predict(treeFit,newdata=test))
  })
  output$classTest<-renderDataTable({
    data.frame(treeTest()$results)
  })
  
  #Random Forest for Model Fitting Tab
  rf<-eventReactive(input$start,{
    set.seed(1231)
    vars<-unlist(input$rfVars)
    mtry<-input$mtry
    props <- input$proportions
    split <- createDataPartition(y = disciplines$CHARTER_IND , 
                                 p = props, list = FALSE)
    train <- disciplines[-split, ]
    test <- disciplines[split, ]
    cv<-input$cv
    ctrl <- trainControl(method="repeatedcv",number=cv, repeats = 3)
    rfFit <- train(CHARTER_IND ~ ., data = train[,c("CHARTER_IND",vars)], 
                   method = "rf", trControl = ctrl, 
                   preProcess = c("center","scale"),
                   tuneGrid = expand.grid(mtry=mtry))
    return(rfFit)
  })
  output$randForest<-renderDataTable({
    data.frame(rf()$results)
  })
  rfTest<-eventReactive(input$start,{
    set.seed(1231)
    vars<-unlist(input$rfVars)
    mtry<-input$mtry
    props <- input$proportions
    split <- createDataPartition(y = disciplines$CHARTER_IND, 
                                 p = props, list = FALSE)
    train <- disciplines[-split, ]
    test <- disciplines[split, ]
    cv<-input$cv
    ctrl <- trainControl(method="repeatedcv",number=cv, repeats = 3)
    rfFit <- train(CHARTER_IND ~ ., data = train[,c("CHARTER_IND",vars)], 
                   method = "rf", trControl = ctrl, 
                   preProcess = c("center","scale"),
                   tuneGrid = expand.grid(mtry=mtry))
    confusionMatrix(data=test$CHARTER_IND,
                    reference=predict(rfFit,newdata=test))
  })
  output$rfTest2<-renderDataTable({
    data.frame(rfTest()$results)
  })
  
  #Obtaining predictions for Prediction tab
  preds<-eventReactive(input$start,{
    if (input$modelType == "randFor"){
      set.seed(1231)
      vars<-unlist(input$rfVars)
      mtry<-input$mtry
      props <- input$proportions
      split <- createDataPartition(y = disciplines$CHARTER_IND, 
                                   p = props, list = FALSE)
      train <- disciplines[-split, ]
      test <- disciplines[split, ]
      cv<-input$cv
      ctrl <- trainControl(method="repeatedcv",number=cv, repeats = 3)
      rfFit <- train(CHARTER_IND ~ ., data = train[,c("CHARTER_IND",vars)], 
                     method = "rf", trControl = ctrl, 
                     preProcess = c("center","scale"),
                     tuneGrid = expand.grid(mtry=mtry))
      preds <- predict(rfFit, test)
    }
      else if (input$modelType == 'tree'){
        set.seed(234)
        cv<-input$cv
        cp<-input$cp
        vars<-unlist(input$treeVars)
        props <- input$proportions
        split <- createDataPartition(y = disciplines$CHARTER_IND, 
                                     p = props, list = FALSE)
        train <- disciplines[-split, ]
        test <- disciplines[split, ]
        ctrl <- trainControl(method="repeatedcv",number=cv, repeats = 3)
        treeFit <- train(BEHAVIOUR_TYPE ~ .,
                         data = train[,c("CHARTER_IND",vars)], 
                         method = "rpart", trControl = ctrl, 
                         preProcess = c("center","scale"),
                         cp = cp)
        predict(treeFit,test,type="class")
      }
    else if (input$modelType == 'logReg'){
      set.seed(8758)
      props <- as.numeric(input$proportions)
      cv<-as.numeric(input$cv)
      vars<-unlist(input$regVars)
      split<- createDataPartition(y = disciplines$CHARTER_IND, 
                                  p = props, list = FALSE)
      train <- disciplines[-split, ]
      test <- disciplines[split, ]
      train0<-train(CHARTER_IND~.,
                    data=train[,c("CHARTER_IND",vars)],
                    method="glm",
                    family="binomial",
                    preProcess=c("center","scale"),
                    trControl=trainControl(method="cv",number=cv))
      confusionMatrix(data=test$CHARTER_IND,
                      reference=predict(train0,newdata=test))
      predict(train0,test)
    }
  })
  #Get variables for the model predictions
  output$treePredInputs <- renderUI({
    treeVars <- input$treeVars
    tags$ul(tagList(
      lapply(treeVars, function(variable) {
        selectInput(
          inputId = paste0(variable, "Value"),
          label = paste0("Input ", variable, " Value"),
          choices = pull(disciplines[, variable])
        )
      })
    ))
  })
  output$logRegPredInputs <- renderUI({
    treeVars <- input$regVars
    tags$ul(tagList(
      lapply(treeVars, function(variable) {
        selectInput(
          inputId = paste0(variable, "Value"),
          label = paste0("Input ", variable, " Value"),
          choices = pull(disciplines[, variable])
        )
      })
    ))
  })
  output$randForPredInputs <- renderUI({
    treeVars <- input$rfVars
    tags$ul(tagList(
      lapply(treeVars, function(variable) {
        selectInput(
          inputId = paste0(variable, "Value"),
          label = paste0("Input ", variable, " Value"),
          choices = pull(disciplines[, variable])
        )
      })
    ))
  })
  output$pred<-renderDataTable({
    data.frame(preds()$results)
  })
})