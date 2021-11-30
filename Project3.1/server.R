library(shiny)
library(dplyr)
library(stringr)
library(DT)
library(caret)

setwd("C:\\Users\\awarhus_piusxi\\Desktop\\ST558\\Shiny Apps\\Project3")

disciplines <- read_csv(".\\disciplines_final_2.csv")

shinyServer(function(input, output, session) {
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
    output$dataTab <- renderDataTable({
        school<-input$typeOfSchool
        grade<-input$gradeGroup
        district<-input$district
        schoolFilt<-str_detect(disciplines$AGENCY_TYPE, school)
        disciplines1<-filter(disciplines, schoolFilt)
        gradeFilt<-str_detect(disciplines1$GRADE_GROUP, grade)
        disciplines2<-filter(disciplines1, gradeFilt)
        districtFilt<-str_detect(disciplines2$DISTRICT_NAME, district)
        disciplines3<-filter(disciplines2, districtFilt)
        disciplines4<-select(disciplines3,columns)
        disciplines4
    })
    output$downloadData <- downloadHandler( #need to fix
        download <- function(file) {
            write.csv(disciplines4,file = "disciplines.csv")
        }
    )
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
        return(train0)
       
        })
    output$logistic<-renderDataTable({
        data.frame(logReg)
    })
    tree<-eventReactive(input$start,{
        set.seed(234)
        cv<-input$cv
        cp<-input$cp
        vars<-unlist(input$treeVars)
        props <- input$proportions
        split <- createDataPartition(y = disciplines$BEHAVIOR_TYPE, 
                                     p = props, list = FALSE)
        train <- disciplines[-split, ]
        test <- disciplines[split, ]
        ctrl <- trainControl(method="repeatedcv",number=cv, repeats = 3)
        treeFit <- train(BEHAVIOUR_TYPE ~ .,
                         data = train[,c("BEHAVIOR_TYPE",vars)], 
                         method = "rpart", trControl = ctrl, 
                         preProcess = c("center","scale"),
                         cp = cp)
        return(treeFit)
    })
    output$classTree<-renderDataTable({
        data.frame(tree)
    })
    rf<-eventReactive(input$start,{
        set.seed(1231)
        vars<-unlist(input$rfVars)
        mtry<-input$mtry
        props <- input$proportions
        split <- createDataPartition(y = disciplines$BEHAVIOR_TYPE, 
                                     p = props, list = FALSE)
        train <- disciplines[-split, ]
        test <- disciplines[split, ]
        cv<-input$cv
        ctrl <- trainControl(method="repeatedcv",number=cv, repeats = 3)
        rfFit <- train(BEHAVIOR_TYPE ~ ., data = train[,c("BEHAVIOR_TYPE",vars)], 
                       method = "rf", trControl = ctrl, 
                       preProcess = c("center","scale"),
                       tuneGrid = expand.grid(mtry=mtry))
        return(rfFit)
    })
    output$randForest<-renderDataTable({
        data.frame(rf)
    })
})
   




