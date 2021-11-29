library(shiny)
library(dplyr)
library(stringr)
library(DT)


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
        type<-input$schoolSumm
        filt<-str_detect(disciplines$AGENCY_TYPE, type)
        disciplines1<-filter(disciplines, filt)
        disciplines1$counts<-as.numeric(disciplines1$INCIDENTS_COUNT)
        fiver<-disciplines1 %>% select(AGENCY_TYPE,GRADE_GROUP, 
                                       DISTRICT_NAME,BEHAVIOR_TYPE, counts) %>%
            group_by(BEHAVIOR_TYPE) %>% na.omit() %>% summarize(median(counts))
        fiver
    })
    output$mean <- renderDataTable({
        type<-input$schoolSumm
        filt<-str_detect(disciplines$AGENCY_TYPE, type)
        disciplines1<-filter(disciplines, filt)
        disciplines1$counts<-as.numeric(disciplines1$INCIDENTS_COUNT)
        fiver<-disciplines1 %>% select(AGENCY_TYPE,GRADE_GROUP, 
                                       DISTRICT_NAME,BEHAVIOR_TYPE, counts) %>%
            group_by(BEHAVIOR_TYPE) %>% na.omit() %>% summarize(mean(counts))
        fiver
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
    
   
})



