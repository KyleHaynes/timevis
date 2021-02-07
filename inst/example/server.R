setwd("C:\\Users\\kyleh\\OneDrive\\Desktop\\timevis\\inst\\example")

# install.packages("shinydisconnect")


library(timevis)
library(shinydisconnect)
library(data.table)

# source("sampleData.R")
source("utils.R")

# source("ui.R")
    #   

dt = structure(list(id = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), content = c("Rec", 
"Sick", "Rec", "RDO", "RDO", "RDO", "RDO", "RDO", "RDO"), start = c("2016-01-10", 
"2016-01-11", "2016-01-20", "2016-02-14", "2016-03-14", "2016-03-16", 
"2016-03-14", "2016-03-01", "2016-03-14"), end = c(NA, "2016-03-04", 
"2016-02-04", NA, "2016-03-16", "2016-03-17", "2016-04-04", "2016-03-04", 
"2016-03-14"), group = c("kzhayn", "szlit", "kzhayn", "szlit", 
"personx", "persony", "personz", "personc", "persond")), class = "data.frame", row.names = c(NA, 
-9L))

dt$V1 = NULL

dt$group = c(1,2,1,2,3,4,5,6,7)
dt$subgroup = c(1,3,3,3,1,1,1,2,2)

dt_groups <- data.frame(id = 1:7, content = c("zz", "qq", "ww", "sdf", "ee", "sd", "x"))

# dt_groups <- data.table(id = c("kzhayn", "szlit"), content = c("Kyle Haynes", "Simon Diddle"))


#   runApp()



#   output$timelineGroups <- renderTimevis({
#     timevis(data = timevisData, groups = timevisDataGroups, options = list(editable = TRUE))
#   })




function(input, output, session) {


#   output$timelineBasic <- renderTimevis({
#     timevis(data = dt, groups = dt$group, options = list(editable = TRUE))
#   })


  output$timelineInteractive <- renderTimevis({
    config <- list(
      editable = TRUE,
      multiselect = TRUE
    )
    timevis(data = dt, groups = dt_groups, options = config)
  })

  output$visible <- renderText(
    paste(input$timelineInteractive_visible, collapse = " ")
  )
  output$selected <- renderText(
    paste(input$timelineInteractive_selected, collapse = " ")
  )
  output$window <- renderText(
    paste(prettyDate(input$timelineInteractive_window[1]),
          "to",
          prettyDate(input$timelineInteractive_window[2]))
  )
  output$table <- renderTable({
    data <- input$timelineInteractive_data
    data$start <- prettyDate(data$start)
    if (!is.null(data$end)) {
      data$end <- prettyDate(data$end)
    }
    data
  })
  output$selectIdsOutput <- renderUI({
    selectInput("selectIds", tags$h4("Select items:"), input$timelineInteractive_ids,
                multiple = TRUE)
  })
  output$removeIdsOutput <- renderUI({
    selectInput("removeIds", tags$h4("Remove item"), input$timelineInteractive_ids)
  })

  observeEvent(input$fit, {
    fitWindow("timelineInteractive")
  })
  observeEvent(input$setWindowAnim, {
    setWindow("timelineInteractive", "2016-01-07", "2016-01-25")
  })
  observeEvent(input$setWindowNoAnim, {
    setWindow("timelineInteractive", "2016-01-07", "2016-01-25",
              options = list(animation = FALSE))
  })
  observeEvent(input$center, {
    centerTime("timelineInteractive", "2016-01-23")
  })
  observeEvent(input$focus2, {
    centerItem("timelineInteractive", 4)
  })
  observeEvent(input$focusSelection, {
    centerItem("timelineInteractive", input$timelineInteractive_selected)
  })
  observeEvent(input$selectItems, {
    setSelection("timelineInteractive", input$selectIds,
                 options = list(focus = input$selectFocus))
  })
  observeEvent(input$addBtn, {
    addItem("timelineInteractive",
            data = list(id = randomID(),
                        content = input$addText,
                        start = input$addDate))
  })
  observeEvent(input$removeItem, {
    removeItem("timelineInteractive", input$removeIds)
  })
  observeEvent(input$addTime, {
    addCustomTime("timelineInteractive", "2016-01-17", randomID())
  })
}
