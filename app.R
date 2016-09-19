
library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)

source("jpmcc2016.R")
data <- getData()
data <- cleanData(data)
company_table <- table(data$Company)
company_levels <- names(company_table)[order(company_table)]
company_levels <- names(company_table)[order(company_table, decreasing=TRUE)]
data$Company.Sort <- factor(data$Company, levels=company_levels)
top20 <- data %>% filter(Company %in% company_levels[1:20])
company_names <- unique(top20$Company)

time.breaks <- seq(0, 4800, 60 * 10)

ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("JP Morgan Chase Corporate Challenge - Company Comparison"),
   
   sidebarLayout(
     sidebarPanel(
       selectInput('co1', 'Company #1', company_names, "SALESFORCE"),
       selectInput('co2', 'Company #2', company_names, "J.P. MORGAN CHASE"),
       selectInput('co3', 'Company #3', company_names, "INTEL")
     ),
     
      mainPanel(
        fluidRow(
          column(12, plotOutput("plot"))
        ),
        fluidRow(
          br(),
          tableOutput("summary_table")
        ),
        fluidRow(
          br(),
          tableOutput("references")
        )
      )
   )
))

server <- shinyServer(function(input, output) {
  
   output$plot <- renderPlot({
     sel <- top20 %>% filter(Company %in% input$co1 | Company %in% input$co2 | Company %in% input$co3)
     sel$Company.Sort <- factor(sel$Company, levels=c(input$co1, input$co2, input$co3))
     p <- ggplot(sel, aes(Company.Sort, Time.Seconds)) +
       geom_jitter(width=0.5, aes(color=Gender)) +
       theme(axis.title.x=element_blank()) +
       scale_y_continuous(breaks = time.breaks, labels = secondsToTimestr(time.breaks), name="elapsed time (hh:mm:ss)") +
       expand_limits(y=0)
     print(p)
   })
   output$summary_table <- renderTable({
     co1 <- top20 %>% filter(Company %in% input$co1)
     sum1 <- summary(co1$Time.Seconds)
     co2 <- top20 %>% filter(Company %in% input$co2)
     sum2 <- summary(co2$Time.Seconds)
     co3 <- top20 %>% filter(Company %in% input$co3)
     sum3 <- summary(co3$Time.Seconds)
     
     sum_names <- names(sum1)
     
     sum1 <- secondsToTimestr(sum1)
     names(sum1) <- sum_names
     sum2 <- secondsToTimestr(sum2)
     names(sum2) <- sum_names
     sum3 <- secondsToTimestr(sum3)
     names(sum3) <- sum_names
     
     data.frame(
       company = c(input$co1, input$co2, input$co3),
       runners = c(nrow(co1), nrow(co2), nrow(co3)),
       min = c(sum1["Min."], sum2["Min."], sum3["Min."]),
       mean = c(sum1["Mean"], sum2["Mean"], sum3["Mean"]),
       median = c(sum1["Median"], sum2["Median"], sum3["Median"]),
       max = c(sum1["Max."], sum2["Max."], sum3["Max."])
     )
   })
   output$references <- renderTable({
     labels <- c(
       "JP Morgan Chase Site",
       "Data Exploration",
       "Source Code"
     )
     urls <- c(
       "https://www.jpmorganchasecc.com/",
       "http://rpubs.com/aaronferrucci/208500",
       "https://github.com/aaronferrucci/jpmcc2016"
     )
     references <- paste0(
       labels, ": ",
       "<a href='",  urls, "' target='_blank'>",
       urls, "</a>")

     data.frame(references)

   }, sanitize.text.function = function(x) x)

})

# Run the application 
shinyApp(ui = ui, server = server)
