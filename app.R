# app.R 

# Source file that reads in data, misc. set ups, and other static things
source("source.R")

ui <- dashboardPage(
  
  # Top level ----
  title = "R-Ladies Datathon 2021",
  skin = "purple",
  header = dashboardHeader(title = "R-Ladies Philly"),
  
  # Sidebars ----
  ## left sidebar ====
  sidebar = dashboardSidebar(
    shinydashboard::sidebarMenu(
      menuItem("Judges", tabName = "JudgeTab", icon = icon("user")),
      menuItem("Offenses", tabName = "OffTab", icon = icon("ban-circle", lib = "glyphicon")),
      menuItem("Bail", tabName = "BailTab", icon = icon("warning-sign", lib = "glyphicon")),
      menuItem("About", tabName = "AboutTab", icon = icon("info"))
    )
  ),
  
  ## right sidebar ====
  controlbar = dashboardControlbar(
    controlbarMenu(
      controlbarItem("Controls",
                     actionBttn("Debugger", "Debug", block = TRUE, color = "danger")
      )
    )
  ),
  
  body = dashboardBody(
    
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "datathon-2021.css")),
    
    tabItems(
      
      # First tab ----
      tabItem(tabName = "JudgeTab",
              
              ## main page ====
              fluidPage(
                
                ### top row ####
                fluidRow(
                  tabBox(id = "PlotsTabset", width = 12,
                         
                         ##### first plot panel ####
                         tabPanel(title = "Common Offenses by Judge", value = "Plot1Tab",
                                  fluidRow(
                                    column(width = 10, plotOutput("Plot1Output")),
                                    column(width = 2, 
                                           selectInput('judge', 'Judge', judge_options),
                                           sliderInput('year', 'Year Range (disposition date)', 
                                                       min=2010, max=2020,
                                                       value=c(2010, 2020), step=1, round=T, sep="")
                                    )
                                  )
                         ),
                         
                         ##### second plot panel ####
                         tabPanel(title = "Add Here", value = "Plot2Tab",
                                  fluidRow(
                                    column(width = 10),
                                    column(width = 2)
                                  )
                         )
                  )
                ),
                
                ### bottom row ####
                fluidRow(
                  column(width = 12, reactableOutput("TableOutput1"))
                )
              )
      ),
      
      
      # Second tab ----
      tabItem(tabName = "OffTab",
              
              ## main page ====
              fluidPage(
                
                ### top row ####
                fluidRow(
                  tabBox(id = "PlotsTabset", width = 12,
                         
                         ##### first plot panel ####
                         tabPanel(title = "Max Sentence by Offense", value = "OffPlot1Tab",
                                  fluidRow(
                                    column(width = 10, plotOutput("Plot2Output")),
                                    column(width = 2,
                                           selectInput("description", 
                                                       "Offense Description", 
                                                       description_options, 
                                                       selected = description_options[1:3],
                                                       multiple = FALSE),
                                           selectInput('grade', "Grade", unique(od_clean$grade), 
                                                       selected = unique(od_clean$grade),
                                                       multiple = TRUE))
                                  )
                         )
                  )
                ),
                
                ##### second plot panel ####
                tabPanel(title = "Add Here", value = "OffPlot2Tab",
                         fluidRow(
                           column(width = 10),
                           column(width = 2)
                         )
                ),
                ### bottom row ####
                fluidRow(
                  column(width = 12, reactableOutput("TableOutput2"))
                )
              )
      ),
      
      
      # About tab ----
      tabItem(tabName = "AboutTab",
              fluidPage(
                fluidRow(
                  column(width = 2),
                  column(width = 10)
                )
              )
      )
      
    )
    
  ),
  
  # misc ----
  footer = dashboardFooter()
)

server <- function(input, output) {
  
  # The data
  # - Might wanna think about data structure for multiple plot dfs
  # - Packing dfs into a list in a single reactive(Val)?
  # dat <- reactive({
  #   mtcars[1:input$user_number,]
  # })
  # Added some example jat data to test
  # Plot1
  data_judge_filter <- reactive({
    merged %>% 
      filter(judge==input$judge,
             disposition_year >= input$year[1] & disposition_year <= input$year[2])
  })
  # For plot2
  data_offense_filter <- reactive({
    merged %>% 
      filter(description_clean %in% input$description,
             grade %in% input$grade)
  })
  
  
  # Scatter plot on panel 1
  # output$Plot1Output <- renderPlot({
  #   ggplot(dat()) +
  #     geom_point(
  #       aes(hp, mpg, fill = factor(cyl)),
  #       pch = 21, stroke = .7, color = "white",
  #       show.legend = FALSE
  #     ) +
  #     labs(
  #       title = glue("The letter is {input$user_letters}"),
  #       subtitle = glue("First {input$user_number} observations")
  #     )
  # }, res = 150)
  
  # Replaced with plot from judges data 
  output$Plot1Output <- renderPlot({
    data_judge_filter() %>% 
      dplyr::mutate(description_clean = forcats::fct_lump(description_clean, n= 5)) %>% 
      ggplot(aes(x=description_clean)) + 
      geom_bar(position = "dodge") + 
      labs(title="Most common offenses",
           subtitle = paste0("Selected judge: ", input$judge),
           x = "Offense") + 
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) + 
      theme(axis.text.x = element_text(size=5)) + 
      NULL
    
  }, res = 150)
  
  
  # Sentence plot on panel 2
  output$Plot2Output <- renderPlot({
    data_offense_filter() %>% 
      dplyr::filter(sentence_type %in% c("Confinement","Probation")) %>% 
    ggplot(
           aes(x = sentence_type,
               y = max_period_days 
           )) +
      geom_boxplot(
        alpha = .5,
        show.legend = FALSE
      ) +
      geom_jitter(
        alpha = .5,
        height = 0,
        width = 0.2,
        show.legend = FALSE
      ) + 
      labs(
        y = "Maximum Sentence (days)",
        x = "Sentence Type",
        title = "Maximum Sentence"
      )
  }, res = 150)
  
  # Reactable for tab 1, on row 2
  output$TableOutput1 <- renderReactable({
    reactable(data_judge_filter())
  })
  
  # Reactable for tab 2, on row 2
  output$TableOutput2 <- renderReactable({
    reactable(data_offense_filter())
  })
  
  # Debugging button
  # observeEvent(input$Debugger, {browser()})
  
}

shinyApp(ui, server)