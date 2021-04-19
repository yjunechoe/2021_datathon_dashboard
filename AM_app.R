# app.R 

# Source file that reads in data, misc. set ups, and other static things
source("source.R")

merged.narrow <- readRDS("Docket_Offenses_Merged_Narrowed.Rds")

options <- c("Judge","min_grade","max_grade","statute_description","disposition","disposition_method","sentence_type","Title_Description","Chapter_Description","gender","race","in_select_judges")


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
      menuItem("Sentences", tabName = "SentenceTab", icon = icon("warning-sign", lib = "glyphicon")),
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
      
      # Sentences tab ----
      tabItem(tabName = "SentenceTab",

              ## main page ====
              fluidPage(

                ### top row ####
                fluidRow(
                  tabBox(id = "PlotsTabset", width = 12,

                         ##### first plot panel ####
                         tabPanel(title = "Visualization of Sentence Length", value = "SenPlot1Tab",
                                  fluidRow(
                                    column(width = 2,style = "height:800px",
                                           pickerInput('judges_of_interest', 'Judges of Interest', choices = unique(merged.narrow$Judge),selected = unique(merged.narrow$Judge), options = list(`actions-box` = TRUE), multiple = TRUE),
                                           textInput("crime_descriptions","Crime Description",value = ""),
                                           textInput("title_descriptions","Title Description",value = ""),
                                           textInput("disposition_methods","Disposition Method",value = ""),
                                           pickerInput("races","Race", choices = unique(merged.narrow$race), multiple = TRUE, selected = unique(merged.narrow$race),options = list(`actions-box` = TRUE)),
                                           numericInput("nfactors","Number Categories",value = 6, min = 1, max = 10),
                                           selectInput("x.axis","On X Axis",choices = options, selected = "Chapter_Description"),
                                           selectInput("facet","Separate By", choices = options, selected = "Judge")
                                    ),
                                    column(width = 10, style = "height:800px",
                                           align = "left",plotOutput("PlotSenOutput", height = "800px"))
                                  )
                                  #   fluidRow(pickerInput('judges_of_interest', 'Judges of Interest', choices = unique(merged.narrow$Judge),options = list(`actions-box` = TRUE), multiple = TRUE),
                                  #            textInput("crime_descriptions","Crime Description",value = ""),
                                  #            textInput("title_descriptions","Title Description",value = ""),
                                  #                     textInput("disposition_methods","Disposition Method",value = ""),
                                  #                     pickerInput("races","Race", choices = unique(merged.narrow$race), multiple = TRUE, selected = unique(merged.narrow$race),options = list(`actions-box` = TRUE)),
                                  #                     numericInput("nfactors","Number Categories",value = 6, min = 1, max = 10),
                                  #                     selectInput("x.axis","On X Axis",choices = options, selected = "Chapter_Description"),
                                  #                     selectInput("facet","Separate By", choices = options, selected = "Judge")
                                  #              ),
                                  # fluidRow(plotOutput("PlotSenOutput"))
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
                  column(width = 12, reactableOutput("TableOutputSentences"))
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
  
  
  
  
  ##Sentences Plot
  filtered.data <- reactive({
    merged.narrow %>%
      #filter based on selection
      dplyr::mutate(Confinement_Time = max_period_days_Confinement/365) %>%
      dplyr::mutate(in_select_judges = ifelse(grepl(paste(input$judges_of_interest, collapse = "|"),Judge), Judge, "Other Judges")) %>%
      filter(grepl(paste(input$crime_descriptions, collapse = "|"), Chapter_Description)) %>%
      filter(grepl(paste(input$title_descriptions, collapse = "|"), Title_Description)) %>%
      filter(grepl(paste(input$disposition_methods, collapse = "|"), disposition_method)) %>%
      filter(grepl(paste(input$races, collapse = "|"), race))
  })

  output$PlotSenOutput <- renderPlot({
    filtered.data() %>%
      dplyr::mutate(on.x.axis = forcats::fct_lump_n(eval(parse(text = input$x.axis)), n = input$nfactors)) %>% #head()
      dplyr::mutate(to.facet = forcats::fct_lump_n(eval(parse(text = input$facet)), n = input$nfactors)) %>%
      ggplot(aes(x = on.x.axis, y = Confinement_Time, fill = race, size = Age_at_Arrest, shape = gender)) +
      geom_jitter(pch = 21, width = 0.3) + scale_size_continuous(name="Age_at_Arrest", range = c(.2,3)) +
      theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.text = element_text(size = 5)) +
      labs(y="Max Confinement Time (Years)", x = paste(x.axis)) +
      guides(fill = guide_legend(override.aes = list(size = 3))) + scale_x_discrete(labels = scales::wrap_format(20)) + scale_fill_discrete(labels = scales::wrap_format(10)) +
      facet_wrap(.~to.facet)
  }, res = 150)
  
  # Reactable for tab 3, on row 2
  output$TableOutputSentences <- renderReactable({
    reactable(filtered.data())
  })
  
}

shinyApp(ui, server)
