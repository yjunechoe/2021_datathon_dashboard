# app.R 

# Source file that reads in data, misc. set ups, and other static things
source("source.R")


# ui ----
ui <- dashboardPage(
  
  # Top level ----
  title = "R-Ladies Datathon 2021",
  skin = "purple",
  header = dashboardHeader(title = "R-Ladies Philly"),
  
  # Sidebars ----
  ## left sidebar ====
  sidebar = dashboardSidebar(
    shinydashboard::sidebarMenu(
      menuItem("Sentences", tabName = "SentenceTab", icon = icon("warning-sign", lib = "glyphicon")),
      menuItem("Bail", tabName = "BailTab", icon = icon("dollar")),
      menuItem("Basic Judge Info", tabName = "JudgeTab", icon = icon("user")),
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
      
      
  # Judge tab ----
  tabItem(tabName = "JudgeTab",
              
              ## main page ====
              fluidPage(
                
              ### top row ####
              fluidRow(
                  tabBox(id = "PlotsTabset", width = 12,
                         
              ##### judge first plot panel ####
              tabPanel(title = "Common Offenses by Judge", value = "Judge1Tab",
                          fluidRow(
                            column(width = 10, plotOutput("JudgePlot1Output")),
                            column(width = 2, 
                                    selectInput('judge', 'Judge', judge_options),
                                    sliderInput('year', 'Year Range (disposition date)', 
                                               min=2010, max=2020,
                                              value=c(2010, 2020), step=1, 
                                              round=T, sep="")
                                    )
                                  )
                         ),
                         
              ##### judge second plot panel ####
                 tabPanel(title = "Add Here", value = "Judge2Tab",
                                  fluidRow(
                                    column(width = 10),
                                    column(width = 2)
                                  )
                         )
                  )
                ),
                
              
              ### bottom row ####
              fluidRow(
                  column(width = 12, reactableOutput("TableOutputJudge1"))
                )
              )
      ),
      
      
      
  # Bail tab ----
      tabItem(tabName = "BailTab",
              
              ## main page ====
              fluidPage(
                
              ### top row ####
              fluidRow(
                  tabBox(id = "PlotsTabset", width = 12,
                         
                         ##### bail plot panel 1 ####
                         tabPanel(title = "Cumulative total of bail changes", value = "BailPlot1Tab",
                         fluidRow(
                           column(width = 10, plotOutput("BailPlot1Output")),
                           column(width = 2)
                         )
                        ),
                        ##### bail plot panel 2 ####
                        tabPanel(title = "Total # of Bail Amount Changes", value = "BailPlot2Tab",
                       fluidRow(
                         column(width = 10, plotOutput("BailPlot2Output")),
                         column(width = 2,
                                pickerInput('bail_judges', 
                                            'Judges of Interest', 
                                            choices = unique(bail_net_change_by_judge$judge),
                                            selected = c("Judge Ann Butchart", "Judge Abbe Fletman"), 
                                            options = list(`actions-box` = TRUE), 
                                            multiple = TRUE))
                       )
                      )
                  )
              ),
              
                ### bottom row ####
                fluidRow(
                  column(width = 12, 
                         reactableOutput("TableOutputBail1")
                         )
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

              ##### Sentence first plot panel ####
              tabPanel(title = "Visualization of Sentence Length", 
                       value = "SenPlot1Tab",
                                  fluidRow(
                                    column(width = 2,style = "height:800px",
                                           pickerInput('judges_of_interest', 
                                                       'Judges of Interest', 
                                                       choices = unique(merged.narrow$Judge),
                                                       selected = unique(merged.narrow$Judge), 
                                                       options = list(`actions-box` = TRUE), 
                                                       multiple = TRUE),
                                           textInput("crime_descriptions","Crime Description",
                                                     value = ""),
                                           textInput("title_descriptions","Title Description",
                                                     value = ""),
                                           textInput("disposition_methods","Disposition Method",
                                                     value = ""),
                                           pickerInput("races","Race", 
                                                       choices = unique(merged.narrow$race), 
                                                       multiple = TRUE, 
                                                       selected = unique(merged.narrow$race),
                                                       options = list(`actions-box` = TRUE)),
                                           numericInput("nfactors","Number Categories",
                                                        value = 6, min = 1, max = 10),
                                           selectInput("x.axis","On X Axis",
                                                       choices = options, 
                                                       selected = "Chapter_Description"),
                                           selectInput("facet","Separate By", 
                                                       choices = options, selected = "Judge")
                                    ),
                                    column(width = 10, style = "height:800px",
                                           align = "left",
                                           plotOutput("PlotSenOutput", height = "800px"))
                                  ),
                       ### bottom row tab 1 ####
                       fluidRow(
                         column(width = 12, reactableOutput("TableOutputSentences1"))
                       )
                                  
                         ),

                         ##### Sentence Second plot panel ####
                         tabPanel(title = "Max Sentence by Offense", value = "SenPlot2Tab",
                                  fluidRow(
                                    column(width = 10, plotOutput("SenPlot2Output")),
                                    column(width = 2,
                                           selectInput("description", 
                                                       "Offense Description", 
                                                       description_options, 
                                                       selected = description_options[1:3],
                                                       multiple = FALSE),
                                           selectInput('grade', "Grade", unique(merged$grade_backfilled), 
                                                       selected = unique(merged$grade_backfilled),
                                                       multiple = TRUE))
                                  ),
                                  ### bottom row tab 2 ####
                                  fluidRow(
                                    column(width = 12, reactableOutput("TableOutputSentences2"))
                                  )
                         )
                  )
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
  #
  # The data ----
  # - Might wanna think about data structure for multiple plot dfs
  # - Packing dfs into a list in a single reactive(Val)?
  
  
  # JudgePlot1 data
  data_judge_filter <- reactive({
    merged %>% 
      filter(judge==input$judge,
             disposition_year >= input$year[1] & disposition_year <= input$year[2])
  })
  # SentencePlot2 data
  data_offense_filter <- reactive({
    merged %>% 
      filter(description_clean %in% input$description,
             grade %in% input$grade)
  })
  # Sentence plot 1 data
  filtered.data <- reactive({
    merged.narrow %>%
      #filter based on selection
      dplyr::mutate(in_select_judges = ifelse(Judge %in% input$judges_of_interest, 
                                              Judge, "Other Judges")) %>%
      filter(race %in% input$races,
             stringr::str_detect(tolower(statute_description),
                                       paste(tolower(input$crime_descriptions), collapse = "|") ),
             stringr::str_detect(tolower(Title_Description),
                                paste(tolower(input$title_descriptions), collapse = "|") ),
             stringr::str_detect(tolower(disposition_method),
                                paste(tolower(input$disposition_methods), collapse = "|") ),
             
             )
      # filter(grepl(paste(input$crime_descriptions, collapse = "|"), statute_description)) %>%
      # filter(grepl(paste(input$title_descriptions, collapse = "|"), Title_Description)) %>%
      # filter(grepl(paste(input$disposition_methods, collapse = "|"), disposition_method)) %>%

  })
  
  # Roy bail data filtered
  bail_filtered <- reactive({
    bail_net_change_by_judge %>% 
      filter(judge %in% input$bail_judges) 
  })
  
  #
  # Plots -----
  #
  # Judge plot 1
  output$JudgePlot1Output <- renderPlot({
    data_judge_filter() %>% 
      dplyr::mutate(description_clean = forcats::fct_lump(description_clean, n= 5)) %>% 
      ggplot(aes(x=description_clean)) + 
      geom_bar(position = "dodge") + 
      labs(title="What are the most common offenses adjudicated?",
           subtitle = paste0("Selected judge: ", input$judge),
           x = "Offense") + 
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) + 
      theme(axis.text.x = element_text(size=5)) + 
      NULL
    
  }, res = 150)
  
  # Sentence plot on panel 2
  output$SenPlot2Output <- renderPlot({
    data_offense_filter() %>% 
      dplyr::filter(sentence_type %in% c("Confinement","Probation"),
                    !is.na(max_period_days)) %>% 
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

  # Sentences Plot 1
  output$PlotSenOutput <- renderPlot({
    filtered.data() %>%
      dplyr::mutate(on.x.axis = forcats::fct_lump_n(eval(parse(text = input$x.axis)), n = input$nfactors)) %>% #head()
      dplyr::mutate(to.facet = forcats::fct_lump_n(eval(parse(text = input$facet)), n = input$nfactors)) %>%
      ggplot(aes(x = on.x.axis, 
                 y = Confinement_Time, 
                 fill = race, 
                 size = Age_at_Arrest, 
                 shape = gender)) +
      geom_jitter(pch = 21, width = 0.3) + 
      scale_size_continuous(name="Age_at_Arrest", range = c(.2,3)) +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.text = element_text(size = 5)) +
      labs(y="Max Confinement Time (Years)", x = paste(input$x.axis)) +
      guides(fill = guide_legend(override.aes = list(size = 3))) + 
      scale_x_discrete(labels = scales::wrap_format(20)) + 
      scale_fill_discrete(labels = scales::wrap_format(10)) +
      facet_wrap(.~to.facet)
  }, res = 150)
  
  # Bail plot 1: 
  output$BailPlot1Output <- renderPlot({
  bail_net_change_by_judge %>%
    ggplot(aes(y=net_change, x=reorder(judge, -net_change), fill=n)) +
    geom_bar(stat='identity', width=.5) +
    labs(fill = "Total # of Bail Amount Changes",
         caption = "This plot shows the cumulative total of bail increases and decreases by a given judge.
          Increases equal 1 while decreases equal -1. Judges that increase bail amounts more often
          than they decrease them have a positive value, while the opposite is true for judges
          that decrease bail amounts more often. The bar fill indicates the total number of 
          bail changes (both increases and decreases") +
    xlab("Judges") +
    ylab("Cumulative Total of Bail Increases and Decreases") +
    theme(
      plot.caption = element_text(hjust = 0)
    )}, res = 150)
  
  
  # Bail Plot 2:
  output$BailPlot2Output <- renderPlot({
    bail_filtered() %>%
      ggplot(aes(y=net_change, x=reorder(judge, -net_change), fill=n)) +
      geom_bar(stat='identity', width=.5) +
      labs(fill = "Total # of Bail Amount Changes",
           caption = "This plot shows the cumulative total of bail increases and decreases by a given judge.
          Increases equal 1 while decreases equal -1. Judges that increase bail amounts more often
          than they decrease them have a positive value, while the opposite is true for judges
          that decrease bail amounts more often. The bar fill indicates the total number of 
          bail changes (both increases and decreases)") +
      xlab("Judges") +
      ylab("Cumulative Total of Bail Increases and Decreases") +
      theme(
        plot.caption = element_text(hjust = 0)
      )
      }, res = 150)
  

  
  # Debugging button - shinyapps.io asked me to disable this for deployment
  # observeEvent(input$Debugger, {browser()})
  
  
  #
  # All the reactables ----
  #
  # Reactable for sentences plot 1
  output$TableOutputSentences1 <- renderReactable({
    reactable(filtered.data())
  })
  
  # Reactable for Judge tab 1, on row 2
  output$TableOutputJudge1 <- renderReactable({
    reactable(data_judge_filter())
  })
  
  # Reactable for Sentence tab 2, on row 2
  output$TableOutputSentences2 <- renderReactable({
    reactable(data_offense_filter())
  })
  
  # Reactable for Bail tab 
  # UPDATE THIS WITH THE REAL BAIL DATA
  output$TableOutputBail1 <- renderReactable({
    reactable(bail_filtered())
  })
  
}



shinyApp(ui, server)
