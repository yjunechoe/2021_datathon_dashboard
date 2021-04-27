# app.R 

# Source file that reads in data, misc. set ups, and other static things
source("source.R")


# ui ----
ui <- dashboardPage(
  
  # Top level ----
  title = "R-Ladies Datathon 2021",
  skin = "purple",
  # header = dashboardHeader(title = img(src="rladieslogo.png", height = 50, align = "right")),
  header = dashboardHeader(title = "R-Ladies JAT Datathon 2021",
                           titleWidth = 300),
  
  
  # Sidebars ----
  ## left sidebar ====
  sidebar = dashboardSidebar(
    shinydashboard::sidebarMenu(
      menuItem("About", tabName = "AboutTab", icon = icon("info")),
      menuItem("Sentences", tabName = "SentenceTab", icon = icon("warning-sign", lib = "glyphicon")),
      menuItem("Bail", tabName = "BailTab", icon = icon("dollar")),
      menuItem("Basic Judge Info", tabName = "JudgeTab", icon = icon("user"))
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
              tabPanel(title = "Frequent Judges", value = "Judge2Tab",
                       fluidRow(
                         column(width = 10, plotOutput("JudgePlot2Output")),
                         column(width = 2,
                                sliderInput('year2', 'Year Range (disposition date)', 
                                            min=2010, max=2020,
                                            value=c(2010, 2020), step=1, 
                                            round=T, sep=""))
                       ),
                       ### bottom row ####
                       fluidRow(
                         column(width = 12, reactableOutput("TableOutputJudge2"))
                       )
              ),
                         
              ##### judge second plot panel ####
              tabPanel(title = "Frequent Offenses by Judge", value = "Judge1Tab",
                       fluidRow(
                         column(width = 10, plotOutput("JudgePlot1Output")),
                         column(width = 2, 
                                selectInput('judge', 'Judge', judge_options),
                                sliderInput('year', 'Year Range (disposition date)', 
                                            min=2010, max=2020,
                                            value=c(2010, 2020), step=1, 
                                            round=T, sep="")
                         )
                       ),
                    
                       ### bottom row ####
                       fluidRow(
                         column(width = 12, reactableOutput("TableOutputJudge1"))
                       )
              )
                 
                  )
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
                     tabPanel(title = "Net Bail Amount Actions - Compare All Judges", 
                              value = "BailPlot1Tab",
                              fluidRow(
                                column(width = 12, girafeOutput("BailPlot1Output", height = NULL))
                              ),
                              fluidRow(
                                column(width = 12,
                                       "This plot shows the cumulative total of bail increases and decreases by a given judge.
          Increases equal 1 while decreases equal -1. Judges that increase bail amounts more often
          than they decrease them have a positive value, while the opposite is true for judges
          that decrease bail amounts more often. The bar fill indicates the total number of 
          bail changes (both increases and decreases)."
                                )),
                              fluidRow(
                                column(width = 12, reactableOutput("TableOutputBail1"))
                              )
                     ),
                     ##### bail plot panel 2 ####
                     tabPanel(title = "Net Bail Amount Actions - Compare Selected Judges", 
                              value = "BailPlot2Tab",
                              fluidRow(
                                column(width = 10, plotOutput("BailPlot2Output")),
                                column(width = 2,
                                       pickerInput('bail_judges', 
                                                   'Judges of Interest', 
                                                   choices = unique(bail_net_change_by_judge$judge),
                                                   selected = c("Judge Ann Butchart", "Judge Abbe Fletman"), 
                                                   options = list(`actions-box` = TRUE), 
                                                   multiple = TRUE))
                              ),
                              fluidRow(
                                column(width = 12,
                                       "This plot shows the cumulative total of bail increases and decreases by a given judge.
          Increases equal 1 while decreases equal -1. Judges that increase bail amounts more often
          than they decrease them have a positive value, while the opposite is true for judges
          that decrease bail amounts more often. The bar fill indicates the total number of 
          bail changes (both increases and decreases)."
                                )),
                              ### bottom row ####
                              fluidRow(
                                column(width = 12, 
                                       reactableOutput("TableOutputBail2")
                                )
                              )
                     ),
                     
                     ##### bail plot panel 3 ####
                     tabPanel(title = "Bail Amount and Type", 
                              value = "BailPlot3Tab",
                              
                              fluidRow(
                                column(width = 2,
                                       p("Select a judge and date range. These selections apply to all the visualizations to the right.", align = "center"),
                                      selectInput('bail_judge3', 'Judge', bail_judge_options),
                                      checkboxGroupInput('bail_year3', 'Year Range', 
                                          sort(unique(completerecords$year)),
                                           selected =unique(completerecords$year)
                                          )
                                ),
                                column(width = 10,
                                tabBox(id = "PlotsTabset", 
                                       width = 12,
                                tabPanel(title = "Mean Bail Amount", value="bailsub1",
                                         fluidRow(
                                           column(
                                             width = 12,
                                                  plotOutput("BailPlot3bOutput"))
                                         )),
                                tabPanel(title = "Bail Actions", value="bailsub2",
                                         fluidRow(
                                           column(
                                             width = 12,
                                             plotOutput("BailPlot3cOutput"))
                                         )),
                                tabPanel(title = "Bail by Season", value="bailsub3",
                                         fluidRow(style = "height:800px",
                                                  column(
                                                    width = 12,
                                          plotOutput("BailPlot3Output", height = "800px"))
                                         ))
                                ))
                              )
                              )
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
                                           selectInput("y.axis","Y Axis", 
                                                       choices = c("Confinement_Time",
                                                                   "Probation_Time"), 
                                                       selected = "Confinement_Time"),
                                           pickerInput('judges_of_interest', 
                                                       'Judges of Interest', 
                                                       choices = unique(merged.narrow$Judge),
                                                       selected = judge_options[1:4], 
                                                       options = list(`actions-box` = TRUE), 
                                                       multiple = TRUE),
                                           textInput("crime_descriptions","Crime Description",
                                                     value = ""),
                                           pickerInput("disposition_methods","Disposition Method", 
                                                       choices = unique(merged.narrow$disposition_method), 
                                                       multiple = TRUE, 
                                                       selected = unique(merged.narrow$disposition_method),
                                                       options = list(`actions-box` = TRUE)),
                                           pickerInput("races","Race", 
                                                       choices = unique(merged.narrow$race), 
                                                       multiple = TRUE, 
                                                       selected = unique(merged.narrow$race),
                                                       options = list(`actions-box` = TRUE)),
                                           pickerInput("max_grade","Grade", 
                                                       choices = unique(as.character(merged.narrow$max_grade)), 
                                                       multiple = TRUE, 
                                                       selected = unique(as.character(merged.narrow$max_grade)),
                                                       options = list(`actions-box` = TRUE)),
                                           numericInput("nfactors","Number Categories",
                                                        value = 5, min = 1, max = 10),
                                           # selectInput("x.axis","On X Axis",
                                           #             choices = options, 
                                           #             selected = "Chapter_Description"),
                                           selectInput("facet","Separate By", 
                                                       choices = options, selected = "statute_description")
                                           # actionButton("sent_button", "Plot", icon("paper-plane"))
                                           
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
                         tabPanel(title = "Sentence by judge and offense", value = "SenPlot2Tab",
                                  fluidRow(
                                    column(width = 10, plotOutput("SenPlot2Output")),
                                    column(width = 2,
                                           selectInput("description", 
                                                       "Offense Description", 
                                                       description_options, 
                                                       selected = description_options[1:3],
                                                       multiple = FALSE),
                                           checkboxInput("facet_grade", "Separate by Grade",FALSE),
                                           selectInput('grade', 
                                                       "Grade", 
                                                       unique(merged$grade_backfilled), 
                                                       selected = unique(merged$grade_backfilled),
                                                       multiple = TRUE),
                                           selectInput('judge_sentence', 'Judge', judge_options))
                                  ),
                                  ### middle row with a legend ###
                                  fluidRow(
                                    column(width = 12, 
      "The above plot compares sentencing behavior for a selected judge compared to all other judges. The y-axis is the percent of occurences for a given sentence type across all observations of that offense. The numbers that appear above the bars are the total number of occurences for that sentence type. Keep in mind that the data used here is only for offenses that had an associated sentence and not the occurences of these offenses on dockets without sentences (while other offenses on the same docket had sentences). This visualization does not consider other factors about the docket such as other crimes, the defendants record, or the disposition method.")
                                  ),
      hr(),
                                  ### another row with another plot ####
                                  fluidRow(
                                      column(width = 10, plotOutput("SenPlot2bOutput"))
                                  ),
                                  fluidRow(
                                       column(width = 12, 
               p("The above plot compares sentencing behavior for a selected judge compared to all other judges. The median values are shown."))
                                  ),
      hr(),
                                  ### bottom row tab 2 ####
                                  fluidRow(
                                    column(width = 12, reactableOutput("TableOutputSentences2"))
                                  )
                         ),
              ##### Sentence 3 plot panel ####
              tabPanel(title = "Sentence type by race", value = "SenPlot3Tab",
                       fluidRow(
                         column(width = 2,
                                pickerInput("race_sentence","Race", 
                                            choices = unique(dispo_det$race), 
                                            multiple = TRUE, 
                                            selected = c("White","Black"),
                                            options = list(`actions-box` = TRUE))
                         ),
                         column(width = 10, plotOutput("SenPlot3Output")),
                         hr(),
                       ### bottom row tab 2 ####
                       fluidRow(
                         column(width = 12, p("The above plot examines how for similar offense grade, there was a difference in the sentencing based on the defendant's race. The percentages shown are the percentage of sentences for a given grade and race."))
                       )
              )
                  )
                )
              )
      )),
      
      
      # About tab ----
      tabItem(tabName = "AboutTab",
              fluidPage(
                fluidRow(
                  column(width = 2),
                  column(width = 10,
                         "This dashboard was created as part of the R-Ladies Philly 2021 Datathon.",
                         "To navigate the dashboard, choose a selection (Bail, Sentences) and then view an analysis in each tab of the corresponding pages.")
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
  # 

  # JudgePlot1 data
  data_judge_filter <- reactive({
    merged %>% 
      filter(judge==input$judge,
             disposition_year >= input$year[1] & disposition_year <= input$year[2])
  })
  # JudgePlot2 data
  data_judge2_filter <- reactive({
    merged %>% 
      filter(
             disposition_year >= input$year2[1] & disposition_year <= input$year2[2]) %>% 
      distinct(judge, docket_id, disposition_year)
  })
  
  # SentencePlot2 data
  # This is just for the reactable - choose selected judge
  data_offense_filter <- reactive({
    merged %>% 
      filter(description_clean %in% input$description,
             grade %in% input$grade,
             judge==input$judge_sentence)
  })
  
  sentence_length_summary <- reactive({
    if(input$facet_grade){
    merged %>% 
      filter(description_clean %in% input$description,
             grade %in% input$grade) %>% 
      mutate(select_judge = ifelse(judge==input$judge_sentence,input$judge_sentence,"Other" )) %>% 
      group_by(grade, description_clean, select_judge, sentence_type) %>% 
    summarise(median_min_days = median(min_period_days, na.rm = T),
              mean_min_days = mean(min_period_days, na.rm = T),
              median_max_days = median(max_period_days, na.rm = T),
              mean_max_days = mean(max_period_days, na.rm = T),
              .groups = "drop") 
    } else {
      merged %>% 
        filter(description_clean %in% input$description,
               grade %in% input$grade) %>% 
        mutate(select_judge = ifelse(judge==input$judge_sentence,input$judge_sentence,"Other" )) %>% 
        group_by(description_clean, select_judge, sentence_type) %>% 
        summarise(median_min_days = median(min_period_days, na.rm = T),
                  mean_min_days = mean(min_period_days, na.rm = T),
                  median_max_days = median(max_period_days, na.rm = T),
                  mean_max_days = mean(max_period_days, na.rm = T),
                  .groups = "drop") 
    }
  })
  
  sentence_type_summary <- reactive({
    if (input$facet_grade){
      merged %>% 
        filter(description_clean %in% input$description,
               grade %in% input$grade) %>% 
        mutate(select_judge = ifelse(judge==input$judge_sentence,input$judge_sentence,"Other" )) %>% 
        group_by(grade, description_clean, select_judge) %>% 
        count(sentence_type) %>% 
        mutate(prop_sentence_type = n/sum(n)) %>% 
        ungroup() %>% 
        mutate(axis_label = paste0(sentence_type, " (n=", as.character(n), ")"))
    } else{
      merged %>% 
        filter(description_clean %in% input$description,
               grade %in% input$grade) %>% 
        mutate(select_judge = ifelse(judge==input$judge_sentence,input$judge_sentence,"Other" )) %>% 
        group_by(description_clean, select_judge) %>% 
        count(sentence_type) %>% 
        mutate(prop_sentence_type = n/sum(n)) %>% 
        ungroup() %>% 
        mutate(axis_label = paste0(sentence_type, " (n=", as.character(n), ")"))
    }
    
  })
  
  
  # Sentence plot 1 data (AM)
  # filtered.data <- eventReactive(input$sent_button, {
  filtered.data <- reactive({
    merged.narrow %>%
      #filter based on selection
      dplyr::filter(race %in% input$races,
             max_grade %in% input$max_grade,
             Type == input$y.axis,
             disposition_method %in% input$disposition_methods) %>% 
      dplyr::filter(
             # !is.na(eval(parse(text = input$y.axis))),
             stringr::str_detect((statute_description),
                                       paste(stringr::str_to_lower(input$crime_descriptions), collapse = "|") ) |
             stringr::str_detect((Chapter_Description),
                                   paste(stringr::str_to_lower(input$crime_descriptions), collapse = "|") )
             # stringr::str_detect(tolower(Title_Description),
             #                    paste(tolower(input$title_descriptions), collapse = "|") ),
             # stringr::str_detect(tolower(disposition_method),
             #                    paste(tolower(input$disposition_methods), collapse = "|") )
             
             ) %>% 
      # dplyr::mutate(on.y.axis = eval(parse(text = input$y.axis))) %>%
      dplyr::mutate(in_select_judges = ifelse(Judge %in% input$judges_of_interest, 
                                             Judge, "Other Judges")) %>%
      dplyr::mutate(select_judges = forcats::fct_lump_n(in_select_judges, n = input$nfactors)) %>% 
      dplyr::mutate(to.facet = forcats::fct_lump_n(eval(parse(text = input$facet)), n = input$nfactors))
  })
  # text.y.axis <- eventReactive(input$sent_button, { input$y.axis})
  
  
  
  # Roy bail data filtered
  bail_filtered <- reactive({
    bail_net_change_by_judge %>% 
      filter(judge %in% input$bail_judges) 
  })
  
  # Kulbir data filtered sentences #3
  dispo_race<- reactive({
    dispo_det %>% filter(race %in% input$race_sentence) 
  })
  
  # Sybil bail data filtered
  bail_season_filtered <- reactive({
    completerecords %>% 
      filter(Judge == input$bail_judge3,
             year %in% input$bail_year3
             # race %in% input$bail_race3,
             # action_type_name %in% input$bail_action3
             )
  })
  
  #
  # Plots -----
  #
  
  # Judge plot 1 ######
  output$JudgePlot2Output <- renderPlot({
    data_judge2_filter() %>% 
      dplyr::mutate(judge = forcats::fct_lump(judge, n= 9)) %>% 
      dplyr::filter(judge!="Other") %>% 
      dplyr::count(judge) %>% 
      ggplot(aes(x=reorder(judge, -n), y = n, fill= judge)) + 
      geom_bar(position = "dodge", stat="identity", show.legend = F) + 
      labs(title="What are the most common judges on dispositions?",
           x = "Judge", y = "Number of dockets") + 
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) + 
      theme(axis.text.x = element_text(size=8)) + 
      NULL
    
  }, res = 100)
  
  
  
  
  # Judge plot 2 #######
  output$JudgePlot1Output <- renderPlot({
    data_judge_filter() %>% 
      dplyr::mutate(description_clean = forcats::fct_lump(description_clean, n= 5)) %>% 
      count(description_clean) %>% 
      ggplot(aes(x=reorder(description_clean, -n), y = n, fill = description_clean)) + 
      geom_bar(position = "dodge", stat="identity", show.legend = F) + 
      labs(title="What are the most common offenses adjudicated?",
           subtitle = paste0("Selected judge: ", input$judge),
           x = "Offense") + 
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) + 
      theme(axis.text.x = element_text(size=8)) + 
      NULL
    
  }, res = 100)
  
  

  # Sentences Plot 1 ###########
  output$PlotSenOutput <- renderPlot({
    filtered.data() %>%
      ggplot(aes(x = select_judges, 
                 y = Time, 
                 fill = race)) +
      geom_jitter(pch = 21, width = 0.3) +
      # geom_boxplot() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
            legend.text = element_text(size = 10)) + 
      # labs(y=paste("Max", text.y.axis(),"(Years)"), x = "Judges") +
      labs(y=paste("Max", input$y.axis,"(Years)"), x = "Judges") +
      # guides(fill = guide_legend(override.aes = list(size = 3))) + 
      #ggtitle(paste("Crimes Associated with '",input$crime_descriptions,"'")) +
      scale_x_discrete(labels = scales::wrap_format(20)) + 
      scale_fill_discrete(labels = scales::wrap_format(10)) +
      facet_wrap(.~to.facet,labeller = label_wrap_gen(), scales = "free_x")
  }, res = 100)
  

  
  # Sentence plot on tab2a (alice) #####
  output$SenPlot2Output <- renderPlot({
    if (input$facet_grade){
      pp <- sentence_type_summary() %>% 
        ggplot(aes(x=sentence_type, 
                   y = 100*prop_sentence_type, 
                   fill=select_judge)) + 
        facet_grid(.~grade, scales = "free_x",space="free_x",
                   labeller = labeller(description_clean = label_wrap_gen(25)))
    } else{
      pp <- sentence_type_summary() %>% 
        ggplot(aes(x=sentence_type, 
                   y = 100*prop_sentence_type, 
                   fill=select_judge)) 
    }
    pp + 
      geom_bar(stat = "identity", position = "dodge") + 
      geom_text(aes(label=n), size=2,
                position=position_dodge(width=0.9), vjust=-0.25) + 
      labs(title="Sentence type by judge and offense",
           x = "", y = "Percentage",
           fill = "Judge",
           subtitle = input$description,
           caption = "Only considering offenses with a disposition") + 
      scale_fill_manual(values = c("lightgray","goldenrod3")) + 
      theme(axis.text.x = element_text(angle=90, hjust=1,vjust=0.5))
  }, res = 100)
  
  # Sentence plot on tab2b (alice) #####
  output$SenPlot2bOutput <- renderPlot({
    if (input$facet_grade){
      sent_plotb <- sentence_length_summary() %>% 
        filter(sentence_type %in% c("Confinement","Probation")) %>% 
        tidyr::pivot_longer(ends_with("days")) %>% 
        filter(name %in% c("median_min_days","median_max_days")) %>% 
        mutate(group = paste(select_judge,sentence_type)) %>% 
        ggplot( aes(x=sentence_type, 
                    y = value,
                    group = group,
                    color=select_judge)) + 
        facet_grid(.~grade, scales = "free_x",space="free_x",
                   drop=T,
                   labeller = labeller(grade_desc = label_wrap_gen(25)))
    } else{
      sent_plotb <-  sentence_length_summary() %>% 
        filter(sentence_type %in% c("Confinement","Probation")) %>% 
        tidyr::pivot_longer(ends_with("days")) %>% 
        filter(name %in% c("median_min_days","median_max_days")) %>% 
        mutate(group = paste(select_judge,sentence_type)) %>% 
        ggplot( aes(x=sentence_type, 
                    y = value,
                    group = group,
                    color=select_judge)) 
    }
    
    sent_plotb + 
      geom_point(position = position_dodge(width=0.4), size = 3) + 
      geom_line(position = position_dodge(width=0.4), 
                size=1.5, alpha=0.7) + 
      labs(title="Sentence length by judge and offense",
           subtitle = input$description,
           x = "", y = "Median Sentence min/max (days)",
           color = "Judge",
           caption = "Only considering offenses with a disposition") + 
      scale_color_manual(values = c("lightgray","goldenrod3")) + 
      coord_flip() + 
      theme(axis.text.x = element_text(angle=90, hjust=1,vjust=0.5))
    
  }, res = 100)
  
  
  # Sentence plot by Race tab 3 #####
  output$SenPlot3Output <- renderPlot({
    # Original (denominator is total sentences for given type)
    # perct <- dispo_race() %>%
    #   add_count(sentence_type, name = "sentence_ct") %>%
    #   count(race, grade, sentence_type,sentence_ct, sort = TRUE) %>%
    #   mutate(pct_sentence = n /sentence_ct)
    # Suggestion (denominator is total cases of given race/grade)
    perct <- dispo_race() %>%
      add_count(race, grade, name = "sentence_ct") %>%
      count(race, grade, sentence_type,sentence_ct, sort = TRUE) %>%
      mutate(pct_sentence = n /sentence_ct)
    
    ggplot(perct) +
      aes(x = grade, fill = race, weight = pct_sentence) +
      geom_bar(position = "dodge") +
      scale_fill_hue() +
      labs(y = "percentage") +
      # theme_light() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      coord_flip() +
      facet_wrap(vars(sentence_type))
  }, res = 100)
  

  # Bail plot 1 (interactive):  ####
  output$BailPlot1Output <- renderGirafe({
    thematic_off()
    girafe_plot <- girafe(
      ggobj = bail_net_change_by_judge %>%
        ungroup() %>% 
        ggplot(aes(y=net_change, x=reorder(judge, -net_change), fill=n)) +
        geom_bar_interactive(
          aes(
            tooltip = glue("{judge}\nNet Change: {net_change}"),
            data_id = str_trim(str_remove_all(judge, "[:punct:]"))
          ),
          stat='identity', width=.5
        ) +
        labs(
          title = "Cumulative Total of Bail Increases and Decreases",
          x = "Judges",
          y = NULL,
          fill = "Total # of Bail Amount Changes"
        ) +
        xlab("Judges") +
        scale_x_discrete(labels = function(x) stringr::str_trunc(x, width=20)) + 
        theme(
          plot.caption = element_text(hjust = 0),
          axis.text.x = element_text(angle=90, hjust=1,vjust=0.5, size = 6)
        ),
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_selection(),
        opts_hover()
      ),
      width_svg = 18,
      height_svg = 10
    )
    thematic_on()
    girafe_plot
  })
  
  
  # Bail Plot 2: ####
  output$BailPlot2Output <- renderPlot({
    bail_filtered() %>%
      ggplot(aes(y=net_change, x=reorder(judge, -net_change), fill=n)) +
      geom_bar(stat='identity', width=.5) +
      geom_hline(yintercept = 0) + 
      labs(fill = "Total # of Bail Amount Changes") +
      xlab("Judges") +
      ylab("Cumulative Total of\nBail Increases and Decreases") +
      theme(
        plot.caption = element_text(hjust = 0)
      )

  }, res = 100)
  
  # Bail Plot 3a: ####
  output$BailPlot3Output <- renderPlot({
    
    if (nrow(bail_season_filtered())<1)
      return(NULL)
    bail_season_filtered() %>%
      filter(total_amount < 1e6) %>% 
      ggplot(aes(x = month, y = total_amount, fill = factor(race))) +
      scale_y_log10() + 
      # coord_flip() +
      geom_point(pch = 21, position = position_jitterdodge(), alpha = 0.7) +
      facet_wrap(~action_type_name, labeller = label_wrap_gen(20)) +
      theme(legend.position="bottom") +
      labs(title = paste("Judge:",input$bail_judge3), 
           caption  = "Viewing bail amount upto $1 million only.",
           x = "Season", y = "Bail Amount", fill = "Race")
    
  }, res = 80)
  
  
  # Bail Plot 3b: ####
  output$BailPlot3bOutput <- renderPlot({
    
    if (nrow(bail_season_filtered())<1)
      return(NULL)
    bail_season_filtered() %>%
      group_by(race, action_type_name) %>%
      summarize(mean_total_amount =mean(total_amount)) %>%
      mutate(race=forcats::fct_reorder(race, mean_total_amount)) %>% 
      ggplot(aes(x = factor(action_type_name), y = mean_total_amount, fill = race )) +
      coord_flip() +
      geom_col(width =0.7, position=position_dodge(width=0.8), color="black") +
      labs(title = paste("Judge:",input$bail_judge3), 
           subtitle = "Municipal Court", 
           caption = "Mean bail amount for all years judge has been in office\ngrouped by race and action type.", 
           x = "Action Type", y = "Mean Bail Amount")
  }, res = 100)

  # Bail Plot 3c: ####
  output$BailPlot3cOutput <- renderPlot({
    
    if (nrow(bail_season_filtered())<1)
      return(NULL)
    bail_season_filtered() %>%
      group_by(race, action_type_name) %>%
      summarise(counts = n()) %>%
      mutate(race=fct_reorder(race, counts)) %>% 
      ggplot(aes(x = action_type_name, y = counts, fill = race )) +
      coord_flip() +
      geom_col(position="dodge", color="black") +
      labs(title = paste("Judge:", input$bail_judge3), 
           subtitle = "Municipal Court", 
           caption = "Sum of all types of bail actions taken\nfor all years judge has been in office grouped by race.", 
           x = "Action Type", y = "Count")
  }, res = 100)
  
  # Debugging button - shinyapps.io asked me to disable this for deployment
  observeEvent(input$Debugger, {browser()})
  
  
  #
  # All the reactables ----
  #
  # Reactable for sentences plot 1
  output$TableOutputSentences1 <- renderReactable({
    reactable(filtered.data() %>% 
                select(docket_id, Judge, min_grade, 
                       max_grade, Time, Type,
                       Age_at_Arrest, race, gender, disposition, Title_Description, 
                       Chapter_Description, statute_description))
  })
  
  # Reactable for Judge tab 1, on row 2
  output$TableOutputJudge2 <- renderReactable({
    reactable(data_judge2_filter()%>% 
                select(Judge = judge) %>% 
                count(Judge, sort=T))
  })
  
  # Reactable for Judge tab 2, on row 2
  output$TableOutputJudge1 <- renderReactable({
    reactable(data_judge_filter()%>% 
                select(Judge = judge, `Offense Description` = description_clean) %>% 
                count(Judge, `Offense Description`, sort=T))
  })
  
  # Reactable for Sentence tab 2, on row 2
  output$TableOutputSentences2 <- renderReactable({
    reactable(data_offense_filter())
  })
  
  # Reactable for Bail tab 1
  output$TableOutputBail1 <- renderReactable({
    bail_net_change_by_judge %>% 
      ungroup() %>% 
      select(Judge = judge, `Number of Actions` = n, net_change) %>% 
      arrange(-net_change) %>% 
      reactable(
        columns = list(
          net_change = colDef(
            name = "Cumulative Total of Bail Increases and Decreases",
            cell = data_bars(., fill_color = c("orange", "purple"))
          )
        )
      ) 
  })
  
  # Reactable for Bail tab 2
  output$TableOutputBail2 <- renderReactable({
    reactable(dplyr::select(bail_filtered(), -action_type))
  })
  
  # Reactable for Bail tab 3
  # output$TableOutputBail3 <- renderReactable({
  #   reactable(bail_season_filtered())
  # })
  
  
}



shinyApp(ui, server)
