
# Source file that reads in data, misc. set ups, and other static things
source("source.R")


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
    ggplot(data_offense_filter(), 
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
      scale_x_discrete(drop=TRUE) + 
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
  observeEvent(input$Debugger, {browser()})
  
}
