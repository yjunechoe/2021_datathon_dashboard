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
             grade %in% input$grade,
             disposition_year >= input$year[1] & disposition_year <= input$year[2])
  })
  # For plot2
  data_offense_filter <- reactive({
    merged %>% 
      filter(description_clean %in% input$description)
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
      labs(title="Top offense descriptions",
           subtitle = paste0("Selected judge: ", input$judge)) + 
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) + 
      theme(axis.text.x = element_text(size=5)) + 
      NULL
    
  }, res = 150)
  
  
  # Density plot on panel 2
  output$Plot2Output <- renderPlot({
    ggplot(data_offense_filter()) +
      geom_density(
        aes(max_period_days, fill = grade),
        alpha = .5, color = 'white',
        show.legend = FALSE
      ) +
      labs(
        title = "This is a plot title"
      )
  }, res = 150)
  
  # Reactable on row 2
  output$TableOutput <- renderReactable({
    reactable(data_judge_filter())
  })
  
  # Debugging button
  observeEvent(input$Debugger, {browser()})
  
}
