ui <- dashboardPage(
  
  # Top level ----
  title = "R-Ladies Datathon 2021",
  skin = "purple",
  header = dashboardHeader(title = "R-Ladies Philly"),
  
  # Sidebars ----
  ## left sidebar ====
  sidebar = dashboardSidebar(
    shinydashboard::sidebarMenu(
      menuItem("Home", tabName = "HomeTab", icon = icon("home")),
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
      
      # Home tab ----
      tabItem(tabName = "HomeTab",
            
        ## main page ====
        fluidPage(
          
          ### top row ####
          fluidRow(
            tabBox(id = "PlotsTabset", width = 12,
              
              ##### first plot panel ####
              tabPanel(title = "plot1", value = "Plot1Tab",
                fluidRow(
                  column(width = 10, plotOutput("Plot1Output")),
                  column(width = 2, 
                    sliderTextInput("user_letters", "y", LETTERS[1:10], "A", width = "90%"),
                    sliderInput("user_number", "y", 0, nrow(mtcars), 10, width = "90%"),
                    selectInput('judge', 'Judge', judge_options)
                  )
                )
              ),
              
              ##### second plot panel ####
              tabPanel(title = "plot2", value = "Plot2Tab",
                fluidRow(
                  column(width = 12, plotOutput("Plot2Output"))
                )
              )
            )
          ),
          
          ### bottom row ####
          fluidRow(
            column(width = 12, reactableOutput("TableOutput"))
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