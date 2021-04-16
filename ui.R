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
              tabPanel(title = "Common Offenses by Judge", value = "Plot1Tab",
                fluidRow(
                  column(width = 10, plotOutput("Plot1Output")),
                  column(width = 2, 
                    selectInput('judge', 'Judge', judge_options),
                    selectInput('grade', "Grade", unique(od_clean$grade), 
                                selected = unique(od_clean$grade),
                                multiple = TRUE),
                    sliderInput('year', 'Year Range (disposition date)', min=2010, max=2020,
                                value=c(2010, 2020), step=1, round=T, sep="")
                  )
                )
              ),
              
              ##### second plot panel ####
              tabPanel(title = "Max Sentence by Offense", value = "Plot2Tab",
                fluidRow(
                  column(width = 10, plotOutput("Plot2Output")),
                  column(width = 2,
                         selectInput("description", "Offense Description", description_options, 
                                     selected = description_options[1:3],
                                     multiple = FALSE))
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