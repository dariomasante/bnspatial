library(shiny)

# Define UI for application that draws a histogram
ui = dashboardPage( 
  dashboardHeader(title = 'bnspatial UI'),
  dashboardSidebar( # Side panel ----
    hr(),
    sidebarMenu(id="tabs",
      menuItem("Info", tabName = "info", selected=TRUE),
      menuItem("Load and explore BN", tabName = "load_bn"),
      menuItem("Scenarios", tabName = "scene_bn"),
      menuSubItem('Setup', 'set_scene'),
      menuSubItem('Explore', 'expl_scene'),
      menuItem("Analysis", tabName="plots", icon=icon("line-chart") ),
      menuItem("Edit BN", tabName = "edit_bn")
    ),
    hr(),
    # conditionalPanel("input.tabs == 'info'",
    #   fluidRow( 'Placeholder')
    # ),
    conditionalPanel("input.tabs=='load_bn'",
      fluidRow(
        actionButton("loadButton", "Load network"),
        selectInput('selBN','Select a BN',
          choices=list('Sample from bnspatial'='conwy')
        )
      )
    ),
    # conditionalPanel("input.tabs=='scene_bn'", # Crop calendars
    # fluidRow( selectInput("selTarget", "Choose a target node:",
    #   choices = '', selected=NULL)
    # ),
    # fluidRow(
    #   checkboxGroupInput("selOutput", "Output type:",
      #   choices = list("Expected value" = 'expect',
      #          "Uncertainty" = 'uncert',
      #          "Prob" = 'prob',
      #          "other"= 'oth'),
      #   selected = 'expect')
      # ),
      # fluidRow( actionButton("exeButton", "Execute") )
      # ),
    conditionalPanel("input.tabs=='plots'", # Various themes
      fluidRow( 'placeholder' )
    ),
    conditionalPanel("input.tabs=='edit_bn'", # Various themes
      # fluidRow( 
      #   selectInput("editBN", "Edit node", choices = '', selected=NULL)
      # ),
      # fluidRow( actionButton("addButton", "Add node") ),
      fluidRow( actionButton("saveButton", "Save Network") ),
      fluidRow( actionButton("dwnButton", "Download current network") )
    )
  ),
  dashboardBody( # Main Panel  ----
    tabItems(
      tabItem(tabName = "info", #
        fluidRow( 'This page as manual, contains links, etc. 
          \r\nhttps://cran.r-project.org/web/packages/bnspatial/index.html')
        ),
      tabItem(tabName = "load_bn", #
        fluidRow(
          column(6,
            plotOutput('bnet'),
            column(6,
              selectInput('evidenceNode', 'Set evidence for node:', choices=''),
              fluidRow(
                column(7,
                  radioButtons('evidenceClasses', NULL, choices='')
                ),
                column(5,
                  fluidRow(actionButton('evidSet','Set evidence')),
                  fluidRow(actionButton('evidRm','Remove evidence')),
                  fluidRow(actionButton('evidReset', 'Reset all'))
                )
              ),
              textOutput('evidLog'),
              selectInput('pickScenario', 'Pre-made scenario:', choices=c('scen1','scen2'))
            ),
            column(6,
              plotOutput('marginProb') # 'Marginal probability for selected node:'
            )
          ),
          column(6,
            selectInput("selCPT", "Show conditional probabilities for node:",
              choices = '', selected=NULL),
            DT::dataTableOutput('cpt')
          )
        ) 
      )
      #  tabItem( tabName='run_bn',
      #    tabBox( width = NULL,
      #    tabPanel(h5("Spatial data"),
       #      fileInput('upload','Upload spatial data', multiple=TRUE),
        #      fluidRow(
        #       column(3,'File'),
        #       column(3,'Associate to node:'),
        #       column(1,'Categorical?'),
        #       column(2,'Node classes:'),
        #       column(3,'Values (class or range)')
        #     ),
        #     uiOutput("candidates"),
        #     actionButton('upClass','Upload classification file'),
        #     downloadButton('downClass','Download classification file'),
        #     textOutput('logPrint')
        #     ),
        #     tabPanel(h5("Input data preview"),
        #      selectInput("selPreview", "Show map of:",
        #      choices = '', selected=NULL),
        #      plotOutput("plot_maps")
        #     ),
        #     tabPanel(h5("Explore scenarios"),
        #      selectInput("selPreview", "Show map of:",
        #      choices = '', selected=NULL),
        #      plotOutput("plot_maps")
        #     ),
        #     tabPanel(h5("Expected"),
        #      plotOutput("plot_expected")
        #     ),
        #     tabPanel(h5("Uncertainty"),
        #      plotOutput('plot_uncert')
        #     ),
        #     tabPanel(h5("Prob"),
        #      plotOutput('plot_prob')
        #     ),
        #     tabPanel(h5("other"),
        #      plotOutput('plot_')
        #     )
        #   )
        #   ),
        #   tabItem("plots",
        #  fluidRow('Placeholder for output analysis box')
        #   ),
        #   tabItem("edit_bn",
        #     conditionalPanel( condition = "input.addButton > 0",
        #       h4('New node:'),
        #       textInput("nodeName","Name:"),
        #       selectizeInput('parent','Parents', choices = '', multiple=TRUE),
        #       selectizeInput('children','Children', choices = '',multiple=TRUE),
        #       tags$div(id = 'placeholder'),
        #       actionButton('addClass','Add class'),
        #       actionButton('submitNode','Save node')
        #     )
        #   )
    )
  )
)
