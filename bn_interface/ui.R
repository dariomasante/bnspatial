# Author Dario Masante

library(shiny)
library(shinydashboard)
library(leaflet)

ui = dashboardPage( 
  dashboardHeader(title = 'bnspatial UI'),
  dashboardSidebar( # Side panel ----
    hr(),
    sidebarMenu(id="tabs",
      menuItem("Info", tabName = "info", selected=TRUE),
      menuItem("Load and explore BN", tabName = "load_bn"),
      menuItem("Load and view spatial data", tabName = "spatial_bn"),
      # menuSubItem('Setup', 'set_scene'),
      menuItem('Explore', tabName='expl_scene'),
      menuItem("Analysis", tabName="plots", icon=icon("line-chart") ),
      menuItem("Edit BN", tabName = "edit_bn")
    ),
    hr(),
    conditionalPanel("input.tabs == 'info'",
      fluidRow(column(1),column(11,'Table of content')),
      fluidRow(column(1),column(10, 'I. What this app does')),
      fluidRow(column(1),column(10, 'II. Understanding BBNs')),
      fluidRow(column(1),column(10, 'III. Scenarios in geographical space')),
      fluidRow(column(1),column(10, 'IV. ...'))
    ),
    conditionalPanel("input.tabs=='load_bn'",
      fluidRow(
        fileInput("loadButton", "Load network", accept='.net'),
        selectInput('selBN','Select a BN',
          choices=list('Sample from bnspatial'='conwy')
        )
      )
    ),
    conditionalPanel("input.tabs=='spatial_bn'", NULL
    ),
    conditionalPanel("input.tabs=='expl_scene'", 
      fluidRow( actionButton("exeButton", "Run spatial model") )
    ),
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
      tabItem(tabName = "info", # info page----
        fluidRow( 'This page as manual, contains links, etc. 
          \r\nhttps://cran.r-project.org/web/packages/bnspatial/index.html')
        ),
      tabItem(tabName = "load_bn", # BN page ----
        fluidRow(
          column(6,
            plotOutput('bnet'),
            column(6, #evidenceBlock
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
              textOutput('evidLog')
              # selectInput('pickScenario', 'Pre-made scenario:', choices=c('scen1','scen2'))
            ),
            column(6,
              # plotOutput('marginProb') # 'Marginal probability for selected node:'
              checkboxGroupInput('showBars','Show marginal probability for nodes:',
                                 choices = '', selected=NULL)
            )
          ),
          # column(6,
          #   selectInput("selCPT", "Show conditional probabilities for node:",
          #     choices = '', selected=NULL),
          #   DT::dataTableOutput('cpt')
          # )
          column(6, 
            tabBox( width = NULL, 
              tabPanel(h5("Marginal probabilities"), 
                plotOutput('marginProb', height = 600) 
              ),
              tabPanel(h5("Conditional prob. table"), 
                selectInput("selCPT", "Show conditional probabilities table for node:",
                             choices = '', selected=NULL),
                DT::dataTableOutput('cpt')
              )
            )
          )
        ) 
      ),
      tabItem( tabName='spatial_bn', # Load spatial ----
        tabBox( width = NULL,
        tabPanel(h5("Spatial data"),
          fluidRow(
            column(4, fileInput('upload','Upload spatial data', multiple=TRUE)),
            column(2, actionButton('resetFiles', 'Reset all')),
            column(3, actionButton('upClass','Upload classification file')),
            column(3, downloadButton('downClass','Download classification file'))
          ),
          fluidRow(
            column(1,''),
            column(3,'File'),
            column(2,'Associate to node:'),
            column(1,'Categorical?'),
            column(2,'Node classes:'),
            column(3,'Values (class or range)')
          ),
          uiOutput("candidates"),
          textOutput('logPrint')
        ),
        tabPanel(h5("Input data view"),
          # selectInput("selPreview", "Show map of:",
          # choices = '', selected=NULL),
          # plotOutput("plot_maps")
          fluidRow(
            column(2, 
              fluidRow(radioButtons('selPreview', 'Show:', choices='')),
              fluidRow('Put legend here')
            ),
            column(9, leafletOutput('plot_maps', height = 500) )
          )
        )
        )
      ),
      tabItem('expl_scene', # Spatialize BN page ----
        column(3, style='padding:30px;',
          fluidRow(
            fluidRow( selectInput("selTarget", "Choose a target node:",
                choices = '', selected=NULL)
            ),
            fluidRow(
              checkboxGroupInput("selOutput", "Output type:",
                                 choices = list("Most likely state" = 'class',
                                                "Entropy" = 'entropy',
                                                "Expected value" = 'expected',
                                                "Coeff. of variation"= 'variation',
                                                "Probability" = 'probability'),
                                 selected = 'class'),
              uiOutput('subOut')
            ),
            fluidRow( 
              box(
                selectInput('evidenceNode2', 'Set evidence for node:', choices=''),
                column(6,
                  radioButtons('evidenceClasses2', 'States:', choices='')
                ),
                column(6,
                  fluidRow(actionButton('evidSet2','Set evidence')),
                  fluidRow(actionButton('evidRm2','Remove evidence')),
                  fluidRow(actionButton('evidReset2', 'Reset all'))
                ),
                width = NULL, solidHeader = TRUE, title="Set evidence", collapsible = TRUE, collapsed=TRUE
              ),
              textOutput('evidLog2')
              # selectInput('pickScenario', 'Pre-made scenario:', choices=c('scen1','scen2'))
            )
          )
        ),
        column(9, 
          leafletOutput('plot_output', height = 600),
          fluidRow(
            column(4, 
              uiOutput('outSel') # dinamically add radiobuttons
            ),
            column(2, uiOutput('subShow') ),
            column(6, verbatimTextOutput('msg') )
          )
        )
      )
    #   tabItem("plots", # Analysis --
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
