
library(shinyalert)
library(VennDiagram)
library(tools)
library(shinythemes)
library(gplots)
library(shiny)
library(shinyjs)
library(openxlsx)
library(V8)



jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
jscloseW    <- "shinyjs.closeWindow = function() { window.close(); }"


ui = fluidPage(
  titlePanel("DGINN additional GUI"),
  useShinyjs(),                                           # Include shinyjs in the UI
  extendShinyjs(text = jsResetCode, functions = c("winprint")), 
  extendShinyjs(text = jscloseW , functions = c("closeWindow")  ),
  
  theme = shinytheme("darkly"),
  
  
  titlePanel(h1(
    a(href = "https://insb.cnrs.fr/fr/rencontre-avec-lucie-etienne-chercheuse-au-centre-international-de-recherche-en-infectiologie-ciri",
      "Visualization of DGINN results", style = "color:skyblue; font-size:130% "), align = "center"
  )) ,
  #titlePanel(h1("Insight into genes involved in evolutionary 'arms-races'",style = "color:skyblue; font-size:300% ", align = "center")) ,
  
  
  
  
  
  
  #_________________________________fluidRow de l'espace de t?l?versement du document ? traiter____________________________________________________ 
  fluidRow(
    sidebarLayout(
      sidebarPanel(position = "left", width = 3 ,
                   h6(HTML("Upload your .txt/.csv/.tsv Tab-separated values Or Excel .xlsx sheet (<i> must be the first in case of multiple sheets workbook </i>)") ),
                   tags$div(title = "The tab-separated or .xlsx file must contain the 'Gene', 'GeneSize', 'Omega', 'Method', 'PosSel', 'NbSites' & 'PSS' columns. 
                 In 'Method' column, BUSTED and MEME methods are mandatory." ,
                            fileInput("boutonUpload" , "The file must conform with DGINN's formatting", placeholder = "No DGINN's file selected")
                   )#tags$div(
      ), # sidebarPanel(
      mainPanel ()
    )#sidebarLayout(
  ), #  fluidRow( 
  
  
  #________________________fluidRow tableCsv & heat_____________________________________________________________
  fluidRow(   
    mainPanel(
      column(
        width = 12 , div(style = "height:100px;"),
        align = "right" ,
        offset = 3 ,
        
        #Affichage de texte.
        textOutput("fpath"),
        
        #Affichage de tables.
        tableOutput("tableCsv"),
        
        #Affichage d'un plot.
        plotOutput("heat"     , width = "120%" , height = "1200px")
        #,
        # plotOutput("barPlots" ,height = "1000px") 
        # ,
        # 
        # plotOutput("sitePos" ,height = "1400px" )
        
      )#column(width = 8
    )#mainPanel)
  ),# fluidRow(
  
  #________________________fin fluidRow tableCsv & heat_____________________________________________________________
  
  
  #________________________fluidRow barplots_____________________________________________________________
  fluidRow(
    
    mainPanel(
      column(
        width = 12 , div(style = "height:100px;"),
        align = "right" ,
        offset = 3 ,
        plotOutput("barPlots", width = "120%" ,height = "1200px" )
        
      )#column
    )#mainPanel)
  ),# fluidRow(
  #________________________fin fluidRow barplots_____________________________________________________________
  
  #________________________fluidRow sitePos_____________________________________________________________
  
  fluidRow(
    mainPanel(
      column(
        width = 12 , div(style = "height:100px;"), #height:100px c'est la distance entre les graphs.
        align = "right" ,
        offset = 3 ,
        plotOutput("sitePos" , width = "120%" , height = "2800" )
      )#column
    )#mainPanel)
  ),#fluidRow(
  #________________________fin fluidRow sitePos_____________________________________________________________
  
  #________________________fluidRow vide, jsute pour ne pas arr?ter la page pile ? la fin du dernier graphique_____________________________________________________________
  
  fluidRow(
    mainPanel(
      column(
        width = 12 , div(style = "height:100px;"),
        align = "right" ,
        offset = 3
        #,
        # plotOutput("sitePos" ,height = "1800px" )
      )#column
    )#mainPanel)
  ),#fluidRow(
  #________________________fin fluidRow vide_____________________________________________________________
  
  
  absolutePanel(
    fluidRow(
      column(
        width = 3,
        align = "left",
        offset = 1,
        # titlePanel(
        #   h5('Check for uploaded data and figures', style = "color:royalblue; font-size:100% ; align = center")
        # ) ,
        #-- Display 'counting ...'
        
        hr(),
        actionButton('showHideData'   , 'Show/Hide data'),
        
        hr(),
        actionButton('showHideVenn'   , 'Show/Hide Figures'),
        
        hr(),
        tags$div(title="Before uploading your file, choose how many genes should appear in each pdf page",
                 numericInput("nbGene", "Genes by page", 10 , min = 1, max = 30)
        ),
        hr(),
        downloadButton('download_PDF', 'Download Figures'),
        hr(),
        tags$div(title="Clicking this button will erase current dataset. Reset GUI before uploading a new dataset",
                 actionButton('resetData', 'Reset GUI', icon("refresh"),
                              style="color:snow; background-color: #337ab7; border-color: dodgerblue")
        ),
        hr(), 
        tags$div(title="Close application",
                 actionButton('quit', 'Quit', icon("off" , lib = "glyphicon"),
                              style="color:snow; background-color: orange; border-color: white")
                 
        )
      )
    ) #fluidRow(
    ,
    
    top = 350,
    left = 0,
    right = NULL,
    bottom = NULL,
    width = 600,
    height = 200,
    draggable = F,
    fixed = T,
    cursor = c("auto", "move", "default", "inherit")
    ,
  
    br(),
    br(),
    h6(
      "By the",
      a(href = "http://ciri.inserm.fr/service-bioinformatique-et-biostatistique-bibs/",
        "CIRI-BIBS Service", style = "background-color:purple ; color:white; font-size:80%;"),
      style = "margin-left:10%;"
    )#h6
  )# absolutePanel
  
)# fluidPage
