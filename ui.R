library(shiny)
library(ChaoSpeciesOnline)

shinyUI(pageWithSidebar(
  
  headerPanel('ChaoSpecies Online'),
  sidebarPanel(
    tags$head(
      tags$style(type = "text/css", "textarea{ max-width: 230px;}")
      ),
    
    p(h4("Data Setting")),
    wellPanel(
      selectInput(inputId='datatype', label='Choose the data type', choices=c('abundance', 'incidence')),
      
      uiOutput("dataname"),
      checkboxInput(inputId=, label="", value=
      
        )
      
      
      conditionalPanel(
        condition = "input.datatype == 'abundance'",
          tags$textarea(id="copyAndPaste_abun", rows = 8,
                        "Birds 752 276 194 126 121 97 95 83 72 44 39 0 16 15 0 13 9 9 9 8 7 4 0 0 2 2 1 1 1 \nSpider 0 15 46 2 0 0 0 1 6 1 1 0 1 2 0 1 0 0 1 1 0 0 0 0 0 0 15 0 0 17 0 0 0 0 9 0 0 1 1 22 2 0 0 4 0 0 6 0 1 1 8 1 2 0 0 0 0 0"),
          p(em("(Input format : Species abundance frequency.)"))
      ),
      conditionalPanel(
        condition = "input.datatype == 'incidence'",
        tags$textarea(id="copyAndPaste_inci", rows = 8,
                      "Ant 62 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 5 5 5 5 6 6 7 9 9 9 9 10 10 12 13 14 14 14 15 15 16 18 19 19 20 29"),
        p(em("(Input format : First entry should be the total number of sampling units, and followed by species incidence frequency.)"))
      ),
      checkboxGroupInput(inputId='method', label='Choose the estimated method(s)', 
                         choices=c("all", "Homogeneous", "Chao", "CE", "Jackknife"), selected = "all")
    ),
    
    p(h4("General Setting")),
    wellPanel(
      numericInput(inputId='cutpt', label='Set cut-off point (default is 10)', value=10, min=1, max=20, step=1),
      numericInput(inputId='conf', label='Confidence interval (default is 0.95)', value=0.95, min=0, max=1, step=0.01)
    )
  ),
  
  
  mainPanel(tabsetPanel(
    
    tabPanel("Data Summary",
             h4("BASIC DATA INFORMATION"),
             verbatimTextOutput('datainfo1'),
             downloadLink("dsummary1", "Download as csv file"),
             
             conditionalPanel(
               condition = "input.datatype == 'abundance'",
               h4("Rare Species Group")
               ),
             conditionalPanel(
               condition = "input.datatype == 'incidence'",
               h4("Infrequent Species Group")
             ),
             
             
             verbatimTextOutput('datainfo2'),
             downloadLink("dsummary2", "Download as csv file"),
             
             conditionalPanel(
               condition = "input.datatype == 'abundance'",
               includeMarkdown("MarkD/summary_note_abun.Rmd")),
             
             conditionalPanel(
               condition = "input.datatype == 'incidence'",
               includeMarkdown("MarkD/summary_note_inci.Rmd"))
    ),
    
    tabPanel("Etsimation",
             h3("ESTIMATION OF SPECIES RICHNESS"),
             verbatimTextOutput('out'),
             downloadLink("dest", "Download as csv file"), 
             
             conditionalPanel(
               condition = "input.datatype == 'abundance'",
               includeMarkdown("MarkD/description_abun.Rmd")
             ), 
             
             conditionalPanel(
               condition = "input.datatype == 'incidence'",
               includeMarkdown("MarkD/description_inci.Rmd")
               )
             
    ),
    
    tabPanel("Reference",
             includeMarkdown("MarkD/reference.Rmd")
    )
  ))
))