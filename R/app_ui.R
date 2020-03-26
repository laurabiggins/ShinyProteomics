#' @import shiny

app_ui <- function() {

 # shinyjs::useShinyjs()

  tagList(
   
    #shinyjs::useShinyjs(),
    golem_add_external_resources(),

    #tags$script(src = "www/script.js"),
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
    
    fluidPage(
      br(),
      withTags(
        div(class="title_block",   
          h1("Cell surface proteome of human pluripotent states"),
          br(),
          h4("Cell-surface proteomics identifies differences in signalling and adhesion 
              protein expression between naive and primed human pluripotent stem cells"),
          h4("Wojdyla et al., 2020"),
          br()
        )
      ),
     # actionButton("browser", "browser"), # for debugging
      br(),
      withTags(
        div(class="table_area",  
          h4("Enter gene name, protein name, gene ontology term or other keyword"),
          p("Search across all fields in the box below or use search boxes in individual columns within the table"),
          br(),
          div(id="search_and_table"),
          DT::dataTableOutput("mytable"),
          br(),
          fluidRow(
            column(3,
                   downloadButton(outputId = "download_table", label = "Download Table")
            ),
            column(2,
                   actionButton(inputId = "clear_filters", label = "Clear Filters")
            ),   
            column(2,
                   actionButton(inputId = "clear_plots", label = "Clear selected rows")
            )
          )
        )
      ), 
      br(),
      withTags(
        div(class="plots", 
          h3("Select up to 6 rows in the table to display plots")
        )
      ), 
      br(),
      fluidRow(
        column(2,
               custom_barplotUI("protein_abundance_plot1.1")#,
               # check the names etc!!!!!
               #shinyjs::disable("protein_abundance_plot1.1-download_plot")  
        ),
        column(2,
               custom_barplotUI("protein_abundance_plot2.1")
        ),
        column(2,
                custom_barplotUI("protein_abundance_plot3.1")
        ),
        column(2,
                custom_barplotUI("protein_abundance_plot4.1")
        ),
        column(2,
                custom_barplotUI("protein_abundance_plot5.1")
        ),
        column(2,
                custom_barplotUI("protein_abundance_plot6.1")
        )
      ),
     # shinyjs::disable("protein_abundance_plot3.1-downloadPlot1"),
      br(),
      # fluidRow(
      #   column(2,
      #          uiOutput("download_button_plot1")
      #   ),
      #   column(2,
      #          uiOutput("download_button_plot2")
      #   ),
      #   column(2,
      #          uiOutput("download_button_plot3")
      #   ),
      #   column(2,
      #          uiOutput("download_button_plot4")
      #   ),
      #   column(2,
      #          uiOutput("download_button_plot5")
      #   ),
      #   column(2,
      #          uiOutput("download_button_plot6")
      #   )
      # ),
      br(),
      br(),
      p("Wojdyla et al., 2020"),
      br()#,
      #sliderTextUI("one"),
      #sliderTextUI("two"),
      #fluidRow(
     #   column(2,
      #         custom_barplotUI("protein_abundance_plot1.1")
      #  )#,
       # column(2,
       #        custom_barplotUI("protein_abundance_plot2.1")
       # )
     # )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
#  addResourcePath(
#    'www', system.file('app/www', package = 'ShinyProteomics')
#  )
 
   addResourcePath(
    'www', "/data/private/shiny_scripts/Wojdyla/ShinyProteomics/inst/app/www"
  )
 
 
  tags$head(
   # golem::activate_js(),
    #golem::favicon(ico = "www/favicon.png"),
    tags$script(src = "www/script.js"),
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # Or for example, you can add shinyalert::useShinyalert() here
  )
}
