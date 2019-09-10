#' @import shiny
app_ui <- function() {
  tagList(
   
    # Leave this function for adding external resources
    golem_add_external_resources(),
      tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
      tags$head(tags$script(src="www/script.js")),
     # tags$head(tags$script(src="script.js")),
    # List the first level UI elements here 
  #  tags$script(HTML("document.body.style.backgroundColor = 'skyblue';")),
  #  tags$script(HTML("$('.dataTables_filter').addClass('pull-left');")),
  #  tags$script(src = "script.js"),
    fluidPage(
      #tags$script(src = "script.js"),
      br(),
      h1("Plasma membrane profiling identifies differences in cell surface protein expression
between naïve and primed human pluripotent stem cells"),
      h3("Wojdyla et al."),
      br(),
      #actionButton("browser", "browser"),
      br(),
      br(),
      DT::dataTableOutput("mytable"),
      br(),
      downloadButton(outputId = "download_table",
                     label = "Download Table"),
      br(),
      h3("Select up to 4 rows in the table to display plots"),
      br(),
      br(),
      actionButton("clear_plots", "clear_plots"),
      br(),
      br(),
      fluidRow(
        column(3,
               plotOutput(outputId = "protein_abundance_plot1", height = "300px")
        ),
        column(3,
               plotOutput(outputId = "protein_abundance_plot2", height = "300px")
        ),
        column(3,
               plotOutput(outputId = "protein_abundance_plot3", height = "300px")
        ),
        column(3,
               plotOutput(outputId = "protein_abundance_plot4", height = "300px")
        )
      ),
      br(),
      fluidRow(
          column(3,
                 uiOutput("download_button_plot1")
          ),
          column(3,
                 uiOutput("download_button_plot2")
          ),
          column(3,
                 uiOutput("download_button_plot3")
          ),
           column(3,
                  uiOutput("download_button_plot4")
          )
        )
      # br(),
      # fluidRow(
      #  
      #   column(4,
      #          plotOutput(outputId = "protein_abundance_plot5")
      #   ),
      #   column(4,
      #          plotOutput(outputId = "protein_abundance_plot6")
      #   )
      # )  
    )  
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'ShinyProteomics')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
