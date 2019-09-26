#' @import shiny

app_ui <- function() {


  tagList(
   
    # Leave this function for adding external resources
    golem::favicon(ico = "www/favicon.png"),
    golem_add_external_resources(),
    tags$script(src = "www/script.js"),
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
   # tags$script(src = "www/jquery-3.4.1.js"),
   # tags$script(src = "www/datatables.min.js"),

  fluidPage(
    br(),
    #  tags$script(src = "script.js"),
    withTags(
      div(class="title_block",   
        h1("Cell surface proteome of human pluripotent states"),
        br(),
        h4("Plasma membrane profiling identifies differences in cell surface protein expression 
           between naïve and primed human pluripotent stem cells"),
        h4("Wojdyla et al."),
        br()
      )
    ),
    #actionButton("browser", "browser"),
    br(),
    #  br(),
     # h5("Enter gene name, protein name, gene ontology term etc."),
    withTags(
      div(class="table_area",  
        br(),
        h4("Enter gene name, protein name, gene ontology term etc"),
        p("Search across all fields in the box below or search in individual columns within the table"),
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
        ),         
        br()
      )
    ), 
    br(),
    withTags(
      div(class="plots", 
        h3("Select up to 4 rows in the table to display plots")#,
       # br(),
       # actionButton(inputId = "clear_plots", label = "Clear plots"),
       # br()
      )
    ), 
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
        ),
      br(),
      br(),
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
    p("Paper citation details")
  )
  )
 #   })
#htmlwidgets::saveWidget(pr, "profile_ui.html")  
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
