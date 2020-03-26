#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here
  library(ggplot2)
  library(magrittr)
  library(dplyr)
  library(DT)

  callModule(custom_barplot, "protein_abundance_plot1.1", Wojdyla_data_tidy, gene_information, reactive(selected_genes6()[1]))
  callModule(custom_barplot, "protein_abundance_plot2.1", Wojdyla_data_tidy, gene_information, reactive(selected_genes6()[2]))
  callModule(custom_barplot, "protein_abundance_plot3.1", Wojdyla_data_tidy, gene_information, reactive(selected_genes6()[3]))
  callModule(custom_barplot, "protein_abundance_plot4.1", Wojdyla_data_tidy, gene_information, reactive(selected_genes6()[4]))
  callModule(custom_barplot, "protein_abundance_plot5.1", Wojdyla_data_tidy, gene_information, reactive(selected_genes6()[5]))
  callModule(custom_barplot, "protein_abundance_plot6.1", Wojdyla_data_tidy, gene_information, reactive(selected_genes6()[6]))

  # for debugging
  observeEvent(input$browser,{
      browser()
  })
  
  getGene <- function(row_no){
    gene_information[row_no,]$Gene
  }
  
  selected_genes6 <- reactive(getGene(input$mytable_rows_selected))

  output$mytable <- DT::renderDataTable({
    
   datatable(
      gene_information, 
      escape = FALSE,
      filter = list(
         position = "top"
      ),
      options = dt_options
    ) %>%
      formatStyle( 0, target= 'row', color = 'black', lineHeight='90%')
  })

  # creating a proxy so we use the functions available in dataTableProxy to clear
  # selected rows and filters
  myProxy <- dataTableProxy("mytable")
  
  observeEvent(input$clear_plots, {
    
    selectRows(myProxy, selected = NULL)

  })

  observeEvent(input$clear_filters, {
    
    clearSearch(myProxy)
    
  })

  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$download_table <- downloadHandler(
  
    filename = function() {
      "Wojdyla_data.csv"
    },
    content = function(file) {
      write.csv(gene_information[input[["mytable_rows_all"]], ], file)
    }
  )
}

