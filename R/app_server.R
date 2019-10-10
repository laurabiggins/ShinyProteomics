#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here
    library(ggplot2)
    library(magrittr)
    library(dplyr)
    library(DT)

  #callModule(sliderText, "one")
  #callModule(sliderText, "two")

  #callModule(custom_barplot, "protein_abundance_plot1.1", Wojdyla_data_tidy, selected_gene = reactive(gene1))
 # callModule(custom_barplot, "protein_abundance_plot1.1", Wojdyla_data_tidy, gene_information, NA)
  callModule(custom_barplot, "protein_abundance_plot3.1", Wojdyla_data_tidy, gene_information, reactive(input$mytable_rows_selected[1]))#selected_gene = selectedGenes[1])
  
  # for debugging
  observeEvent(input$browser,{
      browser()
  })
  

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
   # selectedGenes()
    #selectedGenes2()
    
  })

  observeEvent(input$clear_filters, {
    
    clearSearch(myProxy)
    
  })

  observeEvent(input$mytable_cell_clicked, {
    
    #selectedGenes()  
    #selectedGenes2()  
  })
  
  observeEvent(input$downloadPlot1, {
    
    browser()
    
  })
  
  
  # selectedGenes2 <- reactive({
  #   
  #   row_numbers <- input$mytable_rows_selected[1:6]
  #   
  #   all6_genes <- gene_information[row_numbers,]$Gene
  #   
  #   callModule(custom_barplot_wrapper, "protein_abundance_plot1.1", Wojdyla_data_tidy, all6_genes[1])
  #   callModule(custom_barplot_wrapper, "protein_abundance_plot2.1", Wojdyla_data_tidy, all6_genes[2])
  #   callModule(custom_barplot_wrapper, "protein_abundance_plot3.1", Wojdyla_data_tidy, all6_genes[3])
  #   callModule(custom_barplot_wrapper, "protein_abundance_plot4.1", Wojdyla_data_tidy, all6_genes[4])
  # 
  # })
  
  # selectedGenes <- reactiveValues({
  #   
  #   row_numbers <- input$mytable_rows_selected[1:6]
  #   
  #   gene_information[row_numbers,]$Gene
  # })
  
 # selectedGenes <- reactiveValues()
  
#  getGene <- function(index){ 
    
#    gene_information[index,]$Gene
#  }
  
  
 # selectedGenes$gene1 <- getGene(input$mytable_rows_selected[1])
  #selectedGenes$gene2 <- getGene(input$mytable_rows_selected[2])
  
  

  
  # gene1 <- reactive(selectedGenes()[1])
  # gene2 <- reactive(selectedGenes()[2])
  # gene3 <- reactive(selectedGenes()[3])
  # 
  
#  plot1 <- reactive({  
#    callModule(custom_barplot, "protein_abundance_plot1.1", Wojdyla_data_tidy, gene1())
#  })
  
#  plot3 <- reactive({
#    callModule(custom_barplot, "protein_abundance_plot3.1", Wojdyla_data_tidy, gene3())
#  })  

  
  
  
  # 
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

