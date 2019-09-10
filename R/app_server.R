#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
    library(ggplot2)
    library(magrittr)
    library(dplyr)
    library(DT)
    
    custom_bar_plot <- function(dataset, selected_gene){
        
        filtered <- dplyr::filter(dataset, Gene == selected_gene) 
        
        error_bar_max <- dplyr::mutate(filtered, error_max = mean+sd) %>%
            dplyr::pull(error_max)
        
        y_max <- max(error_bar_max)*1.1
        
            ggplot(filtered, aes(x = type, y = mean, fill = type)) +
                geom_bar(stat="identity", color = "black", lwd = 1.2) +
                geom_errorbar(aes(ymin = mean, ymax = error_bar_max), lwd = 1.2, width = 0.3) +
                ggtitle(selected_gene) +
                theme_classic() +
                theme(plot.title = element_text(size=20, face="bold"), 
                      legend.position = "none", 
                      axis.title.y = element_text(size=14, face="bold"), 
                     # axis.text = element_text(face="bold"), 
                      axis.text.x = element_text(size=14, face="bold")
                     ) +
                scale_fill_manual(values=c('#3e5b7c','#bd1e15')) +
                scale_y_continuous(expand = c(0, 0), limits = c(0, y_max), labels = scales::scientific) +
                xlab("") +
                ylab("Protein abundance")
            
    }
    
    observeEvent(input$browser,{
        browser()
    })
    
    
    output$protein_abundance_plot1 <- renderPlot({
      
      if(length(highlightedGene()) >= 1){
      
        custom_bar_plot(Wojdyla_data_tidy, highlightedGene()[1])
      }  
    })
    
    output$protein_abundance_plot2 <- renderPlot({
      
      if(length(highlightedGene()) >= 2){
        
        custom_bar_plot(Wojdyla_data_tidy, highlightedGene()[2])
      }  
    })
    
    output$protein_abundance_plot3 <- renderPlot({
      
      if(length(highlightedGene()) >= 3){
        
        custom_bar_plot(Wojdyla_data_tidy, highlightedGene()[3])
      }  
    })
    
    output$protein_abundance_plot4 <- renderPlot({
      
      if(length(highlightedGene()) >= 4){
        
        custom_bar_plot(Wojdyla_data_tidy, highlightedGene()[4])
      }  
    })
    
    # output$protein_abundance_plot5 <- renderPlot({
    #   
    #   if(length(highlightedGene()) >= 5){
    #     
    #     custom_bar_plot(Wojdyla_data_tidy, highlightedGene()[5])
    #   }  
    # })
    # 
    # output$protein_abundance_plot6 <- renderPlot({
    #   
    #   if(length(highlightedGene()) >= 6){
    #     
    #     custom_bar_plot(Wojdyla_data_tidy, highlightedGene()[6])
    #   }  
    # })
    # 
   
    highlightedGene <- reactive({
      
      highlighted <- input$mytable_rows_selected
      
      if(length(highlighted)){ 
        highlightedRows <- 1:nrow(gene_information) %in% highlighted
        return(gene_information[highlightedRows,]$Gene)
      }
      else {
        return(NULL)
      }
    })
    

    output$mytable <- DT::renderDataTable({
      
      table_data <- gene_information %>%
        mutate(`Uniprot id` = paste0("<a href =", "\"https://www.uniprot.org/uniprot/", `Uniprot id`, "\">",`Uniprot id`, "</a>"))
      
      datatable(table_data, escape = FALSE, filter = "bottom", options = list(
        dom = 'fltip', 
        lengthMenu = c(5, 10, 20, 50), 
        pageLength = 8,
        autoWidth = TRUE,
        columnDefs = list(list(
          targets = c(3,4,8),
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 10 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
            "}")
        ))
      )) %>%
      formatStyle( 0, target= 'row',color = 'black', lineHeight='90%')
    })

    
  #  options = list(
  #    lengthMenu = c(5, 10, 20, 50), pageLength = 5, autoWidth = TRUE)
        
    myProxy = dataTableProxy("mytable")
    
    observeEvent(input$clear_plots, {
      
      myProxy%>%
        selectRows(selected = NULL)
      
    })
    
    # observeEvent(input$download_plot1,{
    #   filename <- "my_test_file.png"
    #   if(length(highlightedGene()) >= 1){
    #    p <- custom_bar_plot(Wojdyla_data_tidy, highlightedGene()[1])
    #    ggsave(filename, p, device = "png")#, units = "in")
    #   }  
    # }) 
    
    customDownloadHandler <- function(n=1){
     # if(length(highlightedGene()) < n) return (NULL)
      downloadHandler(
        filename = function() {
          paste0(highlightedGene()[n], ".png")
        },
        content = function(file) {
          p <- custom_bar_plot(Wojdyla_data_tidy, highlightedGene()[n])
          ggsave(file, p, device = "png")
        }
      )
    }
    output$downloadPlot1 <- customDownloadHandler(1)
    output$downloadPlot2 <- customDownloadHandler(2) 
    output$downloadPlot3 <- customDownloadHandler(3) 
    output$downloadPlot4 <- customDownloadHandler(4) 
    
    output$download_button_plot1 <- renderUI({
      if(length(highlightedGene()) >= 1){
        downloadButton('downloadPlot1', 'Download')
      }
    })
    
    output$download_button_plot2 <- renderUI({
      
      if(length(highlightedGene()) >= 2){
        downloadButton('downloadPlot2', 'Download')
      }
    })
    
    output$download_button_plot3 <- renderUI({
      
      if(length(highlightedGene()) >= 3){
        downloadButton('downloadPlot3', 'Download')
      }
    })
    
    output$download_button_plot4 <- renderUI({
      
      if(length(highlightedGene()) >= 4){
        downloadButton('downloadPlot4', 'Download')
      }
    })
    
    
    
      
    #   if(!is.null(input$file1) & !is.null(input$file2)) {
    #     downloadButton('OutputFile', 'Download Output File')
    #   }
    # })
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




