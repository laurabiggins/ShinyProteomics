#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
    library(ggplot2)
    library(magrittr)
    library(dplyr)
    library(DT)

custom_bar_plot <- function(dataset, selected_gene){
        
        filtered <- dplyr::filter(dataset, Gene == selected_gene)

        #index <- dataset$Gene == selected_gene
        #filtered <- dataset[index,]
        
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
    
    
    output$protein_abundance_plot1 <- renderCachedPlot({

      gene <- highlightedGene1()

      if(is.null(gene)) return (NULL)
      if(!length(gene)) return (NULL)
      if(is.na(gene))   return(NULL)
      
      custom_bar_plot(Wojdyla_data_tidy, gene)

    }, cacheKeyExpr = {input$mytable_rows_selected[1]}, bg = NA)#cacheKeyExpr = {list(input$mytable_rows_selected[1])}, bg = NA)

    
    output$protein_abundance_plot2 <- renderCachedPlot({
      
      gene <- highlightedGene2()
      
      if(is.null(gene)) return (NULL)
      if(!length(gene)) return (NULL)
      if(is.na(gene))   return(NULL)
      
      custom_bar_plot(Wojdyla_data_tidy, gene)
      
    }, cacheKeyExpr = {list(input$mytable_rows_selected[2])}, bg = NA)
    
    
    output$protein_abundance_plot3 <- renderCachedPlot({
      
      gene <- highlightedGene3()
      
      if(is.null(gene)) return (NULL)
      if(!length(gene)) return (NULL)
      if(is.na(gene))   return(NULL)
      
      custom_bar_plot(Wojdyla_data_tidy, gene)
      
    }, cacheKeyExpr = {list(input$mytable_rows_selected[3])}, bg = NA)
    
    
    output$protein_abundance_plot4 <- renderCachedPlot({
      
      gene <- highlightedGene4()
      
      if(is.null(gene)) return (NULL)
      if(!length(gene)) return (NULL)
      if(is.na(gene))   return(NULL)
      
      custom_bar_plot(Wojdyla_data_tidy, gene)
      
    }, cacheKeyExpr = {list(input$mytable_rows_selected[4])}, bg = NA)
    
    
    

    
     highlightedGene1 <- reactive({
       
       row_no <- input$mytable_rows_selected[1]
      
       if(length(row_no)){
         return(gene_information[row_no,]$Gene)
       }else{
         return(NULL)
       }  
    })

     highlightedGene2 <- reactive({
       
       row_no <- input$mytable_rows_selected[2]
       
       if(length(row_no)){
         if (!is.na(row_no)){
           return(gene_information[row_no,]$Gene)
         } else{
           return(NULL)  
         } 
       }else{
         return(NULL)
       }  
     })
     
     highlightedGene3 <- reactive({
       
       row_no <- input$mytable_rows_selected[3]
       
       if(length(row_no)){
         if (!is.na(row_no)){
          return(gene_information[row_no,]$Gene)
         } else{
           return(NULL)  
         } 
       }else{
         return(NULL)
       }  
     })
     
     highlightedGene4 <- reactive({
       
       row_no <- input$mytable_rows_selected[4]
       
       if(length(row_no)){
         if (!is.na(row_no)){
           return(gene_information[row_no,]$Gene)
         } else{
           return(NULL)  
         } 
       }else{
         return(NULL)
       }  
     })
     
    # 
    # highlightedGene <- reactive({
    # 
    #   highlighted <- input$mytable_rows_selected
    # 
    #  # if(length(highlighted) == 3) browser()
    # 
    #   if(length(highlighted)){
    #    # highlightedRows <- 1:nrow(gene_information) %in% highlighted
    #     #return(gene_information[highlightedRows,]$Gene)
    #     return(gene_information[highlighted,]$Gene)
    #   }
    #   else {
    #     return(NULL)
    #   }
    # })
    # 

    output$mytable <- DT::renderDataTable({
      
      table_data <- gene_information %>%
        mutate(`Uniprot id` = paste0("<a href =", "\"https://www.uniprot.org/uniprot/", `Uniprot id`, "\">",`Uniprot id`, "</a>"))
      
      datatable(table_data, escape = FALSE, 
                  filter = list(
                      position = "top", 
                      plain = TRUE,
                      clear = FALSE
                  ),
                  options = list(
                      dom = 'fltip', 
                      lengthMenu = c(5, 10, 20, 50), 
                      pageLength = 8,
                      #autoWidth = TRUE,
                      columnDefs = list(list(
                        targets = c(3,4),
                        render = JS(
                          "function(data, type, row, meta) {",
                          "return type === 'display' && data.length > 15 ?",
                          "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
                          "}")
                      ),
                      list(
                        targets = 8,
                        width = "800px",
                        render = JS(
                          "function(data, type, row, meta) {",
                          "return type === 'display' && data.length > 30 ?",
                          "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                          "}")
                      ),
                      list(
                        targets = c(3,4),
                        width = "300px"
                      ),
                      list(
                        targets = 5,
                        width = "50px"
                      )
                      )
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

    
    customDownloadHandler <- function(gene){

      downloadHandler(
        filename = function() {
          paste0(gene, ".png")
        },
        content = function(file) {
          p <- custom_bar_plot(Wojdyla_data_tidy, gene)
          ggsave(file, p, device = "png")
        }
      )
    }
    
    
    output$downloadPlot1 <- customDownloadHandler(highlightedGene1())
    output$downloadPlot2 <- customDownloadHandler(highlightedGene2())
    output$downloadPlot3 <- customDownloadHandler(highlightedGene3())
    output$downloadPlot4 <- customDownloadHandler(highlightedGene4())

    
    output$download_button_plot1 <- renderUI({

      if(length(highlightedGene1())){

        downloadButton('downloadPlot1', 'Download')
      }
    })
    
    output$download_button_plot2 <- renderUI({
      
      if(length(highlightedGene2())){
      
        downloadButton('downloadPlot2', 'Download')
      }
    })
    
    output$download_button_plot3 <- renderUI({
      
      if(length(highlightedGene3())){
      
        downloadButton('downloadPlot3', 'Download')
      }
    })
    
    output$download_button_plot4 <- renderUI({
      
      if(length(highlightedGene4())){
      
        downloadButton('downloadPlot4', 'Download')
      }
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

