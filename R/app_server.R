#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here
    library(ggplot2)
    library(magrittr)
    library(dplyr)
    library(DT)

  #callModule(sliderText, "one")
 # callModule(sliderText, "two")
  
  #callModule(custom_barplot, "protein_abundance_plot1.1")
 # callModule(custom_barplot, "protein_abundance_plot2.1")
  
  significance <- function(q_value){
    case_when(
      q_value > 0.05 ~ "NS",
      q_value > 0.01 ~ "*",
      q_value > 0.001 ~ "**",
      q_value > 0.0001 ~ "***",
      q_value <= 0.0001 ~ "****",
      
    )
  }
  
  get_q_value <- function(selected_gene){
    gene_information %>%
      dplyr::filter(Gene == selected_gene) %>%
      dplyr::pull(`q-value`)
  }
  
  custom_bar_plot <- function(dataset, selected_gene){
        
        filtered <- dplyr::filter(dataset, Gene == selected_gene)
        
        error_bar_max <- dplyr::mutate(filtered, error_max = mean+sd) %>%
            dplyr::pull(error_max)
        
        q_value <- get_q_value(selected_gene)
        #q_text <- paste0("q = ", q_value)
        q_text <- significance(q_value)
        
        q_size <- if_else(q_text == "NS", 6, 10)
        
        segment_position <- max(error_bar_max)*1.1
        
        y_max <- max(error_bar_max)*1.3
        
        ggplot(filtered, aes(x = type, y = mean, fill = type)) +
            geom_bar(stat="identity", color = "black", lwd = 1.2) +
            geom_errorbar(aes(ymin = mean, ymax = error_bar_max), lwd = 1.2, width = 0.3) +
            ggtitle(selected_gene) +
            theme_classic() +
            theme(plot.title = element_text(size=20, face="bold"), 
                  legend.position = "none", 
                  axis.title.y = element_text(size=14, face="bold"), 
                  axis.text.x = element_text(size=10, face="bold")
                 ) +
            scale_fill_manual(values=c('#3e5b7c','#bd1e15')) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, y_max), labels = scales::scientific) +
            xlab("") +
            ylab("Protein abundance") +
            geom_segment(aes(x = 1.2, y = segment_position, xend = 1.8, yend = segment_position), size = 1.5) +
            geom_text(x = 1.5, y = segment_position*1.1, label = q_text, size = q_size)
            
  }
    
  # for debugging
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
  
  
  output$protein_abundance_plot5 <- renderCachedPlot({
    
    gene <- highlightedGene5()
    
    if(is.null(gene)) return (NULL)
    if(!length(gene)) return (NULL)
    if(is.na(gene))   return(NULL)
    
    custom_bar_plot(Wojdyla_data_tidy, gene)
    
  }, cacheKeyExpr = {list(input$mytable_rows_selected[5])}, bg = NA)
  
  
  output$protein_abundance_plot6 <- renderCachedPlot({
    
    gene <- highlightedGene6()
    
    if(is.null(gene)) return (NULL)
    if(!length(gene)) return (NULL)
    if(is.na(gene))   return(NULL)
    
    custom_bar_plot(Wojdyla_data_tidy, gene)
    
  }, cacheKeyExpr = {list(input$mytable_rows_selected[6])}, bg = NA)
  

  
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
   
   highlightedGene5 <- reactive({
     
     row_no <- input$mytable_rows_selected[5]
     
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
   
   highlightedGene6 <- reactive({
     
     row_no <- input$mytable_rows_selected[6]
     
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
  output$downloadPlot5 <- customDownloadHandler(highlightedGene5())
  output$downloadPlot6 <- customDownloadHandler(highlightedGene6())

  
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
  
  output$download_button_plot5 <- renderUI({
    
    if(length(highlightedGene5())){
      
      downloadButton('downloadPlot5', 'Download')
    }
  })
  
  output$download_button_plot6 <- renderUI({
    
    if(length(highlightedGene6())){
      
      downloadButton('downloadPlot6', 'Download')
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

