#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here
    library(ggplot2)
    library(magrittr)
    library(dplyr)
    library(DT)

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
  
  
  output$protein_abundance_plot1 <- renderPlot({

    req(rv$gene1) 
    custom_bar_plot(Wojdyla_data_tidy, rv$gene1)
  })

  output$protein_abundance_plot2 <- renderPlot({
    
    req(rv$gene2)  
    custom_bar_plot(Wojdyla_data_tidy, rv$gene2)
  })
  
  output$protein_abundance_plot3 <- renderCachedPlot({
      
      req(rv$gene3) 
      custom_bar_plot(Wojdyla_data_tidy, rv$gene3)
      
  }, cacheKeyExpr = {list(rv$gene3)}, bg = NA)
  
  output$protein_abundance_plot4 <- renderPlot({
      
      req(rv$gene4)  
      custom_bar_plot(Wojdyla_data_tidy, rv$gene4)
  })
  
  output$protein_abundance_plot5 <- renderPlot({
      
      req(rv$gene5) 
      custom_bar_plot(Wojdyla_data_tidy, rv$gene5)
  })
  
  output$protein_abundance_plot6 <- renderPlot({
      
      req(rv$gene6)  
      custom_bar_plot(Wojdyla_data_tidy, rv$gene6)
  })
  

  rv <- reactiveValues(
      gene1 = NA,
      gene2 = NA,
      gene3 = NA,
      gene4 = NA,
      gene5 = NA,
      gene6 = NA
  )
  
  observeEvent(input$mytable_rows_selected, {
      # rv$gene1 <- getGene(input$mytable_rows_selected[1])
      # rv$gene2 <- getGene(input$mytable_rows_selected[2])
      # rv$gene3 <- getGene(input$mytable_rows_selected[3])
      # rv$gene4 <- getGene(input$mytable_rows_selected[4])
      # rv$gene5 <- getGene(input$mytable_rows_selected[5])
      # rv$gene6 <- getGene(input$mytable_rows_selected[6])
      for(i in 1:6){
        rv_name <- paste0("gene", i)
        rv[[rv_name]] <- getGene(input$mytable_rows_selected[i])
      }    
  })
  
  getGene <- function(row_no){
     gene_information[row_no,]$Gene
  }

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
    rv$gene1 <- NA
    rv$gene2 <- NA
    rv$gene3 <- NA
    rv$gene4 <- NA
    rv$gene5 <- NA
    rv$gene6 <- NA
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
  
  output$downloadPlot1 <- customDownloadHandler(rv$gene1)
  output$downloadPlot2 <- customDownloadHandler(rv$gene2)
  output$downloadPlot3 <- customDownloadHandler(rv$gene3)
  output$downloadPlot4 <- customDownloadHandler(rv$gene4)
  output$downloadPlot5 <- customDownloadHandler(rv$gene5)
  output$downloadPlot6 <- customDownloadHandler(rv$gene6)

  
  output$download_button_plot1 <- renderUI({
     
      req(rv$gene1)

      downloadButton('downloadPlot1', 'Download')
  })
  
  output$download_button_plot2 <- renderUI({
    
      req(rv$gene2)
    
      downloadButton('downloadPlot2', 'Download')
  })
  
  output$download_button_plot3 <- renderUI({

    req(rv$gene3)
    downloadButton('downloadPlot3', 'Download')
  })

  output$download_button_plot4 <- renderUI({

    req(rv$gene4)
    downloadButton('downloadPlot4', 'Download')
  })

  output$download_button_plot5 <- renderUI({

    req(rv$gene5)
    downloadButton('downloadPlot5', 'Download')
  })

  output$download_button_plot6 <- renderUI({

    req(rv$gene6)
    downloadButton('downloadPlot6', 'Download')
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

