library("shiny")

custom_barplotUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("my_barplot"), height = "240px"),
      flowLayout(
        uiOutput(ns("download_button_plot1")),
        uiOutput(ns("download_button_plot2"))
      )  
  )
}

# I had issues getting the download button to work and appear dynamically. It was
# due to the ns being assigned in the UI
# this line was needed in the server section 
# downloadButton(session$ns("dP"), "Download")

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


the_barplot <- function(dataset, the_gene){

  filtered <- dplyr::filter(dataset, Gene == the_gene)

  error_bar_max <- dplyr::mutate(filtered, error_max = mean+sd) %>%
    dplyr::pull(error_max)

  q_value <- get_q_value(the_gene)
  #q_text <- paste0("q = ", q_value)
  q_text <- significance(q_value)

  q_size <- if_else(q_text == "NS", 6, 10)

  segment_position <- max(error_bar_max)*1.1

  y_max <- max(error_bar_max)*1.3

  ggplot(filtered, aes(x = type, y = mean, fill = type)) +
    geom_bar(stat="identity", color = "black", lwd = 1.2) +
    geom_errorbar(aes(ymin = mean, ymax = error_bar_max), lwd = 1.2, width = 0.3) +
    ggtitle(the_gene) +
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


custom_barplot <- function(input, output, session, dataset, gene_information, selected_gene){ 
  
  # output$my_barplot <- renderPlot({
  #   req(selected_gene())
  #   the_barplot(dataset, selected_gene())
  # })
  
  output$my_barplot <- renderCachedPlot({
    
    req(selected_gene())
    the_barplot(dataset, selected_gene())
  }, cacheKeyExpr = {list(selected_gene())}, bg = NA)
  
  
  customDownloadHandler_pdf <- function(gene){
  
    downloadHandler(
      filename = function() {
        paste0(gene, ".pdf")
      },
      content = function(file) {
        p <- the_barplot(dataset, gene)
        ggsave(file, p, device = "pdf")
      }
    )
  }
  
  customDownloadHandler_png <- function(gene){
    
    downloadHandler(
      filename = function() {
        paste0(gene, ".png")
      },
      content = function(file) {
        p <- the_barplot(dataset, gene)
        ggsave(file, p, device = "png")
      }
    )
  }
  
  output$download_png <- customDownloadHandler_png(selected_gene())
 
  output$download_button_plot1 <- renderUI({

    req(selected_gene())
    downloadButton(session$ns("download_png"), "png")
  })
  
  output$download_pdf <- customDownloadHandler_pdf(selected_gene())
  
  output$download_button_plot2 <- renderUI({
    
    req(selected_gene())
    downloadButton(session$ns("download_pdf"), "pdf")
  })
  
  # output$png_pdf_radio <- renderUI({
  #   
  #   req(selected_gene())
  #   radioButtons(session$ns("plot_radio"), 
  #                choices = c("png", "pdf"), 
  #                selected = "png",
  #                label = NULL)
  # })
  # 
  # plot_type <- reactive(input$plot_radio)
    
}






