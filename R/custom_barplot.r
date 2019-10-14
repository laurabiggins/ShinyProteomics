library("shiny")
library("shinyjs")

custom_barplotUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    #textOutput(ns("gene_name_for_title")),
    plotOutput(ns("my_barplot"), height = "240px"),
    #downloadButton(ns("downloadPlot1"))
    #uiOutput(ns("download_button_plot1")),
    #uiOutput(ns("download_button_plot1"))#,
    downloadLink(ns("downloadPlot1"))
  )
  #shinyjs::useShinyjs()
  #shinyjs::disable(ns("downloadPlot1"))  
}

# I can't get the download link to dynamically appear and work properly.
# Either it's there from the start and works properly once there's a plot, or it
# appears when a gene is selected but then the download doesn't work.
# The code looks like it should work, but the download handler doesn't get used, 
# I don't know whether the module means it doesn't recognise this properly.


customDownloadHandler <- function(dataset, gene){
  
  downloadHandler(

    filename = function() {
      #browser()
      paste0(gene, ".png")
    },
    content = function(file) {
      #p <- custom_barplot(Wojdyla_data_tidy, gene)
      p <- the_barplot(dataset, gene)
      ggsave(file, p, device = "png")
    }
  )
}

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

# custom_barplot_wrapper <- function(input, output, session, dataset, gene_information, selected_gene){ 
# 
#   output$my_barplot <- renderPlot({
# 
#     custom_barplot(dataset, gene_information, selected_gene)
#   })
# }
#   # output$download_button_plot1 <- renderUI({
  #   
  #   if(!is.na(selected_gene)){
  #        downloadButton('downloadPlot1', 'Download')
  #   }
  # })
  
  # if(is.na(selected_gene)){
  #   shinyjs::disable(ns("download_plot"))
  # 
  # } else {
  #   shinyjs::enable(ns("download_plot"))
  #  output$download_plot <- customDownloadHandler(selected_gene)
  #}
#}

getGene <- function(index){ 
  
  gene_information[index,]$Gene
}

# custom_barplot_x <- function(input, output, session, dataset, gene_information, selected_gene){ 
#   
#  # browser()
#   
#   # df <- data.frame(dose=c("D0.5", "D1", "D2"),
#   #                  len=c(4.2, 10, 29.5))
#   #  
#   # output$my_barplot <- renderPlot({
#   #   ggplot2::ggplot(df, aes(x= dose, y=len)) +
#   #     geom_bar(stat="identity")
#   # })
#    
#   gene_selection <- reactive({
#     the_index <- selected_gene()
#     the_index
#   })
#    
#    output$gene_name_for_title <- renderText({
#      
#      getGene(gene_selection())
#      
#    })
# }


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

  
custom_barplot <- function(input, output, session, dataset, gene_information, index){ 
  
  selected_gene <- reactive({
    
    if(is.null(index())) return(NA)
    
    getGene(index())
    
  })

  output$gene_name_for_title <- renderText({
    
    if(is.na(selected_gene())) return (" ")
       
    selected_gene()   
  })
  
  output$download_button_plot1 <- renderUI({

    if(!is.na(selected_gene())){
        downloadLink('downloadPlot1', 'Download it')
    }
  })
    
  output$my_barplot <- renderPlot({
  
    the_gene <- selected_gene()

    if(is.na(the_gene)){
      plot.new()
      return()
    }
    the_barplot(dataset, the_gene)
  })

  
  output$downloadPlot1 <- customDownloadHandler(dataset, selected_gene())
  
  # output$downloadPlot1 <- downloadHandler(
  #   
  #   #browser(),
  #   
  #   filename = function() {
  #     #browser()
  #     paste0(gene, ".png")
  #   },
  #   content = function(file) {
  #     #p <- custom_barplot(Wojdyla_data_tidy, gene)
  #     p <- the_barplot(dataset, gene)
  #     ggsave(file, p, device = "png")
  #   }
  # )
    
    
 # output$download <- customDownloadHandler(dataset, selected_gene())
  
   observeEvent(input$downloadPlot1, {
     
     browser()
  #   customDownloadHandler(dataset, selected_gene())
     
   })
  
  # output$download <- function(x){
  # 
  #   browser()
  #   customDownloadHandler(dataset, selected_gene())
  # }
}



# the ids will be id-gene_name_for_title
# id-not_sure
sliderTextUI <- function(id){
  ns <- NS(id)
  tagList(
    sliderInput(ns("slider"), "Slide me", 0, 100, 1),
    textOutput(ns("number"))
  )
}

#23min callModule
sliderText <- function(input, output, session){
  output$number <- renderText({
    input$slider
  })
}