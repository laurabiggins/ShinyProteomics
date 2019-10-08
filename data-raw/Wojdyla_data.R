## code to prepare `Wojdyla_data` dataset goes here
library("tidyverse")

raw_data <- read_tsv("../Wojdyla_data.txt")

# count(raw_data, `Uniprot id`) %>%
#     arrange(desc(n))
# 
# count(raw_data, Gene) %>%
#     arrange(desc(n))
# 
# sum(duplicated(raw_data$`Uniprot id`))
# sum(duplicated(raw_data$Gene))
# uniprot IDs and primary gene name are all unique


gene_information <- raw_data %>%
# join the GO terms to make searching easier
    unite(`Biological process`, `Cellular component`, `Molecular function`, col = "GO terms", sep = ";") %>%
    select(-starts_with("NAIVE")) %>%
    select(-starts_with("PRIMED"))

gene_information <- rename(gene_information, "log2 fold change" = "log2_fc naive primed")
gene_information$`Pep Count` <- as.integer(gene_information$`Pep Count`)

gene_information <- gene_information %>%
    mutate(`Uniprot id` = paste0("<a href =", "\"https://www.uniprot.org/uniprot/", 
                                 `Uniprot id`, "\">",`Uniprot id`, "</a>"))


Wojdyla_data_tidy  <- raw_data %>%
    gather(`NAIVE_R1`,`NAIVE_R2`,`NAIVE_R3`,`PRIMED_A1`,`PRIMED_A2`,`PRIMED_E1`,`PRIMED_E2`, key = "sample", value = "expression") %>%
    separate(sample, into = c("type", "rep")) %>%
    group_by(Gene, type) %>%
    summarise(mean = mean(expression), sd = sd(expression)) %>%
    ungroup()

# I tried to extract all the DT table code from app_server but the row highlighting didn't work so 
# I've just put some options here, I don't think it improves the speed but it tidies up the server code a bit.
dt_options <- list(
  dom = 'fltip',
  lengthMenu = c(5, 10, 20, 50),
  pageLength = 10,
  columnDefs = list(
   list(
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
)


usethis::use_data(gene_information, overwrite = TRUE)
usethis::use_data(Wojdyla_data_tidy, overwrite = TRUE)
usethis::use_data(dt_options, overwrite = TRUE)
#usethis::use_data(my_datatable3, compress = "xz", overwrite = TRUE)
#saveRDS(my_datatable3, file = "data/my_datatable3.Rds")
#save(my_datatable3, file = "data/my_datatable3.Rdata")
