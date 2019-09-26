## code to prepare `Wojdyla_data` dataset goes here
library("tidyverse")

raw_data <- read_tsv("Wojdyla_data.txt")

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

Wojdyla_data_tidy  <- raw_data %>%
    gather(`NAIVE_R1`,`NAIVE_R2`,`NAIVE_R3`,`PRIMED_A1`,`PRIMED_A2`,`PRIMED_E1`,`PRIMED_E2`, key = "sample", value = "expression") %>%
    separate(sample, into = c("type", "rep")) %>%
    group_by(Gene, type) %>%
    summarise(mean = mean(expression), sd = sd(expression)) %>%
    ungroup()

usethis::use_data(gene_information, overwrite = TRUE)
usethis::use_data(Wojdyla_data_tidy, overwrite = TRUE)
