## code to prepare `Wojdyla_data` dataset goes here
library("tidyverse")

raw_data <- read_tsv("Wojdyla_data.txt")

count(raw_data, `Uniprot accession`) %>%
    arrange(desc(n))

count(raw_data, `Primary gene name`) %>%
    arrange(desc(n)) 


gene_information <- raw_data %>%
    # join the GO terms to make searching easier
    unite(`Biological process`, `Cellular component`, `Molecular function`, col = "GO_terms", sep = ";") %>%
    select(-starts_with("NAIVE")) %>%
    select(-starts_with("PRIMED"))

# uniprot IDs and primary gene name are all unique
    
Wojdyla_data  <- raw_data %>%
    gather(`NAIVE_R1`,`NAIVE_R2`,`NAIVE_R3`,`PRIMED_A1`,`PRIMED_A2`,`PRIMED_E1`,`PRIMED_E2`, key = "sample", value = "expression") %>%
    separate(sample, into = c("type", "rep")) %>%
    group_by(`Primary gene name`, type) %>%
    summarise(mean = mean(expression), sd = sd(expression)) %>%
    gather(stat, value, -(`Primary gene name`:type)) %>%
    unite(type, stat, col = "stats", sep = "_") %>%
    spread(stats, value) %>%
    mutate(log_fc = log2(NAIVE_mean) - log2(PRIMED_mean)) %>%
    left_join(gene_information)

usethis::use_data(Wojdyla_data)
