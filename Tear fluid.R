library(readr)
library(dplyr)


# alle data toevoegen
d37345_all_counts <- read_delim("~/Downloads/tear_split_counts/d37345_all_htseq_dedup_counts.txt", 
                                            delim = "\t", escape_double = FALSE, 
                                            col_names = c("gene_id", "count"), 
                                            trim_ws = TRUE)
d37345_all_counts$code <- "d37345_all"
d37345_all_counts <- d37345_all_counts %>%
  filter(grepl("^ENSG", gene_id))
View(d37345_all_counts)

d37345_long_counts <- read_delim("~/Downloads/tear_split_counts/d37345_long_htseq_dedup_counts.txt", 
                                            delim = "\t", escape_double = FALSE, 
                                            col_names = c("gene_id", "count"), 
                                            trim_ws = TRUE)
d37345_long_counts$code <- "d37345_long"
d37345_long_counts <- d37345_long_counts %>%
  filter(grepl("^ENSG", gene_id))

d37345_short_counts <- read_delim("~/Downloads/tear_split_counts/d37345_short_htseq_dedup_counts.txt", 
                                            delim = "\t", escape_double = FALSE, 
                                            col_names = c("gene_id", "count"), 
                                            trim_ws = TRUE)
d37345_short_counts$code <- "d37345_short"
d37345_short_counts <- d37345_short_counts %>%
  filter(grepl("^ENSG", gene_id))

d37348_long_counts <- read_delim("~/Downloads/tear_split_counts/d37348_long_htseq_dedup_counts.txt", 
                                  delim = "\t", escape_double = FALSE, 
                                  col_names = c("gene_id", "count"), 
                                  trim_ws = TRUE)
d37348_long_counts$code <- "d37348_long"
d37348_long_counts <- d37348_long_counts %>%
  filter(grepl("^ENSG", gene_id))

d37348_short_counts <- read_delim("~/Downloads/tear_split_counts/d37348_short_htseq_dedup_counts.txt", 
                                 delim = "\t", escape_double = FALSE, 
                                 col_names = c("gene_id", "count"), 
                                 trim_ws = TRUE)
d37348_short_counts$code <- "d37348_short"
d37348_short_counts <- d37348_short_counts %>%
  filter(grepl("^ENSG", gene_id))

d37349_long_counts <- read_delim("~/Downloads/tear_split_counts/d37349_long_htseq_dedup_counts.txt", 
                                 delim = "\t", escape_double = FALSE, 
                                 col_names = c("gene_id", "count"), 
                                 trim_ws = TRUE)
d37349_long_counts$code <- "d37349_long"
d37349_long_counts <- d37349_long_counts %>%
  filter(grepl("^ENSG", gene_id))

d37349_short_counts <- read_delim("~/Downloads/tear_split_counts/d37349_short_htseq_dedup_counts.txt", 
                                  delim = "\t", escape_double = FALSE, 
                                  col_names = c("gene_id", "count"), 
                                  trim_ws = TRUE)
d37349_short_counts$code <- "d37349_short"
d37349_short_counts <- d37349_short_counts %>%
  filter(grepl("^ENSG", gene_id))

d37350_long_counts <- read_delim("~/Downloads/tear_split_counts/d37350_long_htseq_dedup_counts.txt", 
                                 delim = "\t", escape_double = FALSE, 
                                 col_names = c("gene_id", "count"), 
                                 trim_ws = TRUE)
d37350_long_counts$code <- "d37350_long"
d37350_long_counts <- d37350_long_counts %>%
  filter(grepl("^ENSG", gene_id))

d37350_short_counts <- read_delim("~/Downloads/tear_split_counts/d37350_short_htseq_dedup_counts.txt", 
                                 delim = "\t", escape_double = FALSE, 
                                 col_names = c("gene_id", "count"), 
                                 trim_ws = TRUE)
d37350_short_counts$code <- "d37350_short"
d37350_short_counts <- d37350_short_counts %>%
  filter(grepl("^ENSG", gene_id))

d37354_long_counts <- read_delim("~/Downloads/tear_split_counts/d37354_long_htseq_dedup_counts.txt", 
                                 delim = "\t", escape_double = FALSE, 
                                 col_names = c("gene_id", "count"), 
                                 trim_ws = TRUE)
d37354_long_counts$code <- "d37354_long"
d37354_long_counts <- d37354_long_counts %>%
  filter(grepl("^ENSG", gene_id))

d37354_short_counts <- read_delim("~/Downloads/tear_split_counts/d37354_short_htseq_dedup_counts.txt", 
                                  delim = "\t", escape_double = FALSE, 
                                  col_names = c("gene_id", "count"), 
                                  trim_ws = TRUE)
d37354_short_counts$code <- "d37354_short"
d37354_short_counts <- d37354_short_counts %>%
  filter(grepl("^ENSG", gene_id))

d37355_long_counts <- read_delim("~/Downloads/tear_split_counts/d37355_long_htseq_dedup_counts.txt", 
                                 delim = "\t", escape_double = FALSE, 
                                 col_names = c("gene_id", "count"), 
                                 trim_ws = TRUE)
d37355_long_counts$code <- "d37355_long"
d37355_long_counts <- d37355_long_counts %>%
  filter(grepl("^ENSG", gene_id))

d37355_short_counts <- read_delim("~/Downloads/tear_split_counts/d37355_short_htseq_dedup_counts.txt", 
                                  delim = "\t", escape_double = FALSE, 
                                  col_names = c("gene_id", "count"), 
                                  trim_ws = TRUE)
d37355_short_counts$code <- "d37355_short"
d37355_short_counts <- d37355_short_counts %>%
  filter(grepl("^ENSG", gene_id))

d37356_long_counts <- read_delim("~/Downloads/tear_split_counts/d37356_long_htseq_dedup_counts.txt", 
                                 delim = "\t", escape_double = FALSE, 
                                 col_names = c("gene_id", "count"), 
                                 trim_ws = TRUE)
d37356_long_counts$code <- "d37356_long"
d37356_long_counts <- d37356_long_counts %>%
  filter(grepl("^ENSG", gene_id))

d37356_short_counts <- read_delim("~/Downloads/tear_split_counts/d37356_short_htseq_dedup_counts.txt", 
                                  delim = "\t", escape_double = FALSE, 
                                  col_names = c("gene_id", "count"), 
                                  trim_ws = TRUE)
d37356_short_counts$code <- "d37356_short"
d37356_short_counts <- d37356_short_counts %>%
  filter(grepl("^ENSG", gene_id))
View(d37356_short_counts)



# data samenvoegen (15 samples in 1 dataset)
combined <- bind_rows(d37345_all_counts,d37345_long_counts, d37345_short_counts,d37348_long_counts, d37348_short_counts, d37349_long_counts, d37349_short_counts, d37350_long_counts, d37350_short_counts, d37354_long_counts, d37354_short_counts, d37355_long_counts, d37355_short_counts, d37356_long_counts, d37356_short_counts)
View(combined)



# 0'en verwijderen
combined_filtered <- subset(combined, count !=0)
View(combined_filtered)



#cumulatieve plot maken
library(ggplot2)
ggplot(combined_filtered, aes(x = count, color = code))+
  stat_ecdf()+
  labs(x='Counts (log)', y = 'Cumulative fraction', color = "Sample") +
  scale_x_log10()+
  theme_minimal()



#range berekenen cumulatieve plot
library(ggplot2)
library(dplyr)

df_filtered <- combined_filtered %>% filter(code == "d37345_long")
ecdf_func <- ecdf(df_filtered$count)
count_50 <- quantile(df_filtered$count, probs = 0.5)
print(count_50)

df_filtered <- combined_filtered %>% filter(code == "d37345_all")
ecdf_func <- ecdf(df_filtered$count)
count_50 <- quantile(df_filtered$count, probs = 0.5)
print(count_50)



#20 meest voorkomende genen per code
d37345_all_counts_sorted <- d37345_all_counts[order(d37345_all_counts$count,decreasing = TRUE),]
d37345_all_counts_20 <- head(d37345_all_counts_sorted, 20)
View(d37345_all_counts_20 )

d37345_long_counts_sorted <- d37345_long_counts[order(d37345_long_counts$count,decreasing = TRUE),]
d37345_long_counts_20 <- head(d37345_long_counts_sorted, 20)
View(d37345_long_counts_20 )

d37345_short_counts_sorted <- d37345_short_counts[order(d37345_short_counts$count,decreasing = TRUE),]
d37345_short_counts_20 <- head(d37345_short_counts_sorted, 20)

d37348_long_counts_sorted <- d37348_long_counts[order(d37348_long_counts$count,decreasing = TRUE),]
d37348_long_counts_20 <- head(d37348_long_counts_sorted, 20)

d37348_short_counts_sorted <- d37348_short_counts[order(d37348_short_counts$count,decreasing = TRUE),]
d37348_short_counts_20 <- head(d37348_short_counts_sorted, 20)

d37349_long_counts_sorted <- d37349_long_counts[order(d37349_long_counts$count,decreasing = TRUE),]
d37349_long_counts_20 <- head(d37349_long_counts_sorted, 20)

d37349_short_counts_sorted <- d37349_short_counts[order(d37349_short_counts$count,decreasing = TRUE),]
d37349_short_counts_20 <- head(d37349_short_counts_sorted, 20)

d37350_long_counts_sorted <- d37350_long_counts[order(d37350_long_counts$count,decreasing = TRUE),]
d37350_long_counts_20 <- head(d37350_long_counts_sorted, 20)

d37350_short_counts_sorted <- d37350_short_counts[order(d37350_short_counts$count,decreasing = TRUE),]
d37350_short_counts_20 <- head(d37350_short_counts_sorted, 20)

d37354_long_counts_sorted <- d37354_long_counts[order(d37354_long_counts$count,decreasing = TRUE),]
d37354_long_counts_20 <- head(d37354_long_counts_sorted, 20)

d37354_short_counts_sorted <- d37354_short_counts[order(d37354_short_counts$count,decreasing = TRUE),]
d37354_short_counts_20 <- head(d37354_short_counts_sorted, 20)

d37355_long_counts_sorted <- d37355_long_counts[order(d37355_long_counts$count,decreasing = TRUE),]
d37355_long_counts_20 <- head(d37355_long_counts_sorted, 20)

d37355_short_counts_sorted <- d37355_short_counts[order(d37355_short_counts$count,decreasing = TRUE),]
d37355_short_counts_20 <- head(d37355_short_counts_sorted, 20)

d37356_long_counts_sorted <- d37356_long_counts[order(d37356_long_counts$count,decreasing = TRUE),]
d37356_long_counts_20 <- head(d37356_long_counts_sorted, 20)

d37356_short_counts_sorted <- d37356_short_counts[order(d37356_short_counts$count,decreasing = TRUE),]
d37356_short_counts_20 <- head(d37356_short_counts_sorted, 20)




#20 meeste samenvoegen van alle 15 codes/ samples
combined_20 <- bind_rows(d37345_all_counts_20, d37345_long_counts_20, d37345_short_counts_20, d37348_long_counts_20, d37348_short_counts_20, d37349_long_counts_20, d37349_short_counts_20, d37350_long_counts_20, d37350_short_counts_20, d37354_long_counts_20, d37354_short_counts_20, d37355_long_counts_20, d37355_short_counts_20, d37356_long_counts_20, d37356_short_counts_20 )
View(combined_20)



#samenvoegen aan hun gene_name en biotype --> uit annotations
library(readr)
annotation <- read_delim("~/Downloads/annotation.tsv", 
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE)

combined_20 <- mutate(combined_20, gene_name = annotation$gene_name[match(combined_20$gene_id, annotation$gene_id)])
combined_20 <- mutate(combined_20, biotype = annotation$biotype[match(combined_20$gene_id, annotation$gene_id)])
View(combined_20 )



#enkel 'long' selecteren
combined_20_long <- combined_20 %>%
  filter(grepl("_long$", code))
View(combined_20_long)

#grafiek 20 meeeste - long
ggplot(combined_20_long, aes(x = ifelse(is.na(gene_name), gene_id, gene_name), y = count, fill = code))+
  geom_bar(stat = 'identity', position = 'stack')+
  labs(x='Gene', y = 'Counts')+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = guide_legend(title = "Sample"))

  


#enkel 'short' selecteren
combined_20_short <- combined_20 %>%
  filter(grepl("_short$", code)) 
View(combined_20_short)

#grafiek 20 meeeste - short
ggplot(combined_20_short, aes(x = ifelse(is.na(gene_name), gene_id, gene_name), y = count, fill = code))+
  geom_bar(stat = 'identity', position = 'stack')+
  labs(x='Gene', y = 'Counts')+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = guide_legend(title = "Sample"))



#vergelijking gehele long en short
ggplot(combined_20, aes(x = ifelse(is.na(gene_name), gene_id, gene_name), y = count, fill = code))+
  geom_bar(stat = 'identity', position = 'stack')+
  labs(x='Gene', y = 'Counts')+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = guide_legend(title = "Sample"))+
  mutate(code = factor(code, levels = c(
    "d37345_short", "d37348_short", "d37349_short", "d37350_short",
    "d37354_short", "d37355_short", "d37356_short",
    "d37345_long", "d37348_long", "d37349_long", "d37350_long",
    "d37354_long", "d37355_long", "d37356_long"
  )))

# Eerst de data muteren om de 'code' kolom in de juiste volgorde te zetten
combined_20 <- combined_20 %>%
  mutate(code = factor(code, levels = c(
    "d37345_short", "d37348_short", "d37349_short", "d37350_short",
    "d37354_short", "d37355_short", "d37356_short",
    "d37345_long", "d37348_long", "d37349_long", "d37350_long",
    "d37354_long", "d37355_long", "d37356_long"
  )))

# Nu de ggplot maken
ggplot(combined_20, aes(x = ifelse(is.na(gene_name), gene_id, gene_name), y = count, fill = code)) +
  geom_bar(stat = 'identity', position = 'stack') +
  labs(x = 'Gene', y = 'Counts') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = guide_legend(title = "Sample"))

View(combined_20)
# andere kleuren geven -> zodat er een duidelijk onderscheidt is tussen short en long
ggplot(combined_20, aes(x = ifelse(is.na(gene_name), gene_id, gene_name), y = count, fill = code)) +
  geom_bar(stat = 'identity', position = 'stack') +
  labs(x = 'Gene', y = 'Counts') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = guide_legend(title = "Sample")) +
  scale_fill_manual(values = c(
    "d37345_short" = "#D8A7D8",  
    "d37348_short" = "#D3A7D3",  
    "d37349_short" = "#E1B7E1", 
    "d37350_short" = "#D9A7D9", 
    "d37354_short" = "#E3AEE3",  
    "d37355_short" = "#D6A7D6", 
    "d37356_short" = "#E5B7E5",  
    "d37345_long" = "#A2C2E0",  
    "d37348_long" = "#A3C9E2",   
    "d37349_long" = "#B2D8E4",  
    "d37350_long" = "#B1D1E6",  
    "d37354_long" = "#A7C9E9",   
    "d37355_long" = "#A0C7E3",   
    "d37356_long" = "#A5C8E7", 
    "d37345_all" = "gray"       
  )) +
  aes(fill = factor(code, levels = c(
    "d37345_short", "d37348_short", "d37349_short", "d37350_short",
    "d37354_short", "d37355_short", "d37356_short", 
    "d37345_long", "d37348_long", "d37349_long", "d37350_long", 
    "d37354_long", "d37355_long", "d37356_long",     
    "d37345_all"   
  )))



#vergelijking enkel 20 grootste counts bij long en 20 hoogste counts bij short
combined_20_long_sorted <- combined_20_long[order(combined_20_long$count, decreasing = TRUE), ]
top_20_long <- head(combined_20_long_sorted, 20)
View(top_20_long)

combined_20_short_sorted <- combined_20_short[order(combined_20_short$count, decreasing = TRUE), ]
top_20_short <- head(combined_20_short_sorted, 20)
View(top_20_short)

combined_top_20 <- rbind(top_20_short, top_20_long)
View(combined_top_20)

ggplot(combined_top_20, aes(x = ifelse(is.na(gene_name), gene_id, gene_name), y = count, fill = code))+
  geom_bar(stat = 'identity', position = 'stack')+
  labs(x='Gene', y = 'Counts')+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = guide_legend(title = "Sample"))



#15 samples linken met hun biotype-verdeling
combined_filtered <- mutate(combined_filtered, gene_name = annotation$gene_name[match(combined_filtered$gene_id, annotation$gene_id)])
combined_filtered <- mutate(combined_filtered, biotype = annotation$biotype[match(combined_filtered$gene_id, annotation$gene_id)])
View(combined_filtered)

combined_filtered_biotype <- summarise(group_by(combined_filtered, code, biotype),Biotype_count = n(),.groups = "drop")
combined_filtered_biotype <- mutate(group_by(combined_filtered_biotype, code), Biotype_percent = Biotype_count / sum(Biotype_count) * 100)  
combined_filtered_biotype <- mutate(combined_filtered_biotype, biotype = ifelse(Biotype_percent < 1, "less than 1%", biotype))
combined_filtered_biotype_5 <- filter(combined_filtered_biotype, Biotype_count >5)
View (combined_filtered_biotype_5)

ggplot(combined_filtered_biotype_5, aes(x = code, y = Biotype_percent, fill = biotype))+
  geom_bar(stat = 'identity', position = 'stack', width = 0.5)+
  labs(x = NULL, y = "Fraction of reads", fill = "Biotype")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#database waar alle ook kleine waarden dan 1% 'bijmogen'
combined_filtered_biotype_1 <- summarise(group_by(combined_filtered, code, biotype, gene_name),Biotype_count = n(),.groups = "drop")
combined_filtered_biotype_1 <- mutate(group_by(combined_filtered_biotype_1, code), Biotype_percent = Biotype_count / sum(Biotype_count) * 100) 
combined_filtered_biotype_1 <- filter(combined_filtered_biotype_1, Biotype_count >5)
View(combined_filtered_biotype_1)


combined_filtered_biotype_1 <- combined_filtered %>%
  mutate(code_type = case_when(
    grepl("long$", code) ~ "long",
    grepl("short$", code) ~ "short",
    grepl("all$", code) ~ "all",
    TRUE ~ "other"
  ))

biotype_avg_genes <- combined_filtered_biotype_1 %>%
  group_by(biotype, code_type) %>%
  summarise(avg_genes = n_distinct(gene_name), .groups = "drop")
View(biotype_avg_genes)

ggplot(biotype_avg_genes, aes(x = biotype, y = avg_genes)) + 
  geom_bar(stat = "identity", fill = "grey", color = "black", position = "dodge") +
  labs(x = "Biotype", y = "Average number of genes", fill = NULL) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#biotype verdeling, nu op counts (voor presentatie)
combined_filtered_biotype_pres <- summarise(group_by(combined_filtered, code, biotype),Biotype_count = sum(count),.groups = "drop")
combined_filtered_biotype_pres <- mutate(group_by(combined_filtered_biotype_pres, code), Biotype_percent = Biotype_count / sum(Biotype_count) * 100)  
combined_filtered_biotype_pres <- mutate(combined_filtered_biotype_pres, biotype = ifelse(Biotype_percent < 1, "less than 1%", biotype))
combined_filtered_biotype_pres5 <- filter(combined_filtered_biotype_pres, Biotype_count >5)
View (combined_filtered_biotype_pres5)

ggplot(combined_filtered_biotype_pres5, aes(x = factor(code, levels = c("d37345_short", "d37348_short", "d37349_short", 
                                                                        "d37350_short", "d37354_short", "d37355_short", 
                                                                        "d37356_short", "d37345_long", "d37348_long", 
                                                                        "d37349_long", "d37350_long", "d37354_long", 
                                                                        "d37355_long", "d37356_long", "d37345_all")), 
                                            y = Biotype_percent, fill = biotype)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.5) +
  labs(x = NULL, y = "Fraction of reads", fill = "Biotype") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


