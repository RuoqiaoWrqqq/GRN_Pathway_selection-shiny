setwd("/Users/rwang4/Library/Mobile Documents/com~apple~CloudDocs/BigU/Box/Ruoqiao Thakar Lab/networkD3-shiny-example/")


library(ggVennDiagram)
DE_genes <- read.csv("./inst/extdata/DE genes disease vs. HC across different subclusters from random forest feature selection---Sup Figure 3 E-F.csv", stringsAsFactors = FALSE) %>%
  rename(V1=Gene)  %>% select(V1)  %>% distinct(V1, .keep_all = TRUE)
Selected_Gene_List <- read.table("./inst/extdata/Selected_Gene_List_2025-10-31.txt",header = F, sep = "\t", stringsAsFactors = FALSE)
AS_Gene_List <- read.table("./inst/extdata/atherosclerosis_genes.txt",header = F, sep = "\t", stringsAsFactors = FALSE)



selected_genes <- list(AS=AS_Gene_List$V1,
                       Reactome=Selected_Gene_List$V1,
                       DE_gene=DE_genes$V1)

ggVennDiagram(selected_genes, 
              category.names = names(selected_genes),
              set_color = c("#A6CEE3","#ffa725","#ff6b6d"),
              set_size = 5,
              label = "count", 
              label_color = "black", 
              label_alpha = 0,
              label_size = 5, 
              label_percent_digit = 1,
              edge_size = 1) +
  scale_fill_gradient(low = "white", high = "white")




main_bar_col <- c("violetred4")
sets_bar_col <- c("turquoise4")
matrix_col <- c("black")
shade_col <- c("grey")

Venn_data <- fromList(selected_genes)
upset(Venn_data,
      sets = names(selected_genes),
      keep.order = T,
      order.by = 'freq',
      point.size = 3,
      line.size = 1,
      number.angles = 0,
      text.scale = 2,
      nintersects = NA,
      main.bar.color = "black",
      matrix.color = matrix_col,
      shade.color = shade_col,
      sets.x.label= "")


genes <- unique( unlist(selected_genes))
write.table(
  genes,
  file = "./inst/extdata/Merged genes from DE genes_reactome_harmonizome.txt",
  quote = FALSE,
  row.names = FALSE,
  col.names = FALSE
)
