
library(ggplot2)
library(dplyr)

#####---------- Human Reactome cvs from Website ----------------------------#################
# Load Pathways hierarchy relationship
Complex_2_Pathway_human <- read.delim(
"https://download.reactome.org/94/Complex_2_Pathway_human.txt",
header = TRUE, sep = "\t", stringsAsFactors = FALSE
)
colnames(Complex_2_Pathway_human) <- c("complex_ID","pathway_ID",'top_level_pathway_ID')
head(Complex_2_Pathway_human)

# Load Complete List of Pathways
ReactomePathways <- read.delim(
  "https://download.reactome.org/94/ReactomePathways.txt",
  header = F, sep = "\t", stringsAsFactors = FALSE
)
colnames(ReactomePathways) <- c("pathway_ID","pathway",'species')
ReactomePathways <- ReactomePathways %>% filter(species %in% c("Homo sapiens"))
head(ReactomePathways)

Complex_2_Pathway_human <- Complex_2_Pathway_human %>%
  left_join(
    ReactomePathways %>%
      select(pathway_ID, pathway),
    by = c("pathway_ID" = "pathway_ID")
  ) %>%
  rename(pathway_name= pathway)  %>%
  left_join(
    ReactomePathways %>%
      select(pathway_ID, pathway),
    by = c("top_level_pathway_ID" = "pathway_ID")
  ) %>%
  rename(top_level_pathway_name= pathway)

df <- Complex_2_Pathway_human %>% mutate(complex_name = substr(complex_ID, 3, 5))
df_complex <- read.csv("inst/extdata/Reactome_species_codes.csv", stringsAsFactors = FALSE)
df <- merge(df,df_complex, by = "complex_name")
write.csv(file = "inst/extdata/Reactome Pathways hierarchy relationship.csv",df)

# ---- create edges and nodes for Sankey Network ----
edges <-  df %>%
  group_by(pathway_name, top_level_pathway_name) %>%
  summarise(value = n(), .groups = "drop")  %>%
  rename(from = top_level_pathway_name, to = pathway_name)  %>% as.data.frame()

nodes <- data.frame(label = unique(c(edges$from, edges$to)))
nodes_group <- data.frame(
  label = df$pathway_name,
  group = df$top_level_pathway_name
)

nodes_group <- rbind(
  nodes_group,
  data.frame(
    label = unique(df$top_level_pathway_name),
    group = unique(df$top_level_pathway_name)
  )
)
nodes <- left_join(nodes, nodes_group, by = "label") %>%
  distinct(label, .keep_all = TRUE)
edges$source <- match(edges$from, nodes$label) - 1
edges$target <- match(edges$to, nodes$label) - 1

nodes$id = nodes$label
head(nodes)
head(edges)

# Save nodes and edges for shiny app
save(nodes, file = "inst/extdata/nodes.RData")
save(edges, file = "inst/extdata/edges.RData")


#####----------Reactome gene sets/pathways gmt file for LLM model----------------------------#################
# Reactome Pathways hierarchy relationship data
Complex_2_Pathway_human <- read.csv("./inst/extdata/Reactome Pathways hierarchy relationship.csv", stringsAsFactors = FALSE) %>%
  select(-complex_name, -X) %>%
  distinct(pathway_name, top_level_pathway_name, species, .keep_all = TRUE)

# load the gmt file frome Reactome DB
library(GSEABase)
predBind <- GSEABase::getGmt("./inst/extdata/Reactome_human.gmt")
numTF <- length(unique(names(predBind)))
# FILTER OUT MOTIFS THAT HAVE EITHER TOO MANY OR TOO FEW PREDICTED SITES
predBindCounts <- sort(setNames(unlist(lapply(predBind, function(x){length(geneIds(x))})), names(predBind)))
predBind_maxMin <- list(max = 10000, min = 10) # SPECIFY CUTOFFS
predBindCounts_postThresh <- predBindCounts[(predBindCounts >= predBind_maxMin$min)&
                                              (predBindCounts <= predBind_maxMin$max)]
predBind <- predBind[names(predBind) %in% names(predBindCounts_postThresh)]
print(length(predBind))
merged_df <- data.frame(pathway_name=names(predBind))
merged_df <- merged_df %>% left_join(Complex_2_Pathway_human %>% filter(pathway_name %in% merged_df$pathway_name), by = "pathway_name")
#merged_df <- merged_df %>% left_join(Complex_2_Pathway_human, by = "pathway_name")
merged_df[merged_df$pathway_name %in% unique(Complex_2_Pathway_human$top_level_pathway_name),]$top_level_pathway_name <- merged_df[merged_df$pathway_name %in% unique(Complex_2_Pathway_human$top_level_pathway_name),]$pathway_name

merged_df %>%
  dplyr::select(pathway_name,top_level_pathway_name) %>% head()
merged_df %>%
  count(pathway_name, sort = TRUE) %>%
  filter(n > 1)

merged_df %>% 
  filter(is.na(top_level_pathway_name)) %>%
  dplyr::select(pathway_name)
write.csv(file = "merged_df.csv",merged_df)

# show the frequency of each top_level_pathway_name category in Reactome
ggplot(merged_df, aes(x = reorder(top_level_pathway_name, table(top_level_pathway_name)[top_level_pathway_name]))) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "Top-level pathway name",
    y = "Count"
  )

#####----------Not using yet----------------------------#################
# Create category based on pattern matching
mutate(top_level_pathway_name = case_when(
  grepl("glucagon|glycosylation|oxidation|homeostasis|insulin|fatty", 
        pathway_name, ignore.case = TRUE) ~ "Metabolic",
  grepl("chemokine|immune|Immune|Immunological|cytokine|Antigen|IFN|Interferon|antigen|B cell|T cell|inflammatory|
          NFkappaB|TNF|Interleukin|IL-6|Immunoregulatory|JAK-STAT|NF-kB|TRAF6|TICAM1|IRF7|MAPK|monocyte|macrophage", 
        pathway_name, ignore.case = TRUE) ~ "Immune System",
  grepl("platelet|Fibrin|hemostasis", 
        pathway_name, ignore.case = TRUE) ~ "Hemostasis",
  grepl("platelet|Fibrin|hemostasis", 
        pathway_name, ignore.case = TRUE) ~ "Hemostasis",
  TRUE ~ "other"
))

# Filter for only relevant categories 
top_markers <- top_markers  %>%
  filter(category %in% c("Immune System", "Hemostasis", "Metabolic", "Cell Cell communication")) %>%
  filter(!str_detect(geneset, regex("disorders|RNA|DNA|biosynthesis|Prolactin|prolactin|
                                    Urea|Nucleotide|nucleotides|cGMP|CoV-2|Diseases|
                                    catabolism|acid|heparin|parasite|Aquaporins|Listeria|keratin|Respiratory", ignore_case = TRUE)))
# add synthesis?
#write.csv(file = "./Data availability/Reactome pathways.csv",top_markers)













