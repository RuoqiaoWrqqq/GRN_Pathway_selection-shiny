# =======================================================
# global.R — Shared Data & Functions
# =======================================================
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(markdown)
library(networkD3)
library(visNetwork)
library(htmlwidgets)
library(UpSetR)
library(DT)
library(GSEABase)
library(genefilter)

# ---- Color Palette ----
pathway_colors <- c(
  "HIV+AS+ vs HIV-AS-" = "#ff6b6d",
  "HIV+AS- vs HIV-AS-" = "#1F78B4",
  "HIV-AS+ vs HIV-AS-" = "#ffa725",
  "HIV+AS+ vs HIV+AS-" = "pink",
  "CD14" = "#FDBF6F",
  "CD14-active" = "#FF7F00",
  "CD16" = "#B15928",
  "Intermediate" = "#A6CEE3",
  "PMC" = "#CCAFCA"
)

# ---- Load Data ----
load("../extdata/nodes.RData")
load("../extdata/edges.RData")

df_meta_sub_group <- read.csv("../extdata/Figure 1C # of cell per group per sub cluster.csv", stringsAsFactors = FALSE)
df <- read.csv("../extdata/DE gene sets disease vs. HC across different subclusters.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(Gene, Cluster, Group) %>%
  dplyr::rename(pathway_name = Gene)
df$Disease <- sub("^[^ ]+ ", "", df$Group)
Reactome_Pathway_Hierarchy <- read.csv("../extdata/Reactome Pathways hierarchy relationship update.csv", stringsAsFactors = FALSE)
df <- df %>% left_join(Reactome_Pathway_Hierarchy, by = "pathway_name")

# ---- Color for Sankey Diagram ----
color_js <- paste0(
  'd3.scaleOrdinal().domain(["', paste(names(pathway_colors), collapse = '","'),
  '"]).range(["', paste(pathway_colors, collapse = '","'), '"]);'
)

# ---- Prepare UpSetR Data ----
combined_Pathways_df_group <- df %>%
  filter(Group != "HIV+AS+ vs HIV+AS-") %>%
  filter(top_level_pathway_name %in% c("Signal Transduction", "Immune System", "Disease", "Metabolism"))
group_list <- combined_Pathways_df_group %>% { split(.$pathway_name, .$Cluster) }
group_list <- lapply(group_list, unique)
Venn_data <- fromList(group_list)

# ---- Compute intersections ----
compute_intersections <- function(set_list) {
  sets <- names(set_list)
  all_combos <- unlist(lapply(seq_along(sets), function(k)
    combn(sets, k, simplify = FALSE)), recursive = FALSE)
  res <- lapply(all_combos, function(ss) {
    g <- Reduce(intersect, set_list[ss])
    data.frame(
      name = paste(ss, collapse = " & "),
      size = length(g),
      Pathways = I(list(unique(g)))
    )
  })
  res <- do.call(rbind, res)
  res <- res[res$size > 0, ]
  res <- res[order(-res$size), ]
  return(res)
}

inter_df <- compute_intersections(group_list)


# ---- get the genes from gmt file ----
predBind <- GSEABase::getGmt("../extdata/Reactome_human.gmt")
predBindCounts <- sort(setNames(unlist(lapply(predBind, function(x){length(geneIds(x))})), names(predBind)))
predBind_maxMin <- list(max = 10000, min = 10)
predBindCounts_postThresh <- predBindCounts[(predBindCounts >= predBind_maxMin$min) &
                                              (predBindCounts <= predBind_maxMin$max)]
predBind <- predBind[names(predBind) %in% names(predBindCounts_postThresh)]
Gene_list <- geneIds(predBind)
