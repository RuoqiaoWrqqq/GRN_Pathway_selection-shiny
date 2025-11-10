# =======================================================
# server.R — Dashboard Logic
# =======================================================
server <- function(input, output, session) {
  
  # ---- Page 1 ----
  output$enrollement <- renderPlot({
    ggplot(df_meta_sub_group, aes(x = Cluster, y = total_cells, fill = Cluster)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = total_cells), vjust = -0.3, size = 6) +
      theme_classic(base_size = 20) +
      ylab("number of cell")+ xlab("")+
      scale_fill_manual(values = pathway_colors) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # ---- Page 2 ----
  output$enrollement2 <- renderPlot({
    df_meta_sub_group <- df %>%
      group_by(Disease, Cluster) %>%
      summarise(total_cells = n())
    ggplot(df_meta_sub_group, aes(x = Disease, y = total_cells, fill = Cluster)) +
      geom_bar(stat = "identity") +
      theme_classic(base_size = 20) +
      scale_fill_manual(values = pathway_colors) +
      ylab("number of pathway")+ xlab("")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$sankey_diagram <- renderSankeyNetwork({
    s <- sankeyNetwork(
      Links = edges_pairs,
      Nodes = nodes,
      Source = "IDsource",
      Target = "IDtarget",
      Value = "weight",
      NodeID = "name",
      colourScale = JS(color_js),
      fontSize = 12, nodeWidth = 10, nodePadding = 5, sinksRight = FALSE
    )
    js_string <- 'function(el, x){ d3.select(el).selectAll(".node text").text(function(d){return d.name+" (n = "+d.value+")";});}'
    onRender(s, js_string)
  })
  
  # ---- Page 3 (UpSetR) ----
  valid_inter_df <- inter_df %>% filter(size > 0 & !is.na(name) & name != "")
  updateSelectInput(session, "intersection_choice",
                    choices = valid_inter_df$name,
                    selected = "CD14 & CD14-active & CD16 & Intermediate & PMC")
  
  output$upset_plot <- renderPlot({
    UpSetR::upset(
      Venn_data,
      sets = names(group_list),
      keep.order = TRUE,
      order.by = "degree",
      main.bar.color = "violetred4",
      sets.bar.color = c("#A6CEE3","#FDBF6F","#FF7F00","#B15928","#CCAFCA"),
      matrix.color = "black",
      shade.color = "grey",
      text.scale = 2
    )
  })
  
  # ---- Reactive version of selected pathways ----
  selected_Pathways_df <- reactive({
    if (length(input$intersection_choice) > 0) {
      selected_rows <- inter_df %>%
        filter(name %in% input$intersection_choice)
      
      selected_Pathways <- unique(unlist(selected_rows$Pathways))
      data.frame(Pathway = selected_Pathways, stringsAsFactors = FALSE)
    } else {
      data.frame(Pathway = character(0))
    }
  })
  
  # ---- Shared pathways table ----
  output$shared_Pathways <- renderDT({
    pathways <- selected_Pathways_df()$Pathway
    req(length(pathways) > 0)  
    
    share_Pathways_df <- combined_Pathways_df_group %>%
      filter(pathway_name %in% pathways) %>%
      dplyr::select(pathway_name, Cluster, Disease, top_level_pathway_name)
    
    datatable(
      share_Pathways_df,
      filter = "top",       
      selection = "multiple",
      extensions = "Buttons",
      options = list(
        pageLength = as.numeric(input$n_rows),
        autoWidth = TRUE,
        scrollY = "350px",
        dom = "Bfrtip",
        buttons = c("copy","csv"),
        lengthMenu = c(50, 100, 200, 300, 400, 500)
      )
    )
  })
  
  
  # ---- Gene table (same data shown differently) ----
  output$gene_table <- renderDT({
    pathways <- selected_Pathways_df()$Pathway
    req(length(pathways) > 0)
    
    subset_list <- Gene_list[pathways]
    subset_list <- subset_list[!sapply(subset_list, is.null)]
    
    # Flatten the list into a 2-column data frame
    gene_df <- do.call(rbind, lapply(names(subset_list), function(set_name) {
      data.frame(
        gene = subset_list[[set_name]],
        pathway = set_name,
        stringsAsFactors = FALSE
      )
    }))
    
    # Display as interactive datatable
    datatable(
      gene_df,
      extensions = "Buttons",
      filter = "top",
      options = list(
        pageLength = as.numeric(input$n_rows),
        autoWidth = TRUE,
        scrollY = "425px",
        dom = '<"d-flex justify-content-between align-items-center"Bfl>tip',
        buttons = c("copy", "csv"),
        lengthMenu = c(50, 100, 200, 300, 400, 500)
      )
    )
  })
  
  # ---- Pathway count ----
  output$pathway_count_box <- renderValueBox({
    pathways <- unique(selected_Pathways_df()$Pathway)
    n_pathways <- length(pathways)
    
    valueBox(
      value = n_pathways,
      subtitle = "Unique Pathways Selected",
      icon = icon("project-diagram"),
      color = ifelse(n_pathways > 0, "aqua", "lightgray")
    )
  })
  
  
  # ---- Gene count ----
  output$gene_count_box <- renderValueBox({
    pathways <- selected_Pathways_df()$Pathway
    req(length(pathways) > 0)
    
    subset_list <- Gene_list[pathways]
    subset_list <- subset_list[!sapply(subset_list, is.null)]
    all_genes <- unique(unlist(subset_list, use.names = FALSE))
    n_genes <- length(all_genes)
    
    valueBox(
      value = n_genes,
      subtitle = "Unique Genes in Selected Pathways",
      icon = icon("dna"),
      color = ifelse(n_genes > 0, "teal", "lightgray")
    )
  })
  
  
  # ---- Pathway list box ----
  output$pathway_list_box <- renderUI({
    pathways <- unique(selected_Pathways_df()$Pathway)
    req(length(pathways) > 0)
    
    box(
      title = paste("Selected Pathways (", length(pathways), ")", sep = ""),
      status = "primary",
      solidHeader = TRUE,
      width = 6,
      style = "max-height:300px; overflow-y:auto;",
      tags$ul(lapply(pathways, tags$li))
    )
  })
  
  
  # ---- Gene list box with download ----
  output$gene_list_box <- renderUI({
    pathways <- selected_Pathways_df()$Pathway
    req(length(pathways) > 0)
    
    subset_list <- Gene_list[pathways]
    subset_list <- subset_list[!sapply(subset_list, is.null)]
    all_genes <- unique(unlist(subset_list, use.names = FALSE))
    req(length(all_genes) > 0)
    
    box(
      title = div(
        style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
        span(paste("Genes in Selected Pathways (", length(all_genes), ")", sep = "")),
        downloadButton("download_genes", "", style = "padding:2px 4px")
      ),
      status = "info",
      solidHeader = TRUE,
      width = 6,
      style = "max-height:300px; overflow-y:auto;",
      tags$ul(lapply(all_genes, tags$li))
    )
  })
  
  # ---- Download Handler ----
  output$download_genes <- downloadHandler(
    filename = function() {
      paste0("Selected_Gene_List_", Sys.Date(), ".txt")
    },
    content = function(file) {
      pathways <- selected_Pathways_df()$Pathway
      req(length(pathways) > 0)
      
      subset_list <- Gene_list[pathways]
      subset_list <- subset_list[!sapply(subset_list, is.null)]
      all_genes <- unique(unlist(subset_list, use.names = FALSE))
      req(length(all_genes) > 0)
      
      writeLines(all_genes, file)
    }
  )
  
  # ---- Visnetwork ----
  output$gene_vis <- renderVisNetwork({
    pathways <- selected_Pathways_df()$Pathway
    req(length(pathways) > 0)
    
    subset_list <- Gene_list[pathways]
    subset_list <- subset_list[!sapply(subset_list, is.null)]
    
    # Build edges
    edges <- do.call(rbind, lapply(names(subset_list), function(set_name) {
      genes <- subset_list[[set_name]]
      data.frame(from = set_name, to = genes, stringsAsFactors = FALSE)
    }))
    
    all_nodes <- unique(c(edges$from, edges$to))
    nodes <- data.frame(
      id = all_nodes,
      label = all_nodes,
      group = ifelse(all_nodes %in% names(subset_list), "Pathway", "Gene"),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        shape = ifelse(group == "Pathway", "box", "ellipse"),
        size = ifelse(group == "Pathway", 25, 10),
        color = ifelse(group == "Pathway", "tomato", "skyblue")
      )
    
    visNetwork(nodes, edges, height = "700px", width = "100%") %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visInteraction(navigationButtons = TRUE, dragNodes = TRUE) %>%
      visGroups(groupname = "Pathway", color = "red") %>%
      visGroups(groupname = "Gene", color = "lightblue") %>%
      visLegend()
  })
}