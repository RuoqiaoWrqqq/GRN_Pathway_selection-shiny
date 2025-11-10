#### Load necessary packages and data ####
library(dplyr)
library(shiny)
library(networkD3)
library(visNetwork)
library(collapsibleTree)
library(htmlwidgets)



server <- function(input, output) {
  
  # --------------------- Force Network ---------------------
  output$force <- renderForceNetwork({
    
    # start with full sets
    nodes_filtered <- nodes
    edges_filtered <- edges
    
    # adjust node size by slider
    nodes_filtered$size <- input$node_size
    # adjust edge thickness by slider
    edges_filtered$value <- edges_filtered$value * input$linkWidth
    
    # ------------- Filter by group (Shiny sidebar) -------------
    if (!is.null(input$group_select) && input$group_select != "All") {
      nodes_filtered <- nodes_filtered[nodes_filtered$group == input$group_select, ]
      edges_filtered  <- edges_filtered[edges_filtered$from == input$group_select, ]
      
      edges_filtered$source <- match(edges_filtered$from, nodes_filtered$label) - 1
      edges_filtered$target <- match(edges_filtered$to, nodes_filtered$label) - 1
    }
    
    # ------------- Build the force network safely -------------
    fn <- forceNetwork(
      Links = edges_filtered,
      Nodes = nodes_filtered,
      Source = "source",
      Target = "target",
      NodeID = "label",
      Group = "group",
      Value = "value",
      Nodesize = "size",
      radiusCalculation = networkD3::JS("Math.sqrt(d.nodesize) * 2"),
      zoom = TRUE,
      bounded = FALSE,
      legend = TRUE,
      colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
      opacity = input$opacity,
      fontSize = input$fontSize,
      opacityNoHover = 0.5
    )
    
    # ------------- Interactive group dropdown in JS -------------
    htmlwidgets::onRender(fn, "
    function(el, x) {
      var groups = [...new Set(x.nodes.map(d => d.group))].sort();
      if (!document.getElementById('groupDropdown')) {
        var container = d3.select(el.parentNode)
          .insert('div', ':first-child')
          .attr('id', 'groupDropdownContainer')
          .style('margin', '10px 0')
          .html('<label style=\"margin-right:6px;\">Select group:</label><select id=\"groupDropdown\"><option value=\"All\">All</option></select>');
        groups.forEach(function(g) {
          d3.select('#groupDropdown').append('option').attr('value', g).text(g);
        });
      }

      d3.select('#groupDropdown').on('change', function() {
        var selected = this.value;
        var nodeSel = d3.select(el).selectAll('.node');
        var linkSel = d3.select(el).selectAll('.link');

        if (selected === 'All') {
          nodeSel.select('circle').style('fill', null).style('fill-opacity', 1);
          nodeSel.select('text').style('fill', '#333').style('font-weight', 'normal');
          linkSel.style('opacity', 0.8);
          return;
        }

        nodeSel.select('circle')
          .style('fill', d => d.group === selected ? 'red' : 'lightgrey')
          .style('fill-opacity', d => d.group === selected ? 1 : 0.2);
        nodeSel.select('text')
          .style('fill', d => d.group === selected ? 'black' : 'lightgrey')
          .style('font-weight', d => d.group === selected ? 'bold' : 'normal');
        linkSel.style('opacity', l => (l.source.group === selected || l.target.group === selected) ? 0.8 : 0.1);
      });
    }
  ")
  })
}

ui <- shinyUI(fluidPage(
  titlePanel("Shiny networkD3"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("opacity", "Opacity",
                  0.6, min = 0.1, max = 1, step = .1),
      sliderInput("node_size", "Node Size",
                  5, min = 1, max = 10, step = 1),
      sliderInput("fontSize", "Font Size",
                  5, min = 1, max = 20, step = 1),
      sliderInput("linkWidth", "Lne Thickness",
                  0.25, min = 0.05, max = 0.5, step = .025),
      selectInput("group_select", "Filter by Group:",
                  choices = c("All", unique(nodes$group)), selected = "All"),
      width = 2
    ),
    mainPanel(
      div(style = "height:950px; overflow-y:auto;",
          tabsetPanel(
            tabPanel("Force Network", forceNetworkOutput("force", height = "850px"))
          )
      ),
      width = 10
    )
  )
))

#### Run ####
shinyApp(ui = ui, server = server)
