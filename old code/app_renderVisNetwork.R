#### Load necessary packages and data ####
library(dplyr)
library(shiny)
library(networkD3)
library(visNetwork)
library(collapsibleTree)
library(htmlwidgets)


load("./inst/extdata/nodes.RData")
load("./inst/extdata/edges.RData")

#### Server ####
server <- function(input, output) {
  output$tree <- renderCollapsibleTree({
    p <- collapsibleTree(
      edges,
      hierarchy = c("from", "to"),
      root = "Reactome",
      width = "100%",
      height = "100%",
      fontSize = 3,
      zoomable = TRUE,
      fill = "lightsteelblue",
      collapsed = TRUE,
      tooltip = TRUE,
      linkLength = 120
    )
    
    htmlwidgets::onRender(
      p,
      "
  function(el, x) {
    function resizeNodes() {
      d3.select(el).selectAll('circle')
        .attr('r', 1)
        .style('stroke-width', 0.1);

      d3.select(el).selectAll('.link')
        .style('stroke-width', 0.5)
        .style('opacity', 0.7);
    }

    resizeNodes();
    d3.select(el).selectAll('.node').on('click.resize', function() {
      setTimeout(resizeNodes, 400);
    });
  }
  "
    )
  })
    
  output$vis <- renderVisNetwork({
    visNetwork(nodes, edges, width = "100%") %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        selectedBy = "group"
      ) %>%
      visLayout(randomSeed = 42) %>%
      visInteraction(
        navigationButtons = TRUE,
        dragNodes = TRUE, dragView = TRUE, zoomView = TRUE
      ) %>%
      # smaller nodes and smaller labels
      visNodes(
        size = 5,                                  # fixed small node size
        scaling = list(min = 3, max = 10),         # reduce scaling range
        font = list(size = 10)                     # smaller text labels
      ) %>%
      # thinner edges, smoother lines, dynamic opacity
      visEdges(
        width = 0.5,                               # default edge thickness
        scaling = list(min = 0.1, max = 1),        # make edges lighter
        physics = FALSE, 
        smooth = TRUE,
        color = list(opacity = input$opacity, highlight = "black")
      ) %>%
      visLegend(width = 0.05, position = "right", main = "Group")
  })

  output$force <- renderForceNetwork({
    forceNetwork(
      Links = edges, Nodes = nodes,
      Source = "source", Target = "target",      # so the network is directed.
      NodeID = "label", Group = "group", Value = "value", 
      width = 1500, height=1500,
      zoom = T,bounded=F,
      legend = T,
      colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"), ## 颜色模板
      opacity = input$opacity, fontSize = 5, 
      charge= -1, # 节点斥力大小(负值越大斥力越大)
      opacityNoHover = 0.5  # 鼠标没有停留时其他节点名称的透明度
    )
  })

}

#### UI ####

ui <- shinyUI(fluidPage(

  titlePanel("Shiny networkD3 "),

  sidebarLayout(
    sidebarPanel(
      sliderInput("opacity", "Opacity (not for Sankey)", 0.6, min = 0.1, max = 1, step = .1),
      width = 1
    ),
    mainPanel(
      div(style = "height:1000px; overflow-y:auto;",
          tabsetPanel(
            tabPanel("collapsible Tree", collapsibleTreeOutput("tree", height = "950px")),
            tabPanel("vis Network", visNetworkOutput("vis", height = "950px")),
            tabPanel("Force Network", forceNetworkOutput("force", height = "950px"))
          )
      ),
      width = 10
    )
  )
))

#### Run ####
shinyApp(ui = ui, server = server)
