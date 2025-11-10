import networkx as nx
import pandas as pd
from d3blocks import D3Blocks

# Load merged graph
G = nx.read_graphml("Merged genes from DE genes_reactome_harmonizome FI Network.graphml")

# Convert to edge list dataframe
edges = nx.to_pandas_edgelist(G)

# Add default weight if missing
if "weight" not in edges.columns:
    edges["weight"] = 1

# Initialize d3blocks
d3 = D3Blocks()

# Create elastic graph visualization
d3.elasticgraph(edges, filepath="Merged genes from DE genes_reactome_harmonizome FI Network.html")