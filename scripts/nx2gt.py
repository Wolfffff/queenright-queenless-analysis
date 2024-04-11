import ast

import glob
import gzip

import graph_tool.all as gt
import matplotlib.colors as colors
import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import pandas as pd


def get_prop_type(value, key=None):
    """
    Performs typing and value conversion for the graph_tool PropertyMap class.
    If a key is provided, it also ensures the key is in a format that can be
    used with the PropertyMap. Returns a tuple, (type name, value, key)
    """
    if isinstance(key, bytes):
        # Encode the key as ASCII
        key = key.encode("ascii", errors="replace")

    # Deal with the value
    if isinstance(value, bool):
        tname = "bool"

    elif isinstance(value, int):
        tname = "float"
        value = float(value)

    elif isinstance(value, float):
        tname = "float"

    elif isinstance(value, bytes):
        tname = "string"
        value = value.encode("ascii", errors="replace")

    elif isinstance(value, dict):
        tname = "object"

    else:
        tname = "string"
        value = str(value)

    return tname, value, key


def nx2gt(nxG):
    """
    Converts a networkx graph to a graph-tool graph.
    """
    # Phase 0: Create a directed or undirected graph-tool Graph
    gtG = gt.Graph(directed=nxG.is_directed())

    # Add the Graph properties as "internal properties"
    for key, value in nxG.graph.items():
        # Convert the value and key into a type for graph-tool
        tname, value, key = get_prop_type(value, key)

        prop = gtG.new_graph_property(tname)  # Create the PropertyMap
        gtG.graph_properties[key] = prop  # Set the PropertyMap
        gtG.graph_properties[key] = value  # Set the actual value

    # Phase 1: Add the vertex and edge property maps
    # Go through all nodes and edges and add seen properties
    # Add the node properties first
    nprops = set()  # cache keys to only add properties once
    for node, data in nxG.nodes(data=True):
        # Go through all the properties if not seen and add them.
        for key, val in data.items():
            if key in nprops:
                continue  # Skip properties already added

            # Convert the value and key into a type for graph-tool
            tname, _, key = get_prop_type(val, key)

            prop = gtG.new_vertex_property(tname)  # Create the PropertyMap
            gtG.vertex_properties[key] = prop  # Set the PropertyMap

            # Add the key to the already seen properties
            nprops.add(key)

    # Also add the node id: in NetworkX a node can be any hashable type, but
    # in graph-tool node are defined as indices. So we capture any strings
    # in a special PropertyMap called 'id' -- modify as needed!
    gtG.vertex_properties["id"] = gtG.new_vertex_property("string")

    # Add the edge properties second
    eprops = set()  # cache keys to only add properties once
    for src, dst, data in nxG.edges(data=True):
        # Go through all the edge properties if not seen and add them.
        for key, val in data.items():
            if key in eprops:
                continue  # Skip properties already added

            # Convert the value and key into a type for graph-tool
            tname, _, key = get_prop_type(val, key)

            prop = gtG.new_edge_property(tname)  # Create the PropertyMap
            gtG.edge_properties[key] = prop  # Set the PropertyMap

            # Add the key to the already seen properties
            eprops.add(key)

    # Phase 2: Actually add all the nodes and vertices with their properties
    # Add the nodes
    vertices = {}  # vertex mapping for tracking edges later
    for node, data in nxG.nodes(data=True):
        # Create the vertex and annotate for our edges later
        v = gtG.add_vertex()
        vertices[node] = v

        # Set the vertex properties, not forgetting the id property
        data["id"] = str(node)
        for key, value in data.items():
            gtG.vp[key][v] = value  # vp is short for vertex_properties

    # Add the edges
    for src, dst, data in nxG.edges(data=True):
        # Look up the vertex structs from our vertices mapping and add edge.
        e = gtG.add_edge(vertices[src], vertices[dst])

        # Add the edge properties
        for key, value in data.items():
            gtG.ep[key][e] = value  # ep is short for edge_properties

    # Done, finally!
    return gtG


GROUPS = [
    "RooibosTea_QR_1216_1646",
    "RooibosTea_QL_1216_1646",
    "MexHotChoc_QR_1216_1646",
    "MexHotChoc_QL_1216_1646",
    "20230213_1745_AlmdudlerGspritzt_C1",
    "20230213_1745_AlmdudlerGspritzt_C0",
    "20221209_1613_QR",
    "20221209_1613_QL",
    "20221123_1543_AmericanoLatte_QR",
    "20221123_1543_AmericanoLatte_QL",
]

# Create a dictionary to store the graphs
graphs = {}

for group in GROUPS:
    files = glob.glob(
        f"/Users/wolf/git/queenright-queenless-analysis/raw_data/edgelists/raw/{group}*.edgelist.gz"
    )
    group_graphs = {}

    # Create a set to store all nodes in the current group
    all_nodes_in_group = set()

    # Create an aggregate graph for the current group
    aggregate_graph = nx.DiGraph()

    for file in files:
        hour = file.split("_")[-1].split(".")[0]

        with gzip.open(file, "rt") as f:
            lines = f.readlines()

        nxG = nx.DiGraph()

        for line in lines:
            data = line.strip().split(" ")
            node1 = data[0].split(".")[0]
            node2 = data[1].split(".")[0]

            all_nodes_in_group.add(node1)
            all_nodes_in_group.add(node2)
            edge_data = ast.literal_eval(" ".join(data[2:]))
            if "weight" not in edge_data:
                edge_data["weight"] = edge_data["count"]

            # 82, #88
            if "#88" in node1 or "#88" in node2:
                continue

            nxG.add_edge(node1, node2, **edge_data)

            # Add the edge to the aggregate graph, summing the weights
            if aggregate_graph.has_edge(node1, node2):
                aggregate_graph[node1][node2]["weight"] += edge_data["weight"]
            else:
                aggregate_graph.add_edge(node1, node2, **edge_data)

        group_graphs[int(hour)] = nxG

    # Add missing nodes to each graph in the current group
    for hour, graph in group_graphs.items():
        for node in all_nodes_in_group:
            if node not in graph.nodes():
                graph.add_node(node)

    # Convert the NetworkX graphs to graph-tool graphs
    for graph in group_graphs:
        group_graphs[graph] = nx2gt(group_graphs[graph])

    # Convert the aggregate graph to a graph-tool graph and add it to the group graphs
    group_graphs["aggregate"] = nx2gt(aggregate_graph)

    graphs[group] = group_graphs

GROUPS = [
    "RooibosTea_QR_1216_1646",
    "RooibosTea_QL_1216_1646",
    "MexHotChoc_QR_1216_1646",
    "MexHotChoc_QL_1216_1646",
    "20230213_1745_AlmdudlerGspritzt_C1",
    "20230213_1745_AlmdudlerGspritzt_C0",
    "20221209_1613_QR",
    "20221209_1613_QL",
    "20221123_1543_AmericanoLatte_QR",
    "20221123_1543_AmericanoLatte_QL",
]
from matplotlib import colors
from scipy.cluster.hierarchy import dendrogram, linkage
from scipy.spatial.distance import pdist

# Normalize color scale
norm = colors.LogNorm()

# Iterate over groups
for group in GROUPS:
    g = graphs[group]["aggregate"]
    node_names = list(g.vp.id)
    adj = (
        gt.adjacency(g, weight=g.ep.weight).todense()
        + gt.adjacency(g, weight=g.ep.weight).T.todense()
    )
    adj = pd.DataFrame(adj, index=node_names, columns=node_names)

    row_sum = np.sum(adj, axis=1)
    col_sum = np.sum(adj, axis=0)

    rows_to_keep = np.where(row_sum >= 9600)[0]
    cols_to_keep = np.where(col_sum >= 9600)[0]

    adj = adj.iloc[rows_to_keep, cols_to_keep]

    # Step 1: Community detection
    state = gt.minimize_blockmodel_dl(g, state=gt.ModularityState)
    print(f"Modularity for {group}: {state.modularity()}")

    clustering = gt.local_clustering(g)
    print(f"Clustering coef for {group}: {clustering}")
    # This function returns a state object that contains the detected structure

    # Compute the distances and linkage
    distances = pdist(adj, metric="euclidean")
    linkage_matrix = linkage(distances, method="complete")

    # Create the dendrogram
    dendro = dendrogram(linkage_matrix, no_plot=True)

    # Reorder the adjacency matrix
    reordered_adj = adj.iloc[dendro["leaves"], dendro["leaves"]]

    # Create the figure and axes
    fig, ax = plt.subplots(figsize=(10, 10))

    # Create the heatmap
    cax = ax.imshow(reordered_adj, cmap="viridis", norm=norm)

    # Get the position of the heatmap axes
    pos = ax.get_position()

    # Create a new axes for the colorbar that matches the height of the heatmap axes
    cbar_ax = fig.add_axes([pos.x1 + 0.01, pos.y0, 0.02, pos.height])

    # Create the colorbar
    cbar = fig.colorbar(cax, cax=cbar_ax)

    # Set the title
    ax.set_title(f"Clustered Adjacency Matrix for Colony: {group}", pad=20)

    # Modify x and y ticks
    xlabels = [label.split("#")[1] for label in reordered_adj.columns]
    ylabels = [label.split("#")[1] for label in reordered_adj.index]

    # Save matrix to csv with x and y labs
    reordered_adj.to_csv(f"../results/clustered_adjacency_matrix_{group}.csv")

    ax.set_xticks(range(len(xlabels)))
    ax.set_xticklabels(xlabels, rotation=90)

    ax.set_yticks(range(len(ylabels)))
    ax.set_yticklabels(ylabels, rotation=0)

    # Save figure with high resolution
    plt.savefig(
        f"../figures/clustered_adjacency_matrix_{group}.png",
        dpi=300,
        bbox_inches="tight",
    )


def calculate_metrics_and_visualize(g):
    # Calculate Betweenness Centrality
    vertex_betweenness, edge_betweenness = gt.betweenness(g)
    g.vertex_properties["vertex_betweenness"] = vertex_betweenness
    g.edge_properties["edge_betweenness"] = edge_betweenness

    # Calculate Closeness Centrality
    closeness = gt.closeness(g)
    g.vertex_properties["closeness"] = closeness

    # Community Detection (Louvain Method)
    state = gt.minimize_blockmodel_dl(g)
    blocks = state.get_blocks()
    g.vertex_properties["blocks"] = blocks

    # Prepare for visualization
    # Normalize betweenness for visualization purposes
    norm_vertex_betweenness = g.new_vertex_property("double")
    max_vertex_betweenness = max(vertex_betweenness.a)
    for v in g.vertices():
        norm_vertex_betweenness[v] = (
            vertex_betweenness[v] / max_vertex_betweenness
        ) * 10  # Scaling for visibility

    return g, norm_vertex_betweenness, blocks


g, nvb, b = calculate_metrics_and_visualize(g)

# plot all vertex_betweenness
plt.hist(nvb.a, bins=20)


def visualize_graph(g, norm_vertex_betweenness, blocks):
    # Map the blocks to a continuous range of colors
    block_color = g.new_vertex_property("vector<double>")
    for v in g.vertices():
        block_color[v] = plt.cm.jet(blocks[v] / float(max(blocks.a) + 1))

    # Draw the graph
    gt.graph_draw(
        g,
        vertex_fill_color=block_color,
        vertex_size=norm_vertex_betweenness,
        output_size=(1000, 1000),
        output="graph_visualization.pdf",
    )


visualize_graph(g, nvb, b)

