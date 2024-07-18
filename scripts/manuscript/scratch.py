import networkx as nx
import pandas as pd

# Hardcoded values
FILENAMES = [
    "data/RooibosTea_QR_1216_1646_Time_AllDays.gml",
    "data/RooibosTea_QL_1216_1646_Time_AllDays.gml",
]
FOCAL_NODES = [
    ["ArUcoTag#52"],  # Replace with actual focal nodes for the first file
    [
        "ArUcoTag#11",
        "ArUcoTag#12",
        "ArUcoTag#17",
        "ArUcoTag#22",
        "ArUcoTag#45",
        "ArUcoTag#47",
        "ArUcoTag#5",
        "ArUcoTag#51",
        "ArUcoTag#53",
        "ArUcoTag#55",
        "ArUcoTag#58",
    ],  # Replace with actual focal nodes for the second file
]
EDGE_WEIGHT = "count"


def load_graph(filename):
    """Load a graph from a GML file."""
    return nx.read_gml(filename)


def calculate_interactions(graph, focal_nodes):
    """Calculate interactions with focal nodes."""
    interaction_counts = {node: 0 for node in graph.nodes}
    for edge in graph.edges:
        if edge[0] in focal_nodes or edge[1] in focal_nodes:
            interaction_counts[edge[0]] += graph.edges[edge].get(EDGE_WEIGHT, 1)
            interaction_counts[edge[1]] += graph.edges[edge].get(EDGE_WEIGHT, 1)

    return interaction_counts


# Load graphs
graphs = [load_graph(filename) for filename in FILENAMES]
qr_g = graphs[0]

# Calculate interactions
node_interactions = calculate_interactions(qr_g, FOCAL_NODES[0])

# Convert interactions to a DataFrame
interaction_df = pd.DataFrame(
    list(node_interactions.items()), columns=["Bee", "interactions_with_queen"]
)

# Read the big data sheet and filter for Rooibos Trial
bds = pd.read_csv("data/BigDataSheet.csv", index_col=0)
bds["Trial"] = bds["Bee"].str.extract(r"(.+?)(?=_)")

# Classify bees based on QR and Influencer status
bds["QR_Queen_Inf"] = bds.apply(
    lambda row: (
        "Queenless Worker"
        if row["QR"] == 0 and row["Infl"] == 0
        else (
            "Queenright Worker"
            if row["QR"] == 1 and row["Queen"] == 0
            else (
                "Queen"
                if row["Queen"] == 1
                else "Influencer" if row["QR"] == 0 and row["Infl"] == 1 else None
            )
        )
    ),
    axis=1,
)
bds["QR_Queen_Inf"] = pd.Categorical(
    bds["QR_Queen_Inf"],
    categories=["Queenless Worker", "Queenright Worker", "Queen", "Influencer"],
)

# Filter the dataset for the Rooibos Trial
bds = bds[(bds["Trial"] == "RooibosTea") & bds["QR"] & ~bds["Queen"]]

# Get mean interactions for each bee
# Bee = RooibosTea_QR_1216_1646_ArUcoTag#1 and we just want ArUcoTag#1
bds["Bee"] = bds["Bee"].str.split("_").str[-1]

# Remove queen

# Get mean of Degree by Bee within BDS
bds_pooled = (
    bds.groupby("Bee")
    .agg(
        {
            "Degree": "mean",
            "AverageOvaryWidth": "mean",
            "AverageWingLength": "mean",
            "move_perc": "mean",
        }
    )
    .reset_index()
)
bds_pooled["ovary_wing_ratio"] = (
    bds_pooled["AverageOvaryWidth"] / bds_pooled["AverageWingLength"]
)
# Merge the interactions with the big data sheet
bds_pooled = pd.merge(bds_pooled, interaction_df, on="Bee", how="left")

bds_pooled.to_csv("data/bds_pooled.csv", index=False)