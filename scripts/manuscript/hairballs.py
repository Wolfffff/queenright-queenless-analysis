import networkx as nx
import matplotlib.pyplot as plt
import os
import numpy as np

# Hardcoded values
FILENAMES = [
    "data/RooibosTea_QR_1216_1646_Time_Day4.gml",
    "data/RooibosTea_QL_1216_1646_Time_Day4.gml",
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
TITLES = ["Queenright", "Queenless"]
COLORS = {
    "Q": "#3f007b",
    "QRW": "#893F71",
    "INF": "#CC5500",
    "QLW": "#FFAE00",
}
EDGE_WEIGHT = "count"
SEED_COUNT = 5  # Number of times to replot with different seeds
THRESHOLD = 20  # Edge weight threshold for pruning
EDGE_WIDTH_SCALE = 5  # Scaling factor for edge widths
NODE_SIZE_SCALE = 50  # Scaling factor for node sizes


def load_graph(filename):
    """Load a graph from a GML file."""
    return nx.read_gml(filename)


def prune_and_normalize_edges(
    edge_weights, threshold, global_min_weight, global_max_weight
):
    """Prune edges below a threshold and normalize edge weights."""
    pruned_weights = {e: w for e, w in edge_weights.items() if w >= threshold}
    if not pruned_weights:
        return {}
    normalized_weights = {
        e: (w - global_min_weight) / (global_max_weight - global_min_weight)
        for e, w in pruned_weights.items()
    }
    normalized_weights = {e: max(0, min(1, w)) for e, w in normalized_weights.items()}
    print("Pruned", len(edge_weights) - len(pruned_weights), "edges")
    print(min(normalized_weights.values()), max(normalized_weights.values()))
    return normalized_weights


def scale_weights(weights, scale):
    """Scale normalized weights."""
    return {e: w * scale for e, w in weights.items()}


def get_normalized_node_sizes(graph, global_min_degree, global_max_degree):
    """Get normalized node sizes based on their degree."""
    degrees = dict(graph.degree(weight=EDGE_WEIGHT))
    if not degrees:
        return {}
    normalized_sizes = {
        node: (degree - global_min_degree) / (global_max_degree - global_min_degree)
        for node, degree in degrees.items()
    }
    normalized_sizes = {node: max(0.1, min(1, size)) for node, size in normalized_sizes.items()}
    print(min(normalized_sizes.values()), max(normalized_sizes.values()))
    return normalized_sizes


def scale_node_sizes(sizes, scale):
    """Scale normalized node sizes."""
    return {node: size * scale for node, size in sizes.items()}


def plot_hairball(
    graph,
    focal_nodes,
    ax,
    title,
    focal_color,
    non_focal_color,
    seed,
    global_min_weight,
    global_max_weight,
    global_min_degree,
    global_max_degree,
):
    """Plot a hairball graph with spring layout."""
    pos = nx.spring_layout(graph, seed=seed, weight=EDGE_WEIGHT)
    focal = set(focal_nodes)
    non_focal = set(graph.nodes) - focal

    edge_weights = nx.get_edge_attributes(graph, EDGE_WEIGHT)
    normalized_weights = prune_and_normalize_edges(
        edge_weights, THRESHOLD, global_min_weight, global_max_weight
    )
    scaled_weights = scale_weights(normalized_weights, EDGE_WIDTH_SCALE)

    focal_edges = [(u, v) for u, v in scaled_weights if u in focal or v in focal]
    non_focal_edges = [
        (u, v) for u, v in scaled_weights if u not in focal and v not in focal
    ]

    normalized_node_sizes = get_normalized_node_sizes(
        graph, global_min_degree, global_max_degree
    )
    scaled_node_sizes = scale_node_sizes(normalized_node_sizes, NODE_SIZE_SCALE)

    nx.draw_networkx_nodes(
        graph,
        pos,
        nodelist=focal,
        node_color=focal_color,
        ax=ax,
        node_size=[scaled_node_sizes.get(node, 1) for node in focal],
    )
    nx.draw_networkx_nodes(
        graph,
        pos,
        nodelist=non_focal,
        node_color=non_focal_color,
        ax=ax,
        node_size=[scaled_node_sizes.get(node, 1) for node in non_focal],
    )

    all_edges = focal_edges + non_focal_edges
    np.random.shuffle(all_edges)

    for edge in all_edges:
        color = focal_color if edge in focal_edges else non_focal_color
        width = scaled_weights.get(edge, 1)
        alpha = 0.25 + 0.5*(width/EDGE_WIDTH_SCALE)
        nx.draw_networkx_edges(
            graph,
            pos,
            edgelist=[edge],
            edge_color=color,
            width=width,
            alpha=alpha,
            ax=ax,
        )

    legend_elements = [
        plt.Line2D(
            [0],
            [0],
            marker="o",
            color="w",
            label="Queen" if title == "Queenright" else "Keystone Individual",
            markerfacecolor=focal_color,
            markersize=10,
        ),
        plt.Line2D(
            [0],
            [0],
            marker="o",
            color="w",
            label="Queenright Worker" if title == "Queenright" else "Queenless Worker",
            markerfacecolor=non_focal_color,
            markersize=10,
        ),
    ]

    ax.legend(
        handles=legend_elements,
        fontsize=10,
        loc="upper right",
        frameon=False,
        bbox_to_anchor=(1.15, 1.15),
    )
    ax.set_aspect("equal", adjustable="datalim")
    ax.axis("off")


def save_plot(fig, set_name):
    """Save the plot with a specific filename format."""
    filename = f"plots/{set_name}_spring_count.png"
    os.makedirs(os.path.dirname(filename), exist_ok=True)
    fig.savefig(filename, dpi=600)
    plt.close(fig)


# Main plotting logic
graphs = [load_graph(filename) for filename in FILENAMES]

# Compute global min and max for edge weights and degrees
all_edge_weights = {}
all_degrees = {}
for graph in graphs:
    edge_weights = nx.get_edge_attributes(graph, EDGE_WEIGHT)
    all_edge_weights.update(edge_weights)
    degrees = dict(graph.degree(weight=EDGE_WEIGHT))
    all_degrees.update(degrees)

global_min_weight = min(all_edge_weights.values(), default=1)
global_max_weight = max(all_edge_weights.values(), default=1)

global_min_degree = min(all_degrees.values(), default=0)
global_max_degree = max(all_degrees.values(), default=1)

for seed in range(SEED_COUNT):
    fig, axs = plt.subplots(1, len(FILENAMES), figsize=(8.5, 4.25))

    for graph, ax, title, focal_nodes in zip(graphs, axs, TITLES, FOCAL_NODES):
        focal_color, non_focal_color = (
            (COLORS["Q"], COLORS["QRW"])
            if title == "Queenright"
            else (COLORS["INF"], COLORS["QLW"])
        )

        plot_hairball(
            graph,
            focal_nodes,
            ax,
            title,
            focal_color,
            non_focal_color,
            seed,
            global_min_weight,
            global_max_weight,
            global_min_degree,
            global_max_degree,
        )

    plt.tight_layout(rect=[0, 0, 1, 0.96])
    save_plot(fig, f"example_seed_{seed}")