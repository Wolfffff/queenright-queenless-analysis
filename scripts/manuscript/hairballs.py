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
SEED_COUNT = 3  # Number of times to replot with different seeds
THRESHOLD = 100  # Edge weight threshold for pruning
EDGE_WIDTH_SCALE = 5  # Increased scaling factor for edge widths
NODE_SIZE_SCALE = 50  # Increased scaling factor for node sizes
EXPONENT = 5  # Exponent for power transformation


def load_graph(filename):
    """Load a graph from a GML file."""
    return nx.read_gml(filename)


def remove_outliers(edge_weights):
    """Remove outliers from edge weights using the IQR method."""
    weights = np.array(list(edge_weights.values()))
    q1 = np.percentile(weights, 25)
    q3 = np.percentile(weights, 75)
    iqr = q3 - q1
    lower_bound = q1 - 1.5 * iqr
    upper_bound = q3 + 1.5 * iqr
    return {e: w for e, w in edge_weights.items() if lower_bound <= w <= upper_bound}


def prune_and_scale_edges(edge_weights, threshold, scale, exponent):
    """Prune edges below a threshold and scale edge weights for visualization using power transformation."""
    edge_weights = remove_outliers(edge_weights)
    pruned_weights = {e: w for e, w in edge_weights.items() if w >= threshold}
    transformed_weights = {e: (w + 1) ** exponent for e, w in pruned_weights.items()}
    max_weight = max(transformed_weights.values(), default=1)
    min_weight = min(transformed_weights.values(), default=1)
    normalized_weights = {
        e: (w - min_weight) / (max_weight - min_weight)
        for e, w in transformed_weights.items()
    }
    scaled_weights = {e: w * scale for e, w in normalized_weights.items()}
    return scaled_weights


def get_node_sizes_by_degree(graph, scale, exponent):
    """Get normalized node sizes based on their degree using power transformation."""
    degrees = dict(graph.degree(weight=EDGE_WEIGHT))
    transformed_degrees = {node: (degree + 1) ** exponent for node, degree in degrees.items()}
    max_degree = max(transformed_degrees.values(), default=1)
    min_degree = min(transformed_degrees.values(), default=0)
    normalized_sizes = {
        node: ((degree - min_degree) / (max_degree - min_degree)) + 0.1
        for node, degree in transformed_degrees.items()
    }
    scaled_sizes = {node: size * scale for node, size in normalized_sizes.items()}
    return scaled_sizes


def plot_hairball(graph, focal_nodes, ax, title, focal_color, non_focal_color, seed=None):
    """Plot a hairball graph with spring layout."""
    pos = nx.spring_layout(graph, seed=seed, weight=EDGE_WEIGHT)

    focal = set(focal_nodes)
    non_focal = set(graph.nodes) - focal

    edge_weights = nx.get_edge_attributes(graph, EDGE_WEIGHT)
    scaled_weights = prune_and_scale_edges(edge_weights, THRESHOLD, EDGE_WIDTH_SCALE, EXPONENT)

    # Keep all nodes, but prune edges
    focal_edges = [(u, v) for u, v in scaled_weights if u in focal or v in focal]
    non_focal_edges = [(u, v) for u, v in scaled_weights if u not in focal and v not in focal]

    node_sizes = get_node_sizes_by_degree(graph, NODE_SIZE_SCALE, EXPONENT)

    nx.draw_networkx_nodes(
        graph,
        pos,
        nodelist=focal,
        node_color=focal_color,
        ax=ax,
        node_size=[node_sizes[node] for node in focal],
    )
    nx.draw_networkx_nodes(
        graph,
        pos,
        nodelist=non_focal,
        node_color=non_focal_color,
        ax=ax,
        node_size=[node_sizes[node] for node in non_focal],
    )

    all_edges = focal_edges + non_focal_edges
    np.random.shuffle(all_edges)

    for edge in all_edges:
        color = focal_color if edge in focal_edges else non_focal_color
        width = scaled_weights.get(edge, 1)
        alpha = 0.5
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
        plt.Line2D([0], [0], marker='o', color='w', label='Queen' if title == "Queenright" else 'Keystone Individual',
                   markerfacecolor=focal_color, markersize=10),
        plt.Line2D([0], [0], marker='o', color='w', label='Queenright Worker' if title == "Queenright" else 'Queenless Worker',
                   markerfacecolor=non_focal_color, markersize=10)
    ]

    ax.legend(handles=legend_elements, fontsize=10, loc='upper right', frameon=False, bbox_to_anchor=(1.15, 1.15))
    ax.set_aspect("equal", adjustable="datalim")
    ax.axis('off')


def save_plot(fig, set_name):
    """Save the plot with a specific filename format."""
    filename = f"plots/{set_name}_spring_count.png"
    os.makedirs(os.path.dirname(filename), exist_ok=True)
    fig.savefig(filename, dpi=600)
    plt.close(fig)


def plot_weighted_degree_distribution(graph, ax, title, edge_weight):
    degrees = [d for n, d in graph.degree(weight=edge_weight)]
    ax.hist(degrees, bins=30)
    ax.set_title(f"{title} - Weighted Degree Distribution", fontsize=14)
    ax.set_xlabel("Weighted Degree", fontsize=12)
    ax.set_ylabel("Frequency", fontsize=12)
    ax.tick_params(axis='both', which='major', labelsize=10)
    ax.grid(True)
    ax.set_aspect("auto")


# Main plotting logic
graphs = [load_graph(filename) for filename in FILENAMES]

for seed in range(SEED_COUNT):
    fig, axs = plt.subplots(1, len(FILENAMES), figsize=(8.5, 4.25))

    for graph, ax, title, focal_nodes in zip(graphs, axs, TITLES, FOCAL_NODES):
        if title == "Queenright":
            focal_color, non_focal_color = COLORS["Q"], COLORS["QRW"]
        else:
            focal_color, non_focal_color = COLORS["INF"], COLORS["QLW"]

        plot_hairball(graph, focal_nodes, ax, title, focal_color, non_focal_color, seed)

    plt.tight_layout(rect=[0, 0, 1, 0.96])
    save_plot(fig, f"example_seed_{seed}")

# Plot additional visualizations
fig, axs = plt.subplots(1, len(FILENAMES), figsize=(8.5, 4.25))

for graph, title, ax in zip(graphs, TITLES, axs):
    plot_weighted_degree_distribution(graph, ax, title, EDGE_WEIGHT)

plt.suptitle("Additional Visualizations - Weighted Degree Distribution", fontsize=16)
plt.tight_layout(rect=[0, 0, 1, 0.96])
filename = "plots/additional_visualizations.png"
os.makedirs(os.path.dirname(filename), exist_ok=True)
fig.savefig(filename, dpi=600)
plt.close(fig)