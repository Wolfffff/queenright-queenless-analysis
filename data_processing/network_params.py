import sys
from itertools import permutations

import networkx as nx
import numpy as np
import pandas as pd


def efficiency_weighted(G, u, v, weight):
    try:
        eff = 1 / nx.shortest_path_length(G, u, v, weight="count_recip")
    except NetworkXNoPath:
        eff = 0
    return eff


def global_efficiency_weighted(G):
    n = len(G)
    denom = n * (n - 1)
    if denom != 0:
        g_eff = (
            sum(
                efficiency_weighted(G, u, v, weight="count_recip")
                for u, v in permutations(G, 2)
            )
            / denom
        )
    else:
        g_eff = 0
    return g_eff


df = pd.read_table(sys.argv[1])
df_onetype = df[df["Interaction Name"] == sys.argv[3]]
counts = pd.DataFrame(
    {
        "count": df_onetype.groupby(
            ["Origin interactor", "Destination interactor"]
        ).size()
    }
).reset_index()
counts["log_count"] = np.log10(counts["count"]) + 1
counts["count_recip"] = 1 / counts["count"]
G = nx.from_pandas_edgelist(
    counts,
    source="Origin interactor",
    target="Destination interactor",
    edge_attr=("count", "log_count", "count_recip"),
)
trans = nx.transitivity(G)
clust = nx.average_clustering(G)
greed = nx.community.greedy_modularity_communities(G)
modul = nx.community.modularity(G, greed, weight="count", resolution=1)
effic = global_efficiency_weighted(G)
degrees = G.degree(weight="count")
print(degrees)
sumedg = sum([v for k, v in degrees])
param_df = pd.DataFrame(
    {
        "params": [
            "Transitivity",
            "Average Clustering",
            "GlobalEfficiency",
            "Sum",
            "Modularity",
        ],
        "values": [trans, clust, effic, sumedg, modul],
    }
)
param_df.to_csv(sys.argv[2])
