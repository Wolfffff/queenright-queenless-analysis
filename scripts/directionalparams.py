import sys

import networkx as nx
import numpy as np
import pandas as pd

prefix = sys.argv[1]
inputname = prefix + "_DirInts.csv"
df = pd.read_csv(inputname)
df_onetype = df[df["Interaction Name"] == sys.argv[2]]
df_dir = df_onetype[df_onetype["Directed"] == "Yes"]
counts = pd.DataFrame(
    {"count": df_dir.groupby(["Origin interactor", "Destination interactor"]).size()}
).reset_index()
counts["log_count"] = np.log10(counts["count"]) + 1
counts["count_recip"] = 1 / counts["count"]
G = nx.from_pandas_edgelist(
    counts,
    source="Origin interactor",
    target="Destination interactor",
    edge_attr=("count", "log_count", "count_recip"),
    create_using=nx.DiGraph(),
)
trans = nx.transitivity(G)
clust = nx.average_clustering(G)
greed = nx.community.greedy_modularity_communities(G)
modul = nx.community.modularity(G, greed, weight="count", resolution=1)
degrees = G.degree(weight="count")
sumedg = sum([v for k, v in degrees])
param_df = pd.DataFrame(
    {
        "params": ["Transitivity", "Average Clustering", "Sum", "Modularity"],
        "values": [trans, clust, sumedg, modul],
    }
)
outputname = prefix + "_HHSpeedDirNWP.csv"
param_df.to_csv(outputname)
assort = nx.degree_assortativity_coefficient(G, weight="count")
indegree_cent = {node: val for (node, val) in G.in_degree(weight="count")}
outdegree_cent = {node: val for (node, val) in G.out_degree(weight="count")}
degree_clos = nx.closeness_centrality(G, distance="count_recip")
degree_betw = nx.betweenness_centrality(G, weight="count_recip")
assort_df = pd.DataFrame({"params": ["Assort"], "values": [assort]})
outputname = prefix + "_HHSpeedDirAssort.csv"
assort_df.to_csv(outputname)
comb_df = pd.DataFrame(
    {
        "InDegree": pd.Series(indegree_cent),
        "OutDegree": pd.Series(outdegree_cent),
        "Closeness": pd.Series(degree_clos),
        "Betweenness": pd.Series(degree_betw),
    }
)
outputname = prefix + "_HHSpeedDirCent.csv"
comb_df.to_csv(outputname)
