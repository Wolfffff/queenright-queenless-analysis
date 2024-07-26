import sys

import networkx as nx
import numpy as np
import pandas as pd

df = pd.read_table(sys.argv[1])
df_onetype = df[df["Interaction Name"] == sys.argv[4]]
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
assort = nx.degree_assortativity_coefficient(G, weight="count")
degree_cent = {node: val for (node, val) in nx.degree(G, weight="count")}
degree_clos = nx.closeness_centrality(G, distance="count_recip")
degree_betw = nx.betweenness_centrality(G, weight="count_recip")
param_df = pd.DataFrame({"params": ["Assort"], "values": [assort]})
param_df.to_csv(sys.argv[2])
comb_df = pd.DataFrame(
    {
        "Degree": pd.Series(degree_cent),
        "Closeness": pd.Series(degree_clos),
        "Betweenness": pd.Series(degree_betw),
    }
)
comb_df.to_csv(sys.argv[3])
