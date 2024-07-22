import sys

import networkx as nx
import numpy as np
import pandas as pd

df = pd.read_csv(sys.argv[1])
df_onetype = df[df["Interaction Name"] == sys.argv[3]]
df_dir = df_onetype[df_onetype["Directed"] == "Yes"]
counts = pd.DataFrame(
    {"count": df_dir.groupby(["Origin interactor", "Destination interactor"]).size()}
).reset_index()
counts["log_count"] = np.log10(counts["count"])
G = nx.from_pandas_edgelist(
    counts,
    source="Origin interactor",
    target="Destination interactor",
    edge_attr=("count", "log_count"),
)
nx.write_edgelist(G, sys.argv[2])
