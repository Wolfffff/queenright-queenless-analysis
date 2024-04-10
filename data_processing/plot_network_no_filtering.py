import sys

import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import pandas as pd

df = pd.read_table(sys.argv[1])
df_onetype = df[df["Interaction Name"] == sys.argv[3]]
counts = pd.DataFrame(
    {
        "count": df_onetype.groupby(
            ["Origin interactor", "Destination interactor"]
        ).size()
    }
).reset_index()
counts["log_count"] = np.log10(counts["count"])
G = nx.from_pandas_edgelist(
    counts,
    source="Origin interactor",
    target="Destination interactor",
    edge_attr=("count", "log_count"),
)
nx.write_edgelist(G, sys.argv[2])
