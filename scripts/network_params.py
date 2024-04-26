from itertools import permutations
import sys
import networkx as nx
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
def groupByBouts (interaction_dataframe, 
                  origin_col = "Origin interactor",
                  dest_col = "Destination interactor",
                  interaction_col = "Interaction Name",
                  frame_col = "Interaction Frame",
                  frame_buffer = 20):
    # Set the bout counter to 1
    bout_counter = 1
    # Create a new dataframe to store the bouts
    bouts_dataframe = pd.DataFrame(columns = interaction_dataframe.columns.tolist() + ["Bout"])
    
    # Iterate by bouts
    for _, bout_dataframe in interaction_dataframe.groupby([interaction_col,
                                                            origin_col,
                                                            dest_col, 
                                                            interaction_dataframe[frame_col].diff().abs().gt(frame_buffer).cumsum()]):

        # Add the bout number to the bout dataframe
        bout_dataframe['Bout'] = bout_counter

        # Appeend the bout dataframe to the bouts dataframe
        bouts_dataframe = bouts_dataframe.append(bout_dataframe, ignore_index = True)

        # Increment the bout counter
        bout_counter += 1
    # Sort the bouts dataframe by the interaction frame
    bouts_dataframe = bouts_dataframe.sort_values(by = frame_col)
    
    # Return the bouts dataframe
    return bouts_dataframe

def efficiency_weighted(G, u, v, weight):
    try:
        eff = 1 / nx.shortest_path_length(G, u, v, weight='count_recip')
    except NetworkXNoPath:
        eff = 0
    return eff

def global_efficiency_weighted(G):
    n = len(G)
    denom = n * (n - 1)
    if denom != 0:
        g_eff = sum(efficiency_weighted(G, u, v, weight='count_recip') for u, v in permutations(G, 2)) / denom
    else:
        g_eff = 0
    return g_eff
bouts = sys.argv[4]
df = pd.read_table(sys.argv[1])
df_onetype = df[df['Interaction Name'] == sys.argv[3]]
if(bouts =="True"):
    df_bouts = groupByBouts(df_onetype)
    df_bouts_unique = df_bouts.drop_duplicates(subset='Bout')
    counts = pd.DataFrame({'count' : df_bouts_unique.groupby( [ "Origin interactor","Destination interactor"] ).size()}).reset_index()
else:
    counts = pd.DataFrame({'count' : df_onetype.groupby( [ "Origin interac\
tor","Destination interactor"] ).size()}).reset_index()
counts["log_count"] = np.log10(counts["count"]) + 1
counts["count_recip"] = 1/counts["count"]
G = nx.from_pandas_edgelist(counts, source='Origin interactor', target='Destination interactor', edge_attr=('count', 'log_count', 'count_recip'))
trans=nx.transitivity(G)
clust = nx.average_clustering(G)
greed = nx.community.greedy_modularity_communities(G)
modul = nx.community.modularity(G, greed, weight='count', resolution=1)
effic = global_efficiency_weighted(G)
degrees = G.degree(weight='count')
print(degrees)
sumedg = sum([v for k, v in degrees])
param_df = pd.DataFrame({'params': ["Transitivity", "Average Clustering", "GlobalEfficiency", "Sum", "Modularity"], 'values': [trans, clust, effic, sumedg, modul]})
param_df.to_csv(sys.argv[2])


