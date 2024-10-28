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
        bouts_dataframe = pd.concat([bouts_dataframe, bout_dataframe], ignore_index=True)

        # Increment the bout counter
        bout_counter += 1
    # Sort the bouts dataframe by the interaction frame
    bouts_dataframe = bouts_dataframe.sort_values(by = frame_col)

    # Return the bouts dataframe
    return bouts_dataframe


Day4 = ["001", "003", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020", "021", "022", "023", "024", "025", "026", "027", "028", "029", "030", "031", "032", "033", "034", "035", "036", "037", "038", "039", "040", "041", "042", "043", "044", "045", "046", "047", "048", "049", "050", "051", "052", "053", "054", "055", "056", "057", "058", "059", "060", "061", "062", "063", "064", "065", "066", "067", "068", "069", "070", "071", "072", "073", "074", "075", "076", "077", "078", "079", "080", "081", "082", "083", "084", "085", "086", "087", "088", "089", "090", "091", "092", "093", "094", "095"]
bouts = sys.argv[3]
prefix = sys.argv[1]
Start = 0
dfcomb = pd.DataFrame()

for i in Day4:
    filename = prefix + "_" + i + "_Reencoded_DirInts.txt"
    df = pd.read_table(filename)
    df["Hour"] = i
    df_onetype = df[df['Interaction Name'] == sys.argv[2]]
    if(bouts == "Bout"):
        df_bouts_unique = df_onetype.drop_duplicates(subset='Bout')
    else:
        df_bouts_unique =  df_onetype
    if Start == 1:
        dfcomb = pd.concat([dfcomb, df_bouts_unique])
    if Start == 0:
        dfcomb = df_bouts_unique
        Start = 1

print(dfcomb.head)
counts = pd.DataFrame({'count' : dfcomb.groupby( [ "Origin interactor","Destination interactor"] ).size()}).reset_index()
counts["log_count"] = np.log10(counts["count"]) + 1
counts["count_recip"] = 1/counts["count"]
G = nx.from_pandas_edgelist(counts, source='Origin interactor', target='Destination interactor', edge_attr=('count', 'log_count', 'count_recip'))
output = prefix + "_" + sys.argv[3] + ".alldaydirfullfixed.gml"
nx.write_gml(G,output)
