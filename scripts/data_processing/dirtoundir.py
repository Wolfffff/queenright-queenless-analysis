from __future__ import division

import math
import sys

import h5py
import numpy as np
import pandas as pd

prefix = sys.argv[1]
filename = prefix + "_interp_filtered.h5"
timecutoff = 20


def euclidean_distance(x1, y1, x2, y2):
    distance = math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2)
    return distance


def groupByBouts(
    interaction_dataframe,
    origin_col="Origin interactor",
    dest_col="Destination interactor",
    interaction_col="Interaction Name",
    frame_col="Interaction Frame",
    frame_buffer=20,
):
    # Set the bout counter to 1
    bout_counter = 1
    # Create a new dataframe to store the bouts
    bouts_dataframe = pd.DataFrame(
        columns=interaction_dataframe.columns.tolist() + ["Bout"]
    )

    # Iterate by bouts
    for _, bout_dataframe in interaction_dataframe.groupby(
        [
            interaction_col,
            origin_col,
            dest_col,
            interaction_dataframe[frame_col].diff().abs().gt(frame_buffer).cumsum(),
        ]
    ):
        # Add the bout number to the bout dataframe
        bout_dataframe["Bout"] = bout_counter

        # Appeend the bout dataframe to the bouts dataframe
        bouts_dataframe = bouts_dataframe.append(bout_dataframe, ignore_index=True)

        # Increment the bout counter
        bout_counter += 1
    # Sort the bouts dataframe by the interaction frame
    bouts_dataframe = bouts_dataframe.sort_values(by=frame_col)

    # Return the bouts dataframe
    return bouts_dataframe


with h5py.File(filename, "r") as f:
    dset_names = list(f.keys())
    locations = f["tracks"][:].T
    node_names = [n.decode() for n in f["node_names"][:]]
    track_names = f["track_names"][:]
    track_names = track_names.astype("U13")
    track_names = [trk.replace(".0", "") for trk in track_names]

txtname = prefix + "_Ints.txt"
df = pd.read_csv(txtname, sep="\t")
df2 = groupByBouts(df)


df2["Directed"] = "DNE"
for i in np.arange(0, df2.shape[0]):
    if df2["Directed"][i] == "DNE":
        print(i)
        x = df2["Bout"][i]
        Ori = df2["Origin interactor"][i].replace(".0", "")
        Des = df2["Destination interactor"][i].replace(".0", "")
        Frame = df2["Interaction Frame"][i]
        if Frame > timecutoff:
            Oria = track_names.index(Ori)
            Desa = track_names.index(Des)
            Orix1 = locations[Frame, 0, 0, Oria]
            Oriy1 = locations[Frame, 0, 1, Oria]
            Orix2 = locations[Frame - timecutoff, 0, 0, Oria]
            Oriy2 = locations[Frame - timecutoff, 0, 1, Oria]
            Oridisp = euclidean_distance(Orix1, Oriy1, Orix2, Oriy2)
            Desx1 = locations[Frame, 0, 0, Desa]
            Desy1 = locations[Frame, 0, 1, Desa]
            Desx2 = locations[Frame - timecutoff, 0, 0, Desa]
            Desy2 = locations[Frame - timecutoff, 0, 1, Desa]
            Desdisp = euclidean_distance(Desx1, Desy1, Desx2, Desy2)
            if abs(Oridisp - Desdisp) < 65.8:
                df2.loc[df2["Bout"] == x, ["Directed"]] = "No"

            else:
                if Oridisp - Desdisp > 65.8:
                    df2.loc[df2["Bout"] == x, ["Directed"]] = "Yes"
                else:
                    df2.loc[df2["Bout"] == x, ["Directed"]] = "Yes"
                    df2.loc[
                        df2["Bout"] == x,
                        ["Origin interactor", "Destination interactor"],
                    ] = df2.loc[
                        df2["Bout"] == x,
                        ["Destination interactor", "Origin interactor"],
                    ].values

outputname = prefix + "_DirInts.csv"

df2.to_csv(outputname)
