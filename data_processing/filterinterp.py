import h5py
from typing import Dict, List, Union
import numpy as np
import sys
prefix = sys.argv[1]
filename = prefix + "_analysis.h5"

import copy
import math
import scipy.stats as stats
import pandas as pd
import scipy.ndimage
from tqdm import tqdm
from scipy.signal import savgol_filter
import matplotlib.colors as colors
import logging
import skvideo.io
import matplotlib.pyplot as plt
from base64 import b64encode

def diff(node_loc, diff_func=np.gradient, **kwargs):
    """
    node_loc is a [frames, 2] arrayF

    win defines the window to smooth over

    poly defines the order of the polynomial
    to fit with

    """
    node_loc_vel = np.zeros_like(node_loc)
    for c in range(node_loc.shape[-1]):
        node_loc_vel[:, c] = diff_func(node_loc[:, c], **kwargs)

    node_vel = np.linalg.norm(node_loc_vel, axis=1)

    return node_vel

def flatten_features(x, axis=0):

    if axis != 0:
        # Move time axis to the first dim
        x = np.moveaxis(x, axis, 0)

    # Flatten to 2D.
    initial_shape = x.shape
    x = x.reshape(len(x), -1)

    return x, initial_shape

def unflatten_features(x, initial_shape, axis=0):
    # Reshape.
    x = x.reshape(initial_shape)

    if axis != 0:
        # Move time axis back
        x = np.moveaxis(x, 0, axis)

    return x

def smooth_median(x, window=5, axis=0, inplace=False):
    if axis != 0 or x.ndim > 1:
        if not inplace:
            x = x.copy()

        # Reshape to (time, D)
        x, initial_shape = flatten_features(x, axis=axis)

        # Apply function to each slice
        for i in range(x.shape[1]):
            x[:, i] = smooth_median(x[:, i], window, axis=0)

        # Restore to original shape
        x = unflatten_features(x, initial_shape, axis=axis)
        return x

    y = scipy.signal.medfilt(x.copy(), window)
    y = y.reshape(x.shape)
    mask = np.isnan(y) & (~np.isnan(x))
    y[mask] = x[mask]
    return y

def fill_missing(x, kind="nearest", axis=0, **kwargs):
    """Fill missing values in a timeseries.

    Args:
        x: Timeseries of shape (time, ...) or with time axis specified by axis.
        kind: Type of interpolation to use. Defaults to "nearest".
        axis: Time axis (default: 0).

    Returns:
        Timeseries of the same shape as the input with NaNs filled in.

    Notes:
        This uses pandas.DataFrame.interpolate and accepts the same kwargs.
    """
    if x.ndim > 2:
        # Reshape to (time, D)
        x, initial_shape = flatten_features(x, axis=axis)

        # Interpolate.
        x = fill_missing(x, kind=kind, axis=0, **kwargs)

        # Restore to original shape
        x = unflatten_features(x, initial_shape, axis=axis)

        return x
    return pd.DataFrame(x).interpolate(method=kind, axis=axis, **kwargs).to_numpy()

def instance_node_velocities(fly_node_locations, start_frame, end_frame):
    frame_count = len(range(start_frame, end_frame))
    if len(fly_node_locations.shape) == 4:
        fly_node_velocities = np.zeros(
            (frame_count, fly_node_locations.shape[1], fly_node_locations.shape[3])
        )
        for fly_idx in range(fly_node_locations.shape[3]):
            for n in range(0, fly_node_locations.shape[1]):
                fly_node_velocities[:, n, fly_idx] = diff(
                    fly_node_locations[start_frame:end_frame, n, :, fly_idx]
                )
    else:
        fly_node_velocities = np.zeros((frame_count, fly_node_locations.shape[1]))
        for n in range(0, fly_node_locations.shape[1] - 1):
            fly_node_velocities[:, n] = diff(
                fly_node_locations[start_frame:end_frame, n, :]
            )

    return fly_node_velocities

def smooth_gaussian(x, std=1, window=5, axis=0, inplace=False):
    if axis != 0 or x.ndim > 1:
        if not inplace:
            x = x.copy()

        # Reshape to (time, D)
        x, initial_shape = flatten_features(x, axis=axis)

        # Apply function to each slice
        for i in range(x.shape[1]):
            x[:, i] = smooth_gaussian(x[:, i], std, window, axis=0)

        # Restore to original shape
        x = unflatten_features(x, initial_shape, axis=axis)
        return x

    y = (
        pd.DataFrame(x.copy())
        .rolling(window, win_type="gaussian", center=True)
        .mean(std=std)
        .to_numpy()
    )
    y = y.reshape(x.shape)
    mask = np.isnan(y) & (~np.isnan(x))
    y[mask] = x[mask]
    return y

def velfilt(locs, thresh):
     filledlocs = fill_missing(locs[:,:,:,:], kind="pad")
     vels = instance_node_velocities(filledlocs, 1, locs.shape[0])
     velsbool = vels > thresh
     return velsbool

def coordfilt(velsbool, limit):
    sums = np.sum(velsbool, axis=1)
    coord1dbool = sums >= limit
    coordbool = np.stack((coord1dbool,coord1dbool,coord1dbool,coord1dbool,coord1dbool,coord1dbool,coord1dbool,coord1dbool,coord1dbool), axis=1) #FIX THIS EVENTUALLY
    return(coordbool)

def integratedfilter(locations, velsbool, extvelsbool, coordbool):
    uncoordvelsbool = velsbool & ~coordbool
    finalbool = uncoordvelsbool + extvelsbool
    coordsaved = np.sum(velsbool) - np.sum(uncoordvelsbool) 
    print("Vel Outliers Saved by Coord: ", coordsaved)
    final2dbool = np.stack((finalbool,finalbool), axis=2)
    print("Final Filter: ", np.sum(finalbool))
    locationsfilter = copy.deepcopy(locations) 
    locationsfilter = locationsfilter[1:,:,:,:]
    previousnans = np.isnan(locationsfilter[:,:,0,:])
    previousandpresent = previousnans & finalbool
    locationsfilter[final2dbool] = np.nan
    return(locationsfilter)

def shrink_locs(locations, node, track1, track2):
    filt1 = ~np.isnan(locations[:,node_names.index(node),0,track1])
    filt2 = ~np.isnan(locations[:,node_names.index(node),0,track2])
    filt = filt1 & filt2
    array1 = locations[filt, node_names.index(node), :, track1]
    array2 = locations[filt, node_names.index(node), :, track2]
    distances = np.linalg.norm(array1 - array2, axis=1)
    return distances

def removelargeedges (locations, frame_start, frame_end, zscore_threshold = 5):
    for track_pos in range(locations.shape[3]):
        for edge_nodeA in range(locations.shape[1]):
            for edge_nodeB in range(locations.shape[1]):
                if edge_nodeA < edge_nodeB:
                    array1 = locations[frame_start:frame_end, edge_nodeA, :, track_pos]
                    array2 = locations[frame_start:frame_end, edge_nodeB, :, track_pos]
                    distances = np.linalg.norm(array1-array2, axis=1)
                    zscore_array = stats.zscore(distances, nan_policy = 'omit')
                    locations[frame_start:frame_end, edge_nodeA, :, track_pos][zscore_array > zscore_threshold] = np.nan
                    locations[frame_start:frame_end, edge_nodeB, :, track_pos][zscore_array > zscore_threshold] = np.nan
    return locations

def removeSharedNodes (locations, frame_start, frame_end, shared_dist_threshold = 50):    
    for track_posA in range(locations.shape[3]):        
        for track_posB in range(locations.shape[3]):
            for nodeA in range(locations.shape[1]):
                for nodeB in range(locations.shape[1]):
                    if track_posA < track_posB:
                        array1 = locations[frame_start:frame_end, nodeA, :, track_posA]
                        array2 = locations[frame_start:frame_end, nodeB, :, track_posB]
                        distance_array = np.linalg.norm(array1-array2, axis=1)
                        locations[frame_start:frame_end,nodeA,:,track_posA][distance_array < shared_dist_threshold] = np.nan
                        locations[frame_start:frame_end,nodeB,:,track_posB][distance_array < shared_dist_threshold] = np.nan
    return locations


def process_tags_to_list_and_queen_per_group(
    file_location: str,
) -> Dict[str, Union[Dict[str, List[int]], Dict[str, int]]]:
    """
    Process tags to list and queen per group.

    Args:
        file_location (str): The location of the CSV file to process.

    Returns:
        dict: A dictionary with two keys: 'tags' and 'queens'.
              'tags' maps from group names to lists of tag IDs.
              'queens' maps from group names to queen tag IDs.
    """
    df = pd.read_csv(file_location, dtype={"Tags": int})
    df = df.fillna("0")
    group_lists = {}
    group_queens = {}

    for i in range(len(df)):
        tag_id = df.loc[i, "Tags"]

        for j in range(1, len(df.columns)):
            column_name = df.columns[j]
            cell_value = df.loc[i, column_name]

            if cell_value == "1" or cell_value == "queen":
                if column_name not in group_lists:
                    group_lists[column_name] = []
                group_lists[column_name].append(tag_id)

            if cell_value == "queen":
                group_queens[column_name] = tag_id

    simplified_group_lists = {k: list(set(v)) for k, v in group_lists.items()}

    return {"tags": simplified_group_lists, "queens": group_queens}

X = process_tags_to_list_and_queen_per_group(sys.argv[2])

with h5py.File(filename, "r") as f:
    dset_names = list(f.keys())
    locations = f["tracks"][:].T
    node_names = [n.decode() for n in f["node_names"][:]]
    track_names = f["track_names"][:]
    point_scores = f["point_scores"][:]
    instance_scores = f["instance_scores"][:]
    track_occupancy = f["track_occupancy"][:]
    tracking_scores = f["tracking_scores"][:]   


print("===locations data shape===")
print(locations.shape)
print()

skeletalset = (0,1,2,3,4,5,14,15,16)
node_names = node_names[0:6] + node_names[14:17]
locations = locations[:,skeletalset,:,:]
print(node_names)
track_names = track_names.astype('U13')
track_names = [trk.replace('.0', '') for trk in track_names]
Tagindex = prefix[:prefix.rfind('_')]
Tagindex = '/'.join(Tagindex.split('/')[1:])
Tags = X['tags'][Tagindex]
formatted_list = ['ArUcoTag#' + str(num) for num in Tags]
indices = sorted([track_names.index(track) for track in formatted_list if track in track_names])
locations = locations[:,:,:,indices]
px_mm = 15.5
sum_of_defined_nodes = np.apply_over_axes(np.sum, ~np.isnan(locations[:, :, 0, :]), [0,2])
velparam = 10
coordparam = 8
edgeparam = 5
spaceparam = 1
interpparam = 10
print(velparam)
print(coordparam)
print("Prefilter: ", np.sum(sum_of_defined_nodes[:]))
velsbool = velfilt(locations,  velparam)
coordbool = coordfilt(velsbool, coordparam)
print("Vels Identified: ", np.sum(velsbool))
print("Coord Identified: ", np.sum(coordbool))
extvelsbool = velfilt(locations, 100)
print("Extreme Vels Filtered: ", np.sum(extvelsbool))
ylocationscut = locations[1:,:,:,:]
integfilter = integratedfilter(locations, velsbool,extvelsbool,coordbool)
sum_of_defined_filtered_nodes = np.apply_over_axes(np.sum, ~np.isnan(integfilter[:, :, 0, :]), [0,2])
print("PostVelFilter:", np.sum(sum_of_defined_filtered_nodes[:]))
tracks = integfilter.T
edgefilterlocs = removelargeedges(integfilter, 0, 71999, zscore_threshold=edgeparam)
sum_of_defined_filtered_nodes = np.apply_over_axes(np.sum, ~np.isnan(edgefilterlocs[:, :, 0, :]), [0,2])
print("PostEdgeFilter:", np.sum(sum_of_defined_filtered_nodes[:]))
spacefilterlocs = removeSharedNodes(edgefilterlocs, 0, 71999, shared_dist_threshold=spaceparam)
sum_of_defined_filtered_nodes = np.apply_over_axes(np.sum, ~np.isnan(spacefilterlocs[:, :, 0, :]), [0,2])
print("PostSpaceFilter:", np.sum(sum_of_defined_filtered_nodes[:]))
interpdlocs = fill_missing(spacefilterlocs[:,:,:,:], kind="linear", limit=interpparam)
sum_of_defined_interp_nodes = np.apply_over_axes(np.sum, ~np.isnan(interpdlocs[:, :, 0, :]), [0,2])
print("PostInterp:", np.sum(sum_of_defined_interp_nodes[:]))
tracks = interpdlocs.T
name = prefix + "_" + str(velparam) + "_" + str(coordparam) + "_" + str(edgeparam) + "_" + str(spaceparam) + "_" + str(interpparam) + "_interp_filtered.h5"
with h5py.File(name, "w") as f:    
    f.create_dataset("instance_scores", data=instance_scores)
    f.create_dataset("node_names", data=node_names)
    f.create_dataset("point_scores", data=point_scores)
    f.create_dataset("track_names", data=track_names)
    f.create_dataset("track_occupancy", data=track_occupancy)
    f.create_dataset("tracking_scores", data=tracking_scores)
    f.create_dataset("tracks", data=tracks)
    f.close()
