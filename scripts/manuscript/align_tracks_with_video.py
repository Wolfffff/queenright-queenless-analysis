# Define the file path
import h5py
import numpy as np

# Define the file path
file_path = (
    "/Users/wolf/20230213_1745_AlmdudlerGspritzt_C1_008_8_8_3_1_10_interp_filtered.h5"
)

# file_path = '/Users/wolf/labels_test.slp'

# Step 1: Open the HDF5 file in read-write mode
with h5py.File(file_path, "r+") as file:
    # Load the "tracks" dataset
    original_tracks = file["tracks"]

    # Get the shape of the "tracks" dataset
    original_shape = original_tracks.shape

    # Create a new dataset with the same shape, filled with zeros
    new_tracks = np.zeros(original_shape, dtype=original_tracks.dtype)

    # Copy the data from the original dataset to the new dataset starting from the second position
    # This effectively shifts all tracks one position to the right and leaves the first position as zeros
    new_tracks[..., 1:] = original_tracks[..., :-1]

    # Delete the original "tracks" dataset
    del file["tracks"]

    # Create a new "tracks" dataset with the modified data
    file.create_dataset("tracks", data=new_tracks)
