import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
import sleap

mpl.style.use("seaborn-v0_8-deep")
sleap.versions()

metrics = sleap.load_metrics(
    "/Users/wolf/Downloads/drive-download-20240723T134021Z-001/Worker.centered_instance",
    split="val",
)

# It doesn't actually matter which slp file we read since we just need the node names.
labels = sleap.load_file(
    "/Users/wolf/Downloads/drive-download-20240723T134021Z-001/Worker.centered_instance/labels_gt.val.slp"
)
node_names = labels.skeleton.node_names

# Define the specific set of node names you are interested in
specific_nodes = [
    "antenna_l",
    "antenna_l_joint",
    "antenna_r",
    "antenna_r_joint",
    "head",
    "thorax_h",
    "tag",
    "thorax_a",
    "abdomen",
]
# Define a dictionary to map node names to human-readable labels
node_name_map = {
    "antenna_l": "Left Antenna",
    "antenna_l_joint": "Left Scape",
    "antenna_r": "Right Antenna",
    "antenna_r_joint": "Right Scape",
    "head": "Head",
    "thorax_h": "Pronotum",
    "tag": "ArUco Tag",
    "thorax_a": "Petiole",
    "abdomen": "Abdomen",
}

# Filter the DataFrame to include only the specific nodes
dists = pd.DataFrame(metrics["dist.dists"], columns=node_names).melt(
    var_name="Part", value_name="Error"
)
dists = dists[dists["Part"].isin(specific_nodes)]

# Map the node names to human-readable labels
dists["Part"] = dists["Part"].map(node_name_map)

# Convert 'Part' column to a categorical type with the order defined by the keys in node_name_map
dists["Part"] = pd.Categorical(
    dists["Part"], categories=node_name_map.values(), ordered=True
)

fig, ax = plt.subplots(figsize=(8.5, 4.5), dpi=600)

sns.boxplot(data=dists, x="Error", y="Part", fliersize=0, color="#ECCBAE", ax=ax)

sns.stripplot(
    data=dists,
    x="Error",
    y="Part",
    alpha=0.5,
    linewidth=0,
    size=2,
    jitter=0.1,
    color="#046C9A",
    ax=ax,
)

ax.set_title("Worker Model Validation Localization Error (Ground Truth vs Prediction)")
dist_1d = metrics["dist.dists"].flatten()

xmax = np.ceil(np.ceil(np.nanpercentile(dist_1d, 95) / 5) + 1) * 5
ax.set_xlim([0, 40])
ax.set_ylabel("")
ax.set_xlabel("Error (px)")
plt.tight_layout()
fig.savefig("figures/manuscript/si/figure_s1_worker.jpeg", dpi=600)
