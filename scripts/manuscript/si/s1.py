import sleap
import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns

mpl.style.use("seaborn-v0_8-deep")
sleap.versions()

metrics = sleap.load_metrics(
    "/Users/wolf/Documents/macro_training/macros_gt/models/240609_123307_outputstride.centered_instance",
    split="val",
)
print("\n".join(metrics.keys()))


print("Error distance (50%):", metrics["dist.p50"])
print("Error distance (90%):", metrics["dist.p90"])
print("Error distance (95%):", metrics["dist.p95"])

plt.figure(figsize=(8.5, 4.25), dpi=600, facecolor="w")
sns.histplot(
    metrics["dist.dists"].flatten(),
    binrange=(0, 20),
    kde=True,
    kde_kws={"clip": (0, 20)},
    stat="probability",
)
plt.xlabel("Localization error (px)")
plt.savefig("figures/queen_localization_hist.jpeg")

plt.figure(figsize=(8.5, 4.25), dpi=600, facecolor="w")
sns.histplot(
    metrics["oks_voc.match_scores"].flatten(),
    binrange=(0, 1),
    kde=True,
    kde_kws={"clip": (0, 1)},
    stat="probability",
)
plt.xlabel("Object Keypoint Similarity")
plt.savefig("figures/queen_oks.jpeg")


plt.figure(figsize=(5.5, 5.5), dpi=600, facecolor="w")
for precision, thresh in zip(
    metrics["oks_voc.precisions"][::2], metrics["oks_voc.match_score_thresholds"][::2]
):
    plt.plot(
        metrics["oks_voc.recall_thresholds"],
        precision,
        "-",
        label=f"OKS @ {thresh:.2f}",
    )
plt.xlabel("Recall")
plt.ylabel("Precision")
plt.legend(loc="lower left")
plt.savefig("figures/queens_precision_recall.jpeg")

print("mAP:", metrics["oks_voc.mAP"])
print("mAR:", metrics["oks_voc.mAR"])

# It doesn't actually matter which slp file we read since we just need the node names.
labels = sleap.load_file(
    "/Users/wolf/Documents/macro_training/macros_gt/models/240609_123307_outputstride.centered_instance/labels_gt.val.slp"
)
node_names = labels.skeleton.node_names

fig, ax = plt.subplots(dpi=600)

dists = pd.DataFrame(metrics["dist.dists"], columns=node_names).melt(
    var_name="Part", value_name="Error"
)

# Get only parts in the set we are using
# ...

# Map naming to human readable names
# ...

sns.boxplot(data=dists, x="Error", y="Part", fliersize=0, ax=ax)

sns.stripplot(
    data=dists, x="Error", y="Part", alpha=0.25, linewidth=1, jitter=0.2, ax=ax
)

ax.set_title("Validation Localization Error (Ground Truth vs Prediction)")
dist_1d = metrics["dist.dists"].flatten()

xmax = np.ceil(np.ceil(np.nanpercentile(dist_1d, 95) / 5) + 1) * 5
ax.set_xlim([0, xmax])
ax.set_xlabel("Error (px)")
fig.savefig("figures/manuscript/si/queen_sleap_metrics.jpeg", dpi=600)
