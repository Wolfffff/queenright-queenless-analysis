from __future__ import division
from scipy.stats import rv_continuous
from sre_parse import expand_template
import sys
import h5py
import numpy as np
import copy
import math
import pandas as pd
import scipy.ndimage
from tqdm import tqdm
from scipy.signal import savgol_filter
import matplotlib.pyplot as plt
from base64 import b64encode
import seaborn as sns
prefix = sys.argv[1]
filename = prefix + "_HeadButt.csv"
df = pd.read_csv(filename, sep=",")
df_onetype = df[df['Interaction Name'] == "Head_to_Body"]
#df_dir = df_onetype[df_onetype['Directed'] == "Yes"]
df_dir = df_onetype[df_onetype['Headbutt'] == "Yes"]
df_undir = df_onetype[df_onetype['Directed'] == "No"]
counts_ori = pd.DataFrame({'count_ori' : df_dir.groupby( [ "Origin interactor"] ).size()}).reset_index()
counts_des = pd.DataFrame({'count_des' : df_dir.groupby( [ "Destination interactor"] ).size()}).reset_index()
merged= pd.merge(counts_ori, counts_des, left_on='Origin interactor', right_on='Destination interactor', how='outer')
merged['Valency'] = merged['count_ori']/(merged['count_ori'] + merged['count_des'])
output = prefix + "_DegHeadButt.csv"
merged.to_csv(output)
