import pandas as pd

# Step 1: Read the CSV file
df = pd.read_csv('experimental_tag_list.csv')

# Step 2: Create empty dictionaries
whitelist = {col: [] for col in df.columns}
queens = {}

# Step 3: Iterate over DataFrame rows
for idx, row in df.iterrows():
    # Step 4: Iterate over the columns
    for col in df.columns:
        # Step 5: If the cell value is 1, add the tag to the whitelist
        if row[col] == 1:
            whitelist[col].append(idx)
        # Step 6: If the cell value is 'queen', add the tag to the queens dictionary
        elif row[col].lower() == 'queen':
            queens[col] = idx

print('Whitelist:', whitelist)
print('Queens:', queens)