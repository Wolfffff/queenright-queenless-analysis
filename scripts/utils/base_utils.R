# Load necessary library
library(readr)

# Step 1: Read the CSV file, all columns as characters
df <- read_csv('meta/experimental_tag_list.csv', col_types = cols(.default = "c"))

# replace empty cells with 0
df[is.na(df)] <- "0"

# Step 2: Create empty lists for whitelist and queens
whitelist <- list()
queens <- list()

# Step 3: Iterate over DataFrame rows
for (i in 1:nrow(df)) {
  # Step 4: Iterate over the columns
  for (j in 2:ncol(df)) {
    # Step 5: If the cell value is 1, add the tag to the whitelist
    if (df[i, j] == '1') {
      whitelist[[colnames(df)[j]]] <- c(whitelist[[colnames(df)[j]]], i)
    }
    # Step 6: If the cell value is 'queen', add the tag to the queens list and whitelist
    if (tolower(df[i, j]) == 'queen') {
      queens[[colnames(df)[j]]] <- i
      whitelist[[colnames(df)[j]]] <- c(whitelist[[colnames(df)[j]]], i)
    }
  }
}

print('Whitelist:')
print(whitelist)
print('Queens:')
print(queens)

# get length of each list
sapply(whitelist, length)