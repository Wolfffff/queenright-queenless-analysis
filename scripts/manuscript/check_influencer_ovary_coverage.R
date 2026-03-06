# Load the behavioral dataset containing bee metrics
source("scripts/manuscript/load_data.R")

# Find hub bees missing ovary index measurements
# Returns unique bee IDs for hub bees with NA ovary index values
unique_hub_bees_without_ovary_idx <- bds_means %>%
  filter(Infl == 1) %>%                  # Select only hub bees
  filter(is.na(ovary_idx)) %>%          # Keep only those with missing ovary index
  distinct(Bee)                          # Get unique bee IDs

# Display hub bees missing ovary measurements
unique_hub_bees_without_ovary_idx


# Find hub bees with valid ovary index measurements
# Returns unique bee IDs for hub bees with recorded ovary index values
unique_hub_bees_with_ovary_idx <- bds_means %>%
  filter(Infl == 1) %>%                  # Select only hub bees
  filter(!is.na(ovary_idx)) %>%         # Keep only those with valid ovary index
  distinct(Bee)                          # Get unique bee IDs

# Display hub bees with ovary measurements
print(unique_hub_bees_with_ovary_idx)
