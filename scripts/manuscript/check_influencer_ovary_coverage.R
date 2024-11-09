# Load the behavioral dataset containing bee metrics
source("scripts/manuscript/load_data.R")

# Find influencers missing ovary index measurements
# Returns unique bee IDs for influencers with NA ovary index values
unique_influencers_without_ovary_idx <- bds_means %>%
  filter(Infl == 1) %>%                  # Select only influencer bees
  filter(is.na(ovary_idx)) %>%          # Keep only those with missing ovary index
  distinct(Bee)                          # Get unique bee IDs

# Display influencers missing ovary measurements
unique_influencers_without_ovary_idx


# Find influencers with valid ovary index measurements  
# Returns unique bee IDs for influencers with recorded ovary index values
unique_influencers_with_ovary_idx <- bds_means %>%
  filter(Infl == 1) %>%                  # Select only influencer bees
  filter(!is.na(ovary_idx)) %>%         # Keep only those with valid ovary index
  distinct(Bee)                          # Get unique bee IDs

# Display influencers with ovary measurements
print(unique_influencers_with_ovary_idx)
