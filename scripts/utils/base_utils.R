library(readr)

process_tags_to_list_and_queen_per_group <- function(file_location) {
    # Read the CSV file, specifying the first column (Tags) as integers and others as characters
    df <- read_csv(file_location, col_types = cols(Tags = "i", .default = "c"))
    
    # Replace empty cells with 0
    df[is.na(df)] <- "0"
    
    # Initialize empty lists to store aggregated tag IDs and queens for each category
    category_lists <- list()
    category_queens <- list()
    
    # Iterate over DataFrame rows
    for (i in 1:nrow(df)) {
        tag_id <- df[i, "Tags"]
        
        # Iterate over the columns (starting from the second one, as the first is 'Tags')
        for (j in 2:ncol(df)) {
            column_name <- colnames(df)[j]
            cell_value <- df[i, j]
            
            # Aggregate tag IDs based on cell value ('1' or 'queen')
            if (cell_value == "1" || tolower(cell_value) == "queen") {
                # Check if the category already has a list initiated for aggregated tags
                if (!is.list(category_lists[[column_name]])) {
                    category_lists[[column_name]] <- list()
                }
                # Append the tag ID to the category's list
                category_lists[[column_name]] <- c(category_lists[[column_name]], tag_id)
            }
            
            # Specifically handle 'queen' values
            if (tolower(cell_value) == "queen") {
                # Assign or overwrite the queen number for the category
                category_queens[[column_name]] <- tag_id
            }
        }
    }
    
    # Simplify the structure by ensuring each category has a single list of tag IDs
    simplified_category_lists <- lapply(category_lists, function(tags_list) {
        unique(unlist(tags_list))
    })
    
    # The result is a list containing both the aggregated tag IDs and queens for each category
    return(list(tags = simplified_category_lists, queens = category_queens))
}

# Example usage with the correct path to your CSV file
file_location <- "meta/experimental_tag_list.csv"
processed_data <- process_tags_to_list_and_queen_per_group(file_location)

# Accessing the lists
category_tags <- processed_data$tags
category_queens <- processed_data$queens