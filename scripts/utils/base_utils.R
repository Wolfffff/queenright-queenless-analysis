library(readr)

#' Process Tags and Identify the "Queen" Per Group from a CSV File
#'
#' This function reads a CSV file where each row represents a tag with its membership
#' in various groups, and possibly marks a tag as the "queen" of its group. It aggregates
#' tag IDs for each group and identifies a "queen" tag if specified.
#'
#' @param file_location The file path of the CSV to read. The first column should be
#' 'Tags', with subsequent columns representing groups. Tags should be integers,
#' and cell values should be '1' (for membership), 'queen' (to designate a queen),
#' or NA/blank (for non-membership).
#'
#' @return A list with two elements:
#' \itemize{
#'   \item{tags}{A named list where each element corresponds to a group containing
#'               the unique tag IDs associated with that group.}
#'   \item{queens}{A named list where each element corresponds to a group containing
#'                 the tag ID designated as 'queen' for that group, if any.}
#' }
#'
#' @examples
#' # Example usage:
#' # file_location <- "path_to_your_file.csv"
#' # processed_data <- process_tags_to_list_and_queen_per_group(file_location)
#' # group_tags <- processed_data$tags
#' # group_queens <- processed_data$queens
#' @import readr
#' 
#' @export
process_tags_to_list_and_queen_per_group <- function(file_location) {
    df <- read_csv(file_location, col_types = cols(Tags = "i", .default = "c"))
    df[is.na(df)] <- "0"
    group_lists <- list()
    group_queens <- list()
    
    for (i in 1:nrow(df)) {
        tag_id <- df[i, "Tags"]
        
        for (j in 2:ncol(df)) {
            column_name <- colnames(df)[j]
            cell_value <- df[i, j]
            
            if (cell_value == "1" || tolower(cell_value) == "queen") {
                if (!is.list(group_lists[[column_name]])) {
                    group_lists[[column_name]] <- list()
                }
                group_lists[[column_name]] <- c(group_lists[[column_name]], tag_id)
            }
            
            if (tolower(cell_value) == "queen") {
                group_queens[[column_name]] <- tag_id
            }
        }
    }
    
    simplified_group_lists <- lapply(group_lists, function(tags_list) {
        unique(unlist(tags_list))
    })
    
    return(list(tags = simplified_group_lists, queens = group_queens))
}
