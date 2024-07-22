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

#' Filter Data Based on Tag Membership and Queen Status
#'
#' This function filters a dataframe based on tag membership and queen status. It uses
#' the output of process_tags_to_list_and_queen_per_group to determine which rows to keep.
#'
#' @param tag_file_path The file path of the CSV to read. The CSV should be in the format
#' expected by process_tags_to_list_and_queen_per_group.
#' @param df The dataframe to filter. It should have a 'Col' column containing group names
#' and an 'ID' column containing tag IDs.
#'
#' @return A dataframe containing only the rows where 'ID' is in the list of tag IDs for
#' the corresponding group, and a new 'Queen' column indicating whether each tag is a queen.
#'
#' @examples
#' # Example usage:
#' # tag_file_path <- "path_to_your_file.csv"
#' # df <- data.frame(Col = c("group1", "group2"), ID = c(1, 2))
#' # filtered_df <- filter_data(tag_file_path, df)
#' @import readr
#'
#' @export
filter_data <- function(tag_file_path, df) {
  tags_list <- process_tags_to_list_and_queen_per_group(tag_file_path)
  # Set ID to be only part after # in ID
  df$tag_id <- as.integer(sub(".*#", "", df$ID))

  list_of_dfs <- list()

  for (col in unique(df$Col)) {
    if (col %in% names(tags_list$tags)) {
      list_of_dfs[[col]] <- df[df$Col == col & df$tag_id %in% tags_list$tags[[col]], ]

      # If tag is in $tags$queens[[col]], set Queen to TRUE
      if (col %in% names(tags_list$queens)) {
        list_of_dfs[[col]]$Queen <- list_of_dfs[[col]]$tag_id %in% tags_list$queens[[col]]
      } else {
        list_of_dfs[[col]]$Queen <- FALSE
      }
    }
  }

  filtered_df <- do.call(rbind, list_of_dfs)

  return(filtered_df)
}

process_files <- function(suffix) {
  prefixes <- c("RooibosTea_QR_1216_1646", "RooibosTea_QL_1216_1646", "MexHotChoc_QR_1216_1646", "MexHotChoc_QL_1216_1646", "20230213_1745_AlmdudlerGspritzt_C1", "20230213_1745_AlmdudlerGspritzt_C0", "20221209_1613_QR", "20221209_1613_QL", "20221123_1543_AmericanoLatte_QR", "20221123_1543_AmericanoLatte_QL")
  Day1 <- c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020", "021", "022", "023", "024")
  Day2 <- c("025", "026", "027", "028", "029", "030", "031", "032", "033", "034", "035", "036", "037", "038", "039", "040", "041", "042", "043", "044", "045", "046", "047", "048")
  Day3 <- c("049", "050", "051", "052", "053", "054", "055", "056", "057", "058", "059", "060", "061", "062", "063", "064", "065", "066", "067", "068", "069", "070", "071", "072")
  Day4 <- c("073", "074", "075", "076", "077", "078", "079", "080", "081", "082", "083", "084", "085", "086", "087", "088", "089", "090", "091", "092", "093", "094", "095", "096")
  Days <- c(Day1, Day2, Day3, Day4)
  Start <- 0

  for (i in 1:10) {
    for (j in 1:96) {
      if (file.exists(paste(prefixes[i], Days[j], suffix, sep = "_"))) {
        df <- read.csv(paste(prefixes[i], Days[j], suffix, sep = "_"))
        df$Node <- sub("\\.0", "", df$X)
        df$Day <- floor((j - 1) / 9) + 1
        df$Hour <- Days[j]
        df$Col <- prefixes[i]
        df$QR <- i %% 2 == 1
        df$ID <- paste(df$Col, df$Node, sep = "_")

        if (Start == 1) {
          agg_df <- rbind(agg_df, df)
        }
        if (Start == 0) {
          agg_df <- df
          Start <- 1
        }
      }
    }
  }

  return(agg_df)
}

process_ovaries <- function(filename) {
  Ovaries <- read.csv(filename)

  OvariesRT <- Ovaries[Ovaries$Colony == "RooibosTea", ]
  OvariesRT$ID <- paste("RooibosTea_", OvariesRT$Treatment, "_1216_1646_ArUcoTag#", OvariesRT$Tag, sep = "")

  OvariesMHC <- Ovaries[Ovaries$Colony == "MexicanHotChocolate", ]
  OvariesMHC$ID <- paste("MexHotChoc_", OvariesMHC$Treatment, "_1216_1646_ArUcoTag#", OvariesMHC$Tag, sep = "")

  OvariesAM <- Ovaries[Ovaries$Colony == "ArgentinanMate", ]
  OvariesAM$ID <- paste("20221209_1613_", OvariesAM$Treatment, "_ArUcoTag#", OvariesAM$Tag, sep = "")

  OvariesAG <- Ovaries[Ovaries$Colony == "AlmdudlerGspritzt", ]
  OvariesAG$ID <- paste("20230213_1745_AlmdudlerGspritzt_", OvariesAG$Treatment, "_ArUcoTag#", OvariesAG$Tag, sep = "")

  OvariesAL <- Ovaries[Ovaries$Colony == "AmericanoLatte", ]
  OvariesAL$ID <- paste("20221123_1543_AmericanoLatte_", OvariesAL$Treatment, "_ArUcoTag#", OvariesAL$Tag, sep = "")

  # Merge the data
  Ovaries <- rbind(OvariesRT, OvariesMHC, OvariesAM, OvariesAG, OvariesAL)
  Ovaries$AverageLength <- (Ovaries$LongestOocyteLength1..mm. + Ovaries$LongestOocyteLength2..mm.) / 2
  Ovaries$AverageWidth <- (Ovaries$LongestOocyteWidth1..mm. + Ovaries$LongestOocyteWidth2..mm.) / 2

  return(Ovaries)
}
