#' Calculates mean score for each area of National Academies' framework
#' 
#' @param scores     data frame with institution/program scores
#' @param areas      data frame with area taxonomy for National Academies'
#' @return data frame with average scores for each area in National Academies' 
#'         framework.
area_scores <- function(scores, areas) {
  # Iterate over all areas
  for (area in unique(x = areas$Area)) {
    # Look at one Area
    area.key <- areas$Key[areas$Area == area & areas$Status == "Area"]
    # Use Subarea to identify column names for calculating mean
    subarea.keys <- areas$Key[areas$Area == area & areas$Status == "Subarea"]
    # Catch situation when Area has only one Subarea (Domain)
    if (length(subarea.keys) > 1) {
      scores[, area.key] <- rowMeans(x = scores[, subarea.keys])
    } else {
      scores[, area.key] <- scores[, subarea.keys]
    }
  }
  
  # Now extract only those columns that have means
  drop.cols <- areas$Key[areas$Status == "Subarea"]
  drop.cols <- c(drop.cols, "Row.Type")
  scores <- scores[, -which(colnames(scores) %in% drop.cols)]
  return(scores)
}