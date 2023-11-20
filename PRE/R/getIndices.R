#' @title Get the indices related to an index
#' 
#' @param data A data frame such as [`measurements`] with the following variables: `date`, `column`, and `depth`.
#' @param i The index of the row of interest.
#' 
#' @export
getIndices <- function(data, i, parameters) {
        with(parameters, {
                
                # get the index of the current depth
                j <- which(depths==data[i,"depth"])
                
                # create a mask of the current column
                c <- data[,"column"] == data[i,"column"]
                
                # create a mask of the current data
                d <- data[,"date"] == data[i,"date"]
                
                # save indices of the top and bottom measurements
                i_top <- which(c & d & data[,"depth"] == depths[j-1])
                i_bottom <- which(c & d & data[,"depth"] == depths[j+1])
                
                # return a list with the relevant indices
                return(list(j = j, i_top = i_top, i_bottom = i_bottom))  
        })
}
