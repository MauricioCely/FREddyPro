removeDuplicates <-
function (data) 
{
    if (any(duplicated(data$timestamp)) == TRUE) {
        print("Duplicate lines found and will be removed", quote = FALSE)
        rowsRemove <- which(duplicated(data$timestamp))
        rowsRemove <- rowsRemove - 1
        rowsRemove <- rowsRemove[!rowsRemove == 0]
        data = data[-rowsRemove, ]
    }
    else {
        print("No duplicate lines found", quote = FALSE)
    }
    return(data)
}
