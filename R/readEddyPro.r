readEddyPro <-
function (dataFile, na = "NaN") 
{
    df <- readFile(dataFile, 3, 1, na = na)
    return(df)
}
