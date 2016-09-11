fillTimestamp <-
function (data, timestamp = "timestamp", flux = FALSE) 
{
    shift_time <- function(df, chg, amount) {
        df = as.POSIXlt(df)
        df[[chg]] <- df[[chg]] + amount
        return(df)
    }
    data = createTimestamp(data, timestamp = timestamp)
    t = invisible(continuity(data, timestamp = timestamp))
    while (!is.character(t)) {
        index = which(data[[timestamp]] == t[1])
        f1 = data[1:(index - 1), ]
        f2 = data[(index):nrow(data), ]
        temprow <- matrix(c(rep.int(NA, length(data))), nrow = 1, 
            ncol = length(data))
        newrow <- data.frame(temprow)
        colnames(newrow) <- colnames(data)
        k <- shift_time(data[[timestamp]][index], "min", -30)
        newrow[[timestamp]] = k
        if (flux) {
            newrow[["date"]] = paste(year(as.character(k)), "-", 
                sprintf("%02d", month(as.character(k))), "-", 
                sprintf("%02d", day(as.character(k))), sep = "")
            newrow[["time"]] = paste(sprintf("%02d", hour(as.character(k))), 
                ":", sprintf("%02d", minute(as.character(k))), 
                sep = "")
            newrow[["DOY"]] = round(yday(as.character(k)) + hour(as.character(k))/24 + 
                minute(as.character(k))/(24 * 60), 3)
        }
        data = rbind(f1, newrow)
        data = rbind(data, f2)
        t = invisible(continuity(data, timestamp = timestamp))
    }
    return(data)
}
