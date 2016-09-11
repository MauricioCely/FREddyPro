buildTimestamp <-
function (data, doy) 
{
    dayOfYear = data[[doy]]
    day = floor(dayOfYear)
    hour = floor((dayOfYear - floor(dayOfYear)) * 24)
    minutes = floor((((dayOfYear - floor(dayOfYear)) * 24) - 
        floor((dayOfYear - floor(dayOfYear)) * 24)) * 60)
    seconds = floor((((((dayOfYear - floor(dayOfYear)) * 24) - 
        floor((dayOfYear - floor(dayOfYear)) * 24)) * 60) - floor((((dayOfYear - 
        floor(dayOfYear)) * 24) - floor((dayOfYear - floor(dayOfYear)) * 
        24)) * 60)) * 60)
    timestamp = paste(as.Date(day - 1, origin = "2016-01-01"), 
        " ", sprintf("%02d", hour), ":", sprintf("%02d", minutes), 
        ":", sprintf("%02d", seconds), sep = "")
    spl = cbind(as.POSIXlt(timestamp), as.data.frame(data))
    names(spl) <- c("timestamp", names(as.data.frame(data)))
    return(spl)
}
