createTimestamp <-
function (data, timestamp = NULL) 
{
    if (is.null(timestamp)) {
        data$timestamp <- paste(data$date, data$time, sep = " ")
        data$timestamp <- as.POSIXct(data$timestamp, tz = "UTC")
        data$year <- year(data$timestamp)
        data$yday <- yday(data$timestamp)
        data$hour <- hour(data$timestamp) + minute(data$timestamp)/60
        data$month <- month(data$timestamp)
    }
    else {
        data[[timestamp]] <- as.POSIXct(data[[timestamp]], tz = "UTC")
        data$year <- year(data[[timestamp]])
        data$yday <- yday(data[[timestamp]])
        data$hour <- hour(data[[timestamp]]) + minute(data[[timestamp]])/60
        data$month <- month(data[[timestamp]])
    }
    return(data)
}
