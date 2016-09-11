addSiteName <-
function (data, name) 
{
    for (i in 1:nrow(data)) {
        data$site[i] = name
    }
    return(data)
}
cleanSecondVar <-
function (x, y, data) 
{
    clean <- which(is.na(data[[x]]) & !is.na(data[[y]]))
    data[[y]][clean] <- NA
    return(data)
}
cleanVar <-
function (x, data, lessThan = NULL, greaterThan = NULL) 
{
    clean <- which(data[[x]] >= greaterThan | data[[x]] <= lessThan)
    data[[x]][clean] <- NA
    return(data)
}
cleanVarG <-
function (data, x, y = NULL, greaterThan = NULL) 
{
    clean <- which(data[[x]] >= greaterThan)
    if (is.null(y)) {
        data[[x]][clean] <- NA
    }
    else {
        data[[y]][clean] <- NA
    }
    return(data)
}
cleanVarL <-
function (data, x, y = NULL, lessThan = NULL) 
{
    clean <- which(data[[x]] <= lessThan)
    if (is.null(y)) {
        data[[x]][clean] <- NA
    }
    else {
        data[[y]][clean] <- NA
    }
    return(data)
}
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
distClean <-
function (var, hour, df) 
{
    m <- aggregate(var ~ hour, data = df, mean)
    q <- as.data.frame(t(as.data.frame(t(aggregate(var ~ hour, 
        data = df, quantile, prob = c(0.05, 0.95))))))
    p <- merge(m, q, "hour")
    names(p)[c(3, 4)] <- c("quant5", "quant95")
    for (i in 1:nrow(p)) {
        var[which(hour == p$hour[i])] <- ifelse(var[which(hour == 
            p$hour[i])] < p$quant5[i], NA, var[which(hour == 
            p$hour[i])])
        var[which(hour == p$hour[i])] <- ifelse(var[which(hour == 
            p$hour[i])] > p$quant95[i], NA, var[which(hour == 
            p$hour[i])])
    }
    return(var)
}
midpoints <-
function (x, dp = 2) 
{
    lower <- as.numeric(gsub(",.*", "", gsub("\\(|\\[|\\)|\\]", 
        "", x)))
    upper <- as.numeric(gsub(".*,", "", gsub("\\(|\\[|\\)|\\]", 
        "", x)))
    return(round(lower + (upper - lower)/2, dp))
}
percentNA <-
function (var) 
{
    if (any(is.na(var))) {
        na = which(is.na(var))
        pc <- round((length(na)/length(var)) * 100, 2)
    }
    else {
        print("No NAs found. You lucky person!", quote = FALSE)
    }
}
percentQC <-
function (data, var = "co2_flux", write = FALSE, dir = "./") 
{
    qc = paste(paste(deparse(substitute(data)), "$", "qc_", var, 
        sep = ""))
    v = paste(paste(deparse(substitute(data)), "$", var, sep = ""))
    qcflag0 <- which(eval(parse(text = qc)) == 0)
    qcflag1 <- which(eval(parse(text = qc)) == 1)
    qcflag2 <- which(eval(parse(text = qc)) == 2)
    gap. <- which(is.na(eval(parse(text = v))))
    prc0.all = round((length(qcflag0)/(length(eval(parse(text = v))))) * 
        100, 2)
    prc1.all = round((length(qcflag1)/(length(eval(parse(text = v))))) * 
        100, 2)
    prc2.all = round((length(qcflag2)/(length(eval(parse(text = v))))) * 
        100, 2)
    prcGap <- round((length(gap.)/length(eval(parse(text = v)))) * 
        100, 2)
    if (write) {
        path = paste(dir, "/percent_qc_flags.txt", sep = "")
        sink(path)
        print(paste(prc0.all, "%", "of QC 0 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prc1.all, "%", "of QC 1 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prc2.all, "%", "of QC 2 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prcGap, "%", "of gap for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        sink()
    }
    else {
        print(paste(prc0.all, "%", "of QC 0 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prc1.all, "%", "of QC 1 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prc2.all, "%", "of QC 2 for", var, "in the dataset", 
            sep = " "), quote = FALSE)
        print(paste(prcGap, "%", "of gap for", var, "in the dataset", 
            sep = " "), quote = FALSE)
    }
}
polar2cart <-
function (x, y, dist, bearing, as.deg = FALSE) 
{
    if (as.deg) {
        bearing = bearing * pi/180
    }
    newx <- x + dist * sin(bearing)
    newy <- y + dist * cos(bearing)
    return(list(x = newx, y = newy))
}
qcClean <-
function (var, qcVar, qc) 
{
    if (length(qc) != 1) {
        for (i in 1:length(qc)) {
            qc.index <- which(qcVar == qc[i])
            var[qc.index] <- NA
        }
    }
    else {
        qc.index <- which(qcVar == qc)
        var[qc.index] <- NA
    }
    return(var)
}
readEddyPro <-
function (dataFile, na = "NaN") 
{
    df <- readFile(dataFile, 3, 1, na = na)
    return(df)
}
readFile <-
function (dataFile, nSkip = 0, nSkipNames = 0, header = FALSE, 
    sep = ",", na = "NaN") 
{
    if (nSkip == 0) 
        header = TRUE
    df <- read.table(dataFile, header = header, skip = nSkip, 
        sep = sep)
    df[df == na] <- NA
    if (nSkip > 0) {
        n <- read.table(dataFile, header = TRUE, skip = nSkipNames, 
            nrows = 1, sep = sep, fill = TRUE)
        names(df) <- names(n)
    }
    return(df)
}
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
removeOutliers <-
function (x, na.rm = TRUE, ...) 
{
    qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, 
        ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}
sdClean <-
function (var, p) 
{
    positive <- which(var > 0)
    negative <- which(var < 0)
    m.positive <- mean(var[positive], na.rm = TRUE)
    m.negative <- mean(var[negative], na.rm = TRUE)
    sd.positive <- sd(var[positive], na.rm = TRUE)
    sd.negative <- sd(var[negative], na.rm = TRUE)
    filter.positive <- m.positive + p * sd.positive
    filter.negative <- m.negative - p * sd.negative
    rm.positive <- which(var > filter.positive)
    rm.negative <- which(var < filter.negative)
    var[rm.positive] <- NA
    var[rm.negative] <- NA
    return(var)
}
