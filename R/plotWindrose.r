plotWindrose <-
function (data, spd, dir, spdres = 2, dirres = 30, spdmin = 2, 
    spdmax = 20, spdseq = NULL, palette = "YlGnBu", countmax = NA, 
    debug = 0, title = NULL) 
{
    if (is.numeric(spd) & is.numeric(dir)) {
        data <- data.frame(spd = spd, dir = dir)
        spd = "spd"
        dir = "dir"
    }
    else if (exists("data")) {
    }
    n.in <- NROW(data)
    dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
    data[[spd]][dnu] <- NA
    data[[dir]][dnu] <- NA
    if (missing(spdseq)) {
        spdseq <- seq(spdmin, spdmax, spdres)
    }
    else {
        if (debug > 0) {
            cat("Using custom speed bins \n")
        }
    }
    n.spd.seq <- length(spdseq)
    n.colors.in.range <- n.spd.seq - 1
    spd.colors <- colorRampPalette(brewer.pal(min(max(3, n.colors.in.range), 
        min(9, n.colors.in.range)), palette))(n.colors.in.range)
    if (max(data[[spd]], na.rm = TRUE) > spdmax) {
        spd.breaks <- c(spdseq, max(data[[spd]], na.rm = TRUE))
        spd.labels <- c(paste(c(spdseq[1:n.spd.seq - 1]), "-", 
            c(spdseq[2:n.spd.seq])), paste(spdmax, "-", max(data[[spd]], 
            na.rm = TRUE)))
        spd.colors <- c(spd.colors, "grey50")
    }
    else {
        spd.breaks <- spdseq
        spd.labels <- paste(c(spdseq[1:n.spd.seq - 1]), "-", 
            c(spdseq[2:n.spd.seq]))
    }
    spd.binned <- cut(x = data[[spd]], breaks = spd.breaks, labels = spd.labels, 
        ordered_result = TRUE)
    dir.breaks <- c(-dirres/2, seq(dirres/2, 360 - dirres/2, 
        by = dirres), 360 + dirres/2)
    dir.labels <- c(paste(360 - dirres/2, "-", dirres/2), paste(seq(dirres/2, 
        360 - 3 * dirres/2, by = dirres), "-", seq(3 * dirres/2, 
        360 - dirres/2, by = dirres)), paste(360 - dirres/2, 
        "-", dirres/2))
    dir.binned <- cut(data[[dir]], breaks = dir.breaks, ordered_result = TRUE)
    levels(dir.binned) <- dir.labels
    data$dir.binned <- dir.binned
    if (debug > 0) {
        cat(dir.breaks, "\n")
        cat(dir.labels, "\n")
        cat(levels(dir.binned), "\n")
    }
    p.windrose <- ggplot(data = data, aes(x = dir.binned, fill = spd.binned)) + 
        geom_bar() + scale_x_discrete(drop = FALSE, labels = waiver()) + 
        coord_polar(start = 0 * (pi/180), direction = 1) + scale_fill_manual(name = "Wind Spee d (m/s)", 
        values = spd.colors, drop = FALSE) + theme(axis.title.x = element_blank()) + 
        ggtitle(title)
    if (!is.na(countmax)) {
        p.windrose <- p.windrose + ylim(c(0, countmax))
    }
    print(p.windrose)
    return(p.windrose)
}
