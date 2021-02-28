allcolorsList <- c("gray", "plasma","white","black")
borderColors <- c("black","white")
bcol <- borderColors[as.integer(((allcolorsList == "black"))) + 1]

myFamily <- "Gill Sans"
myFace <- "plain"

cap <- "CC BY-NC 4.0 John D. Muccigrosso"

temples <- read_csv("~/Documents/github/local/temples/sheet.csv", col_names = TRUE)

# Get temples in Rome with a start date in years BC, but after the legendary period
rometemples <- temples[temples$modernplace == "Rome",]
rometemples <- rometemples[!(is.na(rometemples$startdateearly)),]
rometemples <- rometemples[rometemples$century < 0 & rometemples$century > -7,]

# Whittle the table down a bit
l = c("name", "id","century","startdateearly","startdatelate")
smalltemples <- rometemples[,colnames(rometemples) %in% l]
# Make the missing late dates = early date
smalltemples[is.na(smalltemples$startdatelate),]$startdatelate <- smalltemples[is.na(smalltemples$startdatelate),]$startdateearly

# Correct the years
yearTable <- read_csv("./years_corrected.csv", col_names = TRUE)

# Replace the Varronian years with corrected years
# No dictator years, so 4 total years removed, leaving the anarchy alone
smalltemples <- merge(x = smalltemples, y = yearTable, by.x = "startdateearly", by.y = "Varronian", all.x = TRUE)
smalltemples$startdateearly <- smalltemples$Corrected
smalltemples <- smalltemples[,1:5]
smalltemples <- merge(x = smalltemples, y = yearTable, by.x = "startdatelate", by.y = "Varronian", all.x = TRUE)
smalltemples$startdatelate <- smalltemples$Corrected
# Restore the original column order (not like it matters)
smalltemples <- smalltemples[,c(3,4,5,1,2)]

plotTitle <- "Temple Foundations during the Roman Republic"
plotSubtitle <- "Using probabilities"
xlabel <- "Year BCE (corrected)"
ylabel <- "Count"

startYr <- -510
endYr  <- -10

df <- smalltemples

intervals <- c(25, 50)

# Generate charts for each interval in each color scheme
for (gap in intervals) {
    modEndYr <- endYr + ((startYr - endYr) %% gap)
    colorCt <- floor((modEndYr - startYr) / gap)
    allcolors <-
        c(
            gray.colors(colorCt),
            plasma(colorCt),
            rep("white", colorCt),
            rep("black", colorCt)
        )
    for (i in 1:(length(allcolors) / colorCt)) {
        startColor <- (1 + ((i - 1) * colorCt))
        endColor <- (i) * colorCt
        p <-
            ggplot(df, aes(x = startdateearly)) +
            geom_histogram(
                breaks = seq(startYr, modEndYr, gap),
                color = bcol[i],
                fill = allcolors[startColor:endColor]
            )
        svg(paste("./images/temples_", gap, "_", allcolorsList[i], ".svg", sep = ""))
        plot (
            p +
                theme(
                    text = element_text(family = myFamily, , face = myFace),
                    plot.title = element_text(vjust = 2, size = 16),
                    plot.subtitle = element_text(vjust = 2, size = 12),
                    plot.caption = element_text(vjust = -18, size = 6),
                    axis.title.x = element_text(vjust = -4, size = 14),
                    axis.title.y = element_text(vjust = 4, size = 14),
                    panel.grid.major.x = element_line(linetype = "blank"),
                    panel.grid.minor.x = element_line(linetype = "blank"),
                    axis.text = element_text(size = ifelse(gap < 30, 9, 12)),
                    panel.border = element_rect(linetype = "solid", fill = NA),
                    plot.margin = unit(c(1, 1, 1, 1), "cm")
                ) +
                labs(
                    title = plotTitle,
                    subtitle = plotSubtitle,
                    x = xlabel,
                    y = ylabel,
                    caption = cap
                ) +
                annotate(
                    "text",
                    x = endYr,
                    hjust = 1,
                    y = 0.5 * Inf,
                    vjust = 3,
                    size = 4,
                    label = paste(gap, "Year Intervals", sep = "-"),
                    color = "black",
                    family = myFamily,
                    fontface = myFace
                ) +
                scale_x_continuous(breaks = c(
                    round(ggplot_build(p)$data[[1]]$xmin, 2),
                    max(ggplot_build(p)$data[[1]]$xmin) + gap
                )) +
                scale_y_continuous(
                    breaks = seq(0, 100, 5),
                    minor_breaks = c()
                )        )
        dev.off()
    }
}
