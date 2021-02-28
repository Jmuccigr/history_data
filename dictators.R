allcolorsList <- c("gray", "plasma","white","black")
borderColors <- c("black","white")
bcol <- borderColors[as.integer(((allcolorsList == "black"))) + 1]

myFamily <- "Gill Sans"
myFace <- "plain"

cap <- "CC BY-NC 4.0 John D. Muccigrosso"

dictators <- read_csv("./dictators.csv", col_names = c("Year", "D", "ME", "D2", "ME2"))
dictators$Year <- dictators$Year * -1

# Correct the years
yearTable <- read_csv("/Users/john_muccigrosso/Documents/Academic/Research/History/years_corrected.csv", col_names = TRUE)
# Replace the Varronian years with corrected years
# No dictator years, so 4 total years removed, leavin the anarchy alone
dictators <- merge(x = dictators, y = yearTable, by.x = "Year", by.y = "Varronian", all.x = TRUE)
dictators$Year <- dictators$Corrected
dictators <- dictators[,1:5]

plotTitle <- "Occurrence of Dictators during the Roman Republic"
plotSubtitle <- "after removal of the Dictator Years"
xlabel <- "Year BCE (corrected)"
ylabel <- "Count"

startYr <- -500
endYr  <- 0

df <- dictators

intervals <- c(20, 25, 50)

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
            ggplot(df, aes(x = Year)) +
            geom_histogram(
                breaks = seq(startYr, modEndYr, gap),
                color = bcol[i],
                fill = allcolors[startColor:endColor]
            )
        svg(paste("./images/dictators_", gap, "_", allcolorsList[i], ".svg", sep = ""))
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
                )
        )
        dev.off()
    }
}
