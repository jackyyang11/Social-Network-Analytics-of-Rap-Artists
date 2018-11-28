library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(MASS)
library(proxy)
library(igraph)
library(scales)
library(plm)
library(pglm)

#setwd("C:/Users/Jacky Yang/Desktop/Social Network/final project/2016_rap_network_analysis-master/data")

#rich<-read.csv('top100 richest.csv')
albumrank <- read.csv('billboard_rap_chart_albums_cleaned.csv')
artist <- read.csv('artist_pair_counts.csv')

twoway <- as.data.frame(cbind(as.character(artist$target), as.character(artist$source), artist$weight, as.character(artist$type)))
colnames(twoway) <- c("source", "target", "weight", "type")
artist <- rbind(artist, twoway)
write.csv(data.table(artist), 'Artist.csv')

g <- graph.data.frame(artist, directed = FALSE)
g <- set_edge_attr(g, "weight", value = artist$weight)

plot(g, vertex.label = NA)

#join album
join <- left_join(artist, albumrank, by = c("source" = "Artist"))

# Function 1 : For ploting missing value
plot_missing <- function(data, title = NULL, ggtheme = theme_gray(), theme_config = list("legend.position" = c("bottom"))) {
    ## Declare variable first to pass R CMD check
    feature <- num_missing <- pct_missing <- group <- NULL
    ## Check if input is data.table
    is_data_table <- is.data.table(data)
    ## Detect input data class
    data_class <- class(data)
    ## Set data to data.table
    if (!is_data_table) data <- data.table(data)
    ## Extract missing value distribution
    missing_value <- data.table(
    "feature" = names(data),
    "num_missing" = sapply(data, function(x) { sum(is.na(x)) })
  )
    missing_value[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
    missing_value[, pct_missing := num_missing / nrow(data)]
    missing_value[pct_missing < 0.05, group := "Good"]
    missing_value[pct_missing >= 0.05 & pct_missing < 0.4, group := "OK"]
    missing_value[pct_missing >= 0.4 & pct_missing < 0.8, group := "Bad"]
    missing_value[pct_missing >= 0.8, group := "Remove"][]
    ## Set data class back to original
    if (!is_data_table) class(missing_value) <- data_class
    ## Create ggplot object
    output <- ggplot(missing_value, aes_string(x = "feature", y = "num_missing", fill = "group")) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(100 * pct_missing, 2), "%"))) +
    scale_fill_manual("Group", values = c("Good" = "#1a9641", "OK" = "#a6d96a", "Bad" = "#fdae61", "Remove" = "#d7191c"), breaks = c("Good", "OK", "Bad", "Remove")) +
    scale_y_continuous(labels = comma) +
    coord_flip() +
    xlab("Features") + ylab("Number of missing rows") +
    ggtitle(title) +
    ggtheme + theme_linedraw() +
    do.call(theme, theme_config)
    ## Print plot
    print(output)
    ## Set return object
    return(invisible(missing_value))
}
plot_missing(join)

join[is.na(join)] <- 0

#degree
degree <- as.data.frame(degree(g))
#get the name of 
id = V(g)$name
degree <- as.data.table(list(artist = id, eigen = degree))
#betweenness
betweenness <- as.data.frame(betweenness(g))
#get the name 
id = V(g)$name
between <- as.data.table(list(artist = id, between = betweenness))
#close
close <- as.data.frame(closeness(g))
#get the name 
id = V(g)$name
close <- as.data.table(list(artist = id, close = close))
#calculate eigen vector
eigen <- as.data.table(eigen_centrality(g)$vector)
#get the name of the investor
id = V(g)$name
eigen <- as.data.table(list(artist = id, eigen = eigen))

#join centrality tables
merge1 <- left_join(join, degree, by = c("source" = "artist"))
merge1 <- left_join(merge1, between, by = c("source" = "artist"))
merge1 <- left_join(merge1, close, by = c("source" = "artist"))
merge1 <- left_join(merge1, eigen, by = c("source" = "artist"))

dtMerge <- data.table(merge1)

write.csv(dtMerge, 'Merge.csv')









