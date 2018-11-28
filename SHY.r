
library(data.table)
dtMerge <- fread('Merge.csv')[, -1]
dtArtist <- fread('Artist.csv')[, -1]

dtMerge[, time_on_board := sum(onboard), by = .(source, Album)]
dtMerge[, serial := ifelse(time_on_board == 0, 1, round(range * sqrt(time_on_board / max(time_on_board))))]

range <- 100
palette = colorRampPalette(c('black', 'red'))(range)
dtMerge[, color := palette[serial], by = source]

dtArtist <- merge(dtArtist, unique(dtMerge[, c('source', 'color')]), all.x = TRUE, by = 'source')

library(igraph)
g <- graph.data.frame(dtArtist, directed = FALSE)
g <- set_edge_attr(g, "weight", value = dtArtist[, weight])
g <- set_vertex_attr(g, 'color', index = V(g), value = dtArtist[, color])
g <- simplify(g, remove.multiple = TRUE)


library(qgraph)
#l <- qgraph.layout.fruchtermanreingold(e, vcount = vcount(g), area = 8 * (vcount(g) ^ 2), repulse.rad = (vcount(g) ^ 3.1))

#e <- get.edgelist(g)
#l <- qgraph.layout.fruchtermanreingold(e, area = 8 * (vcount(g) ^ 2), repulse.rad = (vcount(g) ^ 3.1))

plot(g, vertex.label = NA, layout = layout.kamada.kawai)
