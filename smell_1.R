# TODO:
# 1. Nauczyć się visNetwork
# 2. Puścić wizualizację na danych które mam używając kilku algosów
# 3. Do grafu dodać kolejne sąsiedztwa

library('visNetwork') 
library(igraph)


# Barabasi vis
net.bg <- sample_pa(10000) 
# V(net.bg)$size <- 8
# V(net.bg)$frame.color <- "white"
# V(net.bg)$color <- "orange"
# V(net.bg)$label <- 1:100 
# E(net.bg)$arrow.mode <- 0


# layout... gives V1, V2 coordinates scaled [-1, 1]
l <- layout_with_fr(net.bg)
plot(net.bg, layout=l)

plot(net.bg)


# convert network to adj matrix
adj_matrix <- as.matrix(as_adj(net.bg))
l <- cmdscale(-adj_matrix, k = 2)
plot(net.bg, layout=l)

# Different results with mds on 1 neighbor matrix


# Visnetwork
# V(net.bg)$label.size = 8


net.bg.visnet <- toVisNetworkData(net.bg, idToLabel = TRUE)

# net.bg.visnet$nodes$size = 300

visNetwork(net.bg.visnet$nodes,
           net.bg.visnet$edges) %>%
  visNodes(size = 50) %>%
  visIgraphLayout(layout = "layout_randomly") -> a#

# igraph nicely
l <- layout_nicely(net.bg)
a$x$nodes$x <- l[,1]
a$x$nodes$y <- l[,2]
a #%>% visNodes(size = 10)

# Custom mds
adj_matrix <- as.matrix(as_adj(net.bg))
l <- cmdscale(-adj_matrix, k = 2)

a$x$nodes$x <- l[,1]
a$x$nodes$y <- l[,2]
a #%>% visNodes(size = 1)

# igraph mds
l <- layout_with_mds(net.bg)
a$x$nodes$x <- l[,1]
a$x$nodes$y <- l[,2]
a #%>% visNodes(size = 10)


# igraph fr
l <- layout_with_lgl(net.bg)
a$x$nodes$x <- l[,1]
a$x$nodes$y <- l[,2]
a %>% 
  visNodes(size = 30) %>%
  visLayout(randomSeed = 10)


# a %>% visSave(file = "export_net.html")


# last fm
load("C:\\Users\\kmatuszelanski\\Desktop\\last_fm_micro-master\\data\\simulation_results.Rdata")
g <- results_list[[1]]



g_v <- toVisNetworkData(g, idToLabel = TRUE)

g_v$nodes[c("listeners", "playcount", "simulation_playcount", "label")] <- NULL
# net.bg.visnet$nodes$size = 300

visNetwork(g_v$nodes,
           g_v$edges) %>%
  visNodes(size = 50) %>%
  visIgraphLayout(layout = "layout_randomly") -> a#

l <- layout_with_fr(g)
a$x$nodes$x <- l[,1]
a$x$nodes$y <- l[,2]
a



