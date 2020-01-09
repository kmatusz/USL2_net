
library('visNetwork') 
library(igraph)
library(tidyverse)

#' Dragging one point (simulation)
nodes <- tribble(~id, ~color, ~label,
                1, NA, NA,
                2, NA, NA,
                3, "red", "Drag me",
                4, NA, NA
                )

edges <- tribble(~from, ~to,
                 1, 2,
                 2, 3,
                 1, 3,
                 3, 4
                 )

visNetwork(nodes, edges)
