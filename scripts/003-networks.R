
# co-use network
copkgs <-  all_pkgs_per_paper

# drop empty elements
copkgs <- unname(compact(copkgs))

# transform into network structure
copkgs.unique <- sort(unique(unlist(copkgs)))
bipartite.edges = lapply(copkgs, function(x) {copkgs.unique %in% x})
bipartite.edges = do.call("cbind", bipartite.edges)
rownames(bipartite.edges) = copkgs.unique
mat = bipartite.edges %*% t(bipartite.edges) #bipartite to unimode
mat = mat[order(rownames(mat)), order(rownames(mat))]

library(igraph)
library(statnet)
library(ggnetwork)
library(visNetwork)

statnet = as.network(mat,
                     directed = FALSE,
                     names.eval = "edge.lwd",
                     ignore.eval = FALSE)
# basic plot
ggplot(statnet,
       aes(x = x,
           y = y,
           xend = xend,
           yend = yend)) +
  geom_edges(size = 0.01,
             color = "grey50") +
  geom_nodetext(aes(label = vertex.names),
                size = 2) +
  theme_blank()

# plot with node size and communities
library(intergraph)

g <- asIgraph(statnet)
# attach names
g <- set_vertex_attr(g, "name", index = V(g), value = network.vertex.names(statnet))
g <- set_vertex_attr(g, "size", index = V(g), value = pkgs_total_count$n)

cl <- clusters(g)
# use only the largest cluster
g1 <- induced_subgraph(g, which(cl$membership == which.max(cl$csize)))

# identify communities in network
community <- cluster_walktrap(g1, steps = 20)
# take a look inside at the members of each community
communities(community)

# Attach communities to relevant vertices
V(g1)$color <- community$membership

library(ggraph)

# plot with size and community membership
ggraph(g1,
       layout = "fr"
       ) +
  geom_edge_link(width = 0.1,
                 alpha = 0.4,
                 colour="grey") +
  geom_node_point(aes(color = factor(color),
                      size = size)) +
 geom_node_text(aes(label = name),
                repel = TRUE,
                size = 2,
                segment.colour= "grey90") +
  scale_size(name = "Frequency of use",
             breaks = c(1, 5, 25, 75)) +
  scale_color_discrete(name = "Community") +
  theme_void() +
  theme_graph()

ggsave(here::here("figures/packages-network-with-communities.png"),
       w = 8,
       h = 6)

# adjacency matrix, from https://matthewlincoln.net/2014/12/20/adjacency-matrix-plots-with-r-and-ggplot2.html

# Re-generate dataframes for both nodes and edges, now containing
# calculated network attributes
node_list <- get.data.frame(g1, what = "vertices")

# Determine a community for each edge. If two nodes belong to the
# same community, label the edge with that community. If not,
# the edge community value is 'NA'
edge_list <- get.data.frame(g1, what = "edges") %>%
  inner_join(node_list %>% select(name, color), by = c("from" = "name")) %>%
  inner_join(node_list %>% select(name, color), by = c("to" = "name")) %>%
  mutate(group = ifelse(color.x == color.y, color.x, NA) %>% factor())

# Create a character vector containing every node name
all_nodes <- sort(node_list$name)

# Adjust the 'to' and 'from' factor levels so they are equal
# to this complete list of node names
plot_data <- edge_list %>% mutate(
  to = factor(to, levels = all_nodes),
  from = factor(from, levels = all_nodes))

# Create a character vector of node names sorted by their
# community membership. Here, I rearrange the node_list
# table by the "comm" variable, then extract the
# "name" vector
name_order <- (node_list %>% arrange(color))$name

# Reorder edge_list "from" and "to" factor levels based on
# this new name_order
plot_data <- edge_list %>%
  mutate(
     to = factor(to, levels = name_order),
    from = factor(from, levels = name_order)
  )

# Create the adjacency matrix plot
ggplot(plot_data, aes(x = from, y = to, fill = group)) +
  geom_tile() +
  theme_bw() +
  # Because we need the x and y axis to display every node,
  # not just the nodes that have connections to each other,
  # make sure that ggplot does not drop unused factor levels
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  theme(
    # Rotate the x-axis labels so they are legible
    axis.text.x = element_text(angle = 270,
                               hjust = 0,
                               size = 2),
    axis.text.y = element_text(size = 2),
    # Force the plot into a square aspect ratio
    aspect.ratio = 1,
    # Hide the legend (optional)
    legend.position = "none")

ggsave(here::here("figures/packages-adj-mat-with-communities.png"),
       w = 8,
       h = 6)

asDF(community)




