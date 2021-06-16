
# loop over each year and make a network plot for each year
copkgs <-  all_pkgs_per_paper

the_plots <- vector("list", length = length(unique(names(copkgs) )))
the_metrics <- vector("list", length = length(unique(names(copkgs) )))
the_modularity <- vector("list", length = length(unique(names(copkgs) )))
years <- sort(as.numeric(unique(names(copkgs))))


for(i in seq_along(unique(names(copkgs))) ) {


  this_set_of_papers <-   keep(copkgs, names(copkgs) %in% min(years):years[i])

  # drop empty elements
  this_set_of_papers <- compact(this_set_of_papers)

  this_set_pkgs_total_count <-
    enframe(this_set_of_papers) %>%
    unnest(value) %>%
    group_by(value)   %>%
    count(value)

  # transform into network structure
  this_set_of_papers.unique <- sort(unique(unlist(this_set_of_papers)))
  bipartite.edges = lapply(this_set_of_papers, function(x) {this_set_of_papers.unique %in% x})
  bipartite.edges = do.call("cbind", bipartite.edges)
  rownames(bipartite.edges) = this_set_of_papers.unique
  mat = bipartite.edges %*% t(bipartite.edges) #bipartite to unimode
  mat = mat[order(rownames(mat)), order(rownames(mat))]

  this_statnet = as.network(mat, directed = FALSE,
                       names.eval = "edge.lwd",
                       ignore.eval = FALSE)

  this_g <- asIgraph(this_statnet)
  # attach names
  this_g <- set_vertex_attr(this_g, "name", index = V(this_g), value = network.vertex.names(this_statnet))
  this_g <- set_vertex_attr(this_g, "size", index = V(this_g), value = this_set_pkgs_total_count$n)

  # plot with size and community membership
  the_plots[[i]] <-
  ggraph(this_g,
         layout = "fr"
  ) +
    geom_edge_link(width=0.1,
                   alpha = 0.4,
                   colour="grey") +
    geom_node_point(aes(
                       # size = size,
                        colour = size
                        )) +
    scale_colour_viridis_c() +
    geom_node_text(aes(label = name),
                   repel = TRUE,
                   bg.r = 0.1,
                   bg.color = "white",
                   size = 2,
                   max.overlaps = 10,
                   segment.colour= "grey90") +
    ggtitle(paste0("R packages used by archaeologists during ", min(years), " - ", years[i])) +
    theme_void() +
    theme_graph()

  # get stats
 the_metrics[[i]] <-
 left_join(
  enframe(igraph::degree(this_g),
          name = "pkg",
          value = "degree"),
  enframe(igraph::betweenness(this_g,
                              directed = FALSE),
          name = "pkg",
          value = "betweenness")
 )


 the_modularity[[i]] <-  igraph::modularity(this_g,  igraph::multilevel.community(this_g)$membership)

}

# #the_plots

names(the_metrics) <- years
network_metrics <- bind_rows(the_metrics, .id = 'end_year')
network_metrics_long <-
  network_metrics %>%
  select(-pkg) %>%
  pivot_longer(-end_year)

library(ggbeeswarm)
ggplot(network_metrics_long) +
  aes(end_year,
      value) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1) +
  scale_y_log10() +
  facet_wrap( ~ name,
              scales = "free_y",
              ncol = 1) +
  theme_minimal() +
  labs(y = "", x = "")

ggsave(here::here("figures/network-stats-over-time.png"),
       w = 5,
       h = 3)

modularity_tbl <-
tibble(years = years,
       modularity = map_dbl(the_modularity, ~.x))

ggplot(modularity_tbl) +
  aes(years,
      modularity) +
  geom_point(size = 3) +
  theme_minimal() +
  labs( x = "")

ggsave(here::here("figures/network-mod-over-time.png"),
       w = 5,
       h = 3)



