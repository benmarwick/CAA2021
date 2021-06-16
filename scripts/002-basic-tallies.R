
all_pkgs_per_paper_tbl_clean <-
  enframe(all_pkgs_per_paper) %>%
  mutate(the_paper = the_paper,
         the_year = as.numeric(str_extract( basename(the_paper), "\\d{4}"))) %>%
  unnest(value) %>%
  filter(nchar(value) >= 2)

# most popular pkgs overall
pkgs_total_count <-
  all_pkgs_per_paper_tbl_clean %>%
  group_by(value)   %>%
  count(value)

pkgs_total_count_sorted <-
  pkgs_total_count %>%
  arrange(desc(n))

top_10_pkgs <- pkgs_total_count_sorted$value[1:10]
top_20_pkgs <- pkgs_total_count_sorted$value[1:20]
top_30_pkgs <- pkgs_total_count_sorted$value[1:30]
top_50_pkgs <- pkgs_total_count_sorted$value[1:50]

top_50_pkgs_labelled <-
  tribble(~value, ~label,
"ggplot2"     ,   "visualisation",
"knitr"       ,   "writing",
"dplyr"       ,   "data manipulation",
"devtools"    ,   "utilities",
"raster"      ,   "spatial",
"tidyverse"   ,   "data manipulation",
"rgdal"       ,   "spatial",
"plyr"        ,   "data manipulation",
"scales"      ,   "visualisation",
 "sp"          ,  "spatial",
 "reshape2"    ,  "data manipulation",
 "gridExtra"   ,  "visualisation",
 "bookdown"    ,  "writing",
 "git2r"       ,  "utilities",
 "maptools"    ,  "spatial",
 "ggpubr"      ,  "visualisation",
 "tidyr"       ,  "data manipulation",
 "cowplot"     ,  "visualisation",
 "ggrepel"     ,  "visualisation",
 "here"        ,  "file management",
 "RColorBrewer",  "visualisation",
 "rmarkdown"   ,   "writing",
 "grid"        ,  "visualisation",
 "sf"          ,  "spatial",
 "spatstat"    ,  "spatial",
 "stringr"     ,  "data manipulation",
 "readr"       ,  "file management",
 "readxl"      ,  "file management",
 "viridis"     ,  "visualisation",
 "vegan"       ,  "statistics",
 "rcarbon"     ,  "radiocarbon",
 "FactoMineR"  ,  "statistics",
 "rgeos"       ,  "spatial",
 "Bchron"      ,   "radiocarbon",
 "Hmisc"       ,  "statistics",
 "magrittr"    ,  "data manipulation",
 "broom"       ,  "statistics",
 "doParallel"  ,   "parallel",
 "factoextra"  ,  "statistics",
 "foreach"     ,   "parallel",
 "parallel"    ,   "parallel",
 "tibble"      ,   "data manipulation",
 "utils"       ,   "utilities",
 "cluster"     ,   "statistics",
 "ggforce"     ,  "visualisation",
 "kableExtra"  ,   "writing",
 "MASS"        ,   "statistics",
 "purrr"       ,  "data manipulation",
 "zoo"         ,   "statistics",
 "datatable"   ,   "data manipulation"
)

pkgs_total_count_sorted_labelled <-
pkgs_total_count_sorted %>%
  filter(value %in% top_50_pkgs) %>%
  left_join(top_50_pkgs_labelled)

legend_order  <-
  levels(with(pkgs_total_count_sorted_labelled,
              reorder(label, -n)))


ggplot(pkgs_total_count_sorted_labelled) +
  aes(reorder(value, n),
      n,
     fill = label) +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 20) +
  scale_fill_viridis_d(breaks = legend_order,
                       name = "Package purpose") +
  theme( legend.position = c(0.65, 0.4),
         legend.text=element_text(size = 40),
         legend.spacing.y = unit(0.15, 'cm'),
         legend.background = element_rect(fill = "white",
                                          colour = "white"), # get rid of legend bg
         legend.box.background = element_rect(fill = "white",
                                              colour = "white")) +
  labs(x = "", y = "Number of papers")

ggsave(here::here("figures/packages-all-years-rank-labelled.png"),
       w = 15,
       h = 13)

#--------------------------------------------------------------------

# animated ranking by year plot
# https://evamaerey.github.io/little_flipbooks_library/racing_bars/racing_barcharts.html
ranked_by_year <-
  all_pkgs_per_paper_tbl_clean %>%
  filter(value %in% top_30_pkgs) %>%
  group_by(the_year, value) %>%
  tally() %>%
  arrange(the_year, -n) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 20)

ranked_by_year %>%
  ggplot() +
  aes(xmin = 0 ,
      xmax = n) +
  aes(ymin = rank - .45,
      ymax = rank + .45,
      y = rank) +
  facet_wrap(~ the_year) +
  geom_rect(alpha = .7) +
  aes(fill = value) +
  scale_fill_viridis_d(option = "viridis",
                       direction = -1) +
  scale_x_continuous(
    limits = c(-10, 15),
    breaks = seq(-10, 15, 5)) +
  geom_text(col = "gray13",
            hjust = "right",
            aes(label = value),
            x = -0.5,
            size = 5
  ) +
  scale_y_reverse() +
  labs(fill = NULL) +
  labs(x = 'Uses per year') +
  labs(y = "") +
  guides(fill = FALSE) +
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" )
  ) ->
  staticplot

library(gganimate)

# one approach
animated_plot <-
  staticplot +
  facet_null() +
  labs(title = "Most frequently used R packages in archaeology ({round(frame_time, 0)})") +
  aes(group = value) +
  gganimate::transition_time(the_year)

animate(animated_plot,
        nframes = 1000,
        duration = 60,
        end_pause = 100)

anim_save(here::here("figures/packages-by-years-rank-animation.gif"),
       w = 15,
       h = 13)

# another approach
# https://www.r-bloggers.com/2020/01/how-to-create-bar-race-animation-charts-in-r/


# pkgs per year
all_pkgs_per_paper_tbl_clean %>%
  filter(value %in% top_50_pkgs) %>%
  group_by(the_year, value) %>%
  tally() %>%
  left_join(pkgs_total_count_sorted %>%
              mutate(n_tot = n) %>%
              select(-n)) %>%
  ggplot() +
  aes(the_year,
      n,
      colour = value,
      size = n_tot) +
  geom_line() +
  theme_minimal()

#----------------------------------------------------------------------------------
# pkgs per paper
pkgs_per_paper <-
  all_pkgs_per_paper_tbl_clean %>%
  group_by( the_paper) %>%
  tally()

median_number_of_pkgs <- median(pkgs_per_paper$n)

ggplot(pkgs_per_paper) +
  aes(
      n) +
  geom_histogram() +
  annotate("text",
           x = 30,
           y = 15,
           label = paste0("Median number of\nR pkgs per paper is ", median_number_of_pkgs),
           size = 20) +
  theme_minimal(base_size = 30) +
  labs(x = "Number of R packages",
       y = "Frequency of papers")

ggsave(here::here("figures/packages-per-paper.png"),
       w = 15,
       h = 13)

#----------------------------------------------------------------------------------

# pkgs per paper per year
pkgs_per_paper_per_year <-
all_pkgs_per_paper_tbl_clean %>%
  group_by(the_year,
           the_paper) %>%
  tally() %>%
  mutate(pkgs_per_paper = mean(n))

library(ggbeeswarm)

ggplot(pkgs_per_paper_per_year) +
  aes(as.factor(the_year),
      n) +
  geom_boxplot() +
  geom_quasirandom(size = 5,
                   alpha = 0.2) +
  theme_minimal(base_size = 30) +
  labs(y = "Number of R packages per paper",
       x = "Year")

ggsave(here::here("figures/packages-per-paper-per-year.png"),
       w = 15,
       h = 13)



#----------------------------------------------------------------------------------

# papers using R but no pkgs
  enframe(all_pkgs_per_paper) %>%
    mutate(the_paper = the_paper,
           the_year = as.numeric(str_extract( basename(the_paper), "\\d{4}"))) %>%
   mutate(hm_pkgs = map_int(value, ~length(.x)),
          zero = ifelse(hm_pkgs == 0, "zero", "non-zero")) %>%
   group_by(name, zero) %>%
   tally() %>%
   group_by(name, zero) %>%
   summarise(n = sum(n)) %>%
   mutate(freq = n / sum(n)) %>%
   filter(zero == 'non-zero') %>%
    ggplot() +
    aes(as.factor(name),
        freq) +
    geom_point(size = 5) +
    ylim(0,1) +
    theme_minimal() +
    labs(y = "Proportion of R-using paper that use packages",
         x = "Year")

ggsave(here::here("figures/no-packages-per-paper-per-year.png"),
       w = 5,
       h = 4)



