# also at https://gist.github.com/benmarwick/f11ae49ab9afde0071b133012ff76cbc

ctv <- "https://raw.githubusercontent.com/benmarwick/ctv-archaeology/master/README.md"

library(tidyverse)
library(glue)

archy_ctv_readme <- readLines(ctv)

# get just the articles
archy_ctv_readme_start <- str_which(archy_ctv_readme, " Publications that include R code")
archy_ctv_readme <-
  archy_ctv_readme[archy_ctv_readme_start:(length(archy_ctv_readme) - 3)]

# get all dates of publication
archy_ctv_readme <- str_remove_all(archy_ctv_readme, "[[:punct:]]")
archy_ctv_readme_20XX <- str_extract(archy_ctv_readme, " 20[[:digit:]]{2} ")
archy_ctv_readme_20XX <- str_squish(unlist(archy_ctv_readme_20XX))
archy_ctv_readme_20XX <- as.numeric(archy_ctv_readme_20XX)
archy_ctv_readme_20XX <- archy_ctv_readme_20XX[!is.na(archy_ctv_readme_20XX)]
number_of_reproducible_articles <- length(archy_ctv_readme_20XX)

# get a few journal names
# get a few journal names
archy_ctv_readme_journals <- str_which(archy_ctv_readme, glue_collapse(archy_ctv_readme_20XX, "|"))
archy_ctv_readme_journals <- archy_ctv_readme[archy_ctv_readme_journals]


journals <- c("Journal of Archaeological Method and Theory",
              "J Archaeol Sci",
              "Journal of Anthropological Archaeology",
              "Journal of Archaeological Science",
              "PLOS",
              "Journal of Human Evolution",
              "Asian Perspectives",
              "Scientific Reports",
              "PLoS ONE",
              "The Holocene",
              "Archaeology in Oceania",
              "Quaternary International",
              "Internet Archaeology",
              "Quaternary Science Reviews",
              "Quaternary International",
              "Journal of Archaeological Science: Reports",
              "Open Quaternary",
              "Evolution and Human Behavior",
              "PaleoAnthropology",
              "Archaeological and Anthropological Sciences",
              "Transactions of the Royal Society B Biological Sciences",
              "Proceedings of the Royal Society B Biological Sciences",
              "Lithic Technology",
              "Journal of Lithic Studies",
              "Journal of Quaternary Science",
              "Royal Society Open Science",
              "Humanities and Social Sciences Communications",
              "Journal of Paleolithic Archaeology",
              # "Radiocarbon",
              "Geosciences",
              "Nature Ecology and Evolution",
              "Frontiers in Earth Science",
              "Boreas",
              "Journal of Computer Applications in Archaeology",
              "European Journal of Archeology",
              "Vegetation History and Archaeobotany",
              "African Archaeological Review",
              "Advances in Archaeological Practice",
              "American Antiquity",
              "Palgrave Communications",
              "Nature communications",
              "Proceedings of the National Academy of Sciences",
              "Archaeological Research in Asia",
              "Kiva",
              "Science Advances",
              "Archaeological and Anthropological Sciences",
              "Proceedings of the National Academy of Sciences",
              "Antiquity"
)

journals_paste <-
  paste0(journals, collapse = "|")

top_archy_journals <-
  tibble( citation = archy_ctv_readme_journals,
          year = archy_ctv_readme_20XX) %>%
  mutate(journal_name = str_extract(tolower(citation), tolower(journals_paste))) %>%
  mutate(journal_name = ifelse(journal_name == "j archaeol sci",
                               "journal of archaeological science",
                               ifelse(journal_name == "plos",
                                      "plos one",
                                      journal_name))) %>%
  mutate(journal_name = str_to_title(journal_name)) %>%
  mutate(journal_name = str_replace(journal_name, "And", "and")) %>%
  mutate(journal_name = str_replace(journal_name, "Of", "of")) %>%
  mutate(journal_name = str_replace(journal_name, "In ", "in ")) %>%
  mutate(journal_name = str_replace(journal_name, "Plos", "PLOS")) %>%
  mutate(journal_name = str_replace_na(journal_name, "Other")) %>%
  mutate(year = factor(year)) %>%
  add_count(journal_name) %>%
  mutate(journal_name = str_glue('{journal_name} (n = {n})'))

archaeology_articles_r_reproducible <-
  ggplot(top_archy_journals,
         aes(year,
             fill = journal_name)) +
  geom_bar(position = "stack") +
  scale_fill_viridis_d(name = paste0("Archaeology papers with R code openly available", " (total of ",
                                     nrow(top_archy_journals),
                                     " articles and chapters)"))  +
  xlab("Year of publication") +
  ylab("Number of articles") +
  theme_bw(base_size = 14) +
  theme(
        axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5),
        legend.text=element_text(size = 10),
        legend.justification = c(0, 1),
        legend.position = c(0.05, 0.98),
        legend.spacing.y = unit(0.25, 'cm'),
        legend.key.size = unit(0.25, "cm")) +
  guides(fill=guide_legend(ncol=2))

archaeology_articles_r_reproducible

ggsave(here::here("figures/papers-per-year.png"),
       w = 15)
