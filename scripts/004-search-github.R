library(rvest)
# there are 13 pages of results, at the time I did this search
url <- "https://github.com/search?o=desc&q=%22archaeology%22+filename%3ADESCRIPTION+language%3AR&s=updated&type=Repositories"

gh_search_results <- vector("list", length = 13)
for(i in seq_along(gh_search_results)){

  this_page <- paste0(url, "&p=", i)
  print(paste0("Now getting page ", i))
  # to avoid rate limits
  Sys.sleep(30)

  gh_search_results[[i]] <-
    this_page %>%
    read_html() %>%
    html_elements(".v-align-middle") %>%
    html_text2()

}

gh_search_results_clean <- str_remove_all(unlist(gh_search_results), "↵")
gh_search_results_clean <- gh_search_results_clean[!gh_search_results_clean == ""] #

# check if repo is a package by scraping repo contents list
gh_search_results_clean

# get repo creation dates
# https://stackoverflow.com/questions/23611669/how-to-find-the-created-date-of-a-repository-project-on-github
# https://towardsdatascience.com/accessing-data-from-github-api-using-r-3633fb62cb08

#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you
myapp <- oauth_app(appname = "caa2021",
                   key = "282047a5006df29646cf",
                   secret = "9738ae12b797759a3ccacc04a3a9069df36a16b3")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)

gh_search_results_clean_json_data <- vector("list", length = length(gh_search_results_clean))

for(i in seq_along(gh_search_results_clean)){

  print(paste0("Now getting data from GitHub for repo ", gh_search_results_clean[i]))

  req <- GET(paste0("https://api.github.com/repos/", gh_search_results_clean[i]), gtoken)

  # Take action on http error
  stop_for_status(req)

  # Extract content from a request
  json1 = content(req)

  # Convert to a data.frame
  gh_search_results_clean_json_data[[i]] <- jsonlite::fromJSON(jsonlite::toJSON(json1))

}

# get info out of that messy list
gh_search_results_clean_json_data_tbl <-
tibble(
  repo = gh_search_results_clean,
  created_at =
  map_chr(gh_search_results_clean_json_data,
    ~.x$created_at),
  updated_at =
  map_chr(gh_search_results_clean_json_data,
        ~.x$updated_at),
  stargazers_count =
  map_int(gh_search_results_clean_json_data,
        ~.x$stargazers_count),
  size =
map_int(gh_search_results_clean_json_data,
        ~.x$size)
)


#--------------------------
# get the file list for each repo so we can see if it is a pkg, e.g. has DESCRIPTION file, etc

gh_search_results_pk_check <- vector("list", length = length(gh_search_results_clean))

for(i in seq_along(gh_search_results_pk_check)){

  this_page <- paste0("https://github.com/",  gh_search_results_clean[i])
  print(paste0("Now getting repo ", this_page))
  # to avoid rate limits
  # Sys.sleep(30)

  gh_search_results_pk_check[[i]] <-
    this_page %>%
    read_html() %>%
    html_elements(".js-navigation-open") %>%
    html_text2()

}

# search through them for DESCRIPTION file

idx_desc <- map_lgl(gh_search_results_pk_check, ~any(str_detect(.x, "DESCRIPTION")))
gh_search_results_pks <- sort(gh_search_results_clean[idx_desc])

idx_lisc <- map_lgl(gh_search_results_pk_check, ~any(str_detect(.x, "LICENSE")))
gh_search_results_pk_lics <- sort(gh_search_results_clean[idx_lisc])

idx_contr <- map_lgl(gh_search_results_pk_check, ~any(str_detect(.x, "CONTRIBUTING")))
gh_search_results_pk_idx_contr <- sort(gh_search_results_clean[idx_contr])

idx_ving <- map_lgl(gh_search_results_pk_check, ~any(str_detect(.x, "vignettes")))
gh_search_results_pks_vign <- sort(gh_search_results_clean[idx_ving])

idx_analy <- map_lgl(gh_search_results_pk_check, ~any(str_detect(.x, "analysis|paper|manuscript")))
gh_search_results_pks_analy <- sort(gh_search_results_clean[idx_analy])

#-------------------------------------------------

# subset GitHub data for the pkgs
gh_search_results_clean_json_data_tbl_pkgs <-
gh_search_results_clean_json_data_tbl %>%
  filter(repo %in% gh_search_results_pks) %>%
  tidyr::separate(col = repo,
                  sep = "/",
                  into = c("user", "repo1"),
                  remove = FALSE)

library(ggrepel)
gh_search_results_clean_json_data_tbl_pkgs_tally <-
gh_search_results_clean_json_data_tbl_pkgs %>%
  group_by(user) %>%
  summarise(mean_stars = mean(stargazers_count),
            total_repos = n())


  ggplot(gh_search_results_clean_json_data_tbl_pkgs_tally) +
  aes(mean_stars,
      total_repos) +
  geom_point() +
  geom_text_repel(aes(label = user),
                  size = 5,
                  force = 10,
                  max.time = 5,
                  max.iter = 1e8,
                  segment.color = "grey90",
                  max.overlaps = 30) +
  theme_minimal(base_size = 12) +
  xlim(-5, 35) +
  ylim(-5, 10) +
  labs(x = "Average number of stars per repo",
       y = "Total number of package repositories") +
    annotate("text",
             x = 25,
             y = 7,
             size = 8,
             label = paste0(nrow(gh_search_results_clean_json_data_tbl_pkgs), " packages\nby ",
                            nrow(gh_search_results_clean_json_data_tbl_pkgs_tally), " users"))

  ggsave(here::here("figures/github-users-stars-pkgs.png"),
         w = 7,
         h = 5)

#-------------------------------------------------
  # date of creation and last update
  library(lubridate)
  gh_search_results_clean_json_data_tbl_pkgs %>%
    mutate(`Package repo created` = year(created_at)) %>%
    mutate(`Package repo last updated` = year(updated_at)) %>%
    select(repo,
           `Package repo created`,
           `Package repo last updated`) %>%
    pivot_longer(-repo) %>%
    ggplot() +
    aes(value) +
    geom_bar()+
    facet_wrap( ~ name,
                scales = "free_y",
                ncol = 1) +
    theme_bw(base_size = 14)

  ggsave(here::here("figures/github-pkgs-year-create-update.png"),
         w = 7,
         h = 5)

#-------------------------------------------------
# what do the packages do?

gh_pkg_labels <-
    tribble(
      ~pkg, ~label,

      "tesselle/arkhe"                               ,  "manipulation",
      "crp2a/gammaShiny"                             ,  "dating",
      "crp2a/gamma"                                  ,  "dating",
      "parkgayoung/racisminarchy"                    ,  "compendium",
      "tesselle/kairos"                              ,  "dating",
      "tesselle/tabula"                              ,  "analysis",
      "ropensci/c14bazAAR"                           ,  "dating",
      "cran/mortAAR"                                 ,  "analysis",
      "ISAAKiel/mortAAR"                             ,  "analysis",
       "tesselle/nexus"                              ,  "analysis",
       "tesselle/folio"                               ,  "data",
       "DCPollard94/knossoscemeteries"                ,  "compendium",
       "josephlewis/leastcostpath"                    , "analysis",
       "cran/arkhe"                                   , "manipulation",
       "cran/tabula"                                  , "analysis",
       "joeroe/c14"                                   ,  "dating",
       "davidcorton/archSeries"                       ,  "dating",
       "cran/archeofrag"                              ,  "analysis",
       "sebastien-plutniak/archeofrag"                ,  "analysis",
       "lsteinmann/idaifieldR"                        ,  "manipulation",
       "maciejkasinski/quantatools"                   ,  "analysis",
       "joeroe/swapdata"                              ,  "data",
       "arliph/SPARTAAS"                              ,  "analysis",
       "lsteinmann/datplot"                           ,  "visualization",
       "tsdye/allen.archaeology"                      ,  "analysis",
       "cran/SPARTAAS"                                ,  "analysis",
       "cran/folio"                                   ,  "data",
       "benmarwick/mjbnaturepaper"                    ,  "compendium",
       "joeroe/stratigraphr"                          ,  "dating",
       "joeroe/era"                                   ,  "dating",
       "frederic-santos/rdss"                         ,  "analysis",
       "joeroe/rintchron"                             ,  "dating",
       "lsteinmann/clayringsmiletus"                  ,  "compendium",
       "ISAAKiel/pathAAR"                             ,  "analysis",
       "benmarwick/evoarchdata"                       ,  "data",
       "ISAAKiel/quantAAR"                            ,  "analysis",
       "ISAAKiel/aoristAAR"                           ,  "dating",
       "ISAAKiel/shapAAR"                             ,  "analysis",
       "Andros-Spica/cerUB"                           ,  "analysis",
       "f-silva-archaeo/skyscapeR"                    ,  "compendium",
       "benmarwick/signatselect"                      ,  "analysis",
       "ArchaeoStat/ArchaeoChron"                     ,  "dating",
       "nevrome/cultrans.bronzeageburials.article2019",  "compendium",
       "cornelmpop/Lithics3D"                         ,  "analysis",
       "eScienceCenter/SiteExploitationTerritories"   ,  "compendium",
       "Johanna-Mestorf-Academy/sdsanalysis"          ,  "data",
       "Johanna-Mestorf-Academy/sdsbrowser"           ,  "data",
       "ercrema/HERAChp.KandlerCrema"                 ,  "compendium",
       "wccarleton/lamap"                             ,  "compendium",
       "benmarwick/confschedlr"                       ,  "analysis",
       "benmarwick/ktc11"                             ,  "compendium",
       "TimoBremer/R_Strat"                           ,  "analysis",
       "benmarwick/1989-excavation-report-Madjedbebe" ,  "compendium",
       "benmarwick/olympicdamboundaries"              ,  "compendium",
       "benmarwick/binford"                           ,  "data",
       "ISAAKiel/magAAR"                              ,  "analysis",
    )

  gh_pkg_labels %>%
    group_by(label) %>%
    tally(sort = TRUE) %>%
  ggplot() +
  aes(reorder(label, n),
      n) +
  geom_col() +
    theme_minimal(base_size = 18) +
    coord_flip() +
    labs(x = "")

  ggsave(here::here("figures/github-pkgs-purpose.png"),
         w = 7,
         h = 5)

  #-------------------------------------------------
  # do they have pkg contents?

  gh_search_results_pks_tbl <-
    tibble(pkg =   gh_search_results_pks,
           desc = 1)

  gh_search_results_pk_lics_tbl <-
    tibble(pkg =   gh_search_results_pk_lics,
           lic = 1)

  gh_search_results_pk_idx_contr_tbl <-
    tibble(pkg = gh_search_results_pk_idx_contr,
           contr = 1)

  gh_search_results_pks_vign_tbl <-
    tibble(pkg =  gh_search_results_pks_vign,
           vign = 1)

  # combine
  full_join(gh_search_results_pk_lics_tbl,
            gh_search_results_pk_idx_contr_tbl) %>%
    full_join(gh_search_results_pks_vign_tbl) %>%
    full_join(gh_search_results_pks_tbl) %>%
    filter(desc == 1) %>%
    select(pkg,
           license = lic,
           Contributing = contr,
           Vignette = vign) %>%
    pivot_longer(-pkg) %>%
    group_by(name) %>%
    summarise(count = sum(value, na.rm = TRUE)) %>%
    ggplot() +
    aes(name, count) +
    geom_col() +
    labs(x = "") +
    theme_minimal(base_size = 18)

  ggsave(here::here("figures/github-pkgs-contents.png"),
         w = 7,
         h = 5)


  #-------------------------------------------------
  # are they used by archaeologists? Only 5

  gh_search_results_pks_tbl %>%
    separate(pkg, c("user", "value"), remove = FALSE) %>%
    left_join(all_pkgs_per_paper_tbl_clean) %>%
    filter(!is.na(the_paper))



