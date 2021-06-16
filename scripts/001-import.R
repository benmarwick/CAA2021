library(tidyverse)
library(fs)
library(here)
library(readtext)
library(pdftools)

# for each paper, we want to get all the code and treat it as one unit
the_paper <- dir_ls(here("data")) # each item here is one paper

the_plain_text_for_the_paper <- vector("list", length = length(the_paper))

for(i in seq_along(the_paper)) {

   # to help with debugging...
   print(paste0("Now working on ", basename(the_paper[[i]])))

 if( is_file(the_paper[i]) ){
   the_plain_text_for_the_paper[[i]] <- paste0(readLines(the_paper[i], warn = FALSE), collapse = " ")

 } else {

   the_files_for_this_paper <-
     dir_ls(the_paper[i] ,
          recurse = TRUE,
          glob = ".r$|.R$|.rmd$|.RMD$|.Rmd$|.Rnw$|.txt$|.pdf$|.PDF$|.doc$|.DOC$|.docx$|.DOCX$")

   # don't want anything packrat or renv
   the_files_for_this_paper <- the_files_for_this_paper[!str_detect(the_files_for_this_paper, "packrat|renv")]

   # deal with plain text formats
   plain_text_for_this_paper <-
     the_files_for_this_paper[str_detect(the_files_for_this_paper, ".r$|.R$|.rmd$|.RMD$|.Rmd$|.txt$")]

   # read in all  plain text into one character vector
   plain_text_for_this_paper_as_txt <-
     map(plain_text_for_this_paper,
         ~readLines(.x, warn = FALSE)) %>%
     unlist() %>%
     paste0(., collapse = " ")

   # deal with PDFs
   pdfs_for_this_paper <-
     the_files_for_this_paper[str_detect(the_files_for_this_paper, ".pdf$|.PDF$")]

   # read in all PDFs into one character vector
   pdfs_for_this_paper_as_txt <-
     map(pdfs_for_this_paper,
         ~paste0(pdf_text(.x),
                 collapse = " ")) %>%
     unlist() %>%
     paste0(., collapse = " ")

   # deal with word docs
   docs_for_this_paper <-
     the_files_for_this_paper[str_detect(the_files_for_this_paper, ".doc$|.DOC$|.docx$|.DOCX$")]

   # read in all word docs into one character vector
   docs_for_this_paper_as_txt <-
     map(docs_for_this_paper,
         ~readtext(.x) %>%
           .$text %>%
           paste0(., collapse = " ")) %>%
           unlist() %>%
           paste0(., collapse = " ")

   # deal with HTML
   html_for_this_paper <-
      the_files_for_this_paper[str_detect(the_files_for_this_paper, ".html$|.HTML$")]

   # read in all HTML into one character vector
   htmls_for_this_paper_as_txt <-
      map(html_for_this_paper,
          ~  read_html(.x) %>%
             html_text()) %>%
      unlist() %>%
      paste0(., collapse = " ")

   # combine text from files of various formats
   the_plain_text_for_the_paper[[i]] <-
      paste0(plain_text_for_this_paper_as_txt,
             pdfs_for_this_paper_as_txt,
             docs_for_this_paper_as_txt,
             htmls_for_this_paper_as_txt,
             collapse = " ")


 }


}

#---------------------------------------------------------------

source(here("scripts/999-get-pkgs-fn.R"))

all_pkgs_per_paper <-
map(the_plain_text_for_the_paper,
    ~get_pkgs(.x)) %>%
  map( ~ str_remove_all(.x, "[:punct:]")) %>%
  map( ~.x[.x != ""]) %>%
   # remove junk
  map( ~.x[!.x %in% c("2020",
                      "pquietly=Tcharacteronly=T",
                      "fitting devtoolsinstallgithubnspopecorMLPE",
                      "fitting\ndevtoolsinstallgithubnspopecorMLPE",
                      "845",
                      "which",
                      "oo",
                      "rela",
                      "importFrom")])

names(all_pkgs_per_paper) <- as.numeric(str_extract( basename(the_paper), "\\d{4}"))

#---------------------------------------------------------------



# Co-mention network of R packages: Scientific impact and clustering structure
#  https://doi.org/10.1016/j.joi.2017.12.001

# papers per year

# number pkgs per year

# pkgs/paper per year

# % papers with no pkg per year

# top 10 pkgs and change over time

# Co-mention network of all R packages

# pkg clusters

# archaeologist pkgs vs other pkgs
