FROM rocker/verse
MAINTAINER Zhenmin Li <zhenmin@live.unc.edu>
RUN R -e "install.packages(c('knitr','tidyr','dplyr','readr','ggplot2','tibble','stringr','gridExtra','scales','lubridate','ggrepel','reshape2','kableExtra','tm','wordcloud','wordcloud2','tidytext','textdata','broom','bit64'))"
RUN R -e "install.packages(c('rmarkdown','webshot','htmlwidgets'))"

RUN apt update && apt-get install -y texlive*