# Value dissonance in research(er) assessment: Individual and institutional priorities in review, promotion and tenure criteria related to research quality, quantity, openness and responsibility

This repository holds code and data for the preprint "Value dissonance in research(er) assessment: Individual and institutional priorities in review, promotion and tenure criteria related to research quality, quantity, openness and responsibility". 

Not all raw data is shared, to avoid re-identification of participants. We 
provide a cleaned version of the data with qualitative answers and country
information removed.


## Reproducible code
The analytic pipeline for generating tables and figures is reproducible through the
`targets` package. Running `tar_make()` will rebuild all notebooks. The initial
pipeline for data pre-processing involved multiple steps, however, they are not
available for reproduction because we cannot openly share the original raw data.

The pipeline is specified in the file `_targets.R`. You can visualise the
dependencies with the following code:

```r
targets::tar_visnetwork()
```


All packages that are used during the analysis are specified in `R/packages.R`.
For the analysis files to render you will need to install the font "Hind" (for
example from [Google Fonts](https://fonts.google.com/)) and
register it with
[`extrafont`](https://cran.r-project.org/web/packages/extrafont/README.html).
See the file `load_fonts.R` for specific instructions.


### Code files

The code to reproduce all values and figures can be found in five RMarkdown 
documents, located in the folder `analysis-notebooks`:

- 01-demography.Rmd
- 02-institutional-context.Rmd
- 03-attitudes-towards-promotion-criteria.Rmd
- 04-opinion-vs-practice.Rmd
- 05-research-vs-policies.Rmd

Rendered versions are available as `.md`-files (which are displayed on GitHub)
or `.html` files which can be viewed locally.

Note that this repository does not contain data necessary to reproduce 
`01-demography.Rmd`. To protect the identities of our participants, this data
was removed from the public files.

Further core files for the analysis pipeline's reproducibility are the following:

|File name                        |Purpose                                                                    |
|:-------------------------------|:--------------------------------------------------------------------------|
|_targets.R                      |Core file for building the analysis. Run with `targets::tar_make()`.|
|R/packages.R                    |Lists all packages relevant to the analysis.                 |
|R/functions.R                   |Functions that wrap preprocessing tasks as outlined in `_targets.R`.|
|R/helpers.R                     |Some helper functions that are re-used throughout the analysis.|

## Data description
Procedures for collecting the data are outlined in the preprint. 

### Overview of data files

|File name                        |Purpose                                                               |
|:-------------------------------|:---------------------------------------------------------------------|
|data/external/world_bank_country_classification.xlsx |Categorisations of countries into world regions. Obtained from the [World Bank](https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups), and made available under CC-BY.|
|data/processed/shareable_data.csv|This is the main data file that underlies the analysis. A description of variables is provided in the next file.|
|data/processed/var_overview.csv |Provides full column names as key-value pairs for the abbreviated column names from the main data file.|
|data/processed/labels_expanded.csv |Small file used to generate lables for some of the figures.|
|data/processed/disciplines_lookup.xlsx |Manual mapping of respondents' disciplines to disciplines from Web of Science.|


## Further resources
You can find more information on the `targets` package here:
https://books.ropensci.org/targets/ 



## Package versions used
The analysis was last rendered with the following package versions:

```r
sessioninfo::session_info()
─ Session info ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.2.2 (2022-10-31 ucrt)
 os       Windows 10 x64 (build 19045)
 system   x86_64, mingw32
 ui       RStudio
 language (EN)
 collate  German_Austria.utf8
 ctype    German_Austria.utf8
 tz       Europe/Berlin
 date     2022-12-20
 rstudio  2022.12.0+353 Elsbeth Geranium (desktop)
 pandoc   2.19.2 @ C:/Users/tklebel/AppData/Local/Programs/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)

─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 package       * version  date (UTC) lib source
 ash             1.0-15   2015-09-01 [1] CRAN (R 4.2.0)
 assertthat      0.2.1    2019-03-21 [1] CRAN (R 4.2.1)
 backports       1.4.1    2021-12-13 [1] CRAN (R 4.2.0)
 base64enc       0.1-3    2015-07-28 [1] CRAN (R 4.2.0)
 base64url       1.4      2018-05-14 [1] CRAN (R 4.2.1)
 boot            1.3-28   2021-05-03 [2] CRAN (R 4.2.2)
 broom           1.0.1    2022-08-29 [1] CRAN (R 4.2.1)
 ca            * 0.71.1   2020-01-24 [1] CRAN (R 4.2.2)
 cachem          1.0.6    2021-08-19 [1] CRAN (R 4.2.1)
 callr           3.7.3    2022-11-02 [1] CRAN (R 4.2.2)
 cellranger      1.1.0    2016-07-27 [1] CRAN (R 4.2.1)
 checkmate       2.1.0    2022-04-21 [1] CRAN (R 4.2.1)
 class           7.3-20   2022-01-16 [2] CRAN (R 4.2.2)
 cli             3.4.0    2022-09-08 [1] CRAN (R 4.2.1)
 clisymbols      1.2.0    2017-05-21 [1] CRAN (R 4.2.2)
 cluster         2.1.4    2022-08-22 [2] CRAN (R 4.2.2)
 codetools       0.2-18   2020-11-04 [2] CRAN (R 4.2.2)
 colorspace      2.0-3    2022-02-21 [1] CRAN (R 4.2.1)
 corrplot      * 0.92     2021-11-18 [1] CRAN (R 4.2.2)
 crayon          1.5.2    2022-09-29 [1] CRAN (R 4.2.2)
 data.table      1.14.4   2022-10-17 [1] CRAN (R 4.2.2)
 DBI             1.1.3    2022-06-18 [1] CRAN (R 4.2.1)
 dbplyr          2.2.1    2022-06-27 [1] CRAN (R 4.2.1)
 deldir          1.0-6    2021-10-23 [1] CRAN (R 4.2.1)
 DescTools     * 0.99.47  2022-10-22 [1] CRAN (R 4.2.2)
 devtools        2.4.5    2022-10-11 [1] CRAN (R 4.2.1)
 digest          0.6.30   2022-10-18 [1] CRAN (R 4.2.2)
 dplyr         * 1.0.10   2022-09-01 [1] CRAN (R 4.2.1)
 e1071           1.7-12   2022-10-24 [1] CRAN (R 4.2.2)
 ellipsis        0.3.2    2021-04-29 [1] CRAN (R 4.2.1)
 evaluate        0.18     2022-11-07 [1] CRAN (R 4.2.2)
 Exact           3.2      2022-09-25 [1] CRAN (R 4.2.1)
 expm            0.999-6  2021-01-13 [1] CRAN (R 4.2.2)
 extrafont     * 0.18     2022-04-12 [1] CRAN (R 4.2.0)
 extrafontdb     1.0      2012-06-11 [1] CRAN (R 4.2.0)
 fansi           1.0.3    2022-03-24 [1] CRAN (R 4.2.1)
 fastmap         1.1.0    2021-01-25 [1] CRAN (R 4.2.1)
 forcats       * 0.5.2    2022-08-19 [1] CRAN (R 4.2.1)
 foreign         0.8-83   2022-09-28 [2] CRAN (R 4.2.2)
 Formula         1.2-4    2020-10-16 [1] CRAN (R 4.2.0)
 fs              1.5.2    2021-12-08 [1] CRAN (R 4.2.1)
 gargle          1.2.1    2022-09-08 [1] CRAN (R 4.2.1)
 gdtools         0.2.4    2022-02-14 [1] CRAN (R 4.2.1)
 generics        0.1.3    2022-07-05 [1] CRAN (R 4.2.1)
 ggalt         * 0.4.0    2017-02-15 [1] CRAN (R 4.2.2)
 ggchicklet    * 0.5.0    2022-12-13 [1] local
 ggplot2       * 3.4.0    2022-11-04 [1] CRAN (R 4.2.2)
 ggrepel         0.9.2    2022-11-06 [1] CRAN (R 4.2.2)
 gld             2.6.6    2022-10-23 [1] CRAN (R 4.2.2)
 glue            1.6.2    2022-02-24 [1] CRAN (R 4.2.1)
 googledrive     2.0.0    2021-07-08 [1] CRAN (R 4.2.1)
 googlesheets4   1.0.1    2022-08-13 [1] CRAN (R 4.2.1)
 gridExtra       2.3      2017-09-09 [1] CRAN (R 4.2.1)
 gtable          0.3.1    2022-09-01 [1] CRAN (R 4.2.1)
 haven           2.5.1    2022-08-22 [1] CRAN (R 4.2.1)
 Hmisc           4.7-2    2022-11-18 [1] CRAN (R 4.2.2)
 hms             1.1.2    2022-08-19 [1] CRAN (R 4.2.1)
 hrbrthemes    * 0.8.0    2020-03-06 [1] CRAN (R 4.2.1)
 htmlTable       2.4.1    2022-07-07 [1] CRAN (R 4.2.2)
 htmltools       0.5.3    2022-07-18 [1] CRAN (R 4.2.1)
 htmlwidgets     1.5.4    2021-09-08 [1] CRAN (R 4.2.1)
 httpuv          1.6.6    2022-09-08 [1] CRAN (R 4.2.1)
 httr            1.4.4    2022-08-17 [1] CRAN (R 4.2.1)
 igraph          1.3.5    2022-09-22 [1] CRAN (R 4.2.2)
 interp          1.1-3    2022-07-13 [1] CRAN (R 4.2.2)
 janitor       * 2.1.0    2021-01-05 [1] CRAN (R 4.2.2)
 jpeg            0.1-10   2022-11-29 [1] CRAN (R 4.2.2)
 jsonlite        1.8.3    2022-10-21 [1] CRAN (R 4.2.2)
 KernSmooth      2.23-20  2021-05-03 [2] CRAN (R 4.2.2)
 knitr           1.40     2022-08-24 [1] CRAN (R 4.2.1)
 later           1.3.0    2021-08-18 [1] CRAN (R 4.2.1)
 lattice         0.20-45  2021-09-22 [2] CRAN (R 4.2.2)
 latticeExtra    0.6-30   2022-07-04 [1] CRAN (R 4.2.2)
 lifecycle       1.0.3    2022-10-07 [1] CRAN (R 4.2.2)
 lmom            2.9      2022-05-29 [1] CRAN (R 4.2.0)
 lubridate       1.9.0    2022-11-06 [1] CRAN (R 4.2.2)
 magrittr        2.0.3    2022-03-30 [1] CRAN (R 4.2.1)
 maps            3.4.1    2022-10-30 [1] CRAN (R 4.2.2)
 MASS            7.3-58.1 2022-08-03 [2] CRAN (R 4.2.2)
 Matrix          1.5-3    2022-11-11 [1] CRAN (R 4.2.2)
 memoise         2.0.1    2021-11-26 [1] CRAN (R 4.2.1)
 mime            0.12     2021-09-28 [1] CRAN (R 4.2.0)
 miniUI          0.1.1.1  2018-05-18 [1] CRAN (R 4.2.1)
 modelr          0.1.10   2022-11-11 [1] CRAN (R 4.2.2)
 munsell         0.5.0    2018-06-12 [1] CRAN (R 4.2.1)
 mvtnorm         1.1-3    2021-10-08 [1] CRAN (R 4.2.0)
 nnet            7.3-18   2022-09-28 [2] CRAN (R 4.2.2)
 patchwork     * 1.1.2    2022-08-19 [1] CRAN (R 4.2.1)
 pillar          1.8.1    2022-08-19 [1] CRAN (R 4.2.1)
 pkgbuild        1.3.1    2021-12-20 [1] CRAN (R 4.2.1)
 pkgconfig       2.0.3    2019-09-22 [1] CRAN (R 4.2.1)
 pkgload         1.3.1    2022-10-28 [1] CRAN (R 4.2.1)
 png             0.1-8    2022-11-29 [1] CRAN (R 4.2.2)
 prettyunits     1.1.1    2020-01-24 [1] CRAN (R 4.2.1)
 processx        3.8.0    2022-10-26 [1] CRAN (R 4.2.2)
 profvis         0.3.7    2020-11-02 [1] CRAN (R 4.2.1)
 proj4           1.0-12   2022-11-30 [1] CRAN (R 4.2.2)
 promises        1.2.0.1  2021-02-11 [1] CRAN (R 4.2.1)
 prompt          1.0.1    2021-03-12 [1] CRAN (R 4.2.1)
 proxy           0.4-27   2022-06-09 [1] CRAN (R 4.2.2)
 ps              1.7.2    2022-10-26 [1] CRAN (R 4.2.2)
 purrr         * 0.3.5    2022-10-06 [1] CRAN (R 4.2.2)
 R6              2.5.1    2021-08-19 [1] CRAN (R 4.2.1)
 RColorBrewer    1.1-3    2022-04-03 [1] CRAN (R 4.2.0)
 Rcpp            1.0.9    2022-07-08 [1] CRAN (R 4.2.1)
 readr         * 2.1.3    2022-10-01 [1] CRAN (R 4.2.2)
 readxl          1.4.1    2022-08-17 [1] CRAN (R 4.2.1)
 remotes         2.4.2    2021-11-30 [1] CRAN (R 4.2.1)
 reprex          2.0.2    2022-08-17 [1] CRAN (R 4.2.1)
 rlang           1.0.6    2022-09-24 [1] CRAN (R 4.2.2)
 rmarkdown       2.18     2022-11-09 [1] CRAN (R 4.2.2)
 rootSolve       1.8.2.3  2021-09-29 [1] CRAN (R 4.2.0)
 rpart           4.1.19   2022-10-21 [2] CRAN (R 4.2.2)
 rstudioapi      0.14     2022-08-22 [1] CRAN (R 4.2.1)
 Rttf2pt1        1.3.11   2022-10-08 [1] CRAN (R 4.2.1)
 rvest           1.0.3    2022-08-19 [1] CRAN (R 4.2.1)
 scales          1.2.1    2022-08-20 [1] CRAN (R 4.2.1)
 sessioninfo     1.2.2    2021-12-06 [1] CRAN (R 4.2.1)
 shiny           1.7.3    2022-10-25 [1] CRAN (R 4.2.2)
 snakecase       0.11.0   2019-05-25 [1] CRAN (R 4.2.2)
 stringi         1.7.8    2022-07-11 [1] CRAN (R 4.2.1)
 stringr       * 1.4.1    2022-08-20 [1] CRAN (R 4.2.1)
 survival        3.4-0    2022-08-09 [2] CRAN (R 4.2.2)
 systemfonts     1.0.4    2022-02-11 [1] CRAN (R 4.2.1)
 tarchetypes   * 0.7.2    2022-10-31 [1] CRAN (R 4.2.2)
 targets       * 0.14.0   2022-11-01 [1] CRAN (R 4.2.2)
 tibble        * 3.1.8    2022-07-22 [1] CRAN (R 4.2.1)
 tidylog       * 1.0.2    2020-07-03 [1] CRAN (R 4.2.2)
 tidyr         * 1.2.1    2022-09-08 [1] CRAN (R 4.2.1)
 tidyselect      1.2.0    2022-10-10 [1] CRAN (R 4.2.2)
 tidyverse     * 1.3.2    2022-07-18 [1] CRAN (R 4.2.1)
 timechange      0.1.1    2022-11-04 [1] CRAN (R 4.2.2)
 tzdb            0.3.0    2022-03-28 [1] CRAN (R 4.2.1)
 urlchecker      1.0.1    2021-11-30 [1] CRAN (R 4.2.1)
 usethis         2.1.6    2022-05-25 [1] CRAN (R 4.2.1)
 utf8            1.2.2    2021-07-24 [1] CRAN (R 4.2.1)
 vctrs           0.5.0    2022-10-22 [1] CRAN (R 4.2.2)
 withr           2.5.0    2022-03-03 [1] CRAN (R 4.2.1)
 xfun            0.34     2022-10-18 [1] CRAN (R 4.2.2)
 xml2            1.3.3    2021-11-30 [1] CRAN (R 4.2.1)
 xtable          1.8-4    2019-04-21 [1] CRAN (R 4.2.1)
 yaml            2.3.6    2022-10-18 [1] CRAN (R 4.2.1)

 [1] C:/Users/tklebel/AppData/Local/R/win-library/4.2
 [2] C:/Program Files/R/R-4.2.2/library

────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
```

