## ---- include = FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  width = 95,
  fig.dpi = 96
)

opts.old <- options()
options(width = 95, cli.unicode = FALSE, cli.width = 95)

## confirm RePORTER is currently available before building this, for CRAN
r <- httr::POST("https://api.reporter.nih.gov/v2/projects/search",
           httr::accept("application/json"),
           httr::content_type_json(),
           body = "{\"criteria\":{\"foa\":[\"RFA-NS-19-036\"]},\"include_fields\":[\"ApplId\"],\"offset\":0,\"limit\":10,\"sort_field\":\"appl_id\",\"sort_order\":\"desc\"}",
           encode = "raw"
           )
if (r$status_code != 200) knitr::knit_exit()

## ---- echo = FALSE, message = FALSE, fig.width=7, fig.height=4, fig.align='center'-----------
knitr::include_graphics("covid_plot.png")

## ---- eval = FALSE---------------------------------------------------------------------------
#  install.packages("repoRter.nih")

## ---- eval = FALSE---------------------------------------------------------------------------
#  devtools::install_github('bikeactuary/repoRter.nih@dev')

## ---- message = FALSE------------------------------------------------------------------------
library(tibble)
library(repoRter.nih)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(scales)
library(tufte)

## --------------------------------------------------------------------------------------------
# all projects funded by the Paycheck Protection Act, Coronavirus Response and
# Relief Act, and American Rescue Plan, in fiscal year 2021
req <- make_req(criteria =
                  list(fiscal_years = 2021,
                       covid_response = c("C4", "C5", "C6")))

## --------------------------------------------------------------------------------------------
res <- get_nih_data(req)
class(res)

## --------------------------------------------------------------------------------------------
res %>% glimpse(width = getOption("cli.width"))

## --------------------------------------------------------------------------------------------
data("nih_fields")
nih_fields %>% print

## --------------------------------------------------------------------------------------------
req <- make_req()

## --------------------------------------------------------------------------------------------
data("nih_fields")
fields <- nih_fields %>%
  filter(response_name %in% 
           c("appl_id", "subproject_id", "project_title", "fiscal_year",
             "award_amount", "is_active", "project_start_date")) %>%
  pull(include_name)

req <- make_req(include_fields = fields,
                limit = 500,
                message = FALSE) # default
res <- get_nih_data(query = req,
                    max_pages = 1)

res %>% glimpse(width = getOption("cli.width"))

## --------------------------------------------------------------------------------------------
req <- make_req(criteria = 
                  list(
                    fiscal_years = 2010:2011,
                    include_active_projects = TRUE,
                    org_names = c("Yale", "New Haven")
                  ),
                include_fields = c("Organization", "FiscalYear", "AwardAmount"),
                message = FALSE)

## --------------------------------------------------------------------------------------------
res <- get_nih_data(req, max_pages = 1)
res %>% glimpse(width = getOption("cli.width"))

## --------------------------------------------------------------------------------------------
res <- get_nih_data(req,
                    max_pages = 1,
                    flatten_result = TRUE)

res %>% glimpse(width = getOption("cli.width"))

## ---- echo = FALSE---------------------------------------------------------------------------
evl <- class(res)[1] == "tbl_df"

## ---- eval=evl-------------------------------------------------------------------------------
res %>% 
  group_by(organization_org_name) %>%
  summarise(project_count = n())

## --------------------------------------------------------------------------------------------
## A valid request but probably not what we want
req <- make_req(criteria = 
                  list(
                    fiscal_years = 2010:2011,
                    include_active_projects = TRUE,
                    org_cities = "New Haven",
                    org_states = "WY"
                  ),
                include_fields = c("Organization", "FiscalYear", "AwardAmount"),
                message = FALSE ## suppress printed message
)

res <- get_nih_data(req,
                    max_pages = 5,
                    flatten_result = TRUE)

## --------------------------------------------------------------------------------------------
req <- make_req(criteria =
                  list(
                    fiscal_years = 2015,
                    include_active_projects = TRUE,
                    org_states = "WY"
                  ),
                include_fields = c("ApplId", "Organization", "FiscalYear", "AwardAmount"),
                sort_field = "AwardAmount",
                sort_order = "desc",
                message = FALSE)

res <- get_nih_data(req,
                    flatten_result = TRUE)

res %>% glimpse(width = getOption("cli.width"))

## --------------------------------------------------------------------------------------------
## all projects funded by the Paycheck Protection Act, Coronavirus Response and Relief Act,
## and American Rescue Plan, over all years
req <- make_req(criteria =
                  list(covid_response = c("All")),
                include_fields = nih_fields %>%
                  filter(payload_name %in% c("award_amount_range", "covid_response"))
                %>% pull(include_name))

res <- get_nih_data(req, max_pages = 1)

## ---- echo=FALSE-----------------------------------------------------------------------------
evl <- class(res)[1] == "tbl_df"

## ---- eval=evl-------------------------------------------------------------------------------
res$covid_response %>% class()
res$covid_response[[1]]

## ----covid, eval=FALSE-----------------------------------------------------------------------
#  ## all projects funded by the Paycheck Protection Act, Coronavirus Response and Relief Act,
#  ## and American Rescue Plan, in fiscal year 2021
#  req <- make_req(criteria =
#                    list(covid_response = c("All")),
#                  message = FALSE)
#  
#  res <- get_nih_data(req,
#                      flatten_result = TRUE)

## ---- eval=FALSE-----------------------------------------------------------------------------
#  unique(res$covid_response)

## ---- echo=FALSE-----------------------------------------------------------------------------
res <- readRDS("cov_res.RDS")
unique(res$covid_response)

## ---- eval=FALSE-----------------------------------------------------------------------------
#  library(ggplot2)
#  
#  res %>%
#    left_join(covid_response_codes, by = "covid_response") %>%
#    mutate(covid_code_desc = case_when(!is.na(fund_src) ~ paste0(covid_response, ": ", fund_src),
#                                       TRUE ~ paste0(covid_response, " (Multiple)"))) %>%
#    group_by(covid_code_desc) %>%
#    summarise(total_awards = sum(award_amount) / 1e6) %>%
#    ungroup() %>%
#    arrange(desc(covid_code_desc)) %>%
#    mutate(prop = total_awards / sum(total_awards),
#           csum = cumsum(prop),
#           ypos = csum - prop/2 ) %>%
#    ggplot(aes(x = "", y = prop, fill = covid_code_desc)) +
#    geom_bar(stat="identity") +
#    geom_text_repel(aes(label =
#                          paste0(dollar(total_awards,
#                                        accuracy = 1,
#                                        suffix = "M"),
#                                 "\n", percent(prop, accuracy = .01)),
#                        y = ypos),
#                    show.legend = FALSE,
#                    nudge_x = .8,
#                    size = 3, color = "grey25") +
#    coord_polar(theta ="y") +
#    theme_void() +
#    theme(legend.position = "right",
#          legend.title = element_text(colour = "grey25"),
#          legend.text = element_text(colour="blue", size=6,
#                                     face="bold"),
#          plot.title = element_text(color = "grey25"),
#          plot.caption = element_text(size = 6)) +
#    labs(caption = "Data Source: NIH RePORTER API v2") +
#    ggtitle("Legislative Source for NIH Covid Response Project Funding")

## ---- echo = FALSE, message = FALSE, fig.width=7, fig.height=4, fig.align='center'-----------
knitr::include_graphics("covid_plot.png")

## --------------------------------------------------------------------------------------------
data("covid_response_codes")
covid_response_codes %>% print

## --------------------------------------------------------------------------------------------
## projects funded in 2021 where the principal investigator first name
##  is "Michael" or begins with "Jo"
req <- make_req(criteria = 
                  list(fiscal_years = 2021,
                       pi_names = list(first_name = c("Michael", "Jo*"),
                                       last_name = c(""), # must specify all pi_names elements always
                                       any_name = character(1))),
                include_fields = nih_fields %>%
                  filter(payload_name == "pi_names") %>%
                  pull(include_name),
                message = FALSE)

res <- get_nih_data(req,
                    max_pages = 1,
                    flatten_result = TRUE)

res %>% glimpse(width = getOption("cli.width"))

## --------------------------------------------------------------------------------------------
## using advanced_text_search with boolean search string
req <- make_req(criteria = 
                  list(advanced_text_search =
                         list(operator = "advanced",
                              search_field = c("terms", "abstract"),
                              search_text = "(head AND trauma) OR \"brain damage\" AND NOT \"psychological\"")),
                include_fields = c("ProjectTitle", "AbstractText", "Terms") )

res <- get_nih_data(req, max_pages = 1)

## ---- echo = FALSE---------------------------------------------------------------------------
evl <- class(res)[1] == "tbl_df"

## ---- eval=evl-------------------------------------------------------------------------------
one_rec <- res %>%
  slice(42) %>%
  mutate(abstract_text = gsub("[\r\n]", " ", abstract_text))

one_rec %>% pull(project_title) %>% print

## ---- eval=evl-------------------------------------------------------------------------------
## substr to avoid LaTeX error exceeding char limit
one_rec %>% pull(abstract_text) %>% substr(1, 85) %>% print

## ---- eval=evl-------------------------------------------------------------------------------
one_rec %>% pull(terms) %>% substr(1, 85) %>% print

## ---- eval = FALSE---------------------------------------------------------------------------
#  all_res <- list()
#  for(y in 2017:2021) { ## five years to loop over, each year is ~80K records
#    ## We only need the AwardAmount for quantiles
#    req_sample <- make_req(criteria = list(fiscal_years = y),
#                           include_fields = "AwardAmount")
#  
#    ## get a sample of the result set - 1000 records should be enough
#    ## return the metadata
#    res_sample <- get_nih_data(req_sample, max_pages = 2, return_meta = TRUE)
#  
#    paste0("There are ", res_sample$meta$total, " results for fiscal year ", y) %>%
#      print()
#  
#    ## deciles of award amount - each decile should contain ~7,314.2 records, approximately
#    qtiles <- res_sample$records %>% pull(award_amount) %>% quantile(na.rm = TRUE, probs = seq(.1, 1, .1))
#  
#    ## list for qtile results (full year)
#    this_res <- list()
#    ## for each qtile
#    for (i in 1:length(qtiles)) {
#      if (i == 1) {
#        award_min <- 0
#      } else {
#        award_min <- ceiling(qtiles[i-1])+.01
#      }
#      if (i == length(qtiles)) {
#        award_max <- 1e9 ## arbitrarily huge
#      } else {
#        award_max <- ceiling(qtiles[i])
#      }
#      req <- make_req(criteria = list(fiscal_years = y,
#                                      award_amount_range = list(min_amount = award_min,
#                                                                max_amount = award_max)))
#      ## result set for quantile
#      this_res[[i]] <- get_nih_data(req, flatten_result = FALSE)
#    }
#    ## list of result sets for each year
#    yr_res[[y %>% as.character()]] <- this_res
#  }
#  
#  ## shape it up
#  all_res <- unlist(yr_res, recursive = FALSE) %>%
#    bind_rows() %>%
#    flatten(recursive = FALSE) %>%
#    clean_names()
#  
#  ## pull out everything that is flat
#  flat_columns <- all_res %>%
#    select_if(is.atomic)
#  
#  ## everything that isn't
#  annoying_columns <- all_res %>%
#    select_if(!is.atomic)

## --------------------------------------------------------------------------------------------
sessionInfo()

## ---- include=FALSE-----------------------------------------------------------
options(opts.old)

