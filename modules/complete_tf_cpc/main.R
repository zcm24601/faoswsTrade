##' title: "Appendix: `complete_tf_cpc` module"
##' author:
##'   - Marco Garieri
##'   - Alexander Matrunich
##'   - Christian A. Mongeau Ospina
##'   - Bo Werth\
##'
##'     Food and Agriculture Organization
##'     of the United Nations
##' date: "`r format(Sys.time(), '%e %B %Y')`"
##' output:
##'    pdf_document
##' ---

##+ setup, include=FALSE
knitr::opts_chunk$set(echo = FALSE, eval = FALSE)

##' This document gives a faithful step-by-step sequence of the operations
##' performed in the `complete_tf_cpc` module. For a narrative version of
##' the module's approach, please see its main document.

##+ init

## Change Log:
##
## - Add unit values to output
## - Remove adjustment factors
## - Revise flags: add **flagObservationStatus** `X` and **flagMethod** `c`, `i`

## **Flow chart:**
##
## ![Aggregate complete_tf to total_trade](assets/diagram/trade_3.png?raw=true "livestock Flow")


# Settings ####

# Package build ID
# It is included into report directory name
build_id <- "master"
# Should we stop after reports on raw data?
stop_after_pre_process <- FALSE
# Should we stop after HS-FCL mapping?
stop_after_mapping <- FALSE


set.seed(2507)

# Size for sampling. Set NULL if no sampling is required.
samplesize <- NULL

# Logging level
# There are following levels:
# trace, debug, info, warn, error, fatal
# Level `trace` shows everything in the log

# Additional logger for technical data
futile.logger::flog.logger("dev", "TRACE")
futile.logger::flog.threshold("TRACE", name = "dev")

# Parallel backend will be used only if required packages
# are installed
# It will be switched to FALSE if packages are not available
multicore <- TRUE

## If true, the reported values will be in $
## If false the reported values will be in k$
dollars <- FALSE

## If TRUE, use adjustments (AKA "conversion notes")
use_adjustments <- FALSE

# If TRUE, use impute outliers
detect_outliers <- FALSE

# Print general log to console
general_log2console <- FALSE

# Save current options (will be reset at the end)
old_options <- options()

dev_sws_set_file <- "modules/complete_tf_cpc/sws.yml"

# Switch off dplyr's progress bars globally
options(dplyr.show_progress = FALSE)

# max.print in RStudio is too small
options(max.print = 99999L)

# Libraries ####
suppressPackageStartupMessages(library(data.table))
library(stringr)
library(magrittr)
library(scales)
library(tidyr, warn.conflicts = FALSE)
library(futile.logger)
suppressPackageStartupMessages(library(dplyr, warn.conflicts = FALSE))
library(faosws)
library(faoswsUtil)
library(faoswsTrade)
library(faoswsFlag)

# Development (SWS-outside) mode addons ####
if(faosws::CheckDebug()){
  set_sws_dev_settings(dev_sws_set_file)
} else {
  # In order to have all columns aligned. Issue #119
  options(width = 1000L)

  # Remove domain from username
  USER <- regmatches(
    swsContext.username,
    regexpr("(?<=/).+$", swsContext.username, perl = TRUE)
  )

  options(error = function(){
    dump.frames()
    filename <- file.path(Sys.getenv("R_SWS_SHARE_PATH"),
                          USER,
                          "complete_tf_cpc")
    dir.create(filename, showWarnings = FALSE, recursive = TRUE)
    save(last.dump, file = file.path(filename, "last.dump.RData"))
  })
}

stopifnot(!any(is.na(USER), USER == ""))

##' - `year`: year for processing.
year <- as.integer(swsContext.computationParams$year)

reportdir <- reportdirectory(USER, year, build_id, browsedir = CheckDebug())

# Send general log messages
if(general_log2console) {
  # to console and a file
    flog.appender(appender.tee(file.path(reportdir,
                                       "report.txt")))
} else {
  # to a file only
  flog.appender(appender.file(file.path(reportdir,
                                        "report.txt")))
}

# Send technical log messages to a file and console
flog.appender(appender.tee(file.path(reportdir,
                                      "development.log")),
              name = "dev")

flog.info("SWS-session is run by user %s", USER, name = "dev")

flog.debug("User's computation parameters:",
           swsContext.computationParams, capture = TRUE,
           name = "dev")

flog.info("R session environment: ",
           sessionInfo(), capture = TRUE, name = "dev")

PID <- Sys.getpid()

# Check that all packages are up to date ####

check_versions(c("faoswsUtil", "faoswsTrade",
                 "dplyr"),
               c('0.2.11', '0.1.1', '0.5.0'))

# Register CPU cores ####
if(multicore) multicore <- register_cpu_cores()

##+ swsdebug

## ## local data
## install.packages("//hqfile4/ess/Team_working_folder/A/SWS/faosws_0.8.2.9901.tar.gz",
##                  repos = NULL,
##                  type = "source")
## ## SWS data
## install.packages("faosws",
##                  repos = "http://hqlprsws1.hq.un.fao.org/fao-sws-cran/")

# Read SWS module run parameters ####

stopifnot(
  !is.null(swsContext.computationParams$out_coef))

##' # Parameters

##' - `out_coef`: coefficient for outlier detection, i.e., the `k` parameter in
##' the *Outlier Detection and Imputation* section.
# See coef argument in ?boxplot.stats
out_coef <- as.numeric(swsContext.computationParams$out_coef)
flog.info("Coefficient for outlier detection: %s", out_coef)
##'   can not be set by the user as it is provided by Team B/C and harcoded).
##'   The HS chapters are the following:

##+ hschapters, eval = TRUE

hs_chapters <- c(1:24, 33, 35, 38, 40:41, 43, 50:53) %>%
  formatC(width = 2, format = "d", flag = "0") %>%
  as.character %>%
  shQuote(type = "sh") %>%
  paste(collapse = ", ")

flog.info("HS chapters to be selected:", hs_chapters,  capture = T)
##'     `r paste(formatC(hs_chapters, width = 2, format = "d", flag = "0"), collapse = ' ')`

startTime = Sys.time()

# Load raw data (ES and TL) ####

flog.trace("[%s] Reading in Eurostat data", PID, name = "dev")

esdata <- ReadDatatable(
  paste0("ce_combinednomenclature_unlogged_", year),
  columns = c(
    "period",
    "declarant",
    "partner",
    "flow",
    "product_nc",
    "value_1k_euro",
    "qty_ton",
    "sup_quantity",
    "stat_regime"
  ),
  where = paste0("chapter IN (", hs_chapters, ")")
) %>% tbl_df()

stopifnot(nrow(esdata) > 0)

flog.info("Raw Eurostat data preview:",
          rprt_glimpse0(esdata), capture = TRUE)

##' 1. Keep only `stat_regime`=4.

## Only regime 4 is relevant for Eurostat data
esdata <- esdata %>%
  filter_(~stat_regime == "4") %>%
## Removing stat_regime as it is not needed anymore
  select_(~-stat_regime) %>%
  # Remove totals
  filter_(~declarant != "EU")

flog.info("Records after filtering by 4th stat regime and removing EU totals: %s", nrow(esdata))

## Download TL data ####

flog.trace("[%s] Reading in Tariffline data", PID, name = "dev")

tldata <- ReadDatatable(
  paste0("ct_tariffline_unlogged_", year),
  columns = c(
    "tyear",
    "rep",
    "prt",
    "flow",
    "comm",
    "tvalue",
    "weight",
    "qty",
    "qunit",
    "chapter"
  ),
  where = paste0("chapter IN (", hs_chapters, ")")
)

stopifnot(nrow(tldata) > 0)

##' 1. Use standard (common) variable names (e.g., `declarant` becomes `reporter`).

esdata <- adaptTradeDataNames(tradedata = esdata, origin = "ES")
tldata <- adaptTradeDataNames(tradedata = tldata, origin = "TL")

esdata <- adaptTradeDataTypes(esdata, "ES")

##' 1. Convert ES geonomenclature country/area codes to FAO codes.

##+ geonom2fao
esdata <- esdata %>%
  mutate(
    reporter = convertGeonom2FAO(reporter),
    partner = convertGeonom2FAO(partner)
  )


# M49 to FAO area list ####

##' 1. Tariffline M49 codes (which are different from official M49)
##' are converted in FAO country codes using a specific convertion
##' table provided by Team ENV.

flog.trace("TL: converting M49 to FAO area list", name = "dev")

tldata <- tldata %>%
  # Workaround
  mutate(partner = as.numeric(partner)) %>%
  left_join(
    unsdpartnersblocks %>%
      select_(
        wholepartner = ~rtCode,
        part = ~formula
      ) %>%
      # Exclude EU grouping and old countries
      filter_(
        ~wholepartner %in% c(251, 381, 579, 581, 711, 757, 842)
      ),
    by = c("partner" = "part")
  ) %>%
  mutate_(
    partner = ~ifelse(is.na(wholepartner), partner, wholepartner),
    m49rep = ~reporter,
    m49par = ~partner,
    # Conversion from Comtrade M49 to FAO area list
    reporter = ~as.integer(faoswsTrade::convertComtradeM49ToFAO(m49rep)),
    partner = ~as.integer(faoswsTrade::convertComtradeM49ToFAO(m49par))
  )

flog.trace("TL: dropping reporters already found in Eurostat data", name = "dev")
# They will be replaced by ES data
tldata <- tldata %>%
  anti_join(
    esdata %>%
      select_(~reporter) %>%
      distinct(),
    by = "reporter"
  )

##' 1. Use standard (common) variable types.


tldata <- adaptTradeDataTypes(tldata, "TL")


# XXX this is a duplication: a function should be created.
to_mirror_raw <- bind_rows(
    esdata %>%
      select(year, reporter, partner, flow),
    tldata %>%
      filter(!(reporter %in% unique(esdata$reporter))) %>%
      select(year, reporter, partner, flow)
  ) %>%
  mutate(flow = recode(flow, '4' = 1L, '3' = 2L)) %>%
  flowsToMirror(names = TRUE)

rprt_writetable(to_mirror_raw, 'flows', subdir = 'preproc')

if(stop_after_pre_process) stop("Stop after reports on raw data")

# Loading of help datasets ####

##' - `hsfclmap3`: Mapping between HS and FCL codes extracted from MDB files
##' used to archive information existing in the previous trade system
##' (Shark/Jellyfish). This mapping is provided by a separate package:
##' https://github.com/SWS-Methodology/hsfclmap

flog.debug("[%s] Reading in hs-fcl mapping", PID, name = "dev")
#data("hsfclmap3", package = "hsfclmap", envir = environment())
hsfclmap3 <- tbl_df(ReadDatatable("hsfclmap3"))

# See issue 127. The fix is done for a single country on purpose.
# TODO A general approach should be designed.

# FAL = 255, M49 = 784
map225 <- filter(hsfclmap3, area == 225)

replicate_rows <- function(data) {
  res <- data.frame(
             leading = stringr::str_sub(data$fromcode, 1, 1),
             area = data$area,
             flow = data$flow,
             fromcode = as.numeric(data$fromcode):as.numeric(data$tocode),
             tocode = as.numeric(data$fromcode):as.numeric(data$tocode),
             fcl = data$fcl,
             startyear = as.integer(data$startyear),
             endyear = as.integer(data$endyear),
             recordnumb = data$recordnumb
             )

  res <- mutate(res,
                fromcode = ifelse(leading == '0',
                                  paste0('0', fromcode),
                                  as.character(fromcode)),
                tocode = fromcode)

  return(res %>% select(-leading))
}


map225_ext <- plyr::mdply(1:nrow(map225),
                          function(x) replicate_rows(map225[x,]),
                          .progress = "text") %>%
  tbl_df() %>%
  select(-X1)

map225_ext <- mutate(
                     map225_ext,
                     startyear = ifelse(startyear == 2005, 2004, startyear),
                     endyear = ifelse(endyear > 2004, 2050, endyear)
                     )

hsfclmap3_subset <- hsfclmap3 %>% filter(area != 225)

map225_ext$recordnumb <- max(hsfclmap3_subset$recordnumb) + 1:nrow(map225_ext)

hsfclmap3 <- bind_rows(hsfclmap3_subset, map225_ext)

# Add unmapped codes.

unmapped_codes <- frame_data(
  ~"area",~"flow",~"fromcode",~"tocode",~"fcl",~"startyear",~"endyear",~"recordnumb",
  225,1,"010121","010121",1096,2013,2050,5923193,
  225,1,"010129","010129",1096,2013,2050,5923194,
  225,1,"010130","010130",1107,2014,2050,5923195,
  225,1,"010513","010513",1068,2013,2050,5923196,
  225,1,"010514","010514",1072,2015,2050,5923197,
  225,1,"010613","010613",1126,2013,2050,5923198,
  225,1,"010614","010614",1140,2013,2050,5923199,
  225,1,"010633","010633",1171,2014,2050,5923200,
  225,1,"010641","010641",1181,2013,2050,5923201,
  225,1,"010649","010649",1169,2014,2050,5923202,
  225,1,"020744","020744",1069,2013,2050,5923203,
  225,1,"020745","020745",1069,2013,2050,5923204,
  225,1,"020751","020751",1073,2013,2050,5923205,
  225,1,"020752","020752",1073,2013,2050,5923206,
  225,1,"020753","020753",1074,2013,2050,5923207,
  225,1,"020754","020754",1073,2013,2050,5923208,
  225,1,"020755","020755",1073,2013,2050,5923209,
  225,1,"020760","020760",1058,2013,2050,5923210,
  225,1,"020860","020860",1127,2013,2050,5923211,
  225,1,"020910","020910",1037,2014,2050,5923212,
  225,1,"020990","020990",1065,2013,2050,5923213,
  225,1,"040140","040140",885,2013,2050,5923214,
  225,1,"040150","040150",885,2013,2050,5923215,
  225,1,"040711","040711",1062,2013,2050,5923216,
  225,1,"040719","040719",1091,2013,2050,5923217,
  225,1,"040721","040721",1062,2013,2050,5923218,
  225,1,"040729","040729",1091,2013,2050,5923219,
  225,1,"040790","040790",1062,2013,2050,5923220,
  225,1,"070991","070991",366,2013,2050,5923221,
  225,1,"070992","070992",260,2013,2050,5923222,
  225,1,"070993","070993",394,2013,2050,5923223,
  225,1,"070999","070999",463,2013,2050,5923224,
  225,1,"071334","071334",203,2013,2050,5923225,
  225,1,"071335","071335",195,2013,2050,5923226,
  225,1,"071360","071360",197,2013,2050,5923227,
  225,1,"071430","071430",137,2013,2050,5923228,
  225,1,"071440","071440",136,2013,2050,5923229,
  225,1,"071450","071450",135,2014,2050,5923230,
  225,1,"080112","080112",249,2013,2050,5923231,
  225,1,"080261","080261",234,2013,2050,5923232,
  225,1,"080262","080262",234,2013,2050,5923233,
  225,1,"080270","080270",224,2013,2050,5923234,
  225,1,"080280","080280",226,2013,2050,5923235,
  225,1,"080310","080310",489,2013,2050,5923236,
  225,1,"080390","080390",486,2013,2050,5923237,
  225,1,"080830","080830",521,2013,2050,5923238,
  225,1,"080840","080840",523,2013,2050,5923239,
  225,1,"081070","081070",587,2013,2050,5923240,
  225,1,"100840","100840",94,2015,2050,5923241,
  225,1,"100850","100850",92,2013,2050,5923242,
  225,1,"100860","100860",97,2015,2050,5923243,
  225,1,"120230","120230",242,2013,2050,5923244,
  225,1,"120241","120241",242,2013,2050,5923245,
  225,1,"120242","120242",243,2013,2050,5923246,
  225,1,"120770","120770",299,2013,2050,5923247,
  225,1,"121293","121293",156,2013,2050,5923248,
  225,1,"150110","150110",1043,2014,2050,5923249,
  225,1,"150190","150190",1066,2013,2050,5923250,
  225,1,"150210","150210",1225,2013,2050,5923251,
  225,1,"150290","150290",979,2013,2050,5923252,
  225,1,"170113","170113",162,2013,2050,5923253,
  225,1,"170114","170114",162,2013,2050,5923254,
  225,1,"200893","200893",623,2013,2050,5923255,
  225,1,"200897","200897",623,2013,2050,5923256,
  225,1,"350190","350190",917,2013,2050,5923257,
  225,1,"530810","530810",813,2013,2050,5923258,
  225,2,"010121","010121",1096,2013,2050,5923259,
  225,2,"010129","010129",1096,2013,2050,5923260,
  225,2,"010130","010130",1107,2013,2050,5923261,
  225,2,"010514","010514",1072,2015,2050,5923262,
  225,2,"010515","010515",1057,2015,2050,5923263,
  225,2,"010613","010613",1126,2013,2050,5923264,
  225,2,"010614","010614",1140,2013,2050,5923265,
  225,2,"010633","010633",1171,2013,2050,5923266,
  225,2,"010641","010641",1181,2013,2050,5923267,
  225,2,"010649","010649",1169,2013,2050,5923268,
  225,2,"020744","020744",1069,2013,2050,5923269,
  225,2,"020745","020745",1069,2013,2050,5923270,
  225,2,"020751","020751",1073,2013,2050,5923271,
  225,2,"020752","020752",1073,2013,2050,5923272,
  225,2,"020753","020753",1074,2014,2050,5923273,
  225,2,"020754","020754",1073,2013,2050,5923274,
  225,2,"020755","020755",1073,2013,2050,5923275,
  225,2,"020760","020760",1058,2014,2050,5923276,
  225,2,"020860","020860",1127,2013,2050,5923277,
  225,2,"020910","020910",1037,2015,2050,5923278,
  225,2,"020990","020990",1065,2013,2050,5923279,
  225,2,"040140","040140",885,2013,2050,5923280,
  225,2,"040150","040150",885,2013,2050,5923281,
  225,2,"040711","040711",1062,2013,2050,5923282,
  225,2,"040719","040719",1091,2013,2050,5923283,
  225,2,"040721","040721",1062,2013,2050,5923284,
  225,2,"040729","040729",1091,2013,2050,5923285,
  225,2,"040790","040790",1062,2013,2050,5923286,
  225,2,"070991","070991",366,2013,2050,5923287,
  225,2,"070992","070992",260,2013,2050,5923288,
  225,2,"070993","070993",394,2013,2050,5923289,
  225,2,"070999","070999",463,2013,2050,5923290,
  225,2,"071334","071334",203,2014,2050,5923291,
  225,2,"071335","071335",195,2013,2050,5923292,
  225,2,"071360","071360",197,2013,2050,5923293,
  225,2,"071430","071430",137,2013,2050,5923294,
  225,2,"071440","071440",136,2013,2050,5923295,
  225,2,"071450","071450",135,2014,2050,5923296,
  225,2,"080112","080112",249,2013,2050,5923297,
  225,2,"080261","080261",234,2013,2050,5923298,
  225,2,"080262","080262",234,2013,2050,5923299,
  225,2,"080270","080270",224,2014,2050,5923300,
  225,2,"080280","080280",226,2015,2050,5923301,
  225,2,"080310","080310",489,2013,2050,5923302,
  225,2,"080390","080390",486,2013,2050,5923303,
  225,2,"080830","080830",521,2013,2050,5923304,
  225,2,"080840","080840",523,2015,2050,5923305,
  225,2,"081070","081070",587,2013,2050,5923306,
  225,2,"100850","100850",92,2013,2050,5923307,
  225,2,"120230","120230",242,2014,2050,5923308,
  225,2,"120241","120241",242,2013,2050,5923309,
  225,2,"120242","120242",243,2013,2050,5923310,
  225,2,"120770","120770",299,2014,2050,5923311,
  225,2,"121293","121293",156,2013,2050,5923312,
  225,2,"121294","121294",459,2013,2050,5923313,
  225,2,"150110","150110",1043,2014,2050,5923314,
  225,2,"150190","150190",1066,2013,2050,5923315,
  225,2,"150290","150290",979,2013,2050,5923316,
  225,2,"170113","170113",162,2013,2050,5923317,
  225,2,"170114","170114",162,2013,2050,5923318,
  225,2,"200893","200893",623,2013,2050,5923319,
  225,2,"200897","200897",623,2013,2050,5923320,
  225,2,"350190","350190",917,2013,2050,5923321
) %>%
  mutate(
         area = as.integer(area),
         flow = as.integer(flow),
         fcl = as.integer(fcl),
         startyear = as.integer(startyear),
         endyear = as.integer(endyear),
         recordnumb = as.integer(recordnumb)
         )

unmapped_codes$recordnumb <- max(hsfclmap3$recordnumb) + 1:nrow(unmapped_codes)

hsfclmap3 <- bind_rows(unmapped_codes, hsfclmap3) %>%
  mutate(
         startyear = as.integer(startyear),
         endyear = as.integer(endyear)
         )


# / END fix country specific mapping



# START NEW MAPPING FOR REMAINING COUNTRIES

unmapped_codes <- frame_data(
~"area",~"flow",~"fromcode",~"tocode",~"fcl",~"startyear",~"endyear",~"recordnumb",~"details",~"tl_description",
7,1,"01011000","01011000",1096,2007,2050,5923193,"Country TL description (WITS), Old SWS series (could also be mapped to 1107,1110)","Pure-bred breeding horses and asses",
7,1,"01012100","01012100",1096,2014,2050,5923194,"Standard_HS12","FaoStatName: HORSES",
7,1,"01012900","01012900",1096,2014,2050,5923195,"Standard_HS12","FaoStatName: HORSES",
7,1,"01019000","01019000",1110,2007,2050,5923196,"Standard_HS12","FaoStatName: MULES",
7,1,"01021000","01021000",866,2007,2050,5923197,"Country TL description (WITS), Old SWS series (could also be mapped to 946)","Pure-bred breeding bovines",
7,1,"01022100","01022100",866,2014,2050,5923198,"Standard_HS12","FaoStatName: CATTLE",
7,1,"01022900","01022900",866,2014,2050,5923199,"Standard_HS12","FaoStatName: CATTLE",
7,1,"01023100","01023100",946,2014,2050,5923200,"Standard_HS12","FaoStatName: BUFFALOES",
7,1,"01023900","01023900",946,2014,2050,5923201,"Standard_HS12","FaoStatName: BUFFALOES",
7,1,"01029000","01029000",867,2007,2050,5923202,"Country TL description (WITS), Old SWS series (could also be mapped to 946)","Live bovine animals (excl. pure-bred for breeding)",
7,1,"01031000","01031000",1034,2009,2050,5923203,"Standard_HS12","FaoStatName: PIGS",
7,1,"01039100","01039100",1034,2010,2050,5923204,"Standard_HS12","FaoStatName: PIGS",
7,1,"01041000","01041000",976,2007,2050,5923205,"Standard_HS12","FaoStatName: SHEEP",
7,1,"01041012","01041012",976,2014,2050,5923206,"Standard_HS12","FaoStatName: SHEEP",
7,1,"01041019","01041019",976,2014,2050,5923207,"Standard_HS12","FaoStatName: SHEEP",
7,1,"01042000","01042000",1016,2007,2050,5923208,"Standard_HS12","FaoStatName: GOATS",
7,1,"01042021","01042021",1016,2014,2050,5923209,"Standard_HS12","FaoStatName: GOATS",
7,1,"01042029","01042029",1016,2014,2050,5923210,"Standard_HS12","FaoStatName: GOATS",
7,1,"01051100","01051100",1057,2007,2050,5923211,"Standard_HS12","FaoStatName: CHICKENS",
7,1,"01051200","01051200",1079,2009,2050,5923212,"Standard_HS12","FaoStatName: TURKEYS",
7,1,"01051300","01051300",1068,2014,2050,5923213,"Standard_HS12","FaoStatName: DUCKS",
7,1,"01051400","01051400",1072,2014,2050,5923214,"Standard_HS12","FaoStatName: GEESE",
7,1,"01051500","01051500",1057,2015,2050,5923215,"Standard_HS12","FaoStatName: CHICKENS",
7,1,"01051900","01051900",1068,2007,2050,5923216,"Country TL description (WITS) (could also be mapped to 1072)","Live domestic ducks, geese and guinea fowls, weighing <= 185 g",
7,1,"01059200","01059200",1057,2007,2050,5923217,"HS2007 to FCL unique six-digit match","Live fowls of the species Gallus domesticus, weighing > 185 g but <= 2 kg",
7,1,"01059300","01059300",1057,2007,2050,5923218,"HS2007 to FCL unique six-digit match","Live fowls of the species Gallus domesticus, weighing > 2 kg",
7,1,"01059400","01059400",1057,2010,2050,5923219,"Standard_HS12","FaoStatName: CHICKENS",
7,1,"01059900","01059900",1068,2009,2050,5923220,"Country TL description (WITS) (could also be mapped to 1072)","Patos, gansos, perus, peruas e pintadas, das espécies domésticas, vivos, com peso > 185 g",
7,1,"01060000","01060000",1171,2009,2050,5923221,"Trademap TL description","Animals, live nes",
7,1,"01061100","01061100",1169,2007,2050,5923222,"Country TL description (WITS) (could also be mapped to 1171)","Live primates",
7,1,"01061900","01061900",1169,2007,2050,5923223,"Country TL description (WITS) (could also be mapped to 1171)","Live mammals (excl. primates, whales, dolphins and purpoises mammals of the order Cetacea, manatees and dugongs mammals of the order Sirenia and horses, asses, mules, hinnies, bovines, pigs, sheep and goats)",
7,1,"01063100","01063100",1169,2009,2050,5923224,"Country TL description (WITS)","Aves de rapina, vivas",
7,1,"01063200","01063200",1169,2007,2050,5923225,"Country TL description (WITS) (could also be mapped to 1171)","Live psittaciformes incl. parrots, parrakeets, macaws and cockatoos",
7,1,"01063300","01063300",1171,2014,2050,5923226,"Standard_HS12","FaoStatName: ANIMALS LIVE NES",
7,1,"01063900","01063900",1169,2007,2050,5923227,"Country TL description (WITS) (could also be mapped to 1171)","Live birds (excl. birds of prey and psittaciformes incl. parrots, parrakeets, macaws and cockatoos)",
7,1,"01064900","01064900",1169,2014,2050,5923228,"Trademap TL description","Outros animais vivos : insetos : outros",
7,1,"01069000","01069000",1169,2007,2050,5923229,"Country TL description (WITS) (could also be mapped to 1171)","Live animals (excl. mammals, reptiles, birds, fish, crustaceans, molluscs and other aquatic invertebrates and cultures of micro-organisms, etc.)",
7,1,"02011000","02011000",867,2007,2050,5923230,"Trademap TL descriptions (WITS), trading partners (could also be mapped to 947)","Carcases or half-carcases of bovine animals, fresh or chilled",
7,1,"02012000","02012000",867,2007,2050,5923231,"Trademap TL descriptions (WITS), trading partners (could also be mapped to 947)","Fresh or chilled bovine cuts, with bone in (excl. carcases and 1/2 carcases)",
7,1,"02013000","02013000",870,2007,2050,5923232,"Trademap TL descriptions (WITS), trading partners (could also be mapped to 947)","Fresh or chilled bovine meat, boneless",
7,1,"02021000","02021000",867,2007,2050,5923233,"Trademap TL descriptions (WITS), trading partners (could also be mapped to 947)","Frozen bovine carcases and halfcarcases",
7,1,"02022000","02022000",867,2007,2050,5923234,"Trademap TL descriptions (WITS), trading partners (could also be mapped to 947)","Frozen bovine cuts, with bone in (excl. carcases and halfcarcases)",
7,1,"02023000","02023000",870,2007,2050,5923235,"Trademap TL descriptions (WITS), trading partners (could also be mapped to 947)","Frozen, boneless meat of bovine animals",
7,1,"02031100","02031100",1035,2007,2050,5923236,"Standard_HS12","FaoStatName: MEAT OF PIGS (PIGMEAT)",
7,1,"02031200","02031200",1035,2007,2050,5923237,"Standard_HS12","FaoStatName: MEAT OF PIGS (PIGMEAT)",
7,1,"02031900","02031900",1035,2007,2050,5923238,"Country TL description (WITS), Old SWS series (could also be mapped to 1038)","Fresh or chilled meat of swine (excl. carcases and half-carcases, and hams, shoulders and cuts thereof, with bone in)",
7,1,"02032100","02032100",1035,2007,2050,5923239,"Standard_HS12","FaoStatName: MEAT OF PIGS (PIGMEAT)",
7,1,"02032200","02032200",1035,2007,2050,5923240,"Standard_HS12","FaoStatName: MEAT OF PIGS (PIGMEAT)",
7,1,"02032900","02032900",1035,2007,2050,5923241,"Country TL description (WITS), Old SWS series (could also be mapped to 1038)","Frozen meat of swine (excl. carcases and half-carcases, and hams, shoulders and cuts thereof, boneless)",
7,1,"02041000","02041000",977,2007,2050,5923242,"Standard_HS12","FaoStatName: MEAT OF SHEEP (MUTTON & LAMB)",
7,1,"02042100","02042100",977,2007,2050,5923243,"Standard_HS12","FaoStatName: MEAT OF SHEEP (MUTTON & LAMB)",
7,1,"02042200","02042200",977,2007,2050,5923244,"Standard_HS12","FaoStatName: MEAT OF SHEEP (MUTTON & LAMB)",
7,1,"02042300","02042300",977,2007,2050,5923245,"Standard_HS12","FaoStatName: MEAT OF SHEEP (MUTTON & LAMB)",
7,1,"02043000","02043000",977,2007,2050,5923246,"Standard_HS12","FaoStatName: MEAT OF SHEEP (MUTTON & LAMB)",
7,1,"02044100","02044100",977,2007,2050,5923247,"Standard_HS12","FaoStatName: MEAT OF SHEEP (MUTTON & LAMB)",
7,1,"02044200","02044200",977,2007,2050,5923248,"Standard_HS12","FaoStatName: MEAT OF SHEEP (MUTTON & LAMB)",
7,1,"02044300","02044300",977,2007,2050,5923249,"Standard_HS12","FaoStatName: MEAT OF SHEEP (MUTTON & LAMB)",
7,1,"02045000","02045000",1017,2007,2050,5923250,"Standard_HS12","FaoStatName: MEAT OF GOATS (GOAT MEAT)",
7,1,"02050000","02050000",1097,2007,2050,5923251,"Country TL description (WITS) (could also be mapped to 1108)","Meat of horses, asses, mules or hinnies, fresh, chilled or frozen",
7,1,"02061000","02061000",868,2007,2050,5923252,"Country TL description (WITS), Old SWS series (could also be mapped to 948)","Fresh or chilled edible offal of bovine animals",
7,1,"02062100","02062100",868,2007,2050,5923253,"Country TL description (WITS), Old SWS series (could also be mapped to 948)","Frozen edible bovine tongues",
7,1,"02062200","02062200",868,2007,2050,5923254,"Country TL description (WITS), Old SWS series (could also be mapped to 948)","Frozen edible bovine livers",
7,1,"02062900","02062900",868,2007,2050,5923255,"Country TL description (WITS), Old SWS series (could also be mapped to 948)","Frozen edible bovine offal (excl. tongues and livers)",
7,1,"02063000","02063000",1036,2007,2050,5923256,"Standard_HS12","FaoStatName: OFFALS OF PIGS, EDIBLE",
7,1,"02064100","02064100",1036,2007,2050,5923257,"Standard_HS12","FaoStatName: OFFALS OF PIGS, EDIBLE",
7,1,"02064900","02064900",1036,2007,2050,5923258,"Standard_HS12","FaoStatName: OFFALS OF PIGS, EDIBLE",
7,1,"02068000","02068000",1167,2007,2050,5923259,"Country TL description (WITS), Old SWS series (could also be mapped to 978,1018,1098)","Fresh or chilled edible offal of sheep, goats, horses, asses, mules and hinnies",
7,1,"02069000","02069000",1167,2007,2050,5923260,"Country TL description (WITS), Old SWS series (could also be mapped to 978,1018,1098)","Frozen edible offal of sheep, goats, horses, asses, mules and hinnies",
7,1,"02071100","02071100",1058,2007,2050,5923261,"Standard_HS12","FaoStatName: MEAT OF CHICKENS",
7,1,"02071200","02071200",1058,2007,2050,5923262,"Standard_HS12","FaoStatName: MEAT OF CHICKENS",
7,1,"02071300","02071300",1058,2007,2050,5923263,"Country TL description (WITS), Old SWS series (could also be mapped to 1059)","Fresh or chilled cuts and edible offal of fowls of the species Gallus domesticus",
7,1,"02071400","02071400",1058,2007,2050,5923264,"Country TL description (WITS), Old SWS series (could also be mapped to 1059)","Frozen cuts and edible offal of fowls of the species Gallus domesticus",
7,1,"02072400","02072400",1080,2007,2050,5923265,"Standard_HS12","FaoStatName: MEAT OF TURKEYS",
7,1,"02072500","02072500",1080,2007,2050,5923266,"Standard_HS12","FaoStatName: MEAT OF TURKEYS",
7,1,"02072600","02072600",1080,2007,2050,5923267,"Country TL description (WITS) (could also be mapped to 1081)","Fresh or chilled cuts and edible offal of turkeys of the species domesticus",
7,1,"02072700","02072700",1080,2007,2050,5923268,"Country TL description (WITS) (could also be mapped to 1081)","Frozen cuts and edible offal of turkeys of the species domesticus",
7,1,"02073200","02073200",1069,2007,2050,5923269,"Country TL description (WITS) (could also be mapped to 1073)","Fresh or chilled ducks, geese and guinea fowls of the species domesticus, not cut into pieces",
7,1,"02073300","02073300",1069,2007,2050,5923270,"Country TL description (WITS) (could also be mapped to 1073)","Frozen ducks, geese and guinea fowls of the species domesticus, not cut into pieces",
7,1,"02073400","02073400",1074,2007,2050,5923271,"Country TL description (WITS) (could also be mapped to 1075)","Fresh or chilled edible fatty livers of ducks or geese of the species domesticus",
7,1,"02073500","02073500",1069,2007,2050,5923272,"Country TL description (WITS) (could also be mapped to 1073)","Fresh or chilled cuts and edible offal of ducks, geese or guinea fowls of the species domesticus (excl. fatty livers)",
7,1,"02073600","02073600",1069,2007,2050,5923273,"Country TL description (WITS) (could also be mapped to 1073)","Frozen cuts and edible offal of ducks, geese or guinea fowls of the species domesticus",
7,1,"02074100","02074100",1069,2014,2050,5923274,"Standard_HS12","FaoStatName: MEAT OF DUCKS",
7,1,"02074200","02074200",1069,2014,2050,5923275,"Standard_HS12","FaoStatName: MEAT OF DUCKS",
7,1,"02074300","02074300",1075,2014,2050,5923276,"Standard_HS12","FaoStatName: OFFALS LIVER DUCK",
7,1,"02074400","02074400",1069,2015,2050,5923277,"Country TL description (WITS) (could also be mapped to 1075)","Carnes e miudezas, comestíveis, frescas, refrigeradas ou congeladas, das aves da posição 0105 : de patos : outras, frescas ou refrigeradas",
7,1,"02074500","02074500",1069,2014,2050,5923278,"Country TL description (WITS) (could also be mapped to 1075)","Carnes e miudezas, comestíveis, frescas, refrigeradas ou congeladas, das aves da posição 0105 : de patos : outras, congeladas",
7,1,"02075100","02075100",1073,2015,2050,5923279,"Standard_HS12","FaoStatName: MEAT OF GEESE",
7,1,"02075200","02075200",1073,2014,2050,5923280,"Standard_HS12","FaoStatName: MEAT OF GEESE",
7,1,"02075300","02075300",1074,2014,2050,5923281,"Standard_HS12","FaoStatName: OFFALS LIVER GEESE",
7,1,"02075500","02075500",1073,2014,2050,5923282,"Country TL description (WITS) (could also be mapped to 1074)","Carnes e miudezas, comestíveis, frescas, refrigeradas ou congeladas, das aves da posição 0105 : de gansos : outras, congeladas",
7,1,"02076000","02076000",1058,2015,2050,5923283,"Standard_HS12","FaoStatName: MEAT OF CHICKENS",
7,1,"02081000","02081000",1141,2007,2050,5923284,"Country TL description (WITS) (could also be mapped to 1163)","Fresh, chilled or frozen meat and edible offal of rabbits or hares",
7,1,"02082000","02082000",1166,2007,2050,5923285,"Generic HS 2007 to FCL mapping","Fresh, chilled or frozen frogs' legs",
7,1,"02083000","02083000",1166,2014,2050,5923286,"Country TL description (WITS) (could also be mapped to 1167)","Outras carnes e miudezas comestíveis, frescas, refrigeradas ou congeladas : de primatas",
7,1,"02084000","02084000",1166,2011,2050,5923287,"Country TL description (WITS) (could also be mapped to 1167)","Outras carnes e miudezas comestíveis, frescas, refrigeradas ou congeladas : de baleias, golfinhos e botos (mamíferos da ordem dos cetáceos); de manatins (peixes-boi) e dugongos (mamíferos da ordem dos sirénios); de otárias e focas, leões-marinh",
7,1,"02085000","02085000",1166,2007,2050,5923288,"Generic HS 2007 to FCL mapping","Fresh, chilled or frozen meat and edible offal of reptiles e.g. snakes, turtles, crocodiles",
7,1,"02089000","02089000",1163,2007,2050,5923289,"Country TL description (WITS), Old SWS series (could also be mapped to 1089,1127,1128,1151,1158,1159,1166)","Fresh, chilled or frozen meat and edible offal of pigeons, seals, game, reindeer and other animals (excl. bovine animals, swine, sheep, goats, horses, asses, mules, hinnies, poultry fowls of the species Gallus domesticus, ducks, geese, turkeys, guinea fowl, rabbits, hares, primates, whales, dolphins and purpoises mammals of the order Cetacea, manatees and dugongs mammals of the order Sirenia, reptiles and frogs' legs)",
7,1,"02090000","02090000",1037,2007,2050,5923290,"Country TL description (WITS), Old SWS series (could also be mapped to 1040,1065)","Pig fat, free of lean meat, and poultry fat, not rendered or otherwise extracted, fresh, chilled, frozen, salted, in brine, dried or smoked",
7,1,"02091000","02091000",1037,2014,2050,5923291,"Country TL description (WITS), Old SWS series (could also be mapped to 1040)","Toucinho sem partes magras, gorduras de porco e de aves, não fundidas nem extraídas de outro modo, frescos, refrigerados, congelados, salgados ou em salmoura, secos ou fumados (defumados) : de porco",
7,1,"02099000","02099000",1065,2014,2050,5923292,"Standard_HS12","FaoStatName: FAT OF POULTRY",
7,1,"02101100","02101100",1039,2007,2050,5923293,"Standard_HS12","FaoStatName: BACON AND HAM",
7,1,"02101200","02101200",1039,2007,2050,5923294,"Standard_HS12","FaoStatName: BACON AND HAM",
7,1,"02101900","02101900",1039,2007,2050,5923295,"Standard_HS12","FaoStatName: BACON AND HAM",
7,1,"02102000","02102000",872,2007,2050,5923296,"Standard_HS12","FaoStatName: MEAT OF BEEF,DRD, SLTD,SMKD",
7,1,"02102010","02102010",872,2014,2050,5923297,"Standard_HS12","FaoStatName: MEAT OF BEEF,DRD, SLTD,SMKD",
7,1,"02102090","02102090",872,2014,2050,5923298,"Standard_HS12","FaoStatName: MEAT OF BEEF,DRD, SLTD,SMKD",
7,1,"02109100","02109100",1164,2010,2050,5923299,"Standard_HS12","FaoStatName: MEAT DRIED NES",
7,1,"02109300","02109300",1164,2012,2050,5923300,"Standard_HS12","FaoStatName: MEAT DRIED NES",
7,1,"02109900","02109900",1164,2007,2050,5923301,"Standard_HS12","FaoStatName: MEAT DRIED NES",
7,1,"03076000","03076000",1176,2007,2050,5923302,"Standard_HS12","FaoStatName: SNAILS, NOT SEA SNAILS",
7,1,"04011010","04011010",888,2007,2050,5923303,"Trademap TL description, Old SWS series (could also be mapped to 954,985,1023)","Milk not concentrated and unsweetened not exceeding 1% fat : leite para crianças",
7,1,"04011090","04011090",888,2007,2050,5923304,"Trademap TL description, Old SWS series (could also be mapped to 954,985,1023)","Milk not concentrated and unsweetened not exceeding 1% fat : outros",
7,1,"04012000","04012000",882,2007,2050,5923305,"Country TL description (WITS), Old SWS series (could also be mapped to 883,908,951,982,1020,1130)","Milk and cream of a fat content by weight of > 1% but <= 6%, not concentrated nor containing added sugar or other sweetening matter",
7,1,"04012010","04012010",882,2014,2050,5923306,"Country TL description (WITS), Old SWS series (could also be mapped to 883,908,951,982,1020,1130)","Leite e nata, não concentrados nem adicionados de açúcar ou de outros edulcorantes : com um teor, em peso, de matérias gordas, superior a 1 % mas não superior a 6 %: a granel",
7,1,"04012090","04012090",882,2014,2050,5923307,"Country TL description (WITS), Old SWS series (could also be mapped to 883,908,951,982,1020,1130)","Leite e nata, não concentrados nem adicionados de açúcar ou de outros edulcorantes : com um teor, em peso, de matérias gordas, superior a 1 % mas não superior a 6 %: acondicionado para venda a retalho",
7,1,"04013000","04013000",885,2007,2050,5923308,"Generic HS 2007 to FCL mapping","Milk and cream of a fat content by weight of > 6%, not concentrated nor containing added sugar or other sweetening matter",
7,1,"04014000","04014000",885,2014,2050,5923309,"Country TL description (WITS), previous mapping (anything above 6% was mapped to cream)","Leite e nata, não concentrados nem adicionados de açúcar ou de outros edulcorantes : com um teor, em peso, de matérias gordas, superior a 6 % mas não superior a 10 %",
7,1,"04015000","04015000",885,2014,2050,5923310,"Country TL description (WITS), previous mapping (anything above 6% was mapped to cream)","Leite e nata, não concentrados nem adicionados de açúcar ou de outros edulcorantes : com um teor, em peso, de matérias gordas, superior a 10 %",
7,1,"04021000","04021000",898,2007,2050,5923311,"Standard_HS12","FaoStatName: MILK SKIMMED DRY",
7,1,"04022100","04022100",897,2007,2050,5923312,"Standard_HS12","FaoStatName: MILK WHOLE DRIED",
7,1,"04022900","04022900",897,2007,2050,5923313,"Standard_HS12","FaoStatName: MILK WHOLE DRIED",
7,1,"04029100","04029100",894,2007,2050,5923314,"Country TL description (WITS), Old SWS series (could also be mapped to 895)","Milk and cream, concentrated but unsweetened (excl. in solid forms)",
7,1,"04029900","04029900",889,2007,2050,5923315,"Country TL description (WITS), Old SWS series (could also be mapped to 896)","Milk and cream, concentrated and sweetened (excl. in solid forms)",
7,1,"04031000","04031000",892,2007,2050,5923316,"Country TL description (WITS), Old SWS series (could also be mapped to 891)","Yogurt, whether or not flavoured or containing added sugar or other sweetening matter, fruits, nuts or cocoa",
7,1,"04039000","04039000",893,2007,2050,5923317,"Country TL description (WITS), Old SWS series (could also be mapped to 899)","Buttermilk, curdled milk and cream, kephir and other fermented or acidified milk and cream, whether or not concentrated or flavoured or containing added sugar or other sweetening matter, fruits, nuts or cocoa (excl. yogurt)",
7,1,"04041000","04041000",903,2007,2050,5923318,"Country TL description (WITS), Old SWS series (could also be mapped to 890,900)","Whey and modified whey, whether or not concentrated or containing added sugar or other sweetening matter",
7,1,"04049000","04049000",909,2007,2050,5923319,"Standard_HS12","FaoStatName: prod.of nat.milk constit",
7,1,"04051000","04051000",886,2007,2050,5923320,"Country TL description (WITS), Old SWS series (could also be mapped to 952,983,1022)","Butter (excl. dehydrated butter and ghee)",
7,1,"04052000","04052000",886,2007,2050,5923321,"Country TL description (WITS), Old SWS series (could also be mapped to 952,983,1022)","Dairy spreads of a fat content, by weight, of >= 39% but < 80%",
7,1,"04059000","04059000",887,2007,2050,5923322,"Country TL description (WITS) (could also be mapped to 953,1022)","Fats and oils derived from milk, and dehydrated butter and ghee (excl. natural butter, recombined butter and whey butter)",
7,1,"04061000","04061000",901,2007,2050,5923323,"Country TL description (WITS), Old SWS series (could also be mapped to 904,905,955,984,1021)","Fresh cheese, i.e. unripened or uncured cheese, incl. whey cheese, and curd",
7,1,"04062000","04062000",901,2007,2050,5923324,"Standard_HS12","FaoStatName: CHEESE OF WHOLE COW MILK",
7,1,"04063000","04063000",907,2007,2050,5923325,"Standard_HS12","FaoStatName: PROCESSED CHEESE",
7,1,"04064000","04064000",901,2007,2050,5923326,"Standard_HS12","FaoStatName: CHEESE OF WHOLE COW MILK",
7,1,"04069000","04069000",901,2007,2050,5923327,"Standard_HS12","FaoStatName: CHEESE OF WHOLE COW MILK",
7,1,"04070000","04070000",1062,2007,2050,5923328,"Trademap TL description (could also be mapped to 1091)","Eggs, bird, in shell, fresh, preserved or cooked",
7,1,"04070010","04070010",1062,2007,2050,5923329,"Trademap TL description (could also be mapped to 1091)","Eggs, bird, in shell, fresh, preserved or cooked : ovos de aves, com casca, frescos, conservados ou",
7,1,"04070090","04070090",1062,2007,2050,5923330,"Trademap TL description (could also be mapped to 1091)","Eggs, bird, in shell, fresh, preserved or cooked : ovos germinados para encubação",
7,1,"04071100","04071100",1062,2014,2050,5923331,"Standard_HS12","FaoStatName: HEN EGGS",
7,1,"04071900","04071900",1091,2014,2050,5923332,"Standard_HS12","FaoStatName: EGGS EXCL HENEGGS",
7,1,"04072100","04072100",1062,2014,2050,5923333,"Standard_HS12","FaoStatName: HEN EGGS",
7,1,"04072900","04072900",1091,2014,2050,5923334,"Standard_HS12","FaoStatName: EGGS EXCL HENEGGS",
7,1,"04079000","04079000",1062,2014,2050,5923335,"Country TL description (WITS) (could also be mapped to 1091)","Ovos de aves, com casca, frescos, conservados ou cozidos : outros",
7,1,"04081100","04081100",1064,2007,2050,5923336,"Standard_HS12","FaoStatName: EGGS DRIED",
7,1,"04081900","04081900",1063,2007,2050,5923337,"Standard_HS12","FaoStatName: EGGS LIQUID",
7,1,"04089100","04089100",1064,2007,2050,5923338,"Standard_HS12","FaoStatName: EGGS DRIED",
7,1,"04089900","04089900",1063,2007,2050,5923339,"Standard_HS12","FaoStatName: EGGS LIQUID",
7,1,"04090000","04090000",1182,2007,2050,5923340,"Standard_HS12","FaoStatName: HONEY NATURAL",
7,1,"04100000","04100000",1232,2007,2050,5923341,"Standard_HS12","FaoStatName: FOOD PREP NES",
7,1,"05010000","05010000",1293,2007,2050,5923342,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05021000","05021000",1293,2010,2050,5923343,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05029000","05029000",1293,2009,2050,5923344,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05040000","05040000",1293,2007,2050,5923345,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05051000","05051000",1293,2009,2050,5923346,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05059000","05059000",1293,2011,2050,5923347,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05061000","05061000",1293,2011,2050,5923348,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05069000","05069000",1293,2010,2050,5923349,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05071000","05071000",1293,2014,2050,5923350,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05079000","05079000",1293,2007,2050,5923351,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05080000","05080000",1293,2009,2050,5923352,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05090000","05090000",1293,2007,2050,5923353,"Country TL description (WITS)","Natural sponges of animal origin",
7,1,"05100000","05100000",1293,2011,2050,5923354,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05111000","05111000",1293,2009,2050,5923355,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05119100","05119100",1293,2009,2050,5923356,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"05119900","05119900",1293,2007,2050,5923357,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06011000","06011000",1293,2007,2050,5923358,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06012000","06012000",1293,2009,2050,5923359,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06021000","06021000",1293,2007,2050,5923360,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06022000","06022000",1293,2007,2050,5923361,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06023000","06023000",1293,2009,2050,5923362,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06024000","06024000",1293,2007,2050,5923363,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06029000","06029000",1293,2007,2050,5923364,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06031000","06031000",1293,2007,2050,5923365,"Country TL description (WITS)","Fresh cut flowers and flower buds, for bouquest or for ornamental purposes",
7,1,"06031100","06031100",1293,2009,2050,5923366,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06031200","06031200",1293,2009,2050,5923367,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06031300","06031300",1293,2009,2050,5923368,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06031400","06031400",1293,2009,2050,5923369,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06031500","06031500",1293,2014,2050,5923370,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06031900","06031900",1293,2009,2050,5923371,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06039000","06039000",1293,2007,2050,5923372,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06041000","06041000",1293,2009,2050,5923373,"Trademap TL description","Mosses and lichens suitable for bouquets or for ornamental purposes",
7,1,"06042000","06042000",1293,2014,2050,5923374,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06049000","06049000",1293,2014,2050,5923375,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"06049100","06049100",1293,2007,2050,5923376,"Country TL description (WITS)","Foliage, branches and other parts of plants, without flowers or flower buds, grasses, fresh, for bouquets or ornamental purposes",
7,1,"06049900","06049900",1293,2007,2050,5923377,"Country TL description (WITS)","Foliage, branches and other parts of plants, without flowers or flower buds, grasses, for bouquets or ornamental purposes, dried, dyed, bleached, impregnated or otherwise prepared",
7,1,"07011000","07011000",116,2007,2050,5923378,"Standard_HS12","FaoStatName: POTATOES",
7,1,"07019000","07019000",116,2007,2050,5923379,"Standard_HS12","FaoStatName: POTATOES",
7,1,"07020000","07020000",388,2007,2050,5923380,"Standard_HS12","FaoStatName: TOMATOES FRESH",
7,1,"07031000","07031000",403,2007,2050,5923381,"Country TL description (WITS) (could also be mapped to 402)","Fresh or chilled onions and shallots",
7,1,"07031010","07031010",403,2014,2050,5923382,"Country TL description (WITS) (could also be mapped to 402)","Cebolas, chalotas, alhos, alhos-porros e outros produtos hortícolas aliáceos, frescos ou refrigerados : cebolas e chalotas: cebolas",
7,1,"07031020","07031020",402,2014,2050,5923383,"Country TL description (WITS)","Cebolas, chalotas, alhos, alhos-porros e outros produtos hortícolas aliáceos, frescos ou refrigerados : cebolas e chalotas: outros",
7,1,"07032000","07032000",406,2007,2050,5923384,"Standard_HS12","FaoStatName: GARLIC",
7,1,"07039000","07039000",407,2007,2050,5923385,"Standard_HS12","FaoStatName: LEEKS AND OTHER ALLIACEOUS VEGETABLES",
7,1,"07041000","07041000",393,2007,2050,5923386,"Standard_HS12","FaoStatName: CAULIFLOWER",
7,1,"07042000","07042000",358,2007,2050,5923387,"Standard_HS12","FaoStatName: CABBAGES",
7,1,"07049000","07049000",358,2007,2050,5923388,"Standard_HS12","FaoStatName: CABBAGES",
7,1,"07051100","07051100",372,2007,2050,5923389,"Standard_HS12","FaoStatName: LETTUCE",
7,1,"07051900","07051900",372,2007,2050,5923390,"Standard_HS12","FaoStatName: LETTUCE",
7,1,"07052100","07052100",372,2010,2050,5923391,"Standard_HS12","FaoStatName: LETTUCE",
7,1,"07052900","07052900",372,2007,2050,5923392,"Standard_HS12","FaoStatName: LETTUCE",
7,1,"07061000","07061000",426,2007,2050,5923393,"Country TL description (WITS)","Fresh or chilled carrots and turnips",
7,1,"07061010","07061010",426,2014,2050,5923394,"Country TL description (WITS)","Cenouras, nabos, beterrabas para salada, cercefi, aipo-rábano, rabanetes e raízes comestíveis semelhantes, frescos ou refrigerados : cenouras e nabos: cenouras",
7,1,"07061020","07061020",463,2014,2050,5923395,"Country TL description (WITS)","Cenouras, nabos, beterrabas para salada, cercefi, aipo-rábano, rabanetes e raízes comestíveis semelhantes, frescos ou refrigerados : cenouras e nabos: nabos",
7,1,"07069000","07069000",463,2007,2050,5923396,"Standard_HS12","FaoStatName: VEGETABLES FRESH NES",
7,1,"07070000","07070000",397,2007,2050,5923397,"Standard_HS12","FaoStatName: CUCUMBERS, GHERKINS",
7,1,"07081000","07081000",417,2007,2050,5923398,"Standard_HS12","FaoStatName: PEAS GREEN",
7,1,"07082000","07082000",414,2007,2050,5923399,"Country TL description (WITS)","Fresh or chilled beans Vigna spp., Phaseolus spp., shelled or unshelled",
7,1,"07089000","07089000",420,2007,2050,5923400,"Standard_HS12","FaoStatName: BROADBEANS GREEN",
7,1,"07091000","07091000",366,2007,2050,5923401,"Country TL description (WITS)","Fresh or chilled globe artichokes",
7,1,"07092000","07092000",367,2007,2050,5923402,"Standard_HS12","FaoStatName: ASPARAGUS",
7,1,"07093000","07093000",399,2007,2050,5923403,"Standard_HS12","FaoStatName: EGGPLANTS",
7,1,"07094000","07094000",463,2007,2050,5923404,"Standard_HS12","FaoStatName: VEGETABLES FRESH NES",
7,1,"07095100","07095100",449,2007,2050,5923405,"Standard_HS12","FaoStatName: MUSHROOMS",
7,1,"07095200","07095200",449,2007,2050,5923406,"Country TL description (WITS), FCL descriptions","Fresh or chilled truffles",
7,1,"07095900","07095900",449,2007,2050,5923407,"Standard_HS12","FaoStatName: MUSHROOMS",
7,1,"07096000","07096000",401,2007,2050,5923408,"Standard_HS12","FaoStatName: CHILLIES,PEPPERS GREEN",
7,1,"07097000","07097000",373,2007,2050,5923409,"Standard_HS12","FaoStatName: SPINACH",
7,1,"07099000","07099000",463,2007,2050,5923410,"Country TL description (WITS) (could also be mapped to 446,430,394,378,260)","Fresh or chilled vegetables (excl. potatoes, tomatoes, vegetables of the Allium spp., cabbages of the genus Brassica, lettuces of the species Lactuca sativa and Cichorium, carrots, turnips, salad beetroot, salsify, celeriac, radishes and similar edible roots, cucumbers and gherkins, leguminous vegetables, artichokes, asparagus, aubergines, mushrooms, truffles, fruits of the genus Capsicum or of the genus Pimenta, spinach, New Zealand spinach and orache spinach)",
7,1,"07099100","07099100",366,2014,2050,5923411,"Standard_HS12","FaoStatName: Artichokes ",
7,1,"07099200","07099200",260,2014,2050,5923412,"Standard_HS12","FaoStatName: OLIVES",
7,1,"07099300","07099300",394,2014,2050,5923413,"Standard_HS12","FaoStatName: PUMPKINS,SQUASH, GOURDS",
7,1,"07099900","07099900",463,2014,2050,5923414,"Country TL description (WITS) (could also be mapped to 378,430,446)","Outros produtos hortícolas, frescos ou refrigerados : outros : outros",
7,1,"07101000","07101000",118,2007,2050,5923415,"Standard_HS12","FaoStatName: FROZEN POTATOES",
7,1,"07102100","07102100",473,2007,2050,5923416,"Standard_HS12","FaoStatName: VEGETABLE FROZEN",
7,1,"07102200","07102200",473,2007,2050,5923417,"Standard_HS12","FaoStatName: VEGETABLE FROZEN",
7,1,"07102900","07102900",473,2007,2050,5923418,"Standard_HS12","FaoStatName: VEGETABLE FROZEN",
7,1,"07103000","07103000",473,2007,2050,5923419,"Standard_HS12","FaoStatName: VEGETABLE FROZEN",
7,1,"07104000","07104000",447,2007,2050,5923420,"Standard_HS12","FaoStatName: SWEET CORN FROZEN",
7,1,"07108000","07108000",473,2007,2050,5923421,"Standard_HS12","FaoStatName: VEGETABLE FROZEN",
7,1,"07109000","07109000",473,2007,2050,5923422,"Standard_HS12","FaoStatName: VEGETABLE FROZEN",
7,1,"07112000","07112000",262,2007,2050,5923423,"Standard_HS12","FaoStatName: OLIVES PRESERVED",
7,1,"07113000","07113000",474,2007,2050,5923424,"Country TL description (WITS)","Capers provisionally preserved, e.g. by sulphur dioxide gas, in brine, in sulphur water or in other preservative solutions, but unsuitable in that state for immediate consumption",
7,1,"07114000","07114000",474,2007,2050,5923425,"Standard_HS12","FaoStatName: VEG.IN TEM. PRESERVATIVES",
7,1,"07115100","07115100",474,2007,2050,5923426,"Standard_HS12","FaoStatName: VEG.IN TEM. PRESERVATIVES",
7,1,"07115900","07115900",474,2007,2050,5923427,"Standard_HS12","FaoStatName: VEG.IN TEM. PRESERVATIVES",
7,1,"07119000","07119000",474,2007,2050,5923428,"Standard_HS12","FaoStatName: VEG.IN TEM. PRESERVATIVES",
7,1,"07122000","07122000",469,2007,2050,5923429,"Standard_HS12","FaoStatName: VEGETABLES DEHYDRATED",
7,1,"07123100","07123100",450,2007,2050,5923430,"Standard_HS12","FaoStatName: DRIED MUSHROOMS",
7,1,"07123200","07123200",450,2007,2050,5923431,"Standard_HS12","FaoStatName: DRIED MUSHROOMS",
7,1,"07123300","07123300",450,2009,2050,5923432,"Standard_HS12","FaoStatName: DRIED MUSHROOMS",
7,1,"07123900","07123900",450,2007,2050,5923433,"Standard_HS12","FaoStatName: DRIED MUSHROOMS",
7,1,"07129000","07129000",469,2007,2050,5923434,"Standard_HS12","FaoStatName: VEGETABLES DEHYDRATED",
7,1,"07131000","07131000",187,2007,2050,5923435,"Standard_HS12","FaoStatName: PEAS DRY",
7,1,"07132000","07132000",191,2007,2050,5923436,"Standard_HS12","FaoStatName: CHICK-PEAS",
7,1,"07133100","07133100",176,2007,2050,5923437,"Standard_HS12","FaoStatName: BEANS DRY",
7,1,"07133200","07133200",176,2007,2050,5923438,"Standard_HS12","FaoStatName: BEANS DRY",
7,1,"07133300","07133300",176,2007,2050,5923439,"Standard_HS12","FaoStatName: BEANS DRY",
7,1,"07133310","07133310",176,2009,2050,5923440,"Standard_HS12","FaoStatName: BEANS DRY",
7,1,"07133320","07133320",176,2009,2050,5923441,"Standard_HS12","FaoStatName: BEANS DRY",
7,1,"07133390","07133390",176,2009,2050,5923442,"Standard_HS12","FaoStatName: BEANS DRY",
7,1,"07133400","07133400",203,2015,2050,5923443,"Standard_HS12","FaoStatName: BAMBARA BEANS",
7,1,"07133500","07133500",195,2014,2050,5923444,"Standard_HS12","FaoStatName: COW PEAS DRY",
7,1,"07133900","07133900",176,2007,2050,5923445,"Standard_HS12","FaoStatName: BEANS DRY",
7,1,"07134000","07134000",201,2007,2050,5923446,"Standard_HS12","FaoStatName: LENTILS",
7,1,"07135000","07135000",181,2007,2050,5923447,"Standard_HS12","FaoStatName: BROAD BEANS DRY",
7,1,"07136000","07136000",197,2014,2050,5923448,"Standard_HS12","FaoStatName: PIGEON PEAS",
7,1,"07139000","07139000",211,2007,2050,5923449,"Standard_HS12","FaoStatName: PULSES, NES",
7,1,"07141000","07141000",125,2010,2050,5923450,"Country TL description (WITS) (could also be mapped to 128)","Manioc (cassava), fresh or dried, whether or not sliced or pelleted",
7,1,"07142000","07142000",122,2007,2050,5923451,"Standard_HS12","FaoStatName: SWEET POTATOES",
7,1,"07143000","07143000",137,2014,2050,5923452,"Standard_HS12","FaoStatName: YAMS",
7,1,"07144000","07144000",136,2014,2050,5923453,"Standard_HS12","FaoStatName: TARO (COCOYAM)",
7,1,"07145000","07145000",135,2015,2050,5923454,"Standard_HS12","FaoStatName: YAUTIA (COCOYAM)",
7,1,"07149000","07149000",149,2007,2050,5923455,"Country TL description (WITS), subsequent data series where the bulk of imports enter under 0714.90 (could also be  mapped to 135,136,137,151)","Roots and tubers of arrowroot, salep, Jerusalem artichokes and similar roots and tubers with high starch or inulin content, fresh, chilled, frozen or dried, whether or not sliced or in the form of pellets and sago pith (excl. manioc cassava and sweet potatoes)",
7,1,"08011100","08011100",250,2007,2050,5923456,"Standard_HS12","FaoStatName: COCONUTS DESICCATED",
7,1,"08011200","08011200",249,2014,2050,5923457,"Standard_HS12","FaoStatName: COCONUTS",
7,1,"08011900","08011900",249,2007,2050,5923458,"Standard_HS12","FaoStatName: COCONUTS",
7,1,"08012100","08012100",216,2007,2050,5923459,"Standard_HS12","FaoStatName: BRAZIL NUTS IN SHELL",
7,1,"08012200","08012200",229,2007,2050,5923460,"Standard_HS12","FaoStatName: BRAZIL NUTS SHELLED",
7,1,"08013100","08013100",217,2007,2050,5923461,"Standard_HS12","FaoStatName: CASHEW NUTS IN SHELL",
7,1,"08013200","08013200",230,2007,2050,5923462,"Standard_HS12","FaoStatName: CASHEW NUTS SHELLED",
7,1,"08021100","08021100",221,2007,2050,5923463,"Standard_HS12","FaoStatName: ALMONDS IN SHELL",
7,1,"08021200","08021200",231,2007,2050,5923464,"Standard_HS12","FaoStatName: ALMONDS SHELLED",
7,1,"08022100","08022100",225,2007,2050,5923465,"Standard_HS12","FaoStatName: HAZELNUTS (FILBERTS) IN SHELL",
7,1,"08022200","08022200",233,2007,2050,5923466,"Standard_HS12","FaoStatName: HAZELNUTS SHELLED",
7,1,"08023100","08023100",222,2007,2050,5923467,"Standard_HS12","FaoStatName: WALNUTS IN SHELL",
7,1,"08023200","08023200",232,2007,2050,5923468,"Standard_HS12","FaoStatName: WALNUTS SHELLED",
7,1,"08024000","08024000",220,2007,2050,5923469,"Country TL description (WITS)","Fresh or dried chestnuts Castanea spp., whether or not shelled or peeled",
7,1,"08024100","08024100",220,2014,2050,5923470,"Standard_HS12","FaoStatName: CHESTNUTS",
7,1,"08024200","08024200",220,2014,2050,5923471,"Standard_HS12","FaoStatName: CHESTNUTS",
7,1,"08025000","08025000",223,2007,2050,5923472,"Country TL description (WITS)","Fresh or dried pistachios, whether or not shelled or peeled",
7,1,"08025100","08025100",223,2014,2050,5923473,"Standard_HS12","FaoStatName: PISTACHIOS",
7,1,"08025200","08025200",223,2014,2050,5923474,"Standard_HS12","FaoStatName: PISTACHIOS",
7,1,"08026000","08026000",234,2009,2050,5923475,"Trademap TL description","Los demás frutos de cáscara frescos o secos, incluso sin cáscara o mondados : Nueces de macadamia",
7,1,"08026100","08026100",234,2014,2050,5923476,"Standard_HS12","FaoStatName: Nuts nes (MACADAMIA NUTS)",
7,1,"08026200","08026200",234,2014,2050,5923477,"Standard_HS12","FaoStatName: Nuts nes (MACADAMIA NUTS)",
7,1,"08027000","08027000",224,2015,2050,5923478,"Standard_HS12","FaoStatName: KOLANUTS",
7,1,"08029000","08029000",234,2007,2050,5923479,"Standard_HS12","FaoStatName: NUTS, NES",
7,1,"08030000","08030000",489,2007,2050,5923480,"Country TL description (WITS), subsequent data series (could also be mapped to 486, 604)","Bananas, incl. plantains, fresh or dried",
7,1,"08041000","08041000",577,2007,2050,5923481,"Standard_HS12","FaoStatName: DATES",
7,1,"08042000","08042000",570,2007,2050,5923482,"Country TL description (WITS), Old SWS series (could also be mapped to 569)","Fresh or dried figs",
7,1,"08043000","08043000",574,2007,2050,5923483,"Standard_HS12","FaoStatName: PINEAPPLES",
7,1,"08044000","08044000",572,2007,2050,5923484,"Standard_HS12","FaoStatName: AVOCADOS",
7,1,"08045000","08045000",603,2007,2050,5923485,"Country TL description (WITS), Old SWS series (could also be mapped to 571, 604)","Fresh or dried guavas, mangoes and mangosteens",
7,1,"08051000","08051000",490,2007,2050,5923486,"Standard_HS12","FaoStatName: ORANGES",
7,1,"08052000","08052000",495,2007,2050,5923487,"Standard_HS12","FaoStatName: TANG.MAND. CLEMENT.SATSMA",
7,1,"08052010","08052010",495,2014,2050,5923488,"Standard_HS12","FaoStatName: TANG.MAND. CLEMENT.SATSMA",
7,1,"08052020","08052020",495,2014,2050,5923489,"Standard_HS12","FaoStatName: TANG.MAND. CLEMENT.SATSMA",
7,1,"08054000","08054000",507,2007,2050,5923490,"Standard_HS12","FaoStatName: GRAPEFRUIT&POMELO 0",
7,1,"08055000","08055000",497,2007,2050,5923491,"Standard_HS12","FaoStatName: LEMONS AND LIMES",
7,1,"08059000","08059000",512,2007,2050,5923492,"Standard_HS12","FaoStatName: CITRUS FRUIT NES",
7,1,"08061000","08061000",560,2007,2050,5923493,"Standard_HS12","FaoStatName: GRAPES",
7,1,"08062000","08062000",561,2007,2050,5923494,"Standard_HS12","FaoStatName: RAISINS",
7,1,"08071100","08071100",567,2007,2050,5923495,"Standard_HS12","FaoStatName: WATERMELONS",
7,1,"08071900","08071900",568,2007,2050,5923496,"Standard_HS12","FaoStatName: MELONS INCL. CANTALOUPES",
7,1,"08072000","08072000",600,2007,2050,5923497,"Standard_HS12","FaoStatName: PAPAYAS",
7,1,"08081000","08081000",515,2007,2050,5923498,"Standard_HS12","FaoStatName: APPLES",
7,1,"08082000","08082000",521,2007,2050,5923499,"Country TL description (WITS), Old SWS series (could also be mapped to 523)","Fresh pears and quinces",
7,1,"08083000","08083000",521,2014,2050,5923500,"Standard_HS12","FaoStatName: PEARS",
7,1,"08084000","08084000",523,2014,2050,5923501,"Standard_HS12","FaoStatName: QUINCES",
7,1,"08091000","08091000",526,2007,2050,5923502,"Standard_HS12","FaoStatName: APRICOTS",
7,1,"08092000","08092000",531,2007,2050,5923503,"Country TL description (WITS) (could also be mapped to 530)","Fresh cherries",
7,1,"08092900","08092900",531,2014,2050,5923504,"Standard_HS12","FaoStatName: CHERRIES",
7,1,"08093000","08093000",534,2007,2050,5923505,"Standard_HS12","FaoStatName: PEACHES AND NECTARINES",
7,1,"08094000","08094000",536,2007,2050,5923506,"Standard_HS12","FaoStatName: PLUMS",
7,1,"08101000","08101000",544,2007,2050,5923507,"Standard_HS12","FaoStatName: STRAWBERRIES",
7,1,"08102000","08102000",547,2007,2050,5923508,"Country TL description (WITS) (could also be mapped to 558)","Fresh raspberries, blackberries, mulberries and loganberries",
7,1,"08103000","08103000",550,2014,2050,5923509,"Country TL description (WITS) (could also be mapped to 549)","Outras frutas frescas : groselhas, incluindo o cássis",
7,1,"08104000","08104000",558,2009,2050,5923510,"Country TL description (WITS) (could also be mapped to 552,554)","Outras frutas frescas : airelas, mirtilos e outras frutas do género vaccinium",
7,1,"08105000","08105000",592,2007,2050,5923511,"Standard_HS12","FaoStatName: kiwi fruit",
7,1,"08106000","08106000",603,2007,2050,5923512,"Standard_HS12","FaoStatName: FRUIT TROPICAL FRESH NES",
7,1,"08107000","08107000",587,2014,2050,5923513,"Standard_HS12","FaoStatName: PERSIMMONS",
7,1,"08109000","08109000",619,2007,2050,5923514,"Country TL description (WITS) (could also be mapped to 587,591)","Fresh tamarinds, cashew apples, jackfruit, lychees, sapodillo plums, passion fruit, carambola, pitahaya and other edible fruit (excl. nuts, bananas, dates, figs, pineapples, avocadoes, guavas, mangoes, mangosteens, papaws papayas, citrus fruit, grapes, melons, apples, pears quinces, apricots, cherries, peaches, plums, sloes, strawberries, raspberries, mulberries, blackberries, loganberries, black, white or redcurrants, gooseberries, cranberries, fruits of the genus Vaccinium, kiwifruit and durians)",
7,1,"08111000","08111000",623,2007,2050,5923515,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"08112000","08112000",623,2007,2050,5923516,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"08119000","08119000",623,2007,2050,5923517,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"08121000","08121000",623,2007,2050,5923518,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"08129000","08129000",623,2007,2050,5923519,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"08131000","08131000",527,2007,2050,5923520,"Standard_HS12","FaoStatName: DRY APRICOTS",
7,1,"08132000","08132000",537,2007,2050,5923521,"Standard_HS12","FaoStatName: PLUMS DRIED (PRUNES)",
7,1,"08133000","08133000",620,2007,2050,5923522,"Standard_HS12","FaoStatName: FRUIT DRIED NES",
7,1,"08134000","08134000",620,2007,2050,5923523,"Standard_HS12","FaoStatName: FRUIT DRIED NES",
7,1,"08135000","08135000",620,2007,2050,5923524,"Standard_HS12","FaoStatName: FRUIT DRIED NES",
7,1,"08140000","08140000",623,2009,2050,5923525,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"09011110","09011110",656,2007,2050,5923526,"Standard_HS12","FaoStatName: COFFEE GREEN",
7,1,"09011120","09011120",656,2007,2050,5923527,"Standard_HS12","FaoStatName: COFFEE GREEN",
7,1,"09011190","09011190",656,2007,2050,5923528,"Standard_HS12","FaoStatName: COFFEE GREEN",
7,1,"09011200","09011200",656,2007,2050,5923529,"Standard_HS12","FaoStatName: COFFEE GREEN",
7,1,"09012100","09012100",657,2007,2050,5923530,"Standard_HS12","FaoStatName: COFFEE ROASTED",
7,1,"09012200","09012200",657,2007,2050,5923531,"Standard_HS12","FaoStatName: COFFEE ROASTED",
7,1,"09019000","09019000",660,2007,2050,5923532,"Country TL description (WITS) (could also be mapped to 658)","Coffee husks and skins; coffee substitutes containing coffee in any proportion",
7,1,"09021000","09021000",667,2007,2050,5923533,"Standard_HS12","FaoStatName: TEA",
7,1,"09022000","09022000",667,2007,2050,5923534,"Standard_HS12","FaoStatName: TEA",
7,1,"09023000","09023000",667,2007,2050,5923535,"Standard_HS12","FaoStatName: TEA",
7,1,"09024000","09024000",667,2007,2050,5923536,"Standard_HS12","FaoStatName: TEA",
7,1,"09030000","09030000",671,2007,2050,5923537,"Standard_HS12","FaoStatName: MATE",
7,1,"09041100","09041100",687,2007,2050,5923538,"Standard_HS12","FaoStatName: PEPPER",
7,1,"09041200","09041200",687,2007,2050,5923539,"Standard_HS12","FaoStatName: PEPPER",
7,1,"09042000","09042000",689,2007,2050,5923540,"Generic HS 2007 to FCL mapping","Fruits of the genus Capsicum or of the genus Pimenta, dried or crushed or ground",
7,1,"09042100","09042100",689,2014,2050,5923541,"Standard_HS12","FaoStatName: PIMENTO",
7,1,"09042200","09042200",689,2014,2050,5923542,"Standard_HS12","FaoStatName: PIMENTO",
7,1,"09050000","09050000",692,2007,2050,5923543,"Generic HS 2007 to FCL mapping","Vanilla",
7,1,"09051000","09051000",692,2014,2050,5923544,"Standard_HS12","FaoStatName: VANILLA",
7,1,"09052000","09052000",692,2014,2050,5923545,"Standard_HS12","FaoStatName: VANILLA",
7,1,"09061000","09061000",693,2007,2050,5923546,"Generic HS 2007 to FCL mapping","Cinnamon and cinnamon-tree flowers (excl. crushed and ground)",
7,1,"09061100","09061100",693,2009,2050,5923547,"Standard_HS12","FaoStatName: CINNAMON(CANELLA)",
7,1,"09061900","09061900",693,2009,2050,5923548,"Standard_HS12","FaoStatName: CINNAMON(CANELLA)",
7,1,"09062000","09062000",693,2007,2050,5923549,"Standard_HS12","FaoStatName: CINNAMON(CANELLA)",
7,1,"09070000","09070000",698,2007,2050,5923550,"Generic HS 2007 to FCL mapping","Cloves, whole fruit, cloves and stems",
7,1,"09071000","09071000",698,2014,2050,5923551,"Standard_HS12","FaoStatName: CLOVESWHOLE STEM",
7,1,"09072000","09072000",698,2014,2050,5923552,"Standard_HS12","FaoStatName: CLOVESWHOLE STEM",
7,1,"09081000","09081000",702,2007,2050,5923553,"Generic HS 2007 to FCL mapping","Nutmeg",
7,1,"09081100","09081100",702,2014,2050,5923554,"Standard_HS12","FaoStatName: NUTMEG MACE CARDAMONS",
7,1,"09081200","09081200",702,2014,2050,5923555,"Standard_HS12","FaoStatName: NUTMEG MACE CARDAMONS",
7,1,"09082000","09082000",702,2007,2050,5923556,"Generic HS 2007 to FCL mapping","Mace",
7,1,"09082200","09082200",702,2014,2050,5923557,"Standard_HS12","FaoStatName: NUTMEG MACE CARDAMONS",
7,1,"09083000","09083000",702,2007,2050,5923558,"Generic HS 2007 to FCL mapping","Cardamoms",
7,1,"09083100","09083100",702,2014,2050,5923559,"Standard_HS12","FaoStatName: NUTMEG MACE CARDAMONS",
7,1,"09083200","09083200",702,2014,2050,5923560,"Standard_HS12","FaoStatName: NUTMEG MACE CARDAMONS",
7,1,"09091000","09091000",711,2007,2050,5923561,"Generic HS 2007 to FCL mapping","Seeds of anise or badian",
7,1,"09092000","09092000",711,2007,2050,5923562,"Generic HS 2007 to FCL mapping","Coriander seeds",
7,1,"09092100","09092100",711,2014,2050,5923563,"Standard_HS12","FaoStatName: ANISE BADIAN FENNEL",
7,1,"09092200","09092200",711,2014,2050,5923564,"Standard_HS12","FaoStatName: ANISE BADIAN FENNEL",
7,1,"09093000","09093000",711,2007,2050,5923565,"Generic HS 2007 to FCL mapping","Cumin seeds",
7,1,"09093100","09093100",711,2014,2050,5923566,"Standard_HS12","FaoStatName: ANISE BADIAN FENNEL",
7,1,"09093200","09093200",711,2014,2050,5923567,"Standard_HS12","FaoStatName: ANISE BADIAN FENNEL",
7,1,"09094000","09094000",711,2007,2050,5923568,"Generic HS 2007 to FCL mapping","Caraway seeds",
7,1,"09095000","09095000",711,2007,2050,5923569,"Generic HS 2007 to FCL mapping","Seeds of fennel; juniper berries",
7,1,"09096100","09096100",711,2014,2050,5923570,"Standard_HS12","FaoStatName: ANISE BADIAN FENNEL",
7,1,"09096200","09096200",711,2014,2050,5923571,"Standard_HS12","FaoStatName: ANISE BADIAN FENNEL",
7,1,"09101000","09101000",720,2007,2050,5923572,"Generic HS 2007 to FCL mapping","Ginger",
7,1,"09101100","09101100",720,2014,2050,5923573,"Standard_HS12","FaoStatName: GINGER",
7,1,"09101200","09101200",720,2014,2050,5923574,"Standard_HS12","FaoStatName: GINGER",
7,1,"09102000","09102000",723,2007,2050,5923575,"Standard_HS12","FaoStatName: SPICES NES",
7,1,"09103000","09103000",723,2007,2050,5923576,"Standard_HS12","FaoStatName: SPICES NES",
7,1,"09104000","09104000",723,2007,2050,5923577,"Generic HS 2007 to FCL mapping","Thyme and bay leaves",
7,1,"09105000","09105000",723,2007,2050,5923578,"Generic HS 2007 to FCL mapping","Curry",
7,1,"09109100","09109100",723,2007,2050,5923579,"Standard_HS12","FaoStatName: SPICES NES",
7,1,"09109900","09109900",723,2007,2050,5923580,"Standard_HS12","FaoStatName: SPICES NES",
7,1,"10011000","10011000",15,2007,2050,5923581,"Generic HS 2007 to FCL mapping","Durum wheat",
7,1,"10011900","10011900",15,2014,2050,5923582,"Standard_HS12","FaoStatName: WHEAT",
7,1,"10019000","10019000",15,2007,2050,5923583,"Generic HS 2007 to FCL mapping","Wheat and meslin (excl. durum wheat)",
7,1,"10019100","10019100",15,2015,2050,5923584,"Standard_HS12","FaoStatName: WHEAT",
7,1,"10019900","10019900",15,2014,2050,5923585,"Standard_HS12","FaoStatName: WHEAT",
7,1,"10020000","10020000",71,2007,2050,5923586,"Generic HS 2007 to FCL mapping","Rye",
7,1,"10029000","10029000",71,2014,2050,5923587,"Standard_HS12","FaoStatName: RYE",
7,1,"10030000","10030000",44,2007,2050,5923588,"Generic HS 2007 to FCL mapping","Barley",
7,1,"10031000","10031000",44,2014,2050,5923589,"Standard_HS12","FaoStatName: BARLEY",
7,1,"10039000","10039000",44,2014,2050,5923590,"Standard_HS12","FaoStatName: BARLEY",
7,1,"10040000","10040000",75,2007,2050,5923591,"Generic HS 2007 to FCL mapping","Oats",
7,1,"10041000","10041000",75,2015,2050,5923592,"Standard_HS12","FaoStatName: OATS",
7,1,"10049000","10049000",75,2014,2050,5923593,"Standard_HS12","FaoStatName: OATS",
7,1,"10051000","10051000",56,2007,2050,5923594,"Standard_HS12","FaoStatName: MAIZE",
7,1,"10051010","10051010",56,2009,2050,5923595,"Standard_HS12","FaoStatName: MAIZE",
7,1,"10051020","10051020",56,2009,2050,5923596,"Standard_HS12","FaoStatName: MAIZE",
7,1,"10059000","10059000",56,2007,2050,5923597,"Standard_HS12","FaoStatName: MAIZE",
7,1,"10059010","10059010",56,2009,2050,5923598,"Standard_HS12","FaoStatName: MAIZE",
7,1,"10059020","10059020",56,2009,2050,5923599,"Standard_HS12","FaoStatName: MAIZE",
7,1,"10061000","10061000",27,2007,2050,5923600,"Standard_HS12","FaoStatName: RICE PADDY",
7,1,"10061010","10061010",27,2009,2050,5923601,"Standard_HS12","FaoStatName: RICE PADDY",
7,1,"10061020","10061020",27,2010,2050,5923602,"Standard_HS12","FaoStatName: RICE PADDY",
7,1,"10061090","10061090",27,2009,2050,5923603,"Standard_HS12","FaoStatName: RICE PADDY",
7,1,"10062010","10062010",28,2007,2050,5923604,"Standard_HS12","FaoStatName: RICE HUSKED",
7,1,"10062020","10062020",28,2007,2050,5923605,"Standard_HS12","FaoStatName: RICE HUSKED",
7,1,"10062090","10062090",28,2007,2050,5923606,"Standard_HS12","FaoStatName: RICE HUSKED",
7,1,"10063000","10063000",31,2007,2050,5923607,"Trademap TL description (could also be mapped to 29)","Rice, semi-milled or wholly milled, whether or not polished or glazed",
7,1,"10063010","10063010",31,2007,2050,5923608,"Trademap TL description (could also be mapped to 29)","Rice, semi-milled or wholly milled, whether or not polished or glazed : acondicionado para venda a retalho em embalage",
7,1,"10063020","10063020",31,2007,2050,5923609,"Trademap TL description (could also be mapped to 29)","Rice, semi-milled or wholly milled, whether or not polished or glazed : não acondicionado para venda a retalho",
7,1,"10063090","10063090",31,2007,2050,5923610,"Trademap TL description (could also be mapped to 29)","Rice, semi-milled or wholly milled, whether or not polished or glazed : outros",
7,1,"10064000","10064000",32,2007,2050,5923611,"Standard_HS12","FaoStatName: RICE BROKEN",
7,1,"10070000","10070000",83,2007,2050,5923612,"Generic HS 2007 to FCL mapping","Grain sorghum",
7,1,"10071000","10071000",83,2015,2050,5923613,"Standard_HS12","FaoStatName: SORGHUM",
7,1,"10079000","10079000",83,2014,2050,5923614,"Standard_HS12","FaoStatName: SORGHUM",
7,1,"10081000","10081000",89,2007,2050,5923615,"Standard_HS12","FaoStatName: BUCKWHEAT",
7,1,"10082000","10082000",79,2007,2050,5923616,"Generic HS 2007 to FCL mapping","Millet (excl. grain sorghum)",
7,1,"10082900","10082900",79,2014,2050,5923617,"Standard_HS12","FaoStatName: MILLET",
7,1,"10083000","10083000",101,2009,2050,5923618,"Standard_HS12","FaoStatName: CANARY SEED",
7,1,"10085000","10085000",92,2014,2050,5923619,"Standard_HS12","FaoStatName: QUINOA",
7,1,"10086000","10086000",97,2015,2050,5923620,"Standard_HS12","FaoStatName: TRITICALE",
7,1,"10089000","10089000",108,2007,2050,5923621,"Country TL description (WITS), subsequent data series (could also be mapped to 92,94,97,103)","Cereals (excl. wheat and meslin, rye, barley, oats, maize, rice, buckwheat, millet, canary seed and grain sorghum)",
7,1,"11010000","11010000",16,2007,2050,5923622,"Standard_HS12","FaoStatName: FLOUR OF WHEAT",
7,1,"11010010","11010010",16,2007,2050,5923623,"Standard_HS12","FaoStatName: FLOUR OF WHEAT",
7,1,"11010020","11010020",16,2007,2050,5923624,"Standard_HS12","FaoStatName: FLOUR OF WHEAT",
7,1,"11010090","11010090",16,2007,2050,5923625,"Standard_HS12","FaoStatName: FLOUR OF WHEAT",
7,1,"11021000","11021000",72,2007,2050,5923626,"Country TL description (WITS)","Rye flour",
7,1,"11022000","11022000",58,2007,2050,5923627,"Standard_HS12","FaoStatName: FLOUR OF MAIZE",
7,1,"11022010","11022010",58,2009,2050,5923628,"Standard_HS12","FaoStatName: FLOUR OF MAIZE",
7,1,"11022020","11022020",58,2009,2050,5923629,"Standard_HS12","FaoStatName: FLOUR OF MAIZE",
7,1,"11022090","11022090",58,2009,2050,5923630,"Standard_HS12","FaoStatName: FLOUR OF MAIZE",
7,1,"11023000","11023000",38,2007,2050,5923631,"Country TL description (WITS)","Rice flour",
7,1,"11029000","11029000",111,2007,2050,5923632,"Country TL description (WITS) (could also be mapped to 48,80,84,90,95,98,104)","Cereal flours (excl. wheat, meslin, rye, maize and rice)",
7,1,"11031100","11031100",16,2007,2050,5923633,"Standard_HS12","FaoStatName: FLOUR OF WHEAT",
7,1,"11031300","11031300",58,2007,2050,5923634,"Standard_HS12","FaoStatName: FLOUR OF MAIZE",
7,1,"11031900","11031900",111,2007,2050,5923635,"Country TL description (WITS) (could also be mapped to 38,48,72,80,84,90,95,98,104)","Groats and meal of cereals (excl. wheat and maize)",
7,1,"11032000","11032000",111,2009,2050,5923636,"Country TL description (WITS) (could also be mapped to 16,38,48,58,72,80,84,90,95,98,104)","Grumos, sêmolas e pellets, de cereais : pellets",
7,1,"11041200","11041200",76,2007,2050,5923637,"Standard_HS12","FaoStatName: OATS ROLLED",
7,1,"11041900","11041900",113,2007,2050,5923638,"Country TL description (WITS) (could also be mapped to 45,46)","Rolled or flaked grains of cereals (excl. barley and oats)",
7,1,"11042200","11042200",76,2007,2050,5923639,"Standard_HS12","FaoStatName: OATS ROLLED",
7,1,"11042300","11042300",113,2007,2050,5923640,"Standard_HS12","FaoStatName: CEREAL PREPARATIONS, NES",
7,1,"11042900","11042900",113,2007,2050,5923641,"Country TL description (WITS) (could also be mapped to 21,45,46)","Grains of cereals, hulled, pearled, sliced, kibbled or otherwise worked (excl. oats and maize, grain flour and husked and semi- or wholly milled rice and broken rice)",
7,1,"11043000","11043000",57,2007,2050,5923642,"Country TL description (WITS) (could also be mapped to 19)","Germ of cereals, whole, rolled, flaked or ground",
7,1,"11051000","11051000",117,2007,2050,5923643,"Standard_HS12","FaoStatName: POTATOES FLOUR",
7,1,"11052000","11052000",117,2007,2050,5923644,"Standard_HS12","FaoStatName: POTATOES FLOUR",
7,1,"11061000","11061000",212,2007,2050,5923645,"Standard_HS12","FaoStatName: FLOUR OF PULSES",
7,1,"11062000","11062000",126,2007,2050,5923646,"Country TL description (WITS), Old SWS series (could also be mapped to 150)","Flour, meal and powder of sago or of roots or tubers of manioc, arrowroot, salep, sweet potatoes and similar roots and tubers with a high content of starch or inulin of heading 0714",
7,1,"11062010","11062010",126,2014,2050,5923647,"Country TL description (WITS)","Farinhas, sêmolas e pós, dos legumes de vagem, secos, da posição 0713, de sagu ou das raízes ou tubérculos da posição 0714 e dos produtos do capítulo 8 : de sagu ou das raízes ou tubérculos, da posição 0714: farinha de mandioca (fuba de bombo)",
7,1,"11062090","11062090",150,2014,2050,5923648,"Country TL description (WITS)","Farinhas, sêmolas e pós, dos legumes de vagem, secos, da posição 0713, de sagu ou das raízes ou tubérculos da posição 0714 e dos produtos do capítulo 8 : de sagu ou das raízes ou tubérculos, da posição 0714: outras",
7,1,"11063000","11063000",624,2007,2050,5923649,"Standard_HS12","FaoStatName: FLOUR OF FRUITS",
7,1,"11071000","11071000",49,2007,2050,5923650,"Standard_HS12","FaoStatName: MALT",
7,1,"11072000","11072000",49,2009,2050,5923651,"Standard_HS12","FaoStatName: MALT",
7,1,"11081100","11081100",23,2007,2050,5923652,"Standard_HS12","FaoStatName: STARCH OF WHEAT",
7,1,"11081200","11081200",64,2007,2050,5923653,"Standard_HS12","FaoStatName: STARCH OF MAIZE",
7,1,"11081300","11081300",119,2007,2050,5923654,"Standard_HS12","FaoStatName: STARCH OF POTATOES",
7,1,"11081400","11081400",129,2007,2050,5923655,"Standard_HS12","FaoStatName: CASSAVA STARCH",
7,1,"11081900","11081900",34,2007,2050,5923656,"Standard_HS12","FaoStatName: STARCH OF RICE",
7,1,"11090000","11090000",24,2007,2050,5923657,"Standard_HS12","FaoStatName: GLUTEN OF WHEAT",
7,1,"12010000","12010000",236,2007,2050,5923658,"Generic HS 2007 to FCL mapping","Soya beans, whether or not broken",
7,1,"12011000","12011000",236,2014,2050,5923659,"Standard_HS12","FaoStatName: SOYBEANS",
7,1,"12019000","12019000",236,2014,2050,5923660,"Standard_HS12","FaoStatName: SOYBEANS",
7,1,"12021000","12021000",242,2007,2050,5923661,"Generic HS 2007 to FCL mapping","Ground-nuts in shell, not roasted or otherwise cooked",
7,1,"12022000","12022000",243,2007,2050,5923662,"Generic HS 2007 to FCL mapping","Shelled ground-nuts, whether or not broken (excl. roasted or otherwise cooked)",
7,1,"12023000","12023000",242,2014,2050,5923663,"Standard_HS12","FaoStatName: GROUNDNUTS IN SHELL",
7,1,"12024100","12024100",242,2014,2050,5923664,"Standard_HS12","FaoStatName: GROUNDNUTS IN SHELL",
7,1,"12024200","12024200",243,2014,2050,5923665,"Standard_HS12","FaoStatName: GROUNDNUTS SHELLED",
7,1,"12030000","12030000",251,2013,2050,5923666,"Standard_HS12","FaoStatName: COPRA",
7,1,"12040000","12040000",333,2007,2050,5923667,"Standard_HS12","FaoStatName: LINSEED",
7,1,"12051000","12051000",270,2010,2050,5923668,"Standard_HS12","FaoStatName: RAPESEED",
7,1,"12059000","12059000",270,2009,2050,5923669,"Standard_HS12","FaoStatName: RAPESEED",
7,1,"12060000","12060000",267,2007,2050,5923670,"Standard_HS12","FaoStatName: SUNFLOWER SEED",
7,1,"12071000","12071000",256,2014,2050,5923671,"Standard_HS12","FaoStatName: PALMNUT KERNELS",
7,1,"12072000","12072000",329,2009,2050,5923672,"Trademap TL description","Sementes de algodão, mesmo trituradas",
7,1,"12072100","12072100",329,2015,2050,5923673,"Standard_HS12","FaoStatName: COTTONSEED",
7,1,"12072900","12072900",329,2014,2050,5923674,"Standard_HS12","FaoStatName: COTTONSEED",
7,1,"12074000","12074000",289,2007,2050,5923675,"Standard_HS12","FaoStatName: SESAME SEED",
7,1,"12075000","12075000",292,2007,2050,5923676,"Standard_HS12","FaoStatName: MUSTARD SEED",
7,1,"12076000","12076000",280,2007,2050,5923677,"Standard_HS12","FaoStatName: SAFFLOWER SEED",
7,1,"12077000","12077000",299,2014,2050,5923678,"Standard_HS12","FaoStatName: Melonseeds",
7,1,"12079100","12079100",296,2007,2050,5923679,"Standard_HS12","FaoStatName: POPPY SEED",
7,1,"12079900","12079900",339,2007,2050,5923680,"Country TL description (WITS), Old SWS series (could also be mapped to 263,275,277,305,311,312,336","Oil seeds and oleaginous fruits, whether or not broken (excl. edible nuts, olives, soya beans, ground-nuts, copra, linseed, rape or colza seeds, sunflower seeds, palm nuts and kernels, cotton, castor oil, sesamum, mustard, safflower and poppy seeds)",
7,1,"12081000","12081000",343,2007,2050,5923681,"Standard_HS12","FaoStatName: FLOUR OF OILSEEDS",
7,1,"12089000","12089000",343,2007,2050,5923682,"Standard_HS12","FaoStatName: FLOUR OF OILSEEDS",
7,1,"12091000","12091000",1294,2007,2050,5923683,"Standard_HS12","FaoStatName: SEEDS FOR PLANTING",
7,1,"12092100","12092100",1294,2009,2050,5923684,"Standard_HS12","FaoStatName: SEEDS FOR PLANTING",
7,1,"12092200","12092200",1294,2010,2050,5923685,"Standard_HS12","FaoStatName: SEEDS FOR PLANTING",
7,1,"12092300","12092300",1294,2009,2050,5923686,"Standard_HS12","FaoStatName: SEEDS FOR PLANTING",
7,1,"12092400","12092400",1294,2007,2050,5923687,"Standard_HS12","FaoStatName: SEEDS FOR PLANTING",
7,1,"12092500","12092500",1294,2007,2050,5923688,"Standard_HS12","FaoStatName: SEEDS FOR PLANTING",
7,1,"12092900","12092900",1294,2007,2050,5923689,"Standard_HS12","FaoStatName: SEEDS FOR PLANTING",
7,1,"12093000","12093000",1294,2007,2050,5923690,"Standard_HS12","FaoStatName: SEEDS FOR PLANTING",
7,1,"12099100","12099100",1294,2007,2050,5923691,"Standard_HS12","FaoStatName: SEEDS FOR PLANTING",
7,1,"12099900","12099900",1294,2007,2050,5923692,"Standard_HS12","FaoStatName: SEEDS FOR PLANTING",
7,1,"12101000","12101000",677,2013,2050,5923693,"Standard_HS12","FaoStatName: HOPS",
7,1,"12102000","12102000",677,2007,2050,5923694,"Standard_HS12","FaoStatName: HOPS",
7,1,"12112000","12112000",1293,2007,2050,5923695,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"12114000","12114000",1293,2013,2050,5923696,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"12119000","12119000",1293,2007,2050,5923697,"Country TL description (WITS) (could also be mapped to 754)","Plants, parts of plants, incl. seeds and fruits, used primarily in perfumery, in pharmacy or for insecticidal, fungicidal or similar purposes, fresh or dried, whether or not cut, crushed or powdered (excl. liquorice and ginseng roots, coca leaf and poppy straw)",
7,1,"12122000","12122000",1293,2007,2050,5923698,"Generic HS 2007 to FCL mapping","Seaweeds and other algae, fresh, chilled, frozen or dried, whether or not ground",
7,1,"12122100","12122100",1293,2014,2050,5923699,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"12122900","12122900",1293,2014,2050,5923700,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"12129100","12129100",157,2010,2050,5923701,"Standard_HS12","FaoStatName: SUGAR BEET",
7,1,"12129200","12129200",461,2014,2050,5923702,"Standard_HS12","FaoStatName: Carobs",
7,1,"12129900","12129900",460,2007,2050,5923703,"Country TL description (WITS) (could also be mapped to 157,156,161,299,459)","Fruit stones and kernels and other vegetable products, incl. unroasted chicory roots of the variety cichorium intybus sativum, of a kind used primarily for human consumption, n.e.s.",
7,1,"12130000","12130000",635,2007,2050,5923704,"Standard_HS12","FaoStatName: STRAW HUSKS",
7,1,"12141000","12141000",862,2010,2050,5923705,"Standard_HS12","FaoStatName: alfalfa meal and pellets",
7,1,"12149000","12149000",651,2007,2050,5923706,"Country TL description (WITS) (could also be mapped to various other feeds and forages)","Swedes, mangolds, fodder roots, hay, lucerne alfalfa, clover, sainfoin, forage kale, lupines, vetches and similar forage products, whether or not in the form of pellets (excl. lucerne alfalfa meal and pellets)",
7,1,"13012000","13012000",1291,2007,2050,5923707,"Standard_HS12","FaoStatName: ARABIC GUMS",
7,1,"13019000","13019000",1292,2007,2050,5923708,"Standard_HS12","FaoStatName: OTHER RESINS",
7,1,"13021100","13021100",1293,2014,2050,5923709,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"13021200","13021200",1293,2011,2050,5923710,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"13021300","13021300",1293,2007,2050,5923711,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"13021900","13021900",1293,2007,2050,5923712,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"13022000","13022000",1293,2007,2050,5923713,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"13023100","13023100",1293,2007,2050,5923714,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"13023200","13023200",1293,2007,2050,5923715,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"13023900","13023900",1293,2007,2050,5923716,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"14011000","14011000",1293,2007,2050,5923717,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"14012000","14012000",1293,2007,2050,5923718,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"14019000","14019000",1293,2007,2050,5923719,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"14030000","14030000",1293,2007,2050,5923720,"Generic HS 2007 to FCL mapping","Vegetable materials, such as broom-corn, piassava, couch-grass and istle, of a kind used primarily in brooms or in brushes, whether or not in hanks or bundles",
7,1,"14042000","14042000",770,2010,2050,5923721,"Standard_HS12","FaoStatName: COTTON LINTER",
7,1,"14049000","14049000",1293,2007,2050,5923722,"Standard_HS12","FaoStatName: CRUDE MATERIALS",
7,1,"15010000","15010000",1043,2007,2050,5923723,"Country TL description (WITS), Old SWS series (could also be mapped to 1066)","Pig fat, incl. lard, and poultry fat, rendered or otherwise extracted (excl. lard stearin and lard oil)",
7,1,"15011000","15011000",1043,2014,2050,5923724,"Standard_HS12","FaoStatName: LARD",
7,1,"15012000","15012000",1043,2014,2050,5923725,"Standard_HS12","FaoStatName: LARD",
7,1,"15019000","15019000",1066,2014,2050,5923726,"Standard_HS12","FaoStatName: FAT OF PTRY REND",
7,1,"15020000","15020000",1019,2007,2050,5923727,"Country TL description (WITS), largest utilization in SWS series (could also be mapped to 869,871,949,979,1019)","Fats of bovine animals, sheep or goats (excl. lard stearin, lard oil, oleostearin, oleooil and tallow oil, not emulsified or mixed or otherwise prepared)",
7,1,"15021000","15021000",1225,2015,2050,5923728,"Standard_HS12","FaoStatName: TALLOW",
7,1,"15030000","15030000",1221,2010,2050,5923729,"Standard_HS12","FaoStatName: LARD STEARINE OIL",
7,1,"15041000","15041000",1223,2007,2050,5923730,"Standard_HS12","FaoStatName: OIL FISH,MAR MAMM",
7,1,"15042000","15042000",1223,2007,2050,5923731,"Standard_HS12","FaoStatName: OIL FISH,MAR MAMM",
7,1,"15043000","15043000",1223,2007,2050,5923732,"Standard_HS12","FaoStatName: OIL FISH,MAR MAMM",
7,1,"15050000","15050000",994,2010,2050,5923733,"Standard_HS12","FaoStatName: GREASE INCL.",
7,1,"15060000","15060000",1168,2007,2050,5923734,"Country TL description (WITS) (could also be mapped to 1129,1160)","Other animal fats and oils and their fractions, whether or not refined, but not chemically modified (excl. pig fat, poultry fat, fats of bovine animals, sheep and goats, fats of fish and other marine animals, lard stearin, lard oil, oloestearin, oleo-oil, tallow oil, wool grease and fatty substances derived therefrom)",
7,1,"15071000","15071000",237,2014,2050,5923735,"Standard_HS12","FaoStatName: OIL OF SOYBEANS",
7,1,"15071010","15071010",237,2007,2050,5923736,"Standard_HS12","FaoStatName: OIL OF SOYBEANS",
7,1,"15071020","15071020",237,2007,2050,5923737,"Standard_HS12","FaoStatName: OIL OF SOYBEANS",
7,1,"15071090","15071090",237,2007,2050,5923738,"Standard_HS12","FaoStatName: OIL OF SOYBEANS",
7,1,"15079000","15079000",237,2007,2050,5923739,"Standard_HS12","FaoStatName: OIL OF SOYBEANS",
7,1,"15081000","15081000",244,2014,2050,5923740,"Standard_HS12","FaoStatName: OIL OF GROUNDNUTS",
7,1,"15081010","15081010",244,2009,2050,5923741,"Standard_HS12","FaoStatName: OIL OF GROUNDNUTS",
7,1,"15081090","15081090",244,2007,2050,5923742,"Standard_HS12","FaoStatName: OIL OF GROUNDNUTS",
7,1,"15089000","15089000",244,2007,2050,5923743,"Standard_HS12","FaoStatName: OIL OF GROUNDNUTS",
7,1,"15091000","15091000",261,2007,2050,5923744,"Standard_HS12","FaoStatName: OIL OF OLIVES, VIRGIN",
7,1,"15099000","15099000",261,2007,2050,5923745,"Standard_HS12","FaoStatName: OIL OF OLIVES, VIRGIN",
7,1,"15100000","15100000",274,2007,2050,5923746,"Standard_HS12","FaoStatName: OIL OF OLIVE RESIDUES",
7,1,"15111000","15111000",257,2014,2050,5923747,"Standard_HS12","FaoStatName: OIL OF PALM",
7,1,"15111010","15111010",257,2009,2050,5923748,"Standard_HS12","FaoStatName: OIL OF PALM",
7,1,"15111020","15111020",257,2007,2050,5923749,"Standard_HS12","FaoStatName: OIL OF PALM",
7,1,"15111090","15111090",257,2007,2050,5923750,"Standard_HS12","FaoStatName: OIL OF PALM",
7,1,"15119000","15119000",257,2007,2050,5923751,"Standard_HS12","FaoStatName: OIL OF PALM",
7,1,"15121100","15121100",268,2014,2050,5923752,"Country TL description (WITS), Old SWS series (could also be mapped to 281)","Óleos de girassol, de cártamo ou de algodão, e respetivas frações, mesmo refinados, mas não quimicamente modificados : óleos de girassol ou de cártamo, e respetivas frações : óleos em bruto",
7,1,"15121110","15121110",268,2007,2050,5923753,"Country TL description (WITS), Old SWS series (could also be mapped to 281)","Crude sunflower-seed or safflower oil: No description at level 8",
7,1,"15121120","15121120",268,2007,2050,5923754,"Trademap TL description, Old SWS series (could also be mapped to 281)","Sunflower-seed or safflower oil, crude : oleo refinado não acondicionado para venda a",
7,1,"15121190","15121190",268,2007,2050,5923755,"Trademap TL description, Old SWS series (could also be mapped to 281)","Sunflower-seed or safflower oil, crude : outros",
7,1,"15121900","15121900",268,2007,2050,5923756,"Trademap TL description, Old SWS series (could also be mapped to 281)","Sunflower-seed or safflower oil and their fractions, whether or not refined, but not chemically modified (excl. crude)",
7,1,"15122110","15122110",331,2012,2050,5923757,"Standard_HS12","FaoStatName: OIL OF COTTONSEED",
7,1,"15122190","15122190",331,2009,2050,5923758,"Standard_HS12","FaoStatName: OIL OF COTTONSEED",
7,1,"15122900","15122900",331,2007,2050,5923759,"Standard_HS12","FaoStatName: OIL OF COTTONSEED",
7,1,"15131100","15131100",252,2014,2050,5923760,"Standard_HS12","FaoStatName: OIL OF COCONUTS",
7,1,"15131900","15131900",252,2007,2050,5923761,"Standard_HS12","FaoStatName: OIL OF COCONUTS",
7,1,"15132100","15132100",258,2009,2050,5923762,"Standard_HS12","FaoStatName: OIL PALM KERNEL",
7,1,"15132900","15132900",258,2007,2050,5923763,"Standard_HS12","FaoStatName: OIL PALM KERNEL",
7,1,"15141900","15141900",271,2007,2050,5923764,"Country TL description (WITS), Old SWS series (could also be mapped to 293)","Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2% and its fractions, whether or not refined, but not chemically modified (excl. crude)",
7,1,"15149100","15149100",271,2013,2050,5923765,"Country TL description (WITS), Old SWS series (could also be mapped to 293)","Óleos de nabo silvestre, de colza ou de mostarda, e respetivas frações, mesmo refinados, mas não quimicamente modificados : outros : óleos em bruto",
7,1,"15149900","15149900",271,2007,2050,5923766,"Country TL description (WITS), Old SWS series (could also be mapped to 293)","High erucic acid rape or colza oil fixed oil which has an erucic acid content of >= 2%, and mustard oil, and fractions thereof, whether or not refined, but not chemically modified (excl. crude)",
7,1,"15151100","15151100",334,2009,2050,5923767,"Standard_HS12","FaoStatName: OIL OF LINSEED",
7,1,"15151900","15151900",334,2007,2050,5923768,"Standard_HS12","FaoStatName: OIL OF LINSEED",
7,1,"15152120","15152120",60,2007,2050,5923769,"Standard_HS12","FaoStatName: OIL OF MAIZE",
7,1,"15152190","15152190",60,2007,2050,5923770,"Standard_HS12","FaoStatName: OIL OF MAIZE",
7,1,"15152900","15152900",60,2007,2050,5923771,"Standard_HS12","FaoStatName: OIL OF MAIZE",
7,1,"15153000","15153000",266,2007,2050,5923772,"Standard_HS12","FaoStatName: OIL OF CASTOR BEANS",
7,1,"15155000","15155000",290,2007,2050,5923773,"Standard_HS12","FaoStatName: OIL OF SESAME SEED",
7,1,"15159000","15159000",340,2007,2050,5923774,"Country TL description (WITS), Old SWS series (could also be mapped to 36,264,278,297,306,307,313,337)","Fixed vegetable fats and oils and their fractions, whether or not refined, but not chemically modified (excl. soya-bean, ground-nut, olive, palm, sunflower-seed, safflower, cotton-seed, coconut, palm kernel, babassu, rape, colza and mustard, linseed, maize, castor, tung and sesame oil)",
7,1,"15161000","15161000",1275,2007,2050,5923775,"Standard_HS12","FaoStatName: OIL HYDROGENATED",
7,1,"15162000","15162000",1275,2007,2050,5923776,"Country TL description (WITS), Old SWS series (could also be mapped to 1273)","Vegetable fats and oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinized, whether or not refined, but not further prepared",
7,1,"15171000","15171000",1242,2007,2050,5923777,"Standard_HS12","FaoStatName: MARGARINE SHORT",
7,1,"15179000","15179000",1243,2007,2050,5923778,"Country TL description (WITS), Old SWS series (could also be mapped to 1241)","Edible mixtures or preparations of animal or vegetable fats or oils and edible fractions of different fats or oils (excl. fats, oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinized, whether or not refined, but not further prepared, mixtures of olive oils and their fractions, and solid margarine)",
7,1,"15180000","15180000",1274,2007,2050,5923779,"Standard_HS12","FaoStatName: OIL BOILED ETC",
7,1,"15211000","15211000",1296,2007,2050,5923780,"Standard_HS12","FaoStatName: WAXES VEGETABLE",
7,1,"15219000","15219000",1183,2007,2050,5923781,"Country TL description (WITS), Old SWS series (could also be mapped to 1295)","Beeswax, other insect waxes and spermaceti, whether or not refined or coloured",
7,1,"15220000","15220000",1277,2007,2050,5923782,"Country TL description (WITS), Old SWS series (could also be mapped to 1222)","Degras; residues resulting from the treatment of fatty substances or animal or vegetable waxes",
7,1,"16010000","16010000",1041,2007,2050,5923783,"Country TL description (WITS), Old SWS series (could also be mapped to 874)","Sausages and similar products, of meat, offal or blood; food preparations based on these products",
7,1,"16021000","16021000",877,2007,2050,5923784,"Standard_HS12","FaoStatName: homogen.meat prp.",
7,1,"16022000","16022000",1060,2007,2050,5923785,"Country TL description (WITS) (could also be mapped to 878)","Preparations of liver of any animal (excl. sausages and similar products and finely homogenized preparations put up for retail sale as infant food or for dietetic purposes, in containers of a net weight of <= 250 g)",
7,1,"16023100","16023100",1061,2007,2050,5923786,"Standard_HS12","FaoStatName: MEAT OF CHIK CAN",
7,1,"16023200","16023200",1061,2007,2050,5923787,"Standard_HS12","FaoStatName: MEAT OF CHIK CAN",
7,1,"16023900","16023900",1061,2007,2050,5923788,"Standard_HS12","FaoStatName: MEAT OF CHIK CAN",
7,1,"16024100","16024100",1042,2007,2050,5923789,"Standard_HS12","FaoStatName: PREP OF PIG MEAT",
7,1,"16024200","16024200",1042,2007,2050,5923790,"Standard_HS12","FaoStatName: PREP OF PIG MEAT",
7,1,"16024900","16024900",1042,2007,2050,5923791,"Standard_HS12","FaoStatName: PREP OF PIG MEAT",
7,1,"16025000","16025000",875,2007,2050,5923792,"Standard_HS12","FaoStatName: PREPARATIONS OF BEEF MEAT",
7,1,"16029000","16029000",1172,2007,2050,5923793,"Standard_HS12","FaoStatName: PREPARED MEAT NES",
7,1,"16030000","16030000",873,2007,2050,5923794,"Standard_HS12","FaoStatName: MEAT EXTRACTS",
7,1,"16055800","16055800",1176,2014,2050,5923795,"Country TL description (WITS)","Crustáceos, moluscos e outros invertebrados aquáticos, preparados ou em conservas : moluscos : caracóis, exceto os do mar",
7,1,"17011100","17011100",162,2007,2050,5923796,"Country TL description (WITS), Old SWS series (could also be mapped to 163)","Raw cane sugar (excl. added flavouring or colouring)",
7,1,"17011200","17011200",162,2007,2050,5923797,"Country TL description (WITS), Old SWS series (could also be mapped to 163)","Raw beet sugar (excl. added flavouring or colouring)",
7,1,"17011300","17011300",162,2014,2050,5923798,"Country TL description (WITS), Old SWS series (could also be mapped to 163)","Açúcares de cana ou de beterraba e sacarose quimicamente pura, no estado sólido : açúcares brutos, sem adição de aromatizantes ou de corantes : açúcar de cana mencionado na nota 2 de subposição do presente capítulo",
7,1,"17011400","17011400",162,2014,2050,5923799,"Country TL description (WITS), Old SWS series (could also be mapped to 163)","Açúcares de cana ou de beterraba e sacarose quimicamente pura, no estado sólido : açúcares brutos, sem adição de aromatizantes ou de corantes : outros açucares de cana",
7,1,"17019100","17019100",164,2007,2050,5923800,"Standard_HS12","FaoStatName: SUGAR REFINED",
7,1,"17019900","17019900",164,2007,2050,5923801,"Standard_HS12","FaoStatName: SUGAR REFINED",
7,1,"17019910","17019910",164,2007,2050,5923802,"Standard_HS12","FaoStatName: SUGAR REFINED",
7,1,"17019920","17019920",164,2007,2050,5923803,"Standard_HS12","FaoStatName: SUGAR REFINED",
7,1,"17019990","17019990",164,2007,2050,5923804,"Standard_HS12","FaoStatName: SUGAR REFINED",
7,1,"17021100","17021100",173,2007,2050,5923805,"Standard_HS12","FaoStatName: LACTOSE",
7,1,"17021900","17021900",173,2007,2050,5923806,"Standard_HS12","FaoStatName: LACTOSE",
7,1,"17022000","17022000",160,2007,2050,5923807,"Standard_HS12","FaoStatName: MAPLE SUGAR AND SYRUPS",
7,1,"17023000","17023000",172,2007,2050,5923808,"Standard_HS12","FaoStatName: GLUCOSE AND DEXTROSE",
7,1,"17024000","17024000",172,2007,2050,5923809,"Standard_HS12","FaoStatName: GLUCOSE AND DEXTROSE",
7,1,"17025000","17025000",154,2007,2050,5923810,"Standard_HS12","FaoStatName: FRUCTOSE CHEMICALLY PURE",
7,1,"17026000","17026000",166,2007,2050,5923811,"Standard_HS12","FaoStatName: OTHER FRUCTOSE AND SYRUP",
7,1,"17029000","17029000",167,2007,2050,5923812,"Country TL description (WITS), Old SWS series (could also be mapped to 166,155)","Sugars in solid form, incl. invert sugar and chemically pure maltose, and sugar and sugar syrup blends containing in the dry state 50% by weight of fructose, not flavoured or coloured, artifical honey, whether or not mixed with natural honey and caramel (excl. cane or beet sugar, chemically pure sucrose, lactose, maple sugar, glucose, fructose, and syrups thereof)",
7,1,"17031000","17031000",165,2007,2050,5923813,"Standard_HS12","FaoStatName: MOLASSES",
7,1,"17039000","17039000",165,2007,2050,5923814,"Standard_HS12","FaoStatName: MOLASSES",
7,1,"17041000","17041000",168,2007,2050,5923815,"Standard_HS12","FaoStatName: SUGAR CONFECTIONERY",
7,1,"17049000","17049000",168,2007,2050,5923816,"Standard_HS12","FaoStatName: SUGAR CONFECTIONERY",
7,1,"18010000","18010000",661,2007,2050,5923817,"Standard_HS12","FaoStatName: COCOA BEANS",
7,1,"18020000","18020000",663,2007,2050,5923818,"Standard_HS12","FaoStatName: COCOAHUSKS;SHELL",
7,1,"18031000","18031000",662,2010,2050,5923819,"Standard_HS12","FaoStatName: COCOA PASTE",
7,1,"18032000","18032000",665,2007,2050,5923820,"Standard_HS12","FaoStatName: COCOAPOWDER&CAKE",
7,1,"18040000","18040000",664,2007,2050,5923821,"Standard_HS12","FaoStatName: COCOA BUTTER",
7,1,"18050000","18050000",665,2007,2050,5923822,"Standard_HS12","FaoStatName: COCOAPOWDER&CAKE",
7,1,"18061000","18061000",666,2007,2050,5923823,"Standard_HS12","FaoStatName: CHOCOLATE PRSNES",
7,1,"18061001","18061001",666,2009,2050,5923824,"Standard_HS12","FaoStatName: CHOCOLATE PRSNES",
7,1,"18062000","18062000",666,2007,2050,5923825,"Standard_HS12","FaoStatName: CHOCOLATE PRSNES",
7,1,"18063100","18063100",666,2007,2050,5923826,"Standard_HS12","FaoStatName: CHOCOLATE PRSNES",
7,1,"18063200","18063200",666,2007,2050,5923827,"Standard_HS12","FaoStatName: CHOCOLATE PRSNES",
7,1,"18069000","18069000",666,2007,2050,5923828,"Standard_HS12","FaoStatName: CHOCOLATE PRSNES",
7,1,"19011000","19011000",109,2007,2050,5923829,"Standard_HS12","FaoStatName: INFANT FOOD",
7,1,"19012000","19012000",114,2007,2050,5923830,"Standard_HS12","FaoStatName: MIXES AND DOUGHS",
7,1,"19019000","19019000",50,2007,2050,5923831,"Country TL description (WITS), Old SWS series (could also be mapped to 115)","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yoghourt, kefir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excl. for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905)",
7,1,"19021100","19021100",18,2007,2050,5923832,"Standard_HS12","FaoStatName: MACARONI",
7,1,"19021900","19021900",18,2007,2050,5923833,"Standard_HS12","FaoStatName: MACARONI",
7,1,"19022000","19022000",1232,2007,2050,5923834,"Standard_HS12","FaoStatName: FOOD PREP NES",
7,1,"19023000","19023000",1232,2007,2050,5923835,"Standard_HS12","FaoStatName: FOOD PREP NES",
7,1,"19024000","19024000",1232,2007,2050,5923836,"Standard_HS12","FaoStatName: FOOD PREP NES",
7,1,"19030000","19030000",127,2007,2050,5923837,"Country TL description (WITS) (could also be mapped to 121)","Tapioca and substitutes therefor prepared from starch, in the form of flakes, grains, pearls, siftings or similar forms",
7,1,"19041000","19041000",41,2007,2050,5923838,"Standard_HS12","FaoStatName: BREAKFAST CEREALS",
7,1,"19042000","19042000",41,2007,2050,5923839,"Standard_HS12","FaoStatName: BREAKFAST CEREALS",
7,1,"19043000","19043000",41,2007,2050,5923840,"Standard_HS12","FaoStatName: BREAKFAST CEREALS",
7,1,"19049000","19049000",41,2007,2050,5923841,"Standard_HS12","FaoStatName: BREAKFAST CEREALS",
7,1,"19051000","19051000",20,2007,2050,5923842,"Standard_HS12","FaoStatName: BREAD",
7,1,"19052000","19052000",22,2007,2050,5923843,"Standard_HS12","FaoStatName: PASTRY",
7,1,"19053000","19053000",110,2010,2050,5923844,"Trademap TL description","Sweet biscuits, waffles and wafers",
7,1,"19053100","19053100",22,2007,2050,5923845,"Standard_HS12","FaoStatName: PASTRY",
7,1,"19053200","19053200",110,2007,2050,5923846,"Standard_HS12","FaoStatName: WAFERS",
7,1,"19054000","19054000",20,2007,2050,5923847,"Standard_HS12","FaoStatName: BREAD",
7,1,"19059000","19059000",22,2007,2050,5923848,"Country TL description (WITS) (could also be mapped to 110)","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products (excl. crispbread, gingerbread and the like, sweet biscuits, waffles and wafers with water content of <= 10%, rusks, toasted bread and similar toasted products)",
7,1,"19059010","19059010",20,2009,2050,5923849,"Generic HS 2007 to FCL mapping","Produtos de padaria, pastelaria ou da indústria de bolachas e biscoitos, mesmo adicionados de cacau; hóstias, cápsulas vazias para medicamentos, obreias, pastas secas de farinha, amido ou fécula, em folhas, e produtos semelhantes : outros: cápsulas vazias para medicamentos",
7,1,"19059020","19059020",22,2009,2050,5923850,"Generic HS 2007 to FCL mapping","Produtos de padaria, pastelaria ou da indústria de bolachas e biscoitos, mesmo adicionados de cacau; hóstias, cápsulas vazias para medicamentos, obreias, pastas secas de farinha, amido ou fécula, em folhas, e produtos semelhantes : outros: obreias",
7,1,"19059030","19059030",18,2009,2050,5923851,"Trademap TL description","Produtos de padaria, pastelaria ou da indústria de bolachas e biscoitos, mesmo adicionados de cacau; hóstias, cápsulas vazias para medicamentos, obreias, pastas secas de farinha, amido ou fécula, em folhas, e produtos semelhantes : outros: pasta seca de farinha, amido ou de fécula, em folhas, e produtos semelhantes",
7,1,"19059090","19059090",22,2009,2050,5923852,"Country TL description (WITS) (could also be mapped to 110)","Produtos de padaria, pastelaria ou da indústria de bolachas e biscoitos, mesmo adicionados de cacau; hóstias, cápsulas vazias para medicamentos, obreias, pastas secas de farinha, amido ou fécula, em folhas, e produtos semelhantes : outros: outros",
7,1,"20011000","20011000",471,2007,2050,5923853,"Standard_HS12","FaoStatName: VEGETABLES IN VINEGAR",
7,1,"20019000","20019000",471,2007,2050,5923854,"Country TL description (WITS), Old SWS series (could also be mapped to 262)","Vegetables, fruit, nuts and other edible parts of plants, prepared or preserved by vinegar or acetic acid (excl. cucumbers and gherkins)",
7,1,"20021000","20021000",392,2007,2050,5923855,"Standard_HS12","FaoStatName: TOMATO PEELED",
7,1,"20029000","20029000",391,2007,2050,5923856,"Standard_HS12","FaoStatName: PASTE OF TOMATOES",
7,1,"20031000","20031000",451,2007,2050,5923857,"Standard_HS12","FaoStatName: CANNED MUSHROOMS",
7,1,"20032000","20032000",451,2007,2050,5923858,"Generic HS 2007 to FCL mapping","Truffles, prepared or preserved otherwise than by vinegar or acetic acid",
7,1,"20039000","20039000",451,2007,2050,5923859,"Standard_HS12","FaoStatName: CANNED MUSHROOMS",
7,1,"20041000","20041000",118,2007,2050,5923860,"Standard_HS12","FaoStatName: FROZEN POTATOES",
7,1,"20049000","20049000",475,2007,2050,5923861,"Country TL description (WITS) (could also be mapped to 262,447)","Vegetables and mixtures of vegetables, prepared or preserved otherwise than by vinegar or acetic acid, frozen (excl. preserved by sugar, and tomatoes, mushrooms, truffles and potatoes, unmixed)",
7,1,"20051000","20051000",476,2007,2050,5923862,"Standard_HS12","FaoStatName: homogen.veget. prep",
7,1,"20052000","20052000",472,2007,2050,5923863,"Standard_HS12","FaoStatName: VEGETABLES PRESERVED NES",
7,1,"20054000","20054000",472,2007,2050,5923864,"Standard_HS12","FaoStatName: VEGETABLES PRESERVED NES",
7,1,"20055100","20055100",472,2007,2050,5923865,"Standard_HS12","FaoStatName: VEGETABLES PRESERVED NES",
7,1,"20055900","20055900",472,2007,2050,5923866,"Standard_HS12","FaoStatName: VEGETABLES PRESERVED NES",
7,1,"20056000","20056000",472,2007,2050,5923867,"Standard_HS12","FaoStatName: VEGETABLES PRESERVED NES",
7,1,"20057000","20057000",262,2007,2050,5923868,"Standard_HS12","FaoStatName: OLIVES PRESERVED",
7,1,"20058000","20058000",448,2007,2050,5923869,"Standard_HS12","FaoStatName: SWEET CORN PREPARED OR PRESERVED",
7,1,"20059000","20059000",472,2007,2050,5923870,"Generic HS 2007 to FCL mapping","Vegetables and mixtures of vegetables, prepared or preserved otherwise than by vinegar, non-frozen (excl. preserved by sugar, homogenized vegetables of subheading 2005.10, and tomatoes, mushrooms, truffles, potatoes, peas Pisum sativum, beans Vigna, Phaseolus, asparagus, olives and sweet corn Zea Mays var. Saccharata, unmixed)",
7,1,"20059100","20059100",472,2009,2050,5923871,"Standard_HS12","FaoStatName: VEGETABLES PRESERVED NES",
7,1,"20059900","20059900",472,2009,2050,5923872,"Standard_HS12","FaoStatName: VEGETABLES PRESERVED NES",
7,1,"20060000","20060000",625,2007,2050,5923873,"Standard_HS12","FaoStatName: fruit,nut,peel, sugar prs",
7,1,"20071000","20071000",626,2007,2050,5923874,"Standard_HS12","FaoStatName: homogen.cooked fruit prp",
7,1,"20079100","20079100",623,2007,2050,5923875,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"20079900","20079900",623,2007,2050,5923876,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"20081100","20081100",246,2007,2050,5923877,"Country TL description (WITS) (could also be mapped to 247)","Ground-nuts, prepared or preserved (excl. preserved with sugar)",
7,1,"20081900","20081900",235,2007,2050,5923878,"Standard_HS12","FaoStatName: PREPARED NUTS (EXCL.GROUNDNUTS)",
7,1,"20082000","20082000",575,2007,2050,5923879,"Standard_HS12","FaoStatName: PINEAPPLES CAND",
7,1,"20083000","20083000",623,2007,2050,5923880,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"20084000","20084000",623,2007,2050,5923881,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"20085000","20085000",623,2007,2050,5923882,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"20086000","20086000",623,2007,2050,5923883,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"20087000","20087000",623,2007,2050,5923884,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"20088000","20088000",623,2007,2050,5923885,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"20089100","20089100",623,2007,2050,5923886,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"20089200","20089200",623,2007,2050,5923887,"Generic HS 2007 to FCL mapping","Mixtures of fruits, nuts and other edible parts of plants, prepared or preserved, whether or not containing added sugar or other sweetening matter or spirit (excl. mixtures of nuts, groundnuts and other seeds and preparations of the Müsli type based on unroasted cereal flakes of subheading 1904.20.10, and prepared or preserved with vinegar, preserved with sugar but not laid in syrup, jams, fruit jellies, marmalades, fruit purée and pastes, obtained by cooking)",
7,1,"20089300","20089300",623,2014,2050,5923888,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"20089700","20089700",623,2014,2050,5923889,"Standard_HS12","FaoStatName: FRUIT Prp NES",
7,1,"20089900","20089900",623,2007,2050,5923890,"Country TL description (WITS), Old SWS series (could also be mapped to 584)","Fruit and other edible parts of plants, prepared or preserved, whether or not containing added sugar or other sweetening matter or spirit (excl. prepared or preserved with vinegar, preserved with sugar but not laid in syrup, jams, fruit jellies, marmalades, fruit purée and pastes, obtained by cooking, and nuts, groundnuts and other seeds, pineapples, citrus fruits, pears, apricots, cherries, peaches and strawberries)",
7,1,"20091100","20091100",492,2007,2050,5923891,"Country TL description (WITS)","Frozen orange juice, unfermented, whether or not containing added sugar or other sweetening matter (excl. containing spirit)",
7,1,"20091111","20091111",492,2010,2050,5923892,"Trademap six-digit description","Frozen orange juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
7,1,"20091119","20091119",492,2009,2050,5923893,"Trademap six-digit description","Frozen orange juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
7,1,"20091200","20091200",491,2007,2050,5923894,"Standard_HS12","FaoStatName: JUICE OF ORANGES",
7,1,"20091900","20091900",492,2007,2050,5923895,"Standard_HS12","FaoStatName: ORANJUICE CONCENTRATED",
7,1,"20092100","20092100",509,2007,2050,5923896,"Standard_HS12","FaoStatName: JUICE OF GRAPEFRUIT",
7,1,"20092111","20092111",509,2009,2050,5923897,"Standard_HS12","FaoStatName: JUICE OF GRAPEFRUIT",
7,1,"20092119","20092119",509,2010,2050,5923898,"Standard_HS12","FaoStatName: JUICE OF GRAPEFRUIT",
7,1,"20092900","20092900",510,2007,2050,5923899,"Standard_HS12","FaoStatName: GRAPEFRUITJUICE COCENTR",
7,1,"20093100","20093100",513,2007,2050,5923900,"Country TL description (WITS) (could also be mapped to 496,498)","Single citrus fruit juice, unfermented, Brix value <= 20 at 20°C, whether or not containing added sugar or other sweetening matter (excl. containing spirit, mixtures, orange juice and grapefruit juice)",
7,1,"20093111","20093111",513,2010,2050,5923901,"Trademap TL description (could also be mapped to 496,498)","Jugos de frutas u otros frutos (incluido el mosto de uva) o de hortalizas, sin fermentar y sin adición de alcohol, incluso con adición de azúcar u otro edulcorante : Jugo de cualquier otro agrio (cítrico) : De valor Brix inferior o igual a  20",
7,1,"20093119","20093119",513,2009,2050,5923902,"Trademap TL description (could also be mapped to 496,498)","Jugos de frutas u otros frutos (incluido el mosto de uva) o de hortalizas, sin fermentar y sin adición de alcohol, incluso con adición de azúcar u otro edulcorante : Jugo de cualquier otro agrio (cítrico) : De valor Brix inferior o igual a  20 ",
7,1,"20093900","20093900",514,2007,2050,5923903,"Country TL description (WITS) (could also be mapped to 499)","Single citrus fruit juice, unfermented, Brix value > 20 at 20°C, whether or not containing added sugar or other sweetening matter (excl. containing spirit, mixtures, orange juice and grapefruit juice)",
7,1,"20094000","20094000",576,2007,2050,5923904,"Country TL description (WITS) (could also be mapped to 580)","Pineapple juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
7,1,"20094100","20094100",576,2007,2050,5923905,"Standard_HS12","FaoStatName: JUICE OF PINEAPPLES",
7,1,"20094111","20094111",576,2010,2050,5923906,"Standard_HS12","FaoStatName: JUICE OF PINEAPPLES",
7,1,"20094119","20094119",576,2009,2050,5923907,"Standard_HS12","FaoStatName: JUICE OF PINEAPPLES",
7,1,"20094900","20094900",580,2007,2050,5923908,"Standard_HS12","FaoStatName: PINEAPPLE JUICE CONC",
7,1,"20095000","20095000",390,2007,2050,5923909,"Country TL description (WITS) (could also be mapped to 389)","Tomato juice, unfermented, whether or not containing added sugar or other sweetening matter (excl. containing spirit)",
7,1,"20095011","20095011",390,2009,2050,5923910,"Generic HS 2007 to FCL (could also be mapped to 389)","Tomato juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
7,1,"20095019","20095019",390,2009,2050,5923911,"Trademap TL description (could also be mapped to 389)","Jugos de frutas u otros frutos (incluido el mosto de uva) o de hortalizas, sin fermentar y sin adición de alcohol, incluso con adición de azúcar u otro edulcorante : Jugo de tomate",
7,1,"20096100","20096100",562,2007,2050,5923912,"Standard_HS12","FaoStatName: grape juice",
7,1,"20096110","20096110",562,2014,2050,5923913,"Standard_HS12","FaoStatName: grape juice",
7,1,"20096111","20096111",562,2012,2050,5923914,"Standard_HS12","FaoStatName: grape juice",
7,1,"20096119","20096119",562,2009,2050,5923915,"Standard_HS12","FaoStatName: grape juice",
7,1,"20096190","20096190",562,2014,2050,5923916,"Standard_HS12","FaoStatName: grape juice",
7,1,"20096900","20096900",562,2007,2050,5923917,"Standard_HS12","FaoStatName: grape juice",
7,1,"20097100","20097100",518,2007,2050,5923918,"Standard_HS12","FaoStatName: APPLEJUICE SINGLE STRENG",
7,1,"20097111","20097111",518,2010,2050,5923919,"Standard_HS12","FaoStatName: APPLEJUICE SINGLE STRENG",
7,1,"20097119","20097119",518,2009,2050,5923920,"Standard_HS12","FaoStatName: APPLEJUICE SINGLE STRENG",
7,1,"20097900","20097900",519,2007,2050,5923921,"Standard_HS12","FaoStatName: APPLEJUICE CONCENTRATED",
7,1,"20098000","20098000",622,2007,2050,5923922,"Country TL description (WITS) (could also be mapped to 466,538,539,583)","Juice of fruit or vegetables, unfermented, whether or not containing added sugar or other sweetening matter (excl. containing spirit, mixtures, and juice of citrus fruit, pineapples, tomatoes, grapes, incl. grape must and apples)",
7,1,"20098011","20098011",622,2009,2050,5923923,"Trademap TL description (could also be mapped to 466,538,539,583)","Jugos de frutas u otros frutos (incluido el mosto de uva) o de hortalizas, sin fermentar y sin adición de alcohol, incluso con adición de azúcar u otro edulcorante : Jugo de cualquier otra fruta o fruto, u hortaliza",
7,1,"20098019","20098019",622,2009,2050,5923924,"Trademap TL description (could also be mapped to 466,538,539,583)","Jugos de frutas u otros frutos (incluido el mosto de uva) o de hortalizas, sin fermentar y sin adición de alcohol, incluso con adición de azúcar u otro edulcorante : Jugo de cualquier otra fruta o fruto, u hortaliza",
7,1,"20098100","20098100",622,2014,2050,5923925,"Standard_HS12","FaoStatName: FRUIT JUICE NES",
7,1,"20098900","20098900",622,2014,2050,5923926,"Trademap TL description (could also be mapped to 466,538,539,583)","Sumos (sucos) de frutas (incluindo os mostos de uvas) ou de produtos hortícolas, não fermentados, sem adição de álcool, com ou sem adição de açúcar ou de outros edulcorantes : sumo (suco) de qualquer outra fruta ou produto hortícola : outros",
7,1,"20099000","20099000",622,2007,2050,5923927,"Country TL description (WITS) (could also be mapped to 466)","Mixtures of fruit juices, incl. grape must, and vegetable juices, unfermented, whether or not containing added sugar or other sweetening matter (excl. containing spirit)",
7,1,"20099011","20099011",622,2009,2050,5923928,"Trademap TL description (could also be mapped to 466)","Jugos de frutas u otros frutos (incluido el mosto de uva) o de hortalizas, sin fermentar y sin adición de alcohol, incluso con adición de azúcar u otro edulcorante : Mezclas de jugos ",
7,1,"20099019","20099019",622,2009,2050,5923929,"Trademap TL description (could also be mapped to 466)","Jugos de frutas u otros frutos (incluido el mosto de uva) o de hortalizas, sin fermentar y sin adición de alcohol, incluso con adición de azúcar u otro edulcorante : Mezclas de jugos",
7,1,"21011100","21011100",659,2007,2050,5923930,"Standard_HS12","FaoStatName: COFFEE EXTRACTS",
7,1,"21011200","21011200",659,2007,2050,5923931,"Standard_HS12","FaoStatName: COFFEE EXTRACTS",
7,1,"21012000","21012000",672,2007,2050,5923932,"Standard_HS12","FaoStatName: extracts tea, mate,prep.",
7,1,"21013000","21013000",659,2007,2050,5923933,"Standard_HS12","FaoStatName: COFFEE EXTRACTS",
7,1,"21021000","21021000",1232,2007,2050,5923934,"Standard_HS12","FaoStatName: FOOD PREP NES",
7,1,"21022000","21022000",1232,2007,2050,5923935,"Standard_HS12","FaoStatName: FOOD PREP NES",
7,1,"21023000","21023000",1232,2007,2050,5923936,"Standard_HS12","FaoStatName: FOOD PREP NES",
7,1,"21031000","21031000",239,2007,2050,5923937,"Standard_HS12","FaoStatName: SOYA SAUCE",
7,1,"21032000","21032000",1232,2007,2050,5923938,"Standard_HS12","FaoStatName: FOOD PREP NES",
7,1,"21033000","21033000",295,2007,2050,5923939,"Standard_HS12","FaoStatName: FLOUR OF MUSTARD",
7,1,"21039000","21039000",1232,2007,2050,5923940,"Country TL description (WITS) (could also be mapped to 240)","Preparations for sauces and prepared sauces; mixed condiments and seasonings (excl. soya sauce, tomato ketchup and other tomato sauces, mustard, and mustard flour and meal)",
7,1,"21041000","21041000",1232,2007,2050,5923941,"Standard_HS12","FaoStatName: FOOD PREP NES",
7,1,"21042000","21042000",1232,2007,2050,5923942,"Standard_HS12","FaoStatName: FOOD PREP NES",
7,1,"21050000","21050000",910,2007,2050,5923943,"Standard_HS12","FaoStatName: ice cream and edible ice",
7,1,"21061000","21061000",1232,2007,2050,5923944,"Country TL description (WITS) (could also be mapped to 240,241)","Protein concentrates and textured protein substances",
7,1,"21069000","21069000",1232,2007,2050,5923945,"Country TL description (WITS) (could also be mapped to 674)","Food preparations, n.e.s.",
7,1,"21069010","21069010",632,2009,2050,5923946,"Trademap TL description","Preparações alimentícias não especificadas nem compreendidas noutras posições : outras: preparações alcoólicas compostas, dos tipos utilizados na fabricação de bebidas, excepto as preparações à base de substâncias odoríferas",
7,1,"21069020","21069020",633,2009,2050,5923947,"Trademap TL description","Preparações alimentícias não especificadas nem compreendidas noutras posições : outras: bebidas concentradas em embalagens industriais superior a 10 kg",
7,1,"21069090","21069090",1232,2009,2050,5923948,"Trademap TL description","Preparações alimentícias não especificadas nem compreendidas noutras posições : outras: outras",
7,1,"22011000","22011000",631,2007,2050,5923949,"Standard_HS12","FaoStatName: WATERS,ICE ETC",
7,1,"22019000","22019000",631,2007,2050,5923950,"Standard_HS12","FaoStatName: WATERS,ICE ETC",
7,1,"22021000","22021000",633,2007,2050,5923951,"Standard_HS12","FaoStatName: BEVERAGE NON-ALC",
7,1,"22029000","22029000",633,2007,2050,5923952,"Standard_HS12","FaoStatName: BEVERAGE NON-ALC",
7,1,"22030000","22030000",51,2007,2050,5923953,"Country TL description (WITS), Old SWS series (could also be mapped to 66,82,86)","Beer made from malt",
7,1,"22041000","22041000",564,2010,2050,5923954,"Standard_HS12","FaoStatName: WINE",
7,1,"22041010","22041010",564,2007,2050,5923955,"Standard_HS12","FaoStatName: WINE",
7,1,"22041090","22041090",564,2007,2050,5923956,"Standard_HS12","FaoStatName: WINE",
7,1,"22042100","22042100",564,2007,2050,5923957,"Standard_HS12","FaoStatName: WINE",
7,1,"22042110","22042110",564,2007,2050,5923958,"Standard_HS12","FaoStatName: WINE",
7,1,"22042120","22042120",564,2007,2050,5923959,"Standard_HS12","FaoStatName: WINE",
7,1,"22042130","22042130",564,2007,2050,5923960,"Standard_HS12","FaoStatName: WINE",
7,1,"22042140","22042140",564,2007,2050,5923961,"Standard_HS12","FaoStatName: WINE",
7,1,"22042150","22042150",564,2007,2050,5923962,"Standard_HS12","FaoStatName: WINE",
7,1,"22042160","22042160",564,2007,2050,5923963,"Standard_HS12","FaoStatName: WINE",
7,1,"22042170","22042170",564,2007,2050,5923964,"Standard_HS12","FaoStatName: WINE",
7,1,"22042190","22042190",564,2007,2050,5923965,"Standard_HS12","FaoStatName: WINE",
7,1,"22042900","22042900",564,2009,2050,5923966,"Standard_HS12","FaoStatName: WINE",
7,1,"22042910","22042910",564,2007,2050,5923967,"Standard_HS12","FaoStatName: WINE",
7,1,"22042920","22042920",564,2007,2050,5923968,"Standard_HS12","FaoStatName: WINE",
7,1,"22042930","22042930",564,2007,2050,5923969,"Standard_HS12","FaoStatName: WINE",
7,1,"22042990","22042990",564,2007,2050,5923970,"Standard_HS12","FaoStatName: WINE",
7,1,"22043000","22043000",563,2007,2050,5923971,"Standard_HS12","FaoStatName: MUST OF GRAPES",
7,1,"22051000","22051000",565,2007,2050,5923972,"Standard_HS12","FaoStatName: VERMOUTHS&SIMILAR",
7,1,"22059000","22059000",565,2007,2050,5923973,"Standard_HS12","FaoStatName: VERMOUTHS&SIMILAR",
7,1,"22060000","22060000",517,2007,2050,5923974,"Country TL description (WITS), Old SWS series (could also be mapped to 26,39)","Cider, perry, mead and other fermented beverages and mixtures of fermented beverages and non-alcoholic beverages, n.e.s. (excl. beer, wine or fresh grapes, grape must, vermouth and other wine of fresh grapes flavoured with plants or aromatic substances)",
7,1,"22071000","22071000",632,2007,2050,5923975,"Standard_HS12","FaoStatName: ALCOHOL NON FOOD",
7,1,"22072000","22072000",632,2007,2050,5923976,"Standard_HS12","FaoStatName: ALCOHOL NON FOOD",
7,1,"22072010","22072010",632,2014,2050,5923977,"Standard_HS12","FaoStatName: ALCOHOL NON FOOD",
7,1,"22072019","22072019",632,2014,2050,5923978,"Standard_HS12","FaoStatName: ALCOHOL NON FOOD",
7,1,"22082000","22082000",634,2007,2050,5923979,"Standard_HS12","FaoStatName: BEVER. DIST.ALC",
7,1,"22083000","22083000",634,2007,2050,5923980,"Standard_HS12","FaoStatName: BEVER. DIST.ALC",
7,1,"22084000","22084000",634,2007,2050,5923981,"Standard_HS12","FaoStatName: BEVER. DIST.ALC",
7,1,"22085000","22085000",634,2007,2050,5923982,"Standard_HS12","FaoStatName: BEVER. DIST.ALC",
7,1,"22086000","22086000",634,2007,2050,5923983,"Standard_HS12","FaoStatName: BEVER. DIST.ALC",
7,1,"22087000","22087000",634,2007,2050,5923984,"Standard_HS12","FaoStatName: BEVER. DIST.ALC",
7,1,"22089000","22089000",634,2007,2050,5923985,"Standard_HS12","FaoStatName: BEVER. DIST.ALC",
7,1,"22090000","22090000",1232,2007,2050,5923986,"Standard_HS12","FaoStatName: FOOD PREP NES",
7,1,"23011000","23011000",1173,2007,2050,5923987,"Standard_HS12","FaoStatName: MEAL MEAT",
7,1,"23012000","23012000",1174,2009,2050,5923988,"Standard_HS12","FaoStatName: MEAL FISH",
7,1,"23021000","23021000",59,2007,2050,5923989,"Standard_HS12","FaoStatName: BRAN OF MAIZE",
7,1,"23022000","23022000",35,2010,2050,5923990,"Generic HS 2007 to FCL","Bran, sharps and other residues of rice, whether or not in the form of pellets, derived from sifting, milling or other working",
7,1,"23023000","23023000",17,2007,2050,5923991,"Standard_HS12","FaoStatName: BRAN OF WHEAT",
7,1,"23024000","23024000",112,2007,2050,5923992,"Country TL description (WITS) (could also be mapped to 47,73,77,81,85,91,96,99,105)","Bran, sharps and other residues of cereals, whether or not in the form of pellets, derived from sifting, milling or other working (excl. maize, rice and wheat)",
7,1,"23025000","23025000",213,2007,2050,5923993,"Standard_HS12","FaoStatName: BRAN OF PULSES",
7,1,"23031000","23031000",846,2009,2050,5923994,"Standard_HS12","FaoStatName: GLUTEN FEED&MEAL",
7,1,"23032000","23032000",170,2009,2050,5923995,"Trademap TL description, Old SWS series (could also be mapped to 169)","Resíduos da fabricação do amido e resíduos semelhantes, polpas de beterraba, bagaços de cana-de-açúcar e outros desperdícios da indústria do açúcar, borras e desperdícios da indústria da cerveja e das destilarias, mesmo em pellets : polpas de beterraba, bagaços de cana-de-açúcar e outros desperdícios da indústria do açúcar",
7,1,"23040000","23040000",238,2007,2050,5923996,"Standard_HS12","FaoStatName: CAKE OF SOYBEANS",
7,1,"23050000","23050000",245,2010,2050,5923997,"Standard_HS12","FaoStatName: CAKE OF GROUNDNUTS",
7,1,"23062000","23062000",335,2007,2050,5923998,"Standard_HS12","FaoStatName: CAKE OF LINSEED",
7,1,"23063000","23063000",269,2007,2050,5923999,"Standard_HS12","FaoStatName: SUNFLOWER CAKE",
7,1,"23064100","23064100",272,2015,2050,5924000,"Standard_HS12","FaoStatName: CAKE OF RAPESEED",
7,1,"23064900","23064900",272,2009,2050,5924001,"Standard_HS12","FaoStatName: CAKE OF RAPESEED",
7,1,"23065000","23065000",253,2007,2050,5924002,"Standard_HS12","FaoStatName: CAKE OF COPRA",
7,1,"23066000","23066000",259,2010,2050,5924003,"Standard_HS12","FaoStatName: CAKE OF PALM KERNEL",
7,1,"23069000","23069000",341,2007,2050,5924004,"Country TL description (WITS) (could also be mapped to 37,273,282,291,294,298,314,338)","Oil-cake and other solid residues, whether or not ground or in the form of pellets, resulting from the extraction of vegetable fats or oils (excl. of cotton seeds, linseed, sunflower seeds, rape or colza seeds, coconut or copra, palm nuts or kernels, maize corn germ, or from the extraction of soya-bean oil or ground-nut oil)",
7,1,"23070000","23070000",653,2007,2050,5924005,"Standard_HS12","FaoStatName: FOOD WASTES",
7,1,"23080000","23080000",652,2007,2050,5924006,"Country TL description (WITS), Old SWS series (could also be mapped to 120,566,628,629,630,650)","Acorns, horse-chestnuts, marc and other vegetable materials and vegetable waste, vegetable residues and by-products of a kind used in animal feeding, whether or not in the form of pellets, n.e.s.",
7,1,"23091000","23091000",843,2007,2050,5924007,"Standard_HS12","FaoStatName: PET FOOD",
7,1,"23099000","23099000",653,2007,2050,5924008,"Country TL description (WITS), Old SWS series (could also be mapped to 653,840,841,842,843,845,849,850,851,852,853,854,855,1259)","Preparations of a kind used in animal feeding (excl. dog or cat food put up for retail sale)",
7,1,"24011000","24011000",826,2007,2050,5924009,"Standard_HS12","FaoStatName: TOBACCO UNMANFCTED",
7,1,"24012000","24012000",826,2007,2050,5924010,"Standard_HS12","FaoStatName: TOBACCO UNMANFCTED",
7,1,"24013000","24013000",826,2007,2050,5924011,"Standard_HS12","FaoStatName: TOBACCO UNMANFCTED",
7,1,"24021000","24021000",829,2007,2050,5924012,"Standard_HS12","FaoStatName: CIGARS CHEROOTS",
7,1,"24022000","24022000",828,2007,2050,5924013,"Standard_HS12","FaoStatName: CIGARETTES",
7,1,"24029000","24029000",828,2007,2050,5924014,"Country TL description (WITS) (could also be mapped to 829)","Cigars, cheroots, cigarillos and cigarettes consisting wholly of tobacco substitutes",
7,1,"24031000","24031000",831,2007,2050,5924015,"Generic HS 2007 to FCL mapping","Smoking tobacco, whether or not containing tobacco substitutes in any proportion",
7,1,"24031100","24031100",831,2015,2050,5924016,"Standard_HS12","FaoStatName: TOBACCO PRS NES",
7,1,"24031900","24031900",831,2014,2050,5924017,"Standard_HS12","FaoStatName: TOBACCO PRS NES",
7,1,"24039100","24039100",831,2014,2050,5924018,"Standard_HS12","FaoStatName: TOBACCO PRS NES",
7,1,"24039900","24039900",831,2007,2050,5924019,"Standard_HS12","FaoStatName: TOBACCO PRS NES",
7,1,"33011200","33011200",753,2007,2050,5924020,"Standard_HS12","FaoStatName: OIL ESSENTIAL NES",
7,1,"33011300","33011300",753,2007,2050,5924021,"Standard_HS12","FaoStatName: OIL ESSENTIAL NES",
7,1,"33011900","33011900",753,2007,2050,5924022,"Standard_HS12","FaoStatName: OIL ESSENTIAL NES",
7,1,"33012400","33012400",753,2009,2050,5924023,"Standard_HS12","FaoStatName: OIL ESSENTIAL NES",
7,1,"33012500","33012500",753,2007,2050,5924024,"Standard_HS12","FaoStatName: OIL ESSENTIAL NES",
7,1,"33012900","33012900",753,2007,2050,5924025,"Country TL description (WITS) (could also be mapped to 737)","Essential oils, whether or not terpeneless, incl. concretes and absolutes (excl. those of citrus fruit, geramium, jasmine, lavender, lavandine, mint and vetiver)",
7,1,"33013000","33013000",753,2007,2050,5924026,"Standard_HS12","FaoStatName: OIL ESSENTIAL NES",
7,1,"33019000","33019000",753,2007,2050,5924027,"Standard_HS12","FaoStatName: OIL ESSENTIAL NES",
7,1,"35011000","35011000",917,2007,2050,5924028,"Standard_HS12","FaoStatName: CASEIN",
7,1,"35019000","35019000",917,2007,2050,5924029,"Standard_HS12","FaoStatName: CASEIN",
7,1,"35021100","35021100",916,2009,2050,5924030,"Standard_HS12","FaoStatName: egg albumin",
7,1,"35021900","35021900",916,2007,2050,5924031,"Standard_HS12","FaoStatName: egg albumin",
7,1,"38231100","38231100",1276,2009,2050,5924032,"Standard_HS12","FaoStatName: FATTY ACIDS",
7,1,"38231200","38231200",1276,2007,2050,5924033,"Standard_HS12","FaoStatName: FATTY ACIDS",
7,1,"38231300","38231300",1276,2009,2050,5924034,"Standard_HS12","FaoStatName: FATTY ACIDS",
7,1,"38231900","38231900",1276,2007,2050,5924035,"Standard_HS12","FaoStatName: FATTY ACIDS",
7,1,"40011000","40011000",836,2007,2050,5924036,"Standard_HS12","FaoStatName: RUBBER NAT LATEX",
7,1,"40012100","40012100",837,2007,2050,5924037,"Standard_HS12","FaoStatName: RUBBER NAT DRY",
7,1,"40012200","40012200",837,2007,2050,5924038,"Standard_HS12","FaoStatName: RUBBER NAT DRY",
7,1,"40012900","40012900",837,2007,2050,5924039,"Standard_HS12","FaoStatName: RUBBER NAT DRY",
7,1,"40013000","40013000",839,2007,2050,5924040,"Standard_HS12","FaoStatName: GUMS NATURAL",
7,1,"41012000","41012000",920,2007,2050,5924041,"Country TL description (WITS) (could also be mapped to various other hides and skins)","Whole raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired or split, of a weight per skin <= 8 kg when simply dried, <= 10 kg when dry-salted, or <= 16 kg when fresh, wet-salted or otherwise preserved (excl. tanned and parchment-dressed)",
7,1,"41015000","41015000",920,2011,2050,5924042,"Country TL description (WITS) (could also be mapped to various other hides and skins)","Couros e peles em bruto de bovinos (incluindo os búfalos) ou de equídeos (frescos ou salgados, secos, tratados pela cal, piquelados ou conservados de outro modo, mas não curtidos, nem apergaminhados, nem preparados de outro modo), mesmo depilad",
7,1,"41019000","41019000",920,2007,2050,5924043,"Country TL description (WITS) (could also be mapped to various other hides and skins)","Butts, bends, bellies and split raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired, fresh, or salted, dried, limed, pickled or otherwise preserved, and whole raw hides and skins of a weight per skin > 8 kg but < 16 kg when simply dried and > 10 kg but < 16 kg when dry-salted (excl. tanned, parchment-dressed or further prepared)",
7,1,"41021000","41021000",999,2010,2050,5924044,"Standard_HS12","FaoStatName: SKINS WITH WOOL SHEEP",
7,1,"41022900","41022900",998,2011,2050,5924045,"Country TL description (WITS) (could also be mapped to 996,977)","Peles em bruto de ovinos (frescas ou salgadas, secas, tratadas pela cal, piqueladas ou conservadas de outro modo, mas não curtidas, nem apergaminhadas, nem preparadas de outro modo), mesmo depiladas ou divididas, com exceção das excluídas pela",
7,1,"41032000","41032000",1216,2013,2050,5924046,"Country TL description (WITS) (could also be mapped to 1214,1215)","Outros couros e peles em bruto (frescos ou salgados, secos, tratados pela cal, piquelados ou conservados de outro modo, mas não curtidos, nem apergaminhados, nem preparados de outro modo), mesmo depilados ou divididos, com exceção dos excluídos",
7,1,"41039000","41039000",1216,2007,2050,5924047,"Country TL description (WITS) (could also be mapped to various other hides and skins)","Raw hides and skins, fresh, or salted, dried, limed, pickled or otherwise preserved, whether or not dehaired, incl. birdskins without feathers or down (excl. parchment-dressed, hides and skins of bovine incl. buffalo animals, equine animals, sheep, lambs, goats, kids, reptiles and swine)",
7,1,"41152000","41152000",1217,2007,2050,5924048,"Standard_HS12","FaoStatName: LEATHER USE&WASTE",
7,1,"43011000","43011000",1195,2010,2050,5924049,"Standard_HS12","FaoStatName: SKIN FURS",
7,1,"43013000","43013000",1002,2012,2050,5924050,"Standard_HS12","FaoStatName: CARAKUL SKINS",
7,1,"43018000","43018000",1195,2009,2050,5924051,"Country TL description (WITS) (could also be mapped to 1146)","Peles com pelo em bruto (incluindo as cabeças, caudas, patas e outras partes utilizáveis na indústria de peles), exceto as peles em bruto das posições 4101, 4102 ou 4103 : de outros animais, inteiras, com ou cabeça, cauda ou patas",
7,1,"43019000","43019000",1195,2013,2050,5924052,"Standard_HS12","FaoStatName: SKIN FURS",
7,1,"50010000","50010000",1185,2009,2050,5924053,"Standard_HS12","FaoStatName: COCOONS REELABLE",
7,1,"50020000","50020000",1186,2007,2050,5924054,"Standard_HS12","FaoStatName: SILK RAW",
7,1,"50030000","50030000",1187,2009,2050,5924055,"Standard_HS12","FaoStatName: COCOON UNR.&WASTE",
7,1,"50039000","50039000",1187,2007,2050,5924056,"Generic HS 2007 to FCL mapping","Silk waste, incl. cocoons unsuitable for reeling, yarn waste and garnetted stock, carded or combed",
7,1,"51011100","51011100",987,2009,2050,5924057,"Standard_HS12","FaoStatName: WOOL GREASY",
7,1,"51011900","51011900",987,2009,2050,5924058,"Standard_HS12","FaoStatName: WOOL GREASY",
7,1,"51012100","51012100",988,2011,2050,5924059,"Standard_HS12","FaoStatName: WOOL DEGREASED",
7,1,"51012900","51012900",988,2009,2050,5924060,"Standard_HS12","FaoStatName: WOOL DEGREASED",
7,1,"51013000","51013000",988,2007,2050,5924061,"Standard_HS12","FaoStatName: WOOL DEGREASED",
7,1,"51021100","51021100",1030,2013,2050,5924062,"Country TL description (WITS)","Pelos finos ou grosseiros, não cardados nem penteados : pelos finos : de cabra de caxemira",
7,1,"51021900","51021900",1218,2007,2050,5924063,"Country TL description (WITS) (could also be mapped to 1030)","Fine animal hair, neither carded nor combed (excl. wool and hair of Kashmir cashmere goats)",
7,1,"51022000","51022000",1219,2014,2050,5924064,"Country TL description (WITS) (could also be mapped to 1031)","Pelos finos ou grosseiros, não cardados nem penteados : pelos grosseiros",
7,1,"51031000","51031000",1009,2009,2050,5924065,"Standard_HS12","FaoStatName: WOOL;HAIR WASTE",
7,1,"51032000","51032000",1009,2007,2050,5924066,"Standard_HS12","FaoStatName: WOOL;HAIR WASTE",
7,1,"51033000","51033000",1009,2007,2050,5924067,"Standard_HS12","FaoStatName: WOOL;HAIR WASTE",
7,1,"51040000","51040000",1007,2010,2050,5924068,"Standard_HS12","FaoStatName: WOOL SHODDY",
7,1,"51051000","51051000",1008,2009,2050,5924069,"Standard_HS12","FaoStatName: HAIR CARDED/ COMBED",
7,1,"51052100","51052100",1008,2012,2050,5924070,"Standard_HS12","FaoStatName: HAIR CARDED/ COMBED",
7,1,"51052900","51052900",1010,2009,2050,5924071,"Trademap TL description (could also be mapped to 1008)","Wool tops and other combed wool, other than combed wool in fragments",
7,1,"51053900","51053900",1008,2011,2050,5924072,"Standard_HS12","FaoStatName: HAIR CARDED/ COMBED",
7,1,"51054000","51054000",1008,2014,2050,5924073,"Standard_HS12","FaoStatName: HAIR CARDED/ COMBED",
7,1,"52010000","52010000",767,2007,2050,5924074,"Standard_HS12","FaoStatName: COTTON LINT",
7,1,"52021000","52021000",769,2007,2050,5924075,"Standard_HS12","FaoStatName: COTTON WASTE",
7,1,"52029100","52029100",769,2007,2050,5924076,"Standard_HS12","FaoStatName: COTTON WASTE",
7,1,"52029900","52029900",769,2007,2050,5924077,"Standard_HS12","FaoStatName: COTTON WASTE",
7,1,"52030000","52030000",768,2007,2050,5924078,"Standard_HS12","FaoStatName: COTTON CARDED,COMBED",
7,1,"53011000","53011000",771,2007,2050,5924079,"Standard_HS12","FaoStatName: FLAX FIBRE RAW",
7,1,"53012100","53012100",773,2009,2050,5924080,"Standard_HS12","FaoStatName: FLAX FIBRE & TOW",
7,1,"53012900","53012900",773,2007,2050,5924081,"Standard_HS12","FaoStatName: FLAX FIBRE & TOW",
7,1,"53013000","53013000",774,2007,2050,5924082,"Standard_HS12","FaoStatName: FLAX TOW WASTE",
7,1,"53021000","53021000",777,2007,2050,5924083,"Standard_HS12","FaoStatName: HEMP TOW WASTE",
7,1,"53029000","53029000",777,2007,2050,5924084,"Standard_HS12","FaoStatName: HEMP TOW WASTE",
7,1,"53031000","53031000",780,2007,2050,5924085,"Country TL description (WITS), Old SWS series (could also be mapped to 782)","Jute and other textile bast fibres, raw or retted (excl. flax, true hemp and ramie)",
7,1,"53039000","53039000",780,2007,2050,5924086,"Country TL description (WITS), Old SWS series (could also be mapped to 782)","Jute and other textile bast fibres, processed but not spun; tow and waste of such fibres, incl. yarn waste and garnetted stock (excl. retted fibres of this kind, flax, true hemp and ramie)",
7,1,"53041000","53041000",789,2007,2050,5924087,"Country TL description (WITS), Old SWS series (could also be mapped to 800)","Sisal and other textile fibres of the genus Agave, raw",
7,1,"53049000","53049000",789,2007,2050,5924088,"Country TL description (WITS), Old SWS series (could also be mapped to 800)","Sisal and other textile fibres of the genus Agave, processed but not spun; tow and waste of such fibres, incl. yarn waste and garnetted stock",
7,1,"53050000","53050000",821,2009,2050,5924089,"Generic HS 2007 to FCL mapping","Coconut, abaca Manila hemp or Musa textilis Nee, ramie, agave and other vegetable textile fibres, n.e.s., raw or processed, but not spun; tow, noils and waste of such fibres, incl. yarn waste and garnetted stock",
7,1,"53052100","53052100",809,2007,2050,5924090,"Generic HS 2007 to FCL mapping","Abaca Manila hemp or Musa textilis Nee, raw",
7,1,"53059000","53059000",821,2007,2050,5924091,"Country TL description (WITS), Old SWS series (could also be mapped to 778,788)","Ramie and other vegetable textile fibres, n.e.s., raw or processed, but not spun; tow, noils and waste of such fibres, incl. yarn waste and garnetted stock",
7,1,"53081000","53081000",813,2009,2050,5924092,"Standard_HS12","FaoStatName: COIR",
7,2,"09011120","09011120",656,2007,2050,5924093,"Standard_HS12","FaoStatName: COFFEE GREEN",
8,1,"010519","010519",1068,2005,2050,5924094,"Generic HS2007 to FCL (could also be mapped to 1072) ","Live domestic ducks, geese and guinea fowls, weighing <= 185 g",
8,1,"010599","010599",1068,2005,2050,5924095,"Generic HS2007 to FCL (could also be mapped to 1072,1079) ","Live domestic ducks, geese, turkeys and guinea fowls, weighing > 185 g",
8,1,"010600","010600",1169,2005,2050,5924096,"Trademap description","010600 - Live animals (excluding horses, asses, mules, hinnies, bovine animals, swine, sheep, goats, poultry, fish, crustaceans, molluscs and other aquatic invertebrates, and microorganic cultures etc.)",
8,1,"020220","020220",867,2005,2050,5924097,"Generic HS2007 to FCL (could also be mapped to 947) ","Frozen bovine cuts, with bone in (excluding carcases and half-carcases)",
8,1,"020230","020230",870,2005,2050,5924098,"Generic HS2007 to FCL (could also be mapped to 947) ","Frozen, boneless meat of bovine animals",
8,1,"020714","020714",1058,2005,2050,5924099,"Generic HS2007 to FCL (could also be mapped to 1059) ","Frozen cuts and edible offal of fowls of the species Gallus domesticus",
8,1,"020727","020727",1080,2005,2050,5924100,"Generic HS2007 to FCL (could also be mapped to 1081) ","Frozen cuts and edible offal of turkeys of the species domesticus",
8,1,"020890","020890",1166,2005,2050,5924101,"Generic HS2007 to FCL (could also be mapped to 1089,1127,1128,1151,1158,1159,1163) ","Fresh, chilled or frozen meat and edible offal of pigeons, game, reindeer and other animals (excluding bovine animals, swine, sheep, goats, horses, asses, mules, hinnies, poultry fowls of the species Gallus domesticus, ducks, geese, turkeys, guinea fowl, rabbits, hares, primates, whales, dolphins and porpoises mammals of the order Cetacea, manatees and dugongs mammals of the order Sirenia, seals, sea lions and walruses mammals of the suborder Pinnipedia and reptiles)",
8,1,"021090","021090",1164,2005,2050,5924102,"Generic HS2007 to FCL unique six-digit match","Meat and edible offal, salted, in brine, dried or smoked, and edible flours and meals of meat or meat offal (excluding bovine and swine meat)",
8,1,"040700","040700",1062,2005,2050,5924103,"Generic HS2007 to FCL (could also be mapped to 1091) ","Birds' eggs, in shell, fresh, preserved or cooked",
8,1,"070990","070990",463,2005,2050,5924104,"Generic HS2007 to FCL (could also be mapped to 260,378,394,430,446) ","Fresh or chilled vegetables (excluding potatoes, tomatoes, vegetables of the Allium spp., cabbages of the genus Brassica, lettuces of the species Lactuca sativa and Cichorium, carrots, turnips, salad beetroot, salsify, celeriac, radishes and similar edible roots, cucumbers and gherkins, leguminous vegetables, asparagus, aubergines, mushrooms, truffles, fruits of the genus Capsicum or of the genus Pimenta, spinach, New Zealand spinach and orache spinach)",
8,1,"071490","071490",149,2005,2050,5924105,"Generic HS2007 to FCL (could also be mapped to 135,136,137,151) ","Arrowroot, salep, Jerusalem artichokes and similar roots and tubers with high starch or inulin content, fresh, chilled, frozen or dried, whether or not sliced or in the form of pellets, and sago pith (excluding manioc cassava, sweet potatoes, yams, taro and yautia)",
8,1,"080300","080300",486,2005,2050,5924106,"Generic HS2007 to FCL (could also be mapped to 489,604) ","Bananas, incl. plantains, fresh or dried",
8,1,"090190","090190",658,2005,2050,5924107,"Generic HS2007 to FCL (could also be mapped to 660) ","Coffee husks and skins; coffee substitutes containing coffee in any proportion",
8,1,"120799","120799",339,2005,2050,5924108,"Generic HS2007 to FCL (could also be mapped to 263,275,277,305,311,312,336) ","Oil seeds and oleaginous fruits, whether or not broken (excluding edible nuts, olives, soya beans, groundnuts, copra, linseed, rape or colza seeds, sunflower seeds, palm nuts and kernels, cotton, castor oil, sesamum, mustard, safflower, melon and poppy seeds)",
8,1,"12129920","12129920",460,2010,2050,5924109,"Country TL description (WITS), Generic HS2007 to FCL","Mauby bark",
8,1,"150100","150100",1043,2005,2050,5924110,"Generic HS2007 to FCL (could also be mapped to 1066) "," Pig fat, incl. lard, and poultry fat, rendered or otherwise extracted (excluding lard stearin and lard oil)",
8,1,"15121100","15121100",268,2014,2050,5924111,"Generic HS2012 to FCL (could also be mapped to 281) ","Crude sunflower-seed or safflower oil",
8,1,"151790","151790",1243,2005,2050,5924112,"Generic HS2007 to FCL (could also be mapped to 1241) ","Edible mixtures or preparations of animal or vegetable fats or oils and edible fractions of different fats or oils (excluding fats, oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared, mixtures of olive oils and their fractions, and solid margarine)",
8,1,"170290","170290",167,2005,2050,5924113,"Generic HS2007 to FCL (could also be mapped to 155,175) ","Sugars in solid form, incl. invert sugar and chemically pure maltose, and sugar and sugar syrup blends containing in the dry state 50% by weight of fructose, not flavoured or coloured, artificial honey, whether or not mixed with natural honey and caramel (excluding cane or beet sugar, chemically pure sucrose, lactose, maple sugar, glucose, fructose, and syrups thereof)",
8,1,"190190","190190",115,2005,2050,5924114,"Generic HS2007 to FCL (could also be mapped to 50) ","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905)",
8,1,"190590","190590",22,2005,2050,5924115,"Generic HS2007 to FCL (could also be mapped to 110) ","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products (excluding crispbread, gingerbread and the like, sweet biscuits, waffles, wafers not mentioned, rusks, toasted bread and similar toasted products)",
8,1,"200490","200490",447,2005,2050,5924116,"Generic HS2007 to FCL, Old SWS series (could also be mapped to 262,475) ","Vegetables and mixtures of vegetables, prepared or preserved otherwise than by vinegar or acetic acid, frozen (excluding preserved by sugar, and tomatoes, mushrooms, truffles and potatoes, unmixed)",
8,1,"200919","200919",492,2005,2050,5924117,"Generic HS2007 to FCL unique six-digit match","Orange juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, frozen, and of a Brix value <= 20 at 20°C)",
8,1,"200920","200920",509,2005,2050,5924118,"Generic HS2007 to FCL (could also be mapped to 510) ","Grapefruit juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
8,1,"200930","200930",514,2005,2050,5924119,"Generic HS2007 to FCL (could also be mapped to 496,498,499,513) ","Juice of citrus fruit, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
8,1,"200940","200940",576,2005,2050,5924120,"Generic HS2007 to FCL, Old SWS series (could also be mapped to 580) ","Pineapple juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
8,1,"200970","200970",518,2005,2050,5924121,"Generic HS2007 to FCL, Old SWS series (could also be mapped to 519) ","Apple juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
8,2,"010599","010599",1068,2005,2050,5924122,"Generic HS2007 to FCL (could also be mapped to 1072,1079) ","Live domestic ducks, geese, turkeys and guinea fowls, weighing > 185 g",
8,2,"010600","010600",1169,2005,2050,5924123,"Trademap description","010600 - Live animals (excluding horses, asses, mules, hinnies, bovine animals, swine, sheep, goats, poultry, fish, crustaceans, molluscs and other aquatic invertebrates, and microorganic cultures etc.)",
8,2,"020714","020714",1058,2005,2050,5924124,"Generic HS2007 to FCL (could also be mapped to 1059) ","Frozen cuts and edible offal of fowls of the species Gallus domesticus",
8,2,"021090","021090",1164,2005,2050,5924125,"Generic HS2007 to FCL unique six-digit match","Meat and edible offal, salted, in brine, dried or smoked, and edible flours and meals of meat or meat offal (excluding bovine and swine meat)",
8,2,"070990","070990",463,2005,2050,5924126,"Generic HS2007 to FCL (could also be mapped to 260,378,394,430,446) ","Fresh or chilled vegetables (excluding potatoes, tomatoes, vegetables of the Allium spp., cabbages of the genus Brassica, lettuces of the species Lactuca sativa and Cichorium, carrots, turnips, salad beetroot, salsify, celeriac, radishes and similar edible roots, cucumbers and gherkins, leguminous vegetables, asparagus, aubergines, mushrooms, truffles, fruits of the genus Capsicum or of the genus Pimenta, spinach, New Zealand spinach and orache spinach)",
8,2,"071490","071490",149,2005,2050,5924127,"Generic HS2007 to FCL (could also be mapped to 135,136,137,151) ","Arrowroot, salep, Jerusalem artichokes and similar roots and tubers with high starch or inulin content, fresh, chilled, frozen or dried, whether or not sliced or in the form of pellets, and sago pith (excluding manioc cassava, sweet potatoes, yams, taro and yautia)",
8,2,"151790","151790",1243,2007,2050,5924128,"Generic HS2007 to FCL (could also be mapped to 1241) ","Edible mixtures or preparations of animal or vegetable fats or oils and edible fractions of different fats or oils (excluding fats, oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared, mixtures of olive oils and their fractions, and solid margarine)",
8,2,"170290","170290",167,2005,2050,5924129,"Generic HS2007 to FCL (could also be mapped to 155,175) ","Sugars in solid form, incl. invert sugar and chemically pure maltose, and sugar and sugar syrup blends containing in the dry state 50% by weight of fructose, not flavoured or coloured, artificial honey, whether or not mixed with natural honey and caramel (excluding cane or beet sugar, chemically pure sucrose, lactose, maple sugar, glucose, fructose, and syrups thereof)",
8,2,"190590","190590",22,2005,2050,5924130,"Generic HS2007 to FCL (could also be mapped to 110) ","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products (excluding crispbread, gingerbread and the like, sweet biscuits, waffles, wafers not mentioned, rusks, toasted bread and similar toasted products)",
8,2,"200919","200919",492,2007,2050,5924131,"Generic HS2007 to FCL unique six-digit match","Orange juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, frozen, and of a Brix value <= 20 at 20°C)",
8,2,"200930","200930",514,2007,2050,5924132,"Generic HS2007 to FCL (could also be mapped to 496,498,499,513) ","Juice of citrus fruit, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
17,1,"0106120","0106120",1169,2012,2050,5924133,"Generic HS2012 to FCL (could also be mapped to 1171) ","-- Whales, dolphins and porpoises (mammals of the order Cetacea); manatees and dugongs (mammals of the order Sirenia); seals, sea lions and walruses (mammals of the suborder Pinnipedia)",
17,1,"1502900","1502900",869,2014,2050,5924134,"Generic HS2012 to FCL (could also be mapped to 871,949,979,1019) ","Fats of bovine animals, sheep or goats (excluding tallow, oleostearin and oleo-oil)",
17,1,"1514910","1514910",271,2012,2050,5924135,"Generic HS2007 to FCL (could also be mapped to 293) ","High erucic acid rape or colza oil fixed oil which has an erucic acid content of >= 2% and mustard oil, crude",
25,1,"02074400","02074400",1069,2015,2050,5924136,"Trademap description (could also be mapped to 1075)","Fresh or chilled cuts and edible offal of domestic ducks (excluding fatty livers)",
25,1,"02074500","02074500",1069,2015,2050,5924137,"Trademap description (could also be mapped to 1075)","Meat and edible offal, of the poultry of heading 01.05, fresh, chilled or frozen:Of ducks:Other, frozen",
25,1,"02075400","02075400",1073,2015,2050,5924138,"Trademap description (could also be mapped to 1074)","Meat and edible offal, of the poultry of heading 01.05, fresh, chilled or frozen:Of geese:Other, fresh or chilled",
25,1,"02075500","02075500",1073,2015,2050,5924139,"Trademap description (could also be mapped to 1074)","Meat and edible offal, of the poultry of heading 01.05, fresh, chilled or frozen:Of geese:Other, frozen",
25,1,"02091000","02091000",1037,2015,2050,5924140,"Trademap description (could also be mapped to 1040)","Pig fat, free of lean meat, not rendered or otherwise extracted, fresh, chilled, frozen, salted, in brine, dried or smoked",
25,1,"04014000","04014000",885,2015,2050,5924141,"Country TL description (WITS), Jellyfish (previously, anything above 6% fat was mapped to 885 cream)","Milk and cream, not concentrated nor, containing added sugar or other sweetening matter:Of a fat content , by weight, exceeding 6% but not exceeding 10%",
25,1,"04015000","04015000",885,2015,2050,5924142,"Country TL description (WITS), Jellyfish (previously, anything above 6% fat was mapped to 885 cream)","Milk and cream, not concentrated nor, containing added sugar or other sweetening matter:Of a fat content, by weight, exceeding 10 %",
25,1,"04079000","04079000",1062,2015,2050,5924143,"Country TL description (WITS), Old SWS series (could also be mapped to 1091)","Bird's egg in shell fresh, preserved, or cooked: Other",
25,1,"07099900","07099900",463,2015,2050,5924144,"Country TL description (WITS) (could also be mapped to 378, 430, 446)","Other vegetables fresh or chilled: Other: Other",
25,1,"15029000","15029000",869,2015,2050,5924145,"Country TL description (WITS) (could also be mapped to 871, 949, 979, 1019)","Fats of bovine animals, sheep or goats, other than those of heading 1503: other",
25,1,"15149100","15149100",271,2014,2050,5924146,"Country TL description (WITS) (could also be mapped to 293)","Rape, colza or mustard oil and fractions thereof, whether or not refined, but not chemically modified: Other: crude oil",
25,1,"17011300","17011300",162,2015,2050,5924147,"Generic HS2012 to FCL mapping, old SWS series (could also be mapped to 163)","Raw cane sugar, in solid form, not containing added flavouring or colouring matter, obtained without centrifugation, with sucrose content 69° to 93°, containing only natural anhedral microcrystals (see subheading note 2.)",
25,1,"17011400","17011400",162,2015,2050,5924148,"Generic HS2012 to FCL mapping, old SWS series (could also be mapped to 163)","Cane or beet sugar and chemically pure sucrose, in solid form:Raw sugar not containing added flavouring or colouring matter:Other cane sugar",
25,1,"20093900","20093900",514,2015,2050,5924149,"Trademap description (could also be mapped to 499)","Fruit juices (including grape must) and vegetable juices, unfermented and not containing added spirit, whether or not containing added sugar or other sweetening matter:Juice of any other single citrus fruit:Other",
25,2,"01061100","01061100",1169,2015,2050,5924150,"Country TL description (WITS) (could also be mapped to 1171)","Other live animals: Mammals: Primates",
25,2,"01061900","01061900",1169,2015,2050,5924151,"Country TL description (WITS) (could also be mapped to 1171)","Other live animals: Mammals: Other",
25,2,"01063910","01063910",1169,2015,2050,5924152,"Country TL description (WITS) (could also be mapped to 1171)","Other live animals: Birds: Other: Wild",
25,2,"01063920","01063920",1169,2015,2050,5924153,"Country TL description (WITS) (could also be mapped to 1171)","Other live animals: Birds: Other: Domesticated",
25,2,"01063990","01063990",1169,2015,2050,5924154,"Country TL description (WITS) (could also be mapped to 1171)","Other live animals: Birds: Other: Other",
25,2,"01069000","01069000",1169,2015,2050,5924155,"Country TL description (WITS) (could also be mapped to 1171)","Other live animals ",
25,2,"17011300","17011300",162,2015,2050,5924156,"Generic HS2012 to FCL mapping, old SWS series (could also be mapped to 163)","Raw cane sugar, in solid form, not containing added flavouring or colouring matter, obtained without centrifugation, with sucrose content 69° to 93°, containing only natural anhedral microcrystals (see subheading note 2.)",
26,1,"01059910","01059910",1068,2004,2050,5924157,"Country TL description (WITS)","Breeding ducks",
26,1,"01059940","01059940",1057,2004,2050,5924158,"Generic HS2002 to FCL, Old SWS series (could also be mapped to 1072,1079) ","Other geese, turkeys and guinea fowls, over 185 g, live (u)",
26,1,"010599900","010599900",1068,2006,2050,5924159,"Generic HS2002 to FCL (could also be mapped to 1072,1079) ","Other birds (u)",
26,1,"01061100","01061100",1169,2004,2050,5924160,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1171) ","Primates",
26,1,"01061200","01061200",1169,2004,2050,5924161,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1171) ","Whales, dolphins, porpoises; manatees and dugongs, live (u)",
26,1,"0106310000","0106310000",1169,2014,2050,5924162,"Country TL description (WITS)","Birds of prey",
26,1,"02050000","02050000",1097,2004,2050,5924163,"Generic HS2002 to FCL (could also be mapped to 1108,1111) ","Meat of horses, asses, mules or hinnies, fresh, chilled or frozen",
26,1,"020727000","020727000",1080,2006,2050,5924164,"Generic HS2002 to FCL (could also be mapped to 1081) ","Other cuts & offal of turkeys frozen (kg)",
26,1,"02072790","02072790",1080,2004,2050,5924165,"Generic HS2002 to FCL (could also be mapped to 1081) ","Other cuts & offal of turkeys, frozen (kg)",
26,1,"020732000","020732000",1069,2006,2050,5924166,"Generic HS2002 to FCL (could also be mapped to 1075) ","Ducks fresh or chilled (kg)",
26,1,"0207440000","0207440000",1069,2015,2050,5924167,"Generic HS2012 to FCL (could also be mapped to 1075) ","Cuts and offal of ducks, fresh or chilled (kg)",
26,1,"04031091","04031091",892,2004,2050,5924168,"Country TL description (WITS), Old SWS series (could also be mapped to 891) ","Yogurt, whether or not flavoured or containing added sugar or other sweetening matter, fruits, nuts or cocoa: in condensed form",
26,1,"04070091","04070091",1062,2004,2050,5924169,"Country TL description (WITS)","Birds' eggs, in shell, fresh, preserved or cooked: Hen's eggs",
26,1,"1211901100","1211901100",1293,2014,2050,5924170,"Country TL description (WITS)","Plants and parts of plants (including seeds and fruits), of a kind used primarily in perfumery, in pharmacy or for insecticidal, fungicidal or similar purposes, fresh or dried, whether or not cut, crushed or powdered: Other: Of a kind used prim",
26,1,"12119013","12119013",1293,2004,2050,5924171,"Country TL description (WITS)","Cannabis: other, in cut, crushed or powdered form",
26,1,"121190300","121190300",1293,2006,2050,5924172,"Trademap TL description","Sandalwood for perfumery insecticidal fungicidal & similar purposes (kg)",
26,1,"12119094","12119094",1293,2004,2050,5924173,"Country TL description (WITS)","Sandalwood",
26,1,"14020010","14020010",1293,2004,2050,5924174,"Generic HS2002 to FCL unique six-digit match","Vegetable materials of a kind used primarily as stuffing or as padding, e.g. kapok, vegetable hair and eel-grass, whether or not put up as a layer, with or without supporting material: Kapok",
26,1,"14020090","14020090",1293,2004,2050,5924175,"Generic HS2002 to FCL unique six-digit match","Vegetable materials of a kind used primarily as stuffing or as padding, e.g. kapok, vegetable hair and eel-grass, whether or not put up as a layer, with or without supporting material: Other",
26,1,"15149190","15149190",293,2004,2050,5924176,"Country TL description (WITS)","Mustard oil, crude (kg)",
26,1,"151540000","151540000",276,2006,2050,5924177,"Trademap TL description","Tung oil & its fractions ; refined (kg)",
26,1,"15154090","15154090",276,2004,2050,5924178,"Generic HS2002 to FCL unique six-digit match","Tung oil & its fractions ; refined (kg)",
26,1,"15162012","15162012",1275,2004,2050,5924179,"Trademap TL description","Palm fruit oil re-esterified fat and oil and its fractions, crude (kg)",
26,1,"15162021","15162021",1275,2004,2050,5924180,"Trademap TL description","Ground nuts, soya-beans, palm fruit oil, palm kernels or coconuts, hydrogenated fats in flakes (kg)",
26,1,"15162042","15162042",1275,2004,2050,5924181,"Country TL description (WITS)","Vegetable fats and oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared: Of coconuts: Refined, bleached and deodorised (RBD)",
26,1,"15162052","15162052",1275,2004,2050,5924182,"Country TL description (WITS)","Vegetable fats and oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared: Of coconuts: Refined, bleached and deodorised (RBD)",
26,1,"15162061","15162061",1275,2004,2050,5924183,"Country TL description (WITS)","Vegetable fats and oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared: Of illipenut oil",
26,1,"15162069","15162069",1275,2004,2050,5924184,"Country TL description (WITS)","Vegetable fats and oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared: Other",
26,1,"15162072","15162072",1275,2004,2050,5924185,"Country TL description (WITS)","Vegetable fats and oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared: Hydrogenated fats in flakes, in packages of a net weight of 10 kg or more",
26,1,"15162086","15162086",1275,2004,2050,5924186,"Country TL description (WITS)","Vegetable fats and oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared: Other",
26,1,"1516209400","1516209400",1275,2014,2050,5924187,"Generic HS2002 to FCL (could also be mapped to 1273) ","Vegetable fats and oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared:  Soya beans fat and oil and its fractions, other (kg)",
26,1,"15179042","15179042",1243,2004,2050,5924188,"Trademap TL description","Shortenings (kg)",
26,1,"1517906600","1517906600",1243,2014,2050,5924189,"Generic HS2002 to FCL (could also be mapped to 1241) ","Edible mixtures or preparations of animal or vegetable fats or oils and edible fractions of different fats or oils (excluding fats, oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared, mixtures of olive oils and their fractions, and solid margarine):  Liquid mixtures or preparations in which palm kernel olein predominates (kg)",
26,1,"15179079","15179079",1243,2004,2050,5924190,"Country TL description (WITS)","Edible mixtures or preparations of animal or vegetable fats or oils and edible fractions of different fats or oils (excluding fats, oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared, mixtures of olive oils and their fractions, and solid margarine):Solid mixtures or preparations: in which groundnut oil predominates: Other",
26,1,"15179085","15179085",1243,2004,2050,5924191,"Country TL description (WITS)","Edible mixtures or preparations of animal or vegetable fats or oils and edible fractions of different fats or oils (excluding fats, oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared, mixtures of olive oils and their fractions, and solid margarine):Solid mixtures or preparations: in which soya bean oil or coconut oil predominates",
26,1,"15219020","15219020",1295,2004,2050,5924192,"Country TL description (WITS)","Spermaceti",
26,1,"1701140000","1701140000",162,2015,2050,5924193,"Generic HS2012 to FCL, Old SWS Series (could also be mapped to 163) ","Other cane sugar, raw (kg)",
26,1,"19019013","19019013",115,2004,2050,5924194,"Country TL description (WITS)","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905): Other medical food",
26,1,"19019033","19019033",115,2004,2050,5924195,"Country TL description (WITS)","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905): Other, not containing cocoa",
26,1,"19019034","19019034",115,2004,2050,5924196,"Country TL description (WITS)","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905): Other, containing cocoa",
26,1,"19019052","19019052",115,2004,2050,5924197,"Country TL description (WITS)","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905): Other, not containing cocoa",
26,1,"19019053","19019053",115,2004,2050,5924198,"Country TL description (WITS)","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905): Other,  containing cocoa",
26,1,"20049020","20049020",447,2004,2050,5924199,"Trademap TL description","Other preparations of sweet corn",
26,1,"41019000","41019000",920,2004,2050,5924200,"Generic HS2002 to FCL (could also be mapped to 921,1103) ","Butts, bends, bellies and split raw hides and skins of bovine incl. buffalo or equine animals, ",
26,1,"53051100","53051100",813,2004,2050,5924201,"Trademap TL description","Coconut coir fibres, raw",
26,2,"020727000","020727000",1080,2006,2050,5924202,"Generic HS2002 to FCL (could also be mapped to 1081) ","Other cuts & offal of turkeys frozen (kg)",
26,2,"41019000","41019000",920,2004,2050,5924203,"Generic HS2002 to FCL (could also be mapped to 921,1103) ","Butts, bends, bellies and split raw hides and skins of bovine incl. buffalo or equine animals, ",
38,1,"020754","020754",1073,2015,2050,5924204,"Generic HS 2012 to FCL (could also be mapped to 1074)","Meat and edible offal, of the poultry of heading 01.05, fresh, chilled, or frozen: Of geese, Other, fresh or chilled",
38,1,"150290","150290",869,2015,2050,5924205,"Generic HS 2012 to FCL (could also be mapped to 871, 949,979, or 1019, but 869 is the largest utilization)","Fats of bovine animals, sheep or goats (excluding tallow, oleostearin and oleo-oil)",
38,2,"040140","040140",885,2015,2050,5924206,"Trademap TL description, Jellyfish (previously, anything above 6% fat was mapped to 885 cream)","Milk and cream of a fat content by weight of > 6% but <= 10%, not concentrated nor containing added sugar or other sweetening matter",
46,1,"07099900000","07099900000",463,2014,2050,5924207,"Trademap TL description, Old SWS series (could also be mapped to 378,430,446) ","Légumes, n.d.a., à létat frais ou réfrigéré (libellé détaillé non disponible)",
46,1,"20098900000","20098900000",622,2014,2050,5924208,"Trademap TL description, Old SWS series (could also be mapped to 466,538,539,583)","Jus de fruits ou de légumes, non fermentés, sans addition d'alcool, avec ou sans addition de sucre ou d'autres édulcorants (à l'excl. des mélanges ainsi que des jus d'agrumes, d'ananas, de tomates, de raisins, y. c. les moûts, de pommes et d'airelles) (libellé détaillé non disponible)",
47,1,"15020000","15020000",869,2009,2050,5924209,"Generic HS 2007 mapping, Old SWS series (could also be mapped to 871, 949, 979, 1019)","Fats of bovine animals, sheep or goats, other than those of heading 1503: other",
47,1,"23080000","23080000",652,2005,2050,5924210,"HS 2002","Vegetable materials and vegetable waste, vegetable residues and by-products, whether or not in the form of pellts, of a kind used in animal feeding, not elsewhere specified or included",
73,1,"150290","150290",869,2010,2050,5924211,"Trademap description, Old SWS series (could also be mapped to 871, 949, 979, 1019)","Fats of bovine animals, sheep or goats (excluding tallow, oleostearin and oleo-oil)",
73,1,"410190","410190",920,2009,2050,5924212,"Trademap TL description, Old SWS series (could also be mapped to 921, 1103)","Butts, bends, bellies and split raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired, fresh, or salted, dried, limed, pickled or otherwise preserved, and whole raw hides and skins of a weight per skin > 8 kg but < 16 kg when simply dried and > 10 kg but < 16 kg when dry-salted (excl. tanned, parchment-dressed or further prepared) (detailed label not available)",
73,1,"530590","530590",780,2008,2050,5924213,"Trademap description, FCL description, Old SWS series","Ramie and other vegetable textile fibres, n.e.s., raw or processed, but not spun; tow, noils and waste of such fibres, incl. yarn waste and garnetted stock",
73,2,"410190","410190",920,2009,2050,5924214,"Trademap TL description, Old SWS series (could also be mapped to 921, 1103)","Butts, bends, bellies and split raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired, fresh, or salted, dried, limed, pickled or otherwise preserved, and whole raw hides and skins of a weight per skin > 8 kg but < 16 kg when simply dried and > 10 kg but < 16 kg when dry-salted (excl. tanned, parchment-dressed or further prepared) (detailed label not available)",
74,1,"070990","070990",463,2005,2050,5924215,"Generic HS 2007 to FCL mapping, Old SWS series (could also be mapped to 260, 378, 394, 430, 446). Description from Trademap","Fresh or chilled vegetables (excluding potatoes, tomatoes, vegetables of the Allium spp., cabbages of the genus Brassica, lettuces of the species Lactuca sativa and Cichorium, carrots, turnips, salad beetroot, salsify, celeriac, radishes and similar edible roots, cucumbers and gherkins, leguminous vegetables, asparagus, aubergines, mushrooms, truffles, fruits of the genus Capsicum or of the genus Pimenta, spinach, New Zealand spinach and orache spinach)",
74,1,"160220","160220",878,2005,2050,5924216,"8-digit code does not exist, but six-digit code has two subcategories (10 foie gras and 90 other), but Old SWS series seems to indicate that bulk of imports come in under other liv preps rather than foie gras). Code from Trademap.","Preparations of liver of any animal (excluding sausages and similar products and finely homogenised preparations put up for retail sale as infant food or for dietetic purposes, in containers of a net weight of <= 250 g)",
74,1,"190190","190190",115,2005,2050,5924217,"Generic HS 2007 mapping, Old SWS series (could also be mapped to 50). Code from Trademap.","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905)",
74,1,"190590","190590",22,2005,2050,5924218,"8-digit code does not exist, but six-digit code could be mapped to 20, 22, 110. Old SWS series suggests bulk of trade is in 22. Code from Trademap.","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products (excluding crispbread, gingerbread and the like, sweet biscuits, waffles, wafers not mentioned, rusks, toasted bread and similar toasted products)",
74,1,"200911","200911",492,2005,2050,5924219,"Generic HS 2007 to FCL unique six-digit match, Trademap","Frozen orange juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
74,1,"200931","200931",513,2005,2050,5924220,"Generic HS 2007 to FCL mapping, Old SWS series (could also be mapped to 496, 498). Code from Trademap.","Single citrus fruit juice, unfermented, Brix value <= 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
74,1,"200939","200939",514,2005,2050,5924221,"Generic HS 2007 to FCL mapping, Old SWS series (could also be mapped to 499). Code from Trademap.","Single citrus fruit juice, unfermented, Brix value > 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
74,1,"200980","200980",622,2005,2050,5924222,"Generic HS 2007 to FCL mapping, Old SWS series (could also be mapped to 466,538,539,583). Code from Trademap.","Single citrus fruit juice, unfermented, Brix value > 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
74,1,"410120","410120",921,2005,2050,5924223,"Generic HS 2007 to FCL mapping (could also be mapped to 920, 922, 928, 929, 930, 958, 959, 1103, 1103, 1105)","Raw hides and skins of bovine (including buffalo) or equine animalswhether or not dehaired or split. Whole hides and skins, of a weight per skin not exceeding 8 kg when simply dried, 10 kg when dry-salted, or 16 kg when fresh, wet-salted or otherwise preserved",
74,2,"200911","200911",492,2005,2050,5924224,"Generic HS 2007 to FCL unique six-digit match, Trademap","Frozen orange juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
83,1,"11031900","11031900",111,2006,2050,5924225,"Generic HS 2007 to FCL mapping (could also be mapped to 38,48,72,80,84,90,95,98,104)","Groats and meal of cereals (excluding wheat and maize)",
83,2,"01051900","01051900",1068,2007,2050,5924226,"Generic HS 2007 to FCL mapping (could also be mapped to 1072)","Live poultry, excl. gallus domestic, turkeys (i.e., can be ducks, geese, guinea fowls)",
86,1,"2009202","2009202",509,2005,2050,5924227,"Country TL description (Trademap & WITS)","Grapefruit juice, unfermented, whetehr or not containing added sugar or other sweetening matter (excluding containing spirit): Preparations for infant use, put up for retail sale",
86,1,"20093040","20093040",513,2008,2050,5924228,"Country TL description (WITS), Old SWS series (could also be mapped to 496, 498)","Preparations of the juice of any other single citrus fruit for infant use, put up for retail sale",
89,1,"02074410","02074410",1075,2015,2050,5924229,"Country TL description (WITS)","Únicamente Higados Los Demás Despojos : De Pato : Los Demás, Frescos O Refrigerados : En Pasta, Deshuesados Mecánicamente",
89,1,"02075410","02075410",1074,2015,2050,5924230,"Country TL description (WITS)","Únicamente Higados Los Demás Despojos : De Ganso : Los Demás, Frescos O Refrigerados : En Pasta, Deshuesados Mecánicamente",
90,1,"010690","010690",1169,2007,2050,5924231,"Generic HS 2007 to FCL (could also be mapped to 1171, 1181)","Other live animals (not mammals, reptiles, or birds)",
90,1,"151419","151419",271,2005,2050,5924232,"Generic HS 2002 to FCL, Old SWS series (could also be mapped to 293)","Low euracic acid rape or colza oil and its fractions: oil other than crude",
90,1,"200931","200931",513,2006,2050,5924233,"Generic HS 2002 to FCL, Old SWS series (could also be mapped to 496, 498)","Juice of any other single citrus fruit, of a brix value not exceeding 20",
90,2,"010619","010619",1169,2007,2050,5924234,"Generic HS 2007 to FCL (could also be mapped to 1126,1140,1150,1157,1171, 1181)","Other live mammals",
101,1,"1502909000","1502909000",869,2015,2050,5924235,"Trademap TL description, Old SWS series (could also be mapped to 871, 949, 979, 1019)","Unedible fats of bovine animals, sheep/ goats, oth than tallow, oth he",
107,1,"080390","080390",486,2015,2050,5924236,"Generic HS2012 to FCL, Old SWS series (could also be mapped to 604)","Fresh or dried bananas (excluding plantains)",
107,2,"080390","080390",486,2015,2050,5924237,"Generic HS2012 to FCL, Old SWS series (could also be mapped to 604)","Fresh or dried bananas (excluding plantains)",
108,1,"020830","020830",1166,2014,2050,5924238,"Country TL description (WITS), Old SWS series (could also be mapped to 1167)","Other meat and edible meat offal, fresh, chilled or frozen: Of primates",
112,1,"020860000","020860000",1127,2014,2050,5924239,"Trademap TL description (could also be mapped to 1128, 1158, or 1159)","Other meat and edible meat offal, fresh, chilled or frozen: Of camels and other camelids (Camelidae)",
112,2,"020621000","020621000",868,2014,2050,5924240,"Trademap TL description, Old SWS series (could also be mapped to 948)","Edible offal of bovine animals, swine, sheep, goats, horses, asses, mules or hinnies, fresh, chilled or frozen: Of bovine animals, frozen: Tongues",
114,1,"190190","190190",115,2010,2050,5924241,"Analysis of Trademap TL data of subcategories (although both 50 malt extracts and 115 food preps are covered under this six-digit code, the overwhelming majority of the imports come in under the subcode for 115 food preps and not 50 malt extracts)","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905)",
114,1,"190590","190590",22,2010,2050,5924242,"Generic HS2012 to FCL, Old SWS series (could also be mapped to 110)","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products (excluding crispbread, gingerbread and the like, sweet biscuits, waffles, wafers not mentioned, rusks, toasted bread and similar toasted products)",
121,1,"010613","010613",1126,2014,2050,5924243,"Trademap description (could also be mapped to 1157 other camelids)","Live camels and other camelids [Camelidae]",
129,1,"080521","080521",495,2015,2050,5924244,"Trademap TL description","Fresh or dried mandarins incl. tangerines and satsumas (excl. clementines)",
129,1,"080522","080522",495,2015,2050,5924245,"Trademap TL description","Fresh or dried clementines incl. monreales",
129,1,"080529","080529",512,2015,2050,5924246,"Trademap TL description","Fresh or dried wilkings and similar citrus hybrids",
129,1,"151591","151591",340,2015,2050,5924247,"Trademap TL description","Autres graisses et huiles vegetales; autres",
129,1,"220291","220291",633,2015,2050,5924248,"Trademap TL description","Non-alcoholic beer <= 0.5% vol alc",
129,1,"220299","220299",633,2015,2050,5924249,"Trademap TL description","Non-alcoholic beverages (excl. water, fruit or vegetable juices, milk and beer)",
129,1,"220422","220422",564,2015,2050,5924250,"Trademap TL description","Wine of fresh grapes, incl. fortified wines, and grape must whose fermentation has been arrested . . .",
129,2,"010649","010649",1169,2014,2050,5924251,"Trademap TL description (could also be mapped to 1171)","Live insects (excluding bees)",
129,2,"080521","080521",495,2015,2050,5924252,"Trademap TL description","Fresh or dried mandarins incl. tangerines and satsumas (excl. clementines)",
129,2,"080529","080529",512,2015,2050,5924253,"Trademap TL description","Fresh or dried wilkings and similar citrus hybrids",
132,1,"0105991010","0105991010",1068,2014,2050,5924254,"Generic HS2012 to FCL (could also be mapped to 1057,1072,1079)","Live domestic ducks, geese, turkeys and guinea fowls, weighing > 185 g",
132,1,"0105991011","0105991011",1068,2015,2050,5924255,"Generic HS2012 to FCL (could also be mapped to 1057,1072,1079)","Live domestic ducks, geese, turkeys and guinea fowls, weighing > 185 g",
132,1,"0105999099","0105999099",1057,2015,2050,5924256,"Generic HS2012 to FCL (could also be mapped to 1068,1072,1079)","Live domestic ducks, geese, turkeys and guinea fowls, weighing > 185 g",
132,1,"1901909016","1901909016",115,2015,2050,5924257,"Trademap TL description","Papadam",
132,1,"2008110010","2008110010",247,2015,2050,5924258,"Trademap TL description","Peanut Butter",
132,1,"2008110099","2008110099",246,2015,2050,5924259,"Trademap TL description","Ground Nuts Prepared Or Preserved",
132,1,"2009391000","2009391000",514,2015,2050,5924260,"Trademap TL description","Single Citrus Fruit Juice(Excl Grape & Orange,Brix Value >20 )Containing Spirit",
132,1,"2309901000","2309901000",841,2014,2050,5924261,"Trademap TL description","Chicken Food",
132,1,"2309909010","2309909010",1259,2014,2050,5924262,"Trademap TL description","Fish Food",
132,1,"2309909011","2309909011",841,2014,2050,5924263,"Trademap TL description","Poltry Food (Other Than Chicken Food)",
132,1,"2309909099","2309909099",1259,2014,2050,5924264,"Trademap TL description","Animal Food (Other Than Cat Food And Chicken Food), Nes",
136,1,"0803009000","0803009000",604,2000,2050,5924265,"Trademap TL description","Bananes, y.c. les plantains, fraîches ou sèches: Bananes seches",
136,1,"1521909000","1521909000",1183,2003,2050,5924266,"Trademap TL description, Old SWS series (could also be mapped to 1295)","Cires d'abeilles ou d'autres insectes et spermaceti, même raffinés ou colorés: Autres cires: autres",
137,1,"01061300","01061300",1126,2015,2050,5924267,"Trademap TL description (could also be mapped to 1157)","Live camels and other camelids [Camelidae]",
137,1,"15029000","15029000",869,2015,2050,5924268,"Trademap TL description (could also be mapped to 871,949,979,1019)","Other fats of bovine animals, sheep or goats, other than those of heading 15.03",
142,1,"01061900","01061900",1171,2004,2050,5924269,"Trademap six-digit description, Old SWS series (Could also be 1169)","Live mammals (excluding primates, whales, dolphins and porpoises, manatees and dugongs, seals, sea lions and walruses, camels and other camelids, rabbits and hares, horses, asses, mules, hinnies, bovines, pigs, sheep and goats)",
142,1,"01069090","01069090",1171,2012,2050,5924270,"Trademap TL description, Old SWS series (Could also be 1169)","Live animals (excl. mammals, reptiles, birds, fish, crustaceans, molluscs and other aquatic invertebrates and cultures of micro-organisms, etc.):Other",
142,1,"02073500","02073500",1069,2006,2050,5924271,"Generic HS 2002 to FCL, Old SWS series (could also be mapped to 1073)","Other meat and edible of of ducks, geese, or guinea fowl: Meat cut in pieces excl fatty livers, fresh or chilled",
142,1,"08026000","08026000",234,2012,2050,5924272,"Generic HS 2007 to FCL","Macadamia nuts",
142,1,"15149900","15149900",271,2011,2050,5924273,"Generic HS 2007 to FCL (Could also be 293)","Rape, colza or mustard oil: Other than low erucic acid rape or colza, not crude",
142,1,"23080000","23080000",652,2012,2050,5924274,"Generic HS 2007 to FCL (Could also be mapped to 120, 566, 628, 629, 630, 650)","Vegetable materials and vegetable waste, vegetable residues and by-products, whether or not in the form of pellets, of a kind used in animal feeding, not elsewhere specified or included.",
142,2,"010600","010600",1171,2004,2050,5924275,"Trademap six-digit description, Old SWS series (Could also be 1169)","Live animals (excluding horses, asses, mules, hinnies, bovine animals, swine, sheep, goats, poultry, fish, crustaceans, molluscs and other aquatic invertebrates, and microorganic cultures etc.)",
142,2,"010619","010619",1171,2005,2050,5924276,"Trademap six-digit description, Old SWS series (Could also be 1169)","Live mammals (excluding primates, whales, dolphins and porpoises, manatees and dugongs, seals, sea lions and walruses, camels and other camelids, rabbits and hares, horses, asses, mules, hinnies, bovines, pigs, sheep and goats)",
142,2,"01061910","01061910",1171,2006,2050,5924277,"Trademap six-digit description, Old SWS series (Could also be 1169)","Live mammals (excluding primates, whales, dolphins and porpoises, manatees and dugongs, seals, sea lions and walruses, camels and other camelids, rabbits and hares, horses, asses, mules, hinnies, bovines, pigs, sheep and goats)",
142,2,"200930","200930",514,2008,2050,5924278,"Trademap six-digit description, Old SWS series (Could also be mapped to 496, 498, 499, 513)","Juice of citrus fruit, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
146,1,"070999100","070999100",463,2015,2050,5924279,"Trademap TL description","Fresh or chilled salad vegetables (excl. lettuce and chicory)",
146,1,"070999600","070999600",446,2015,2050,5924280,"Country TL description (WITS)","Fresh or chilled sweetcorn",
146,1,"080390100","080390100",486,2015,2050,5924281,"Country TL description (WITS)","Bananas, fresh (excl. plantains)",
146,1,"080390900","080390900",604,2015,2050,5924282,"Country TL description (WITS), FCL descriptions","Bananas, dried (excl. plantains)",
146,1,"081090200","081090200",603,2015,2050,5924283,"Country TL description (WITS), FCL descriptions","Fresh tamarinds, cashew apples, lychees, jackfruit, sapodillo plums, passion fruit, carambola and pitahaya",
146,1,"081090750","081090750",619,2015,2050,5924284,"Country TL description (WITS)","Fresh fruit, edible (excl. nuts, bananas, dates, figs, pineapples, avocados, guavas, mangoes, mangosteens, papaws papayas, tamarinds, cashew apples, jackfruit, lychees, sapodillo plums, passion fruit, carambola, pitahaya, citrus fruit, grapes, melons, apples, pears, quinces, apricots, cherries, peaches, plums, sloes, strawberries, raspberries, blackberries, mulberries, loganberries, black, white or red currants, gooseberries, cranberries, fruits of the genus Vaccinium, kiwifruit, durians and persimmons)",
146,1,"110290700","110290700",72,2015,2050,5924285,"Country TL description (WITS)","Rye flour",
146,1,"110319200","110319200",48,2015,2050,5924286,"Country TL description (WITS), Jellyfish mapping of EU countries and ostensible trading partners","Groats and meal of rye or barley",
146,1,"121299950","121299950",460,2015,2050,5924287,"Country TL description (WITS)","Fruit stones and kernels and other vegetable products, of a kind used primarily for human consumption, n.e.s.",
146,1,"150290900","150290900",869,2015,2050,5924288,"Country TL description (WITS), assigned based on largest utilization in Old SWS (could also be mapped to 871, 949, 979, 1019)","Fat of bovine animals, sheep or goats, (excl. for technical/industrial uses, and tallow, oleostearin and oleo-oil)",
146,1,"170114900","170114900",162,2015,2050,5924289,"Country TL description (WITS), Old SWS","Raw cane sugar, in solid form, not containing added flavouring or colouring matter (excl. for refining, and cane sugar of 1701.13)",
146,1,"200190970","200190970",471,2015,2050,5924290,"Country TL description (WITS)","Vegetables, fruit, nuts and other edible parts of plants, prepared or preserved by vinegar or acetic acid (excl. cucumbers and gherkins, mango chutney, fruit of the genus Capsicum other than sweet peppers or pimentos, sweetcorn, yams, sweet potatoes and similar edible parts of plants, containing >= 5% by weight of starch; mushrooms, palm hearts, olives, sweet peppers, guavas, mangoes, mangosteens, papaws papayas, tamarinds, cashew apples, lychees, jackfruit, sapodillo plums, passion fruit, carambola, pitahaya, coconuts, cashew nuts, brazil nuts, areca betel nuts, colanuts and macadamia nuts)",
146,2,"070999600","070999600",446,2015,2050,5924291,"Country TL description (WITS)","Fresh or chilled sweetcorn",
146,2,"081090750","081090750",619,2015,2050,5924292,"Country TL description (WITS)","Fresh fruit, edible (excl. nuts, bananas, dates, figs, pineapples, avocados, guavas, mangoes, mangosteens, papaws papayas, tamarinds, cashew apples, jackfruit, lychees, sapodillo plums, passion fruit, carambola, pitahaya, citrus fruit, grapes, melons, apples, pears, quinces, apricots, cherries, peaches, plums, sloes, strawberries, raspberries, blackberries, mulberries, loganberries, black, white or red currants, gooseberries, cranberries, fruits of the genus Vaccinium, kiwifruit, durians and persimmons)",
146,2,"110319200","110319200",48,2015,2050,5924293,"Country TL description (WITS), Jellyfish mapping of EU countries and ostensible trading partners","Groats and meal of rye or barley",
146,2,"121299950","121299950",460,2015,2050,5924294,"Country TL description (WITS)","Fruit stones and kernels and other vegetable products, of a kind used primarily for human consumption, n.e.s.",
146,2,"200190970","200190970",471,2015,2050,5924295,"Country TL description (WITS)","Vegetables, fruit, nuts and other edible parts of plants, prepared or preserved by vinegar or acetic acid (excl. cucumbers and gherkins, mango chutney, fruit of the genus Capsicum other than sweet peppers or pimentos, sweetcorn, yams, sweet potatoes and similar edible parts of plants, containing >= 5% by weight of starch; mushrooms, palm hearts, olives, sweet peppers, guavas, mangoes, mangosteens, papaws papayas, tamarinds, cashew apples, lychees, jackfruit, sapodillo plums, passion fruit, carambola, pitahaya, coconuts, cashew nuts, brazil nuts, areca betel nuts, colanuts and macadamia nuts)",
153,1,"01064900","01064900",1169,2014,2050,5924296,"Trademap TL description","Live insects (excluding bees): autres animaux vivants",
153,1,"02074411","02074411",1069,2014,2050,5924297,"Trademap TL description","Autres,frais ou refrigeres , de canard : cuisse",
153,1,"02074416","02074416",1069,2014,2050,5924298,"Trademap TL description","Autres , frais ou refrigeres de canard : magret",
153,1,"02074417","02074417",1075,2014,2050,5924299,"Trademap TL description","Autres , frais ou refrigeres de canard : foies",
153,1,"02074511","02074511",1069,2014,2050,5924300,"Trademap TL description","Autres , congeles de canard : cuisse",
153,1,"02074513","02074513",1069,2014,2050,5924301,"Trademap TL description","Autres, congeles de canard :pilon",
153,1,"02074516","02074516",1069,2014,2050,5924302,"Trademap TL description","Autres , congeles de canard : magret",
153,1,"02074517","02074517",1075,2014,2050,5924303,"Trademap TL description","Autres , congeles de canard: foies gras",
153,1,"02074518","02074518",1075,2014,2050,5924304,"Trademap TL description","Autres congeles de canard : foies : autres",
153,1,"02074519","02074519",1075,2014,2050,5924305,"Trademap TL description","Autres , congeles de canard : autres",
153,1,"02091000","02091000",1037,2014,2050,5924306,"Generic HS2012 to FCL, Old SWS series (could also be mapped to 1040)","Lard de porc sans parties maigres, frais, refrigeres, congeles, sales ou en saumure, sec",
153,1,"04014010","04014010",882,2014,2050,5924307,"Trademap TL description","Lait non conc.ni addit. de sucre ou aut.edulco.d'1teneur en pds de mat. grasses excedan",
153,1,"04014020","04014020",885,2014,2050,5924308,"Trademap TL description","Creme de lait non conc.ni addit. de sucre ou aut.edulco.d'1 teneur en pds de mat. gra.e",
153,1,"04015010","04015010",882,2014,2050,5924309,"Trademap TL description","Lait non conc.ni addit. de sucre ou aut.edulco.d'1teneur en pds de mat.gra.exc.10%",
153,1,"04015020","04015020",885,2014,2050,5924310,"Trademap TL description","Creme de lait non conc.ni addit.de sucre ou aut.edulco.d'1 ten.en pds de mat. gra.exc.1",
153,1,"04079000","04079000",1091,2014,2050,5924311,"Trademap TL description","Autres oeufs d'oiseaux,en coquilles, conserves ou cuits",
153,1,"07099913","07099913",394,2014,2050,5924312,"Trademap TL description","Courgette, frais ou refrigeres",
153,1,"07099917","07099917",463,2014,2050,5924313,"Trademap TL description","Alfa-alfa, frais ou refrigeres",
153,1,"07099918","07099918",463,2014,2050,5924314,"Trademap TL description","Persil, frais ou refrigeres",
153,1,"07099919","07099919",463,2015,2050,5924315,"Trademap TL description","Coriandre ou persil chinois, frais ou refrigeres",
153,1,"07099920","07099920",446,2014,2050,5924316,"Trademap TL description","Mais doux, frais ou refrigeres",
153,1,"07099990","07099990",463,2014,2050,5924317,"Trademap TL description","Autres, frais ou refrigeres",
153,1,"08039000","08039000",486,2014,2050,5924318,"Generic HS2012 to FCL, Old SWS series (could also be mapped to 604)","Fresh or dried bananas (excluding plantains): other",
153,1,"15029000","15029000",869,2014,2050,5924319,"Generic HS 2012 to FCL, Old SWS series (could also be mapped to 871, 949, 979, 1019)","Fat of bovine animals, sheep or goats, other than those of heading 1503: fats other than tallow",
153,1,"17011300","17011300",162,2014,2050,5924320,"Trademap TL description, Old SWS series (could also be mapped to 163)","Sucre de canne mentionne dans la note 2 de sous positions du present chapitre",
153,1,"17011400","17011400",162,2014,2050,5924321,"Trademap TL description, Old SWS series (could also be mapped to 163)","Autres sucres de canne",
153,1,"20098910","20098910",622,2014,2050,5924322,"Trademap TL description","Jus de noni (morinda citrifolia)",
153,1,"20098990","20098990",622,2014,2050,5924323,"Trademap TL description","Autres jus",
153,2,"02074416","02074416",1069,2014,2050,5924324,"Trademap TL description","Autres , frais ou refrigeres de canard : magret",
153,2,"02074417","02074417",1075,2014,2050,5924325,"Trademap TL description","Autres , frais ou refrigeres de canard : foies",
153,2,"02074419","02074419",1069,2014,2050,5924326,"Trademap TL description","Autres , frais ou refrigeres de canard : autres",
153,2,"04079000","04079000",1091,2014,2050,5924327,"Trademap TL description","Autres oeufs d'oiseaux,en coquilles, conserves ou cuits",
153,2,"07099913","07099913",394,2014,2050,5924328,"Trademap TL description","Courgette, frais ou refrigeres",
153,2,"07099914","07099914",394,2014,2050,5924329,"Trademap TL description","Squash, frais ou refrigeres",
153,2,"20098990","20098990",622,2014,2050,5924330,"Trademap TL description","Autres jus",
154,1,"01060000501","01060000501",1171,2000,2050,5924331,"Country TL description","Other live animals",
154,1,"01060000601","01060000601",1171,2000,2050,5924332,"Country TL description","Other live animals",
154,1,"01060000901","01060000901",1171,2000,2050,5924333,"Country TL description","Other live animals",
154,1,"01060010001","01060010001",1140,2001,2050,5924334,"Country TL description","Domestic rabbits",
154,1,"02071400001","02071400001",1058,2000,2050,5924335,"Trademap TL description, Old SWS series (could also be mapped to 1059)","frozen cuts and edible offals of fowls of the species gallus domesticus",
154,1,"0207263000","0207263000",1080,2015,2050,5924336,"Country TL description (WITS)","fresh or chilled whole wings, with or without tips, of turkeys of the species domesticus",
154,1,"02072700001","02072700001",1080,2000,2050,5924337,"Trademap TL description, Old SWS series (could also be mapped to 1081)","Frozen cuts and edible offal of turkeys of the species domesticus",
154,1,"0404103600","0404103600",900,2015,2050,5924338,"Country TL description (WITS), Old SWS series (could also be mapped to 890)","whey and modified whey, in powder, granules or other solid formes",
154,1,"0810405000","0810405000",554,2015,2050,5924339,"Trademap TL description","Fresh cranberries, bilberries and other fruits of the genus Vaccinium",
154,1,"1104293000","1104293000",113,2015,2050,5924340,"Trademap TL description","Pearled cereal grains (excl. barley, oats, maize or rice)",
154,1,"1104298500","1104298500",113,2015,2050,5924341,"Trademap TL description","Grains of cereals, hulled, pearled, sliced, kibbled or otherwise worked (excl. oats and maize, . . .",
154,1,"1701139000","1701139000",162,2014,2050,5924342,"Trademap TL description, Old SWS series (could also be mapped to 163)","Raw cane sugar, in solid form, not containing added flavouring or colouring matter, obtained . . .",
154,1,"20097000001","20097000001",519,2000,2050,5924343,"Country TL description, subsequent data series (majority of apple juice imports in closest subsequent years are concentrated AJ) (could also be mapped to 518)","Apple juice",
154,1,"20097011001","20097011001",519,2001,2050,5924344,"Country TL description, subsequent data series (majority of apple juice imports in closest subsequent years are concentrated AJ) (could also be mapped to 518)","Apple juice: Of a value not exceeding 22 EURO per 100 kg net weight",
154,1,"20097019001","20097019001",519,2001,2050,5924345,"Country TL description, subsequent data series (majority of apple juice imports in closest subsequent years are concentrated AJ) (could also be mapped to 518)","Apple juice: Other (e.g., of a value exceeding 22 Euro per 100 kg net weight)",
154,1,"20097030001","20097030001",519,2001,2050,5924346,"Trademap analysis (recent trade data for both concentrated and unconcentrated apple juice indicate that imports of this type are typically of concentrated AJ)","Apple juice: Of a value exceeding 18 EURO per 100 kg net weight, containing added sugar",
154,1,"20097093001","20097093001",519,2001,2050,5924347,"Trademap analysis (recent trade data for both concentrated and unconcentrated apple juice indicate that imports of this type are typically of concentrated AJ)","Apple juice: With an added sugar content not exceeding 30 % by weight",
154,1,"20097099001","20097099001",519,2001,2050,5924348,"Trademap analysis (recent trade data for both concentrated and unconcentrated apple juice indicate that imports of this type are typically of concentrated AJ)","Apple juice: not containing added sugar",
154,1,"23024000001","23024000001",112,2000,2050,5924349,"Trademap TL description","Bran, sharps and other residues of cereals, whether or not in the form of pellets, derived . . .",
154,1,"23099000111","23099000111",843,2000,2050,5924350,"Trademap TL description, Old SWS series (could also be mapped to various and sundry other animal feeds)","Preparations of a kind used in animal feeding",
154,1,"23099000191","23099000191",843,2000,2050,5924351,"Trademap TL description, Old SWS series (could also be mapped to various and sundry other animal feeds)","Preparations of a kind used in animal feeding",
154,1,"23099000201","23099000201",843,2000,2050,5924352,"Trademap TL description, Old SWS series (could also be mapped to various and sundry other animal feeds)","Preparations of a kind used in animal feeding",
154,1,"23099000301","23099000301",843,2000,2050,5924353,"Trademap TL description, Old SWS series (could also be mapped to various and sundry other animal feeds)","Preparations of a kind used in animal feeding",
154,1,"23099000901","23099000901",843,2000,2050,5924354,"Trademap TL description, Old SWS series (could also be mapped to various and sundry other animal feeds)","Preparations of a kind used in animal feeding",
154,2,"0106000040","0106000040",1171,2000,2050,5924355,"Country TL description","Other live animals",
154,2,"0106000050","0106000050",1171,2000,2050,5924356,"Country TL description","Other live animals",
154,2,"0207140000","0207140000",1058,2000,2050,5924357,"Trademap TL description (could also be mapped to 1059)","Frozen cuts and edible offal of fowls of the species Gallus domesticus",
154,2,"0809202900","0809202900",531,2000,2050,5924358,"Country TL description","Cherries",
154,2,"0809203900","0809203900",531,2000,2050,5924359,"Country TL description","Cherries",
154,2,"0809204100","0809204100",531,2000,2050,5924360,"Country TL description","Cherries",
154,2,"0809204900","0809204900",531,2000,2050,5924361,"Country TL description","Cherries",
154,2,"1701139000","1701139000",162,2014,2050,5924362,"Trademap TL description, Old SWS series (could also be mapped to 163)","Raw cane sugar, in solid form, not containing added flavouring or colouring matter, obtained . . .",
154,2,"2009700000","2009700000",519,2000,2050,5924363,"Country TL description, subsequent data series (majority of apple juice imports in closest subsequent years are concentrated AJ) (could also be mapped to 518)","Apple juice",
154,2,"2009701900","2009701900",519,2001,2050,5924364,"Country TL description, subsequent data series (majority of apple juice imports in closest subsequent years are concentrated AJ) (could also be mapped to 518)","Apple juice",
154,2,"2009709100","2009709100",519,2001,2050,5924365,"Country TL description, subsequent data series (majority of apple juice imports in closest subsequent years are concentrated AJ) (could also be mapped to 518)","Apple juice",
154,2,"2009709900","2009709900",519,2001,2050,5924366,"Country TL description, subsequent data series (majority of apple juice imports in closest subsequent years are concentrated AJ) (could also be mapped to 518)","Apple juice",
154,2,"2309900011","2309900011",843,2000,2050,5924367,"Trademap TL description, Old SWS series (could also be mapped to various and sundry other animal feeds)","Preparations of a kind used in animal feeding",
154,2,"2309900019","2309900019",843,2000,2050,5924368,"Trademap TL description, Old SWS series (could also be mapped to various and sundry other animal feeds)","Preparations of a kind used in animal feeding",
154,2,"2309900020","2309900020",843,2001,2050,5924369,"Trademap TL description, Old SWS series (could also be mapped to various and sundry other animal feeds)","Preparations of a kind used in animal feeding",
154,2,"2309900030","2309900030",843,2000,2050,5924370,"Trademap TL description, Old SWS series (could also be mapped to various and sundry other animal feeds)","Preparations of a kind used in animal feeding, containing no starch, glucose, glucose syrup, . . .",
154,2,"2309900090","2309900090",843,2000,2050,5924371,"Trademap TL description, Old SWS series (could also be mapped to various and sundry other animal feeds)","Preparations of a kind used in animal feeding",
166,1,"14030099","14030099",1293,2003,2050,5924372,"Trademap TL description","Sorgo, piasava, grama, ixtle tampico y demás materias vegetales de las especies utilizadas principalmente en la fabricación de escobas, cepillos o brochas, incl. en torcidas o en haces: los demas",
166,2,"22033020","22033020",51,2011,2050,5924373,"Trademap TL description","cerveza de malta (descripción detallada no disponible)",
166,2,"22036030","22036030",51,2011,2050,5924374,"Trademap TL description","cerveza de malta (descripción detallada no disponible)",
168,1,"01011000","01011000",1096,2011,2050,5924375,"Country TL description (WITS) (could also be mapped to 1107)","Pure-bred breeding horses and asses",
168,1,"010599","010599",1057,2004,2050,5924376,"Country TL description (WITS), Old SWS series (could also be mapped to 1072, 1079)","Live domestic ducks, geese, turkeys and guinea fowls, weighing > 185 g",
168,1,"01059900","01059900",1057,2011,2050,5924377,"Country TL description (WITS), Old SWS series (could also be mapped to 1072, 1079)","Live domestic ducks, geese, turkeys and guinea fowls, weighing > 185 g",
168,1,"01061100","01061100",1169,2012,2050,5924378,"Country TL description (WITS)","Live primates",
168,1,"01061910","01061910",1169,2011,2050,5924379,"Country TL description (WITS)","Live mammals (excl. primates, whales, dolphins and purpoises mammals of the order Cetacea, manatees and dugongs mammals of the order Sirenia and horses, asses, mules, hinnies, bovines, pigs, sheep and goats) Cats and Dogs",
168,1,"01063900","01063900",1083,2012,2050,5924380,"Country TL description (WITS)","Live birds (excluding birds of prey, psittaciformes, parrots, parrakeets, macaws, cockatoos, . . .",
168,1,"01064900","01064900",1169,2012,2050,5924381,"Trademap TL description","Live insects (excl. bees) (detailed label not available)",
168,1,"010690","010690",1169,2004,2050,5924382,"Trademap six-digit description","Live animals (excl. mammals, reptiles, birds, insects, fish, crustaceans, molluscs and other . . .",
168,1,"01069020","01069020",1169,2012,2050,5924383,"Country TL description (WITS)","Live animals (excl. mammals, reptiles, birds, fish, crustaceans, molluscs and other aquatic invertebrates and cultures of micro-organisms, etc.) Bees (whether or not in travelling boxes or cages or hives) and other insects",
168,1,"01069090","01069090",1169,2011,2050,5924384,"Country TL description (WITS)","Live animals (excl. mammals, reptiles, birds, fish, crustaceans, molluscs and other aquatic invertebrates and cultures of micro-organisms, etc.) Other",
168,1,"02073200","02073200",1069,2011,2050,5924385,"Country TL description, Old SWS series (could also be mapped to 1073)","Meat of ducks/geese/guinea fowls, not cut in pieces, fresh/chilled",
168,1,"02073500","02073500",1069,2012,2050,5924386,"Country TL description (WITS), Old SWS series (could also be mapped to 1075, 1073, 1074)","Fresh or chilled cuts and edible offal of ducks, geese or guinea fowls of the species domesticus (excluding fatty livers)",
168,1,"020820","020820",1166,2004,2050,5924387,"Trademap TL description","Fresh, chilled or frozen frogs' legs",
168,1,"020900","020900",1037,2004,2050,5924388,"Trademap TL description, Old SWS series (could also be mapped to 1040)","Pig fat, free of lean meat, and poultry fat, not rendered or otherwise extracted, fresh, chilled, frozen, salted, in brine, dried or smoked",
168,1,"02090000","02090000",1037,2011,2050,5924389,"Trademap TL description, Old SWS series (could also be mapped to 1040)","Pig fat, free of lean meat, and poultry fat, not rendered or otherwise extracted, fresh, chilled, frozen, salted, in brine, dried or smoked",
168,1,"02091000","02091000",1037,2012,2050,5924390,"Trademap TL description, Old SWS series (could also be mapped to 1040)","Pig fat, free of lean meat, not rendered or otherwise extracted, fresh, chilled, frozen, salted, in brine, dried or smoked (detailed label not available)",
168,1,"04014010","04014010",885,2012,2050,5924391,"Country TL description (WITS), Historic Jellyfish series (previously, anything above 6% fat was mapped to 885)","Milk and cream of a fat content by weight of > 6% but <= 10%, not concentrated nor containing added sugar or other sweetening matter: UHT processed in retail sale packages",
168,1,"04014020","04014020",885,2012,2050,5924392,"Country TL description (WITS)","Milk and cream of a fat content by weight of > 6% but <= 10%, not concentrated nor containing added sugar or other sweetening matter: Cream",
168,1,"04014090","04014090",885,2012,2050,5924393,"Country TL description (WITS), Historic Jellyfish series (previously, anything above 6% fat was mapped to 885)","Milk and cream of a fat content by weight of > 6% but <= 10%, not concentrated nor containing added sugar or other sweetening matter: Other",
168,1,"04015000","04015000",885,2012,2050,5924394,"Trademap TL description, Historic Jellyfish series (previously, anything above 6% fat was mapped to 885)","Milk and cream of a fat content by weight of > 10%, not concentrated nor containing added sugar or other sweetening matter",
168,1,"04079000","04079000",1062,2012,2050,5924395,"Trademap TL description, Old SWS series (could also be mapped to 1091)","Birds' eggs, in shell, preserved or cooked (detailed label not available)",
168,1,"06041000","06041000",1293,2011,2050,5924396,"Country TL description (WITS)","Mosses and lichens for bouquets or for ornamental purposes, fresh, dried, dyed, bleached, impregnated or otherwise prepared",
168,1,"07099900","07099900",463,2012,2050,5924397,"Country TL description (WITS)","Fresh or chilled vegetables n.e.s.: Other",
168,1,"07099910","07099910",463,2012,2050,5924398,"Country TL description (WITS)","Bamboo shoots and soya bean sprouts",
168,1,"08026000","08026000",234,2011,2050,5924399,"Country TL description (WITS)","MACADAMIA NUTS",
168,1,"08039000","08039000",486,2012,2050,5924400,"Trademap TL description","Fresh or dried bananas (excluding plantains)",
168,1,"100200","100200",71,2004,2050,5924401,"Country TL description (WITS)","Rye",
168,1,"10020000","10020000",71,2011,2050,5924402,"Country TL description (WITS)","Rye",
168,1,"110320","110320",111,2004,2050,5924403,"Country TL description (WITS)","Cereal pellets",
168,1,"140300","140300",1293,2004,2050,5924404,"Country TL description (WITS)","Vegetable materials of a kind used primarily in brooms or in brushes (for example, broom-corn, piassava, cough-grass and istle), whether or not in hanks or bundles.",
168,1,"151411","151411",271,2004,2050,5924405,"Trademap six-digit description, Old SWS series (could also be mapped to 293)","Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2%, crude",
168,1,"15141110","15141110",271,2012,2050,5924406,"Country TL description (WITS), Old SWS series (could also be mapped to 293)","Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2%, Crude oil, packed in containers 150 kg or less or 166 litres or less",
168,1,"15141190","15141190",271,2011,2050,5924407,"Country TL description (WITS), Old SWS series (could also be mapped to 293)","Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2%, Crude oil, packed in containers other than those of 150 kg or less or 166 litres or less",
168,1,"151419","151419",271,2004,2050,5924408,"Trademap six-digit description, Old SWS series (could also be mapped to 293)","Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2% and . . .",
168,1,"15141910","15141910",271,2011,2050,5924409,"Country TL description (WITS), Old SWS series (could also be mapped to 293)","Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2%: Other, packed in containers 150 kg or less or 166 litres or less",
168,1,"15141990","15141990",271,2011,2050,5924410,"Country TL description (WITS), Old SWS series (could also be mapped to 293)","Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2%: Other, packed in containers other than those of 150 kg or less or 166 litres or less",
168,1,"15149110","15149110",271,2011,2050,5924411,"Trademap TL description, Old SWS series (could also be mapped to 293)","High erucic acid rape or colza oil fixed oil which has an erucic acid content of >= 2% and mustard oil, crude Crude oil, packed in containers 150 kg or less or 166 litres or less",
168,1,"15149190","15149190",271,2011,2050,5924412,"Country TL description (WITS), Old SWS series (could also be mapped to 293)","High erucic acid rape or colza oil fixed oil which has an erucic acid content of >= 2% and mustard oil, crude Crude oil, packed in containers other than those of 150 kg or less or 166 litres or less",
168,1,"17011200","17011200",162,2011,2050,5924413,"Trademap TL description, Old SWS series (could also be mapped to 163)","Raw beet sugar",
168,1,"17011400","17011400",162,2012,2050,5924414,"Trademap TL description, Old SWS series (could also be mapped to 163)","Raw cane sugar, in solid form, not containing added flavouring or colouring matter (excluding cane sugar of 1701 13)",
168,1,"200931","200931",513,2004,2050,5924415,"Trademap TL description, Old SWS series (could also be mapped to 496, 498)","Single citrus fruit juice, unfermented, Brix value <= 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
168,1,"20093100","20093100",513,2011,2050,5924416,"Trademap TL description, Old SWS series (could also be mapped to 496, 498)","Single citrus fruit juice, unfermented, Brix value <= 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
168,1,"200939","200939",514,2004,2050,5924417,"Trademap TL description, Old SWS series (could also be mapped to 499)","Single citrus fruit juice, unfermented, Brix value > 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
168,1,"20093900","20093900",514,2011,2050,5924418,"Trademap TL description, Old SWS series (could also be mapped to 499)","Single citrus fruit juice, unfermented, Brix value > 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
168,1,"20098900","20098900",622,2012,2050,5924419,"Trademap TL description, Old SWS series (could also be mapped to 583, 539, 538, 466)","Juice of fruit or vegetables, unfermented, whether or not containing added sugar or other sweetening matter (excl. containing spirit, mixtures, and juice of citrus fruit, pineapples, tomatoes, grapes, incl. grape must, apples and cranberries) (detailed label not available)",
168,1,"23032000","23032000",170,2012,2050,5924420,"Trademap TL description","Beet-pulp, bagasse and other waste of sugar manufacture",
168,1,"230800","230800",652,2004,2050,5924421,"Trademap TL description, Old SWS series (could also be mapped to 120, 566, 628, 629, 630, 650)","Acorns, horse-chestnuts, marc and other vegetable materials and vegetable waste, vegetable residues and by-products of a kind used in animal feeding, whether or not in the form of pellets, n.e.s.",
168,1,"23080000","23080000",652,2011,2050,5924422,"Trademap TL description, Old SWS series (could also be mapped to 120, 566, 628, 629, 630, 650)","Acorns, horse-chestnuts, marc and other vegetable materials and vegetable waste, vegetable residues and by-products of a kind used in animal feeding, whether or not in the form of pellets, n.e.s.",
168,1,"330114","330114",753,2004,2050,5924423,"Country TL description (WITS)","Essential oils: Of lime",
168,1,"41012000","41012000",920,2011,2050,5924424,"Trademap TL description, Old SWS series (could also be mapped to 928,921,922,929,930,958,959,1103,1104,1105)","Whole raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired or split, of a weight per skin <= 8 kg when simply dried, <= 10 kg when dry-salted, or <= 16 kg when fresh, wet-salted or otherwise preserved (excl. tanned and parchment-dressed)",
168,1,"41019000","41019000",920,2012,2050,5924425,"Trademap TL description, Old SWS series (could also be mapped to 928,921,922,929,930,958,959,1103,1104,1105)","Butts, bends, bellies and split raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired, fresh, or salted, dried, limed, pickled or otherwise preserved, and whole raw hides and skins of a weight per skin > 8 kg but < 16 kg when simply dried and > 10 kg but < 16 kg when dry-salted (excl. tanned, parchment-dressed or further prepared)",
168,1,"53050000","53050000",809,2012,2050,5924426,"Trademap TL description (could also be mapped to 821)","Coconut, abaca Manila hemp or Musa textilis Nee, ramie, agave and other vegetable textile fibres, n.e.s., raw or processed, but not spun; tow, noils and waste of such fibres, incl. yarn waste and garnetted stock",
168,2,"020850","020850",1166,2004,2050,5924427,"Trademap six-digit description","Fresh, chilled or frozen meat and edible offal of reptiles e.g. snakes, turtles, crocodiles",
168,2,"02085010","02085010",1166,2011,2050,5924428,"Country TL description (WITS)","Fresh, chilled or frozen meat and edible offal of reptiles e.g. snakes, turtles, crocodiles: Of crocodiles",
168,2,"11032000","11032000",111,2011,2050,5924429,"Country TL description (WITS)","Cereal pellets",
168,2,"15149110","15149110",271,2011,2050,5924430,"Trademap TL description, Old SWS series (could also be mapped to 293)","High erucic acid rape or colza oil fixed oil which has an erucic acid content of >= 2% and mustard oil, crude Crude oil, packed in containers 150 kg or less or 166 litres or less",
168,2,"20098900","20098900",622,2012,2050,5924431,"Trademap TL description, Old SWS series (could also be mapped to 583, 539, 538, 466)","Juice of fruit or vegetables, unfermented, whether or not containing added sugar or other sweetening matter (excl. containing spirit, mixtures, and juice of citrus fruit, pineapples, tomatoes, grapes, incl. grape must, apples and cranberries) (detailed label not available)",
168,2,"410120","410120",920,2004,2050,5924432,"Trademap TL description, Old SWS series (could also be mapped to 928,921,922,929,930,958,959,1103,1104,1105)","Whole raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired or split, of a weight per skin <= 8 kg when simply dried, <= 10 kg when dry-salted, or <= 16 kg when fresh, wet-salted or otherwise preserved (excl. tanned and parchment-dressed)",
168,2,"41012000","41012000",920,2011,2050,5924433,"Trademap TL description, Old SWS series (could also be mapped to 928,921,922,929,930,958,959,1103,1104,1105)","Whole raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired or split, of a weight per skin <= 8 kg when simply dried, <= 10 kg when dry-salted, or <= 16 kg when fresh, wet-salted or otherwise preserved (excl. tanned and parchment-dressed)",
179,1,"02074400","02074400",1069,2014,2050,5924434,"Country TL description (WITS) (could also be mapped to 1075)","Meat and edible offal, of the poultry of heading 01.05, fresh, chilled or frozen: Of ducks: Other, fresh or chilled (i.e. cut in pieces or excl fatty livers)",
179,1,"02074500","02074500",1069,2014,2050,5924435,"Country TL description (WITS) (could also be mapped to 1075)","Meat and edible offal, of the poultry of heading 01.05, fresh, chilled or frozen: Of ducks: Other, frozen (i.e. cut in pieces or excl fatty livers)",
179,1,"04014030","04014030",882,2014,2050,5924436,"Country TL description (WITS), Jellyfish (previously, milk of a fat content about 10% was mapped to milk for this country)","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight, exceeding 6 % but not exceeding 10 %: Long life milk, in containers exceeding 1 litre",
179,1,"04014090","04014090",882,2014,2050,5924437,"Country TL description (WITS), Jellyfish (previously, milk of a fat content about 10% was mapped to milk for this country)","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight, exceeding 6 % but not exceeding 10 %: Other",
179,1,"07145000","07145000",135,2015,2050,5924438,"Trademap TL description","Yautia Xanthosoma spp., fresh, chilled, frozen or dried, whether or not sliced or in the form of pellets",
179,1,"08011200","08011200",249,2014,2050,5924439,"Trademap TL description","Coconuts, in the inner shell (endocarp)",
179,1,"08027000","08027000",224,2014,2050,5924440,"Trademap TL description","- kola nuts (cola spp.)",
179,1,"08028000","08028000",226,2014,2050,5924441,"Trademap TL description","- areca nuts",
179,1,"12023000","12023000",242,2014,2050,5924442,"Trademap TL description","Groundnut seed, for sowing",
179,1,"15021000","15021000",1225,2015,2050,5924443,"Trademap TL description","Tallow of bovine animals, sheep or goats (excluding oil and oleostearin)",
179,1,"15029000","15029000",869,2014,2050,5924444,"Trademap TL description, Old SWS series (could also be mapped to 871, 949, 979, 1019)","Fats of bovine animals, sheep or goats (excluding tallow, oleostearin and oleo-oil)",
179,1,"16010090","16010090",874,2014,2050,5924445,"Trademap TL description, Old SWS series (could also be mapped to 1041)","Sausages and similar products, of meat, offal or blood; food preparations based on these products: Other",
179,1,"17011310","17011310",162,2014,2050,5924446,"Trademap TL description, Old SWS series (could also be mapped to 163)","Raw cane sugar, in solid form, not containing added flavouring or colouring matter, obtained without centrifugation, with sucrose content 69° to 93°, containing only natural anhedral microcrystals (see subheading note 2.): for industrial refining",
179,1,"50030000","50030000",1187,2014,2050,5924447,"Trademap TL description","Silk waste, incl. cocoons unsuitable for reeling, yarn waste and garnetted stock",
179,2,"08027000","08027000",224,2015,2050,5924448,"Trademap TL description","- kola nuts (cola spp.)",
180,1,"01069000","01069000",1171,2014,2050,5924449,"Country TL description (WITS)","Other live animals: Other",
180,1,"02012000","02012000",867,2014,2050,5924450,"Country TL description (WITS) (could also be mapped to 947)","Meat of bovine animals, fresh or chilled: Other cuts with bone in",
180,1,"02013000","02013000",870,2014,2050,5924451,"Country TL description (WITS) (could also be mapped to 947)","Meat of bovine animals, fresh or chilled: Boneless",
180,1,"02021000","02021000",867,2014,2050,5924452,"Country TL description (WITS) (could also be mapped to 947)","Meat of bovine animals, frozen: Carcasses and half-carcasses",
180,1,"02022000","02022000",867,2014,2050,5924453,"Country TL description (WITS) (could also be mapped to 947)","Meat of bovine animals, frozen: Other cuts with bone in",
180,1,"02023000","02023000",870,2014,2050,5924454,"Country TL description (WITS) (could also be mapped to 947)","Meat of bovine animals, frozen: Boneless",
180,1,"02031900","02031900",1035,2014,2050,5924455,"Country TL description (WITS) (could also be mapped to 1038)","Meat of swine, fresh, chilled or frozen: Fresh or chilled: Other",
180,1,"02032900","02032900",1035,2014,2050,5924456,"Country TL description (WITS) (could also be mapped to 1038)","Meat of swine, fresh, chilled or frozen: Frozen: Other",
180,1,"02062100","02062100",868,2014,2050,5924457,"Country TL description (WITS) (could also be mapped to 948,978,1036,1018,1098,1167)","Edible offal of bovine animals, swine, sheep, goats, horses, asses, mules or hinnies, fresh, chilled or frozen: Of bovine animals, fresh or chilled Tongues",
180,1,"02062200","02062200",868,2014,2050,5924458,"Country TL description (WITS) (could also be mapped to 948,978,1036,1018,1098,1167)","Edible offal of bovine animals, swine, sheep, goats, horses, asses, mules or hinnies, fresh, chilled or frozen: Of bovine animals, frozen Livers",
180,1,"02062900","02062900",868,2014,2050,5924459,"Country TL description (WITS) (could also be mapped to 948,978,1036,1018,1098,1167)","Edible offal of bovine animals, swine, sheep, goats, horses, asses, mules or hinnies, fresh, chilled or frozen: Of bovine animals, frozen Other",
180,1,"02069000","02069000",1167,2014,2050,5924460,"Country TL description (WITS) (could also be mapped to 868,948,978,1036,1018,1098)","Edible offal of bovine animals, swine, sheep, goats, horses, asses, mules or hinnies, fresh, chilled or frozen: Other, frozen",
180,1,"02071300","02071300",1058,2015,2050,5924461,"Country TL description (WITS) (could also be mapped to 1059)","Meat and edible offal, of the poultry of heading 01.05, fresh, chilled or frozen: Of fowls of the species Gallus domesticus Cuts and offal, fresh or chilled",
180,1,"02071400","02071400",1058,2014,2050,5924462,"Country TL description (WITS) (could also be mapped to 1059)","Meat and edible offal, of the poultry of heading 01.05, fresh, chilled or frozen: Of fowls of the species Gallus domesticus Cuts and offal, frozen",
180,1,"02072700","02072700",1080,2014,2050,5924463,"Country TL description (WITS) (could also be mapped to 1081)","Meat and edible offal, of the poultry of heading 01.05, fresh, chilled or frozen: Of turkeys Cuts and offal, frozen",
180,1,"02073300","02073300",1069,2014,2050,5924464,"Country TL description (WITS) (could also be mapped to 1075,1073,1074)","Meat and edible offal, of the poultry of heading 01.05, fresh, chilled or frozen: Of ducks, geese or guinea fowls Not cut in pieces, frozen",
180,1,"02074500","02074500",1069,2014,2050,5924465,"Trademap TL description (could also be mapped to 1075)","Meat and edible offal, of the poultry of heading 01.05, fresh, chilled or frozen: Of ducks: Other, frozen",
180,1,"02085090","02085090",1166,2014,2050,5924466,"Country TL description (WITS) (could also be mapped to 1167)","Other meat and edible offal, fresh, chilled or frozen: Of reptiles (including snakes and turtles): Other",
180,1,"02090000","02090000",1037,2014,2050,5924467,"Country TL description (WITS) (could also be mapped to 1040)","Pig fat, free of lean meat, and poultry fat, not rendered or otherwise extracted, fresh, chilled, frozen, salted, in brine, dried or smoked:",
180,1,"04011000","04011000",888,2014,2050,5924468,"Country TL description (could also be mapped to 954,985,1023)","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight, not exceeding 1%",
180,1,"04012000","04012000",882,2014,2050,5924469,"Country TL description (could also be mapped to 883,908,951,982,1020,1130)","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight, exceeding 1% but not exceeding 6%",
180,1,"04014000","04014000",885,2014,2050,5924470,"Country TL description (could also be mapped to 882,883,951,982,1020,1130)","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight, exceeding 6% but not exceeding 10%",
180,1,"04015000","04015000",885,2014,2050,5924471,"Country TL description (could also be mapped to 882,883,951,982,1020,1130)","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight, exceeding 10%",
180,1,"04029100","04029100",894,2014,2050,5924472,"Country TL description (could also be mapped to 895)","Milk and cream, concentrated or containing added sugar or other sweetening matter: Other: Not containing added sugar or other sweetening matter",
180,1,"04029900","04029900",889,2014,2050,5924473,"Country TL description (could also be mapped to 896)","Milk and cream, concentrated or containing added sugar or other sweetening matter: Other: Other",
180,1,"04031000","04031000",891,2014,2050,5924474,"Country TL description (could also be mapped to 892)","Buttermil k, curdled mil k and cream, yogurt, kephir and other fermented or aci dified mil k and cream, whether or not concentrated or containing added sugar or other sweetening matter or flavoured or containing added fruit, nuts or cocoa: Yogurt",
180,1,"04039000","04039000",893,2014,2050,5924475,"Country TL description (could also be mapped to 899)","Buttermil k, curdled mil k and cream, yogurt, kephir and other fermented or aci dified mil k and cream, whether or not concentrated or containing added sugar or other sweetening matter or flavoured or containing added fruit, nuts or cocoa: Other",
180,1,"04041000","04041000",903,2014,2050,5924476,"Country TL description (could also be mapped to 890, 900)","Whey, whether or not concentrated or contai ning added sugar or other sweetening matter; products consisting of natural mil k constituents, whether or not containing added sugar or other sweetening matter, not elsewhere specified or i ncl uded: Whey and modified whey, whether or not concentrated or containing added sugar or other sweetening matter",
180,1,"04051000","04051000",886,2014,2050,5924477,"Country TL description (could also be mapped to 952,983,1022)","Butter and other fats and oils derived from milk; dairy spreads: Butter",
180,1,"04052000","04052000",886,2014,2050,5924478,"Country TL description (could also be mapped to 952,983,1022)","Butter and other fats and oils derived from milk; dairy spreads: Dairy spreads",
180,1,"04059000","04059000",1022,2014,2050,5924479,"Country TL description (could also be mapped to 887,953)","Butter and other fats and oils derived from milk; dairy spreads: Other",
180,1,"04061000","04061000",901,2014,2050,5924480,"Country TL description (WITS) (could also be mapped to 904,905,955,984,1021)","Cheese and curd: Fresh (unripened or uncured) cheese, including whey cheese, and curd",
180,1,"04070000","04070000",1062,2014,2050,5924481,"Trademap TL description (could also be mapped to 1091)","Birds' eggs, in shell, fresh, preserved or cooked",
180,1,"04079000","04079000",1062,2014,2050,5924482,"Country TL description (WITS) (could also be mapped to 1091)","Birds' eggs, in shell, fresh, preserved or cooked: Other",
180,1,"05090000","05090000",1293,2014,2050,5924483,"Country TL description (WITS)","Natural sponges of animal origin",
180,1,"06031000","06031000",1293,2014,2050,5924484,"Country TL description (WITS)","Cut flowers and flower buds of a kind suitable for bouquets or for ornamental purposes, fresh, dried, dyed, bleached, impregnated or otherwise prepared: Fresh",
180,1,"06049100","06049100",1293,2014,2050,5924485,"Country TL description (WITS)","Foliage, branches and other parts of plants, without flowers or flower buds, and grasses, mosses and lichens, being goods of a kind suitable for bouquets or for ornamental purposes, fresh, dried, dyed, bleached, impregnated or otherwise prepared: Mosses and lichens Fresh",
180,1,"07031000","07031000",403,2014,2050,5924486,"Country TL description (WITS) (could also be mapped to 402)","Onions, shallots, garlic, leeks and other alliaceous vegetables, fresh or chilled: Onions and shallots",
180,1,"07061000","07061000",426,2014,2050,5924487,"Country TL description (WITS) (could also be mapped to 463)","Carrots, turnips, salad beetroot, salsify, celeriac, radishes and si milar edible roots, fresh or chilled: Carrots and turnips",
180,1,"07082000","07082000",414,2014,2050,5924488,"Country TL description (WITS) (could also be mapped to 423)","Legumi nous vegetables, shelled or unshelled, fresh or chilled: Beans (vigna spp., phaseolus spp.)",
180,1,"07091000","07091000",366,2014,2050,5924489,"Country TL description (WITS)","Other vegetables, fresh or chilled: Globe artichokes",
180,1,"07099000","07099000",463,2014,2050,5924490,"Country TL description (WITS)","Other vegetables, fresh or chilled: Other",
180,1,"07099900","07099900",463,2014,2050,5924491,"Country TL description (WITS)","Other vegetables, fresh or chilled: Other: Other",
180,1,"07141000","07141000",125,2014,2050,5924492,"Country TL description (WITS) (could also be mapped to 128)","Manioc, arrowroot, salep, jerusalem artichokes, sweet potatoes and si milar roots and tubers with high starch or inulin content, fresh, chilled, frozen or dried, whether or not sliced or in the form of pellets; sago pith: Manioc (cassava)",
180,1,"07149000","07149000",149,2015,2050,5924493,"Country TL description (WITS) (could also be mapped to 151)","Manioc, arrowroot, salep, jerusalem artichokes, sweet potatoes and si milar roots and tubers with high starch or inulin content, fresh, chilled, frozen or dried, whether or not sliced or in the form of pellets; sago pith: Other",
180,1,"08025000","08025000",223,2014,2050,5924494,"Country TL description (WITS)","Other nuts, fresh or dried, whether or not shelled or peeled: Pistachios",
180,1,"08030000","08030000",486,2014,2050,5924495,"Country TL description (WITS) (could also be mapped to 489, 604)","Bananas, including plantains, fresh or dried:",
180,1,"08039000","08039000",486,2014,2050,5924496,"Country TL description (WITS) (could also be mapped to 604)","Bananas, incl uding pl antains, fresh or dried: Other",
180,1,"08042000","08042000",569,2015,2050,5924497,"Country TL description (WITS)","Dates, figs, pineapples, avocados, guavas, mangoes and mangosteens, fresh or dried: Figs",
180,1,"08045000","08045000",571,2014,2050,5924498,"Country TL description (WITS) (could also be mapped to 603, 604)","Dates, figs, pineapples, avocados, guavas, mangoes and mangosteens, fresh or dried: Guavas, mangoes and mangosteens",
180,1,"08082000","08082000",521,2014,2050,5924499,"Country TL description (WITS) (could also be mapped to 523)","Apples, pears and quinces, fresh: Pears and quinces",
180,1,"08102000","08102000",547,2014,2050,5924500,"Country TL description (WITS) (could also be mapped to 558)","Other fruit, fresh: Raspberries, blackberries, mulberries and loganberries",
180,1,"08103000","08103000",549,2014,2050,5924501,"Country TL description (WITS) (could also be mapped to 550)","Other fruit, fresh: Black, white or red currants and gooseberries",
180,1,"08104000","08104000",552,2014,2050,5924502,"Country TL description (WITS) (could also be mapped to 554,558)","Other fruit, fresh: Cranberries, bilberries and other fruits of the genus Vaccinium",
180,1,"08109000","08109000",619,2014,2050,5924503,"Country TL description (WITS) (could also be mapped to 541,591)","Other fruit, fresh: Other",
180,1,"09019000","09019000",658,2014,2050,5924504,"Country TL description (WITS) (could also be mapped to 650)","Coffee, whether or not roasted or decaffeinated; coffee husks and skins; coffee substitutes containing coffee in any proportion: Other",
180,1,"09042000","09042000",689,2014,2050,5924505,"Country TL description (WITS)","Pepper of the genus Piper; dried or crushed or ground fruits of the genus Capsicum or of the genus Pimenta: Fruits of the genus Capsicum or of the genus Pimenta, dried or crushed or ground",
180,1,"09050000","09050000",692,2014,2050,5924506,"Country TL description (WITS)","Vanilla:",
180,1,"09091000","09091000",711,2014,2050,5924507,"Country TL description (WITS)","Seeds of anise, badian, fennel, coriander, cumin or caraway; juniper berries: Seeds of anise or badian",
180,1,"09101000","09101000",720,2014,2050,5924508,"Country TL description (WITS)","Ginger, saffron, turmeric (curcuma), thyme, bay leaves, curry and other spices: Ginger",
180,1,"09105000","09105000",723,2014,2050,5924509,"Country TL description (WITS)","Ginger, saffron, turmeric (curcuma), thyme, bay leaves, curry and other spices: Curry",
180,1,"10030000","10030000",44,2014,2050,5924510,"Country TL description (WITS)","Barley:",
180,1,"10040000","10040000",75,2014,2050,5924511,"Country TL description (WITS)","Oats:",
180,1,"10063000","10063000",31,2014,2050,5924512,"Country TL description (WITS)","Rice: Semi-milled or wholly milled rice, whether or not polished or glazed",
180,1,"10089000","10089000",108,2014,2050,5924513,"Country TL description (WITS)","Buckwheat, millet and canary seed; other cereals: Other cereals",
180,1,"11029000","11029000",111,2014,2050,5924514,"Country TL description (WITS) (could also be mapped to 48,80,84,90,95,98,104)","Cereal flours other than of wheat or meslin: Other (i.e. not rye, maize, or rice)",
180,1,"11031900","11031900",111,2014,2050,5924515,"Country TL description (WITS) (could also be mapped to 38,48,72,80,84,90,95,98,104)","Cereal groats, meal and pellets: Groats and meal Of other cereals",
180,1,"11032000","11032000",111,2014,2050,5924516,"Country TL description (WITS) (could also be mapped to 38,48,72,80,84,90,95,98,104)","Cereal groats, meal and pellets: Pellets",
180,1,"11041900","11041900",113,2014,2050,5924517,"Country TL description (WITS) (could also be mapped to 45,46)","Cereal grains otherwise worked (for example, hulled, rolled, flaked, pearled, sliced or kibbled), except rice of heading 10.06; germ of cereals, whole, rolled, flaked or ground: Rolled or flaked grains Of other cereals (i.e. not oats)",
180,1,"11042900","11042900",113,2014,2050,5924518,"Country TL description (WITS) (could also be mapped to 45,46)","Cereal grains otherwise worked (for example, hulled, rolled, flaked, pearled, sliced or kibbled), except rice of heading 10.06; germ of cereals, whole, rolled, flaked or ground: Other worked grains (for example, hulled, pearled, sliced or kibbled) Of other cereals",
180,1,"11043000","11043000",57,2015,2050,5924519,"Country TL description (WITS) (could also be mapped to 19)","Cereal grains otherwise worked (for example, hulled, rolled, flaked, pearled, sliced or kibbled), except rice of heading 10.06; germ of cereals, whole, rolled, flaked or ground: Germ of cereals, whole, rolled, flaked or ground",
180,1,"11062000","11062000",150,2014,2050,5924520,"Country TL description (WITS) (could also be mapped to 126)","Flour, meal and powder of the dried leguminous vegetables of heading 07.13, of sago or of roots or tubers of heading 07.14 or of the products of Chapter 8: Of sago or of roots or tubers of heading 07.14",
180,1,"12079900","12079900",339,2014,2050,5924521,"Country TL description (WITS) (could also be mapped to 263,275,277,305,311,312,336)","Other oil seeds and oleaginous fruits, whether or not broken: Other Other",
180,1,"12119000","12119000",1293,2015,2050,5924522,"Trademap six-digit description (could also be mapped to 748, 754, 756)","Plants, parts of plants, incl. seeds and fruits, used primarily in perfumery, in pharmacy or for insecticidal, fungicidal or similar purposes, fresh or dried, whether or not cut, crushed or powdered (excluding ginseng roots, coca leaf and poppy straw)",
180,1,"12119090","12119090",1293,2014,2050,5924523,"Country TL description (WITS) (could also be mapped to 748, 754, 756)","Plants and parts of plants (including seeds and fruits), of a kind used primarily in perfumery, in pharmacy or dried, whether or not cut, crushed or powdered: Other",
180,1,"12121000","12121000",461,2014,2050,5924524,"Country TL description (WITS)","Locust beans, seaweeds and other algae, sugar beet and sugar cane, fresh, chilled, frozen or dried, whether or not ground; fruit stones and kernels and other vegetable products (including unroasted chicory roots of the variety Cichorium intybus sativum) of a kind used primarily for human consumption, not elsewhere specified or included: Locust beans, including locust bean seeds",
180,1,"12122000","12122000",1293,2014,2050,5924525,"Country TL description (WITS), FCL descriptions","Locust beans, seaweeds and other algae, sugar beet and sugar cane, fresh, chilled, frozen or dried, whether or not ground; fruit stones and kernels and other vegetable products (including unroasted chicory roots of the variety Cichorium intybus sativum) of a kind used primarily for human consumption, not elsewhere specified or included: Seaweeds and other algae",
180,1,"12129900","12129900",460,2015,2050,5924526,"Trademap six-digit description (could also be mapped to 161)","Fruit stones and kernels and other vegetable products, incl. unroasted chicory roots of the variety cichorium intybus sativum, of a kind used primarily for human consumption, n.e.s.",
180,1,"12149000","12149000",651,2014,2050,5924527,"Country TL description (WITS), (could also be mapped to various other forages/hays)","Swedes, mangolds, fodder roots, hay, lucerne (alfalfa), clover, sainfoin, forage kale, lupines, vetches and similar forage products, whether or not in the form of pellets: Other",
180,1,"15121900","15121900",268,2014,2050,5924528,"Country TL description (WITS) (could also be mapped to 281)","Sunflower-seed, safflower or cotton-seed oil and fractions thereof, whether or not refined, but not chemically modified: Sunflower-seed or safflower oil and fractions thereof Other",
180,1,"15141900","15141900",271,2015,2050,5924529,"Country TL description (WITS) (could also be mapped to 293)","Rape, colza or mustard oil and fractions thereof, whether or not refined, but not chemically modified: Low erucic acid rape or colza oil and its fractions Other",
180,1,"15159000","15159000",340,2014,2050,5924530,"Country TL description (WITS) (could also be mapped to 36,264,278,297,306,307,313,337)","Other fixed vegetable fats and oils (including jojoba oil) and their fractions, whether or not refined, but not chemically modified: Other",
180,1,"15162000","15162000",1275,2014,2050,5924531,"Country TL description (WITS) (could also be mapped to 1273)","Animal or vegetable fats and oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared: Vegetable fats and oils and their fractions",
180,1,"15179000","15179000",1241,2014,2050,5924532,"Country TL description (WITS) (could also be mapped to 1243)","Margarine; edible mixtures or preparations of animal or vegetable fats or oils or of fractions of different fats or oils of this Chapter, other than edible fats or oils or their fractions of heading 15.16: Other",
180,1,"15219000","15219000",1183,2014,2050,5924533,"Country TL description (WITS) (could also be mapped to 1295)","Vegetable waxes (other than triglycerides), beeswax, refined or coloured: Other",
180,1,"16022000","16022000",1060,2014,2050,5924534,"Country TL description (WITS) (could also be mapped to 878)","Other prepared or preserved meat, meat offal or blood: Of liver of any animal",
180,1,"16055800","16055800",1176,2014,2050,5924535,"Country TL description (WITS)","Crustaceans, molluscs and other aquatic invertebrates prepared or preserved: Molluscs: Snails, other than sea snails",
180,1,"17011100","17011100",162,2014,2050,5924536,"Country TL description (WITS) (could also be mapped to 163)","Cane or beet sugar and chemically pure sucrose, in solid form: Other Cane sugar",
180,1,"17011200","17011200",162,2014,2050,5924537,"Country TL description (WITS) (could also be mapped to 163)","Cane or beet sugar and chemically pure sucrose, in solid form: Raw sugar not containing added flavouring or colouring matter Beet sugar",
180,1,"17011300","17011300",162,2014,2050,5924538,"Country TL description (WITS) (could also be mapped to 163)","Cane or beet sugar and chemically pure sucrose, in solid form: Raw sugar not containing added flavouring or colouring matter: Cane sugar specified in subheading note 2 to this chapter",
180,1,"17011400","17011400",162,2014,2050,5924539,"Country TL description (WITS) (could also be mapped to 163)","Cane or beet sugar and chemically pure sucrose, in solid form: Raw sugar not containing added flavouring or colouring matter: Other cane sugar",
180,1,"17029000","17029000",167,2014,2050,5924540,"Country TL description (WITS) (could also be mapped to 155,175)","Other sugars, including chemically pure lactose, maltose, glucose and fructose, in solid form; sugar syrups not containing added flavouring or colouring matter; artificial honey, whether or not mixed with natural honey; caramel: Other, including invert sugar and other sugar and sugar syrup blends containing in the dry state 50 % by weight of fructose",
180,1,"19019000","19019000",115,2014,2050,5924541,"Country TL description (WITS) (could also be mapped to 50)","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing less than 40 % by weight of cocoa calculated on a totally defatted basis, not elsewhere specified or included; food preparations of goods of headings 04.01 to 04.04, not containing cocoa or containing less than 5 % by weight of cocoa calculated on a totally defatted basis, not elsewhere specified or included: Other",
180,1,"19030000","19030000",121,2014,2050,5924542,"Country TL description (WITS) (could also be mapped to 127)","Tapioca and substitutes therefor prepared from starch, in the form of flakes, grains, pearls, siftings or in similar forms:",
180,1,"19059000","19059000",22,2014,2050,5924543,"Country TL description (WITS) (could also be mapped to 110)","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products: Other",
180,1,"20019000","20019000",471,2014,2050,5924544,"Country TL description (WITS) (could also be mapped to 262)","Vegetables, fruit, nuts and other edible parts of plants, prepared or preserved by vinegar or acetic acid: Other",
180,1,"20049000","20049000",475,2014,2050,5924545,"Country TL description (WITS) (could also be mapped to 262,447)","Other vegetables prepared or preserved otherwise than by vinegar or acetic acid, frozen, other than products of heading 20.06: Other vegetables and mixtures of vegetables",
180,1,"20059000","20059000",472,2014,2050,5924546,"Country TL description (WITS)","Other vegetables prepared or preserved otherwise than by vinegar or acetic acid, not frozen, other than products of heading 20.06: Other vegetables and mixtures of vegetables",
180,1,"20081100","20081100",246,2014,2050,5924547,"Country TL description (WITS)","Fruit, nuts and other edible parts of plants, otherwise prepared or preserved, whether or not containing added sugar or other sweetening matter or spirit, not elsewhere specified or included: Other Ground-nuts",
180,1,"20089200","20089200",623,2014,2050,5924548,"Country TL description (WITS)","Fruit, nuts and other edible parts of plants, otherwise prepared or preserved, whether or not containing added sugar or other sweetening matter or spirit, not elsewhere specified or included: Other, including mixtures other than those of subheading 2008.19 Mixtures",
180,1,"20089900","20089900",623,2014,2050,5924549,"Country TL description (WITS) (could also be mapped to 584)","Fruit, nuts and other edible parts of plants, otherwise prepared or preserved, whether or not containing added sugar or other sweetening matter or spirit, not elsewhere specified or included: Other, including mixtures other than those of subheading 2008.19 Other",
180,1,"20093100","20093100",513,2014,2050,5924550,"Country TL description (WITS) (could also be mapped to 496,498)","Fruit juices (including grape must) and vegetable juices, unfermented and not containing added spirit, whether or not containing added sugar or other sweetening matter: Juice of any other single citrus fruit: Of a brix value not exceeding 29",
180,1,"20093900","20093900",514,2014,2050,5924551,"Country TL description (WITS) (could also be mapped to 499)","Fruit juices (including grape must) and vegetable juices, unfermented and not containing added spirit, whether or not containing added sugar or other sweetening matter: Juice of any other single citrus fruit Other",
180,1,"20095000","20095000",390,2014,2050,5924552,"Country TL description (WITS) (could also be mapped to 389)","Fruit juices (including grape must) and vegetable juices, unfermented and not containing added spirit, whether or not containing added sugar or other sweetening matter: Tomato juice",
180,1,"20098090","20098090",622,2014,2050,5924553,"Country TL description (WITS) (could also be mapped to 466,538,539,583)","Fruit juices (including grape must) and vegetable juices, unfermented and not containing added spirit, whether or not containing added sugar or other sweetening matter: Juice of any other single fruit or vegetable Juice of any other single fruit or vegetable",
180,1,"20098900","20098900",622,2014,2050,5924554,"Country TL description (WITS) (could also be mapped to 466,538,539,583)","Fruit juices (including grape must) and vegetable juices, unfermented and not containing added spirit, whether or not containing added sugar or other sweetening matter: Juice of any other single fruit or vegetable: Other",
180,1,"20099000","20099000",622,2014,2050,5924555,"Country TL description (WITS) (could also be mapped to 466)","Fruit juices (including grape must) and vegetable juices, unfermented and not containing added spirit, whether or not containing added sugar or other sweetening matter: Mixtures of juices",
180,1,"21039000","21039000",1232,2014,2050,5924556,"Country TL description (WITS) (could also be mapped to 240)","Sauces and preparations therefor; mixed condiments and mixed seasonings; mustard flour and meal and prepared mustard: Other",
180,1,"21061000","21061000",1232,2014,2050,5924557,"Country TL description (WITS) (could also be mapped to 240,241)","Food preparations not elsewhere specified or included: Protein concentrates and textured protein substances",
180,1,"21069000","21069000",1232,2014,2050,5924558,"Country TL description (WITS)","Food preparations not elsewhere specified or included: Other",
180,1,"22030010","22030010",51,2014,2050,5924559,"Country TL description (WITS)","Beer made from malt: Packed in aluminum cans",
180,1,"22030020","22030020",51,2014,2050,5924560,"Country TL description (WITS)","Beer made from malt: Other",
180,1,"22030090","22030090",51,2014,2050,5924561,"Country TL description (WITS)","Other; not packed in aluminum can:",
180,1,"22060000","22060000",517,2014,2050,5924562,"Country TL description (WITS) (could also be mapped to 39)","Other fermented beverages (for exampl e, ci der, perry, mead); mixtures of fermented beverages and mi xtures of fermented beverages and non-alcoholic beverages, not elsewhere specified or i ncl uded",
180,1,"23069000","23069000",341,2015,2050,5924563,"Country TL description (WITS) (could also be mapped to various other oilcakes)","Oil-cake and other solid residues, whether or not ground or in the form of pellets, resulting from the extraction of vegetable fats or oils, other than those of heading 23.04 or 23.05: Other",
180,1,"23080000","23080000",652,2014,2050,5924564,"Country TL description (WITS) (could also be mapped to various other veg materials used for feed)","Vegetable materials and vegetable waste, vegetable residues and by-products, whether or not in the formof pellets, of a kind used in ani mal feeding, not elsewhere specified or included",
180,1,"23099000","23099000",845,2014,2050,5924565,"Country TL description (WITS) (could also be mapped to various other feeds)","Preparations of a kind used in animal feeding: Other",
180,1,"23099010","23099010",842,2014,2050,5924566,"Country TL description (WITS)","Preparations of a kind used in animal feeding: Other: Feed for swine",
180,1,"23099020","23099020",841,2014,2050,5924567,"Country TL description (WITS)","Preparations of a kind used in animal feeding: Other: Feed for poultry",
180,1,"23099090","23099090",845,2014,2050,5924568,"Country TL description (WITS) (could also be mapped to various other feeds)","Preparations of a kind used in animal feeding: Other: Other",
180,1,"24029000","24029000",828,2014,2050,5924569,"Country TL description (WITS) (could also be mapped to 829)","Cigars, cheroots, cigarillos and cigarettes, of tobacco or of tobacco substitutes: Other",
180,1,"33012900","33012900",753,2014,2050,5924570,"Country TL description (WITS)","Essential oils (terpeneless or not), including concretes and absolutes; resinoids; extracted oleoresins; concentrates of essential oils in fats, in fixed oils, in waxes or the like, obtained by enfleurage or maceration; terpenic by-products of the deterpenation of essential oils; aqueous distillates and aqueous solutions of essential oils: Essential oils other than those of citrus fruit: Other",
180,1,"41012000","41012000",921,2014,2050,5924571,"Country TL description (WITS) (could also be mapped to 920,922,928,929,930,958,959,1103,1104,1105)","Raw hides and ski ns of bovine (including buffalo) or equine animals (fresh, or salted, dried, limed, pickled or otherwise preserved, but not tanned, parchment-dressed or further prepared), whether or not dehaired or split: Whole hides and skins, unsplit of a weight per skin not exceeding 8kg when simply dried, 10kg when dry-salted, or 16kg when fresh, wet-salted or otherwise preserved",
180,1,"41019000","41019000",921,2014,2050,5924572,"Country TL description (WITS) (could also be mapped to 920,1103)","Raw hides and ski ns of bovine (including buffalo) or equine animals (fresh, or salted, dried, limed, pickled or otherwise preserved, but not tanned, parchment-dressed or further prepared), whether or not dehaired or split: Other, including butts, bends and bellies",
180,1,"41022900","41022900",998,2014,2050,5924573,"Country TL description (WITS) (could also be mapped to 996,997)","Raw skins of sheep or lambs (fresh, or salted, dried, limed, pickled or otherwise preserved, but not tanned, parchment-dressed or further prepared), whether or not with wool on or split, other than those excluded by note 1 (c) to this chapter: Without wool on: Other",
180,1,"41039000","41039000",1216,2015,2050,5924574,"Country TL description (WITS) (could also be mapped to 1045,1046,1047,1134,1135,1136,1214,1215)","Other raw hides and ski ns (fresh, or salted, dried, limed, pickledor otherwise preserved, but not tanned, parchment-dressed or further prepared), whether or not dehaired or split, other than those excluded by note 1 (b) or 1 (c) to this chapter: Other",
180,1,"51052900","51052900",1008,2015,2050,5924575,"Country TL description (WITS) (could also be mapped to 1010)","Wool and fine or coarse ani mal hair, carded or combed (including combed wool in fragments): Wool tops and other combed wool: Other",
180,1,"53031000","53031000",780,2014,2050,5924576,"Country TL description (WITS) (could also be mapped to 782)","Jute and other textile bast fibres (excluding flax, true hemp and ramie), raw or processed but not spun; tow and waste of these fibres (including yarn waste and garnetted stock): Jute and other textile bast fibres, raw or retted",
180,1,"53050000","53050000",809,2014,2050,5924577,"Country TL description (WITS) (could also be mapped to 821)","Coconut, abaca (manila hemp ormusatextilis nee), ramie and other vegetable textile fibres, not elsewhere specified or included, raw or processed but not spun; tow, noils and waste of these fibres (including yarn waste and garnetted stock)",
180,2,"01069000","01069000",1171,2014,2050,5924578,"Country TL description (WITS)","Other live animals: Other",
180,2,"19059000","19059000",22,2014,2050,5924579,"Country TL description (WITS) (could also be mapped to 110)","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products: Other",
180,2,"20098900","20098900",622,2014,2050,5924580,"Country TL description (WITS) (could also be mapped to 466,538,539,583)","Fruit juices (including grape must) and vegetable juices, unfermented and not containing added spirit, whether or not containing added sugar or other sweetening matter: Juice of any other single fruit or vegetable: Other",
196,1,"010592","010592",1057,2001,2050,5924581,"Generic HS2002 to FCL unique six-digit match","Live fowls of the species Gallus domesticus, weighing > 185 g but <= 2 kg",
196,1,"010593","010593",1057,2001,2050,5924582,"Generic HS2002 to FCL unique six-digit match","Live fowls of the species Gallus domesticus, weighing > 2 kg",
196,1,"010611","010611",1171,2001,2050,5924583,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live primates",
196,1,"010612","010612",1171,2001,2050,5924584,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live whales, dolphins and porpoises (mammals of the order Cetacea); manatees and dugongs (mammals of the order Sirenia); seals, sea lions and walruses (mammals of the suborder Pinnipedia)",
196,1,"010619","010619",1171,2001,2050,5924585,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live mammals (excluding primates, whales, dolphins and porpoises, manatees and dugongs, seals, sea lions and walruses, camels and other camelids, rabbits and hares, horses, asses, mules, hinnies, bovines, pigs, sheep and goats)",
196,1,"010620","010620",1171,2001,2050,5924586,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live reptiles e.g. snakes, turtles, alligators, caymans, iguanas, gavials and lizards",
196,1,"010631","010631",1171,2001,2050,5924587,"Generic HS2002 to FCL (could also be mapped to 1126,1140,1150,1157,1181,1169) ","Live birds of prey",
196,1,"010632","010632",1171,2001,2050,5924588,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live psittaciformes incl. parrots, parrakeets, macaws and cockatoos",
196,1,"010639","010639",1171,2001,2050,5924589,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live birds (excluding birds of prey, psittaciformes, parrots, parrakeets, macaws, cockatoos, ostriches and emus)",
196,1,"010690","010690",1171,2001,2050,5924590,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live animals (excluding mammals, reptiles, birds, insects, fish, crustaceans, molluscs and other aquatic invertebrates and cultures of micro-organisms, etc.)",
196,1,"020713","020713",1058,2001,2050,5924591,"Generic HS2002 to FCL (could also be mapped to 1059) ","Fresh or chilled cuts and edible offal of fowls of the species Gallus domesticus",
196,1,"020714","020714",1058,2001,2050,5924592,"Generic HS2007 to FCL (could also be mapped to 1059) ","Frozen cuts and edible offal of fowls of the species Gallus domesticus",
196,1,"020726","020726",1081,2001,2050,5924593,"Generic HS2002 to FCL unique six-digit match","Fresh or chilled cuts and edible offal of turkeys of the species domesticus",
196,1,"020727","020727",1080,2001,2050,5924594,"Generic HS2007 to FCL (could also be mapped to 1081) ","Frozen cuts and edible offal of turkeys of the species domesticus",
196,1,"020732","020732",1069,2001,2050,5924595,"Generic HS2002 to FCL (could also be mapped to 1073) ","Fresh or chilled ducks, geese and guinea fowls of the species domesticus, not cut into pieces",
196,1,"020733","020733",1069,2002,2050,5924596,"Generic HS2002 to FCL (could also be mapped to 1073) ","Frozen ducks, geese and guinea fowls of the species domesticus, not cut into pieces",
196,1,"020734","020734",1075,2002,2050,5924597,"Generic HS2002 to FCL (could also be mapped to 1074) ","Fresh or chilled edible fatty livers of ducks or geese of the species domesticus",
196,1,"020735","020735",1069,2001,2050,5924598,"Generic HS2002 to FCL (could also be mapped to 1073) ","Fresh or chilled cuts and edible offal of ducks, geese or guinea fowls of the species domesticus (excluding fatty livers)",
196,1,"020736","020736",1069,2001,2050,5924599,"Generic HS2002 to FCL (could also be mapped to 1073,1074,1075) ","Frozen cuts and edible offal of ducks, geese or guinea fowls of the species domesticus",
196,1,"020830","020830",1163,2001,2050,5924600,"Generic HS2002 to FCL unique six-digit match"," Fresh, chilled or frozen meat and edible offal of primates",
196,1,"020840","020840",1166,2001,2050,5924601,"Generic HS2002 to FCL unique six-digit match","Fresh, chilled or frozen meat and edible offal of whales, dolphins and porpoises (mammals of the order Cetacea), of manatees and dugongs (mammals of the order Sirenia) and of seals, sea lions and walruses (mammals of the suborder Pinnipedia)",
196,1,"020850","020850",1166,2001,2050,5924602,"Generic HS2002 to FCL unique six-digit match","Fresh, chilled or frozen meat and edible offal of reptiles e.g. snakes, turtles, crocodiles",
196,1,"040510","040510",886,2001,2050,5924603,"Generic HS2002 to FCL (could also be mapped to 952,983,1022) ","Butter (excluding dehydrated butter and ghee)",
196,1,"040520","040520",886,2001,2050,5924604,"Generic HS2002 to FCL (could also be mapped to 952,983,1022) ","Dairy spreads of a fat content, by weight, of >= 39% but < 80%",
196,1,"040590","040590",887,2001,2050,5924605,"Generic HS2002 to FCL (could also be mapped to 953, 1022) ","Fats and oils derived from milk, and dehydrated butter and ghee (excluding natural butter, recombined butter and whey butter)",
196,1,"090190","090190",658,2002,2050,5924606,"Generic HS2002 to FCL (could also be mapped to 660) ","Coffee husks and skins; coffee substitutes containing coffee in any proportion",
196,1,"110320","110320",16,2001,2050,5924607,"Generic HS2002 to FCL (could also be mapped to 38,48,58,72,80,84,90,95,98,104,111) ","Cereal pellets",
196,1,"151419","151419",271,2002,2050,5924608,"Generic HS2002 to FCL (could also be mapped to 293) "," Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2% and its fractions, whether or not refined, but not chemically modified (excluding crude)",
196,1,"151499","151499",271,2002,2050,5924609,"Generic HS2002 to FCL (could also be mapped to 293) ","High erucic acid rape or colza oil fixed oil which has an erucic acid content of >= 2%, and mustard oil, and fractions thereof, whether or not refined, but not chemically modified (excluding crude)",
196,1,"200939","200939",499,2001,2050,5924610,"Generic HS2002 to FCL (could also be mapped to 514)","Juice of any other single citrus fruit: of a brix value exceeding 20",
196,1,"230670","230670",61,2001,2050,5924611,"Generic HS2002 to FCL unique six-digit match","Oil cake: Of maize (corn) germ",
196,1,"230800","230800",652,2002,2050,5924612,"Generic HS2002 to FCL (could also be mapped to 120,566,628,629,630,650) ","Vegetable materials and vegetable waste, vegetable residues and by-products, whether or not in the form of pellets, of a kind used in animal feeding, not elsewhere specified or included.",
196,1,"23081000","23081000",652,2004,2050,5924613,"Generic HS2002 to FCL (could also be mapped to 120,566,628,629,630,650) ","Vegetable materials and vegetable waste, vegetable residues and by-products, whether or not in the form of pellets, of a kind used in animal feeding, not elsewhere specified or included: Acorns and horse-chestnuts",
196,1,"38231000","38231000",1276,2000,2050,5924614,"Generic HS2002 to FCL unique six-digit match","Industrial monocarboxylic fatty acids; acid oils from refining; industrial fatty alcohols",
196,1,"410120","410120",920,2002,2050,5924615,"Generic HS2002 to FCL (could also be mapped to 921,922,928,929,930,958,959,1103,1104,1105) ","Raw hides and skins of bovine (including buffalo) or equine animals (fresh, or salted, dried, limed, pickled or otherwise preserved, but not tanned, parchment-dressed or further prepared), whether or not dehaired or split.: Whole hides and skins, of a weight per skin not exceeding 8 kg when simply dried, 10 kg when dry-salted, or 16 kg when fresh, wet-salted or otherwise preserved",
196,1,"530590","530590",821,2001,2050,5924616,"Generic HS2002 to FCL (could also be mapped to 778,788) ","Coconut, abaca Manila hemp or Musa textilis Nee, ramie, agave and other vegetable textile fibres, n.e.s., raw or processed, but not spun; tow, noils and waste of such fibres, incl. yarn waste and garnetted stock: Not of coconut or abaca",
196,2,"010611","010611",1171,2002,2050,5924617,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live primates",
196,2,"010612","010612",1171,2002,2050,5924618,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live whales, dolphins and porpoises (mammals of the order Cetacea); manatees and dugongs (mammals of the order Sirenia); seals, sea lions and walruses (mammals of the suborder Pinnipedia)",
196,2,"010619","010619",1171,2002,2050,5924619,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live mammals (excluding primates, whales, dolphins and porpoises, manatees and dugongs, seals, sea lions and walruses, camels and other camelids, rabbits and hares, horses, asses, mules, hinnies, bovines, pigs, sheep and goats)",
196,2,"010620","010620",1171,2002,2050,5924620,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live reptiles e.g. snakes, turtles, alligators, caymans, iguanas, gavials and lizards",
196,2,"010631","010631",1171,2002,2050,5924621,"Generic HS2002 to FCL (could also be mapped to 1126,1140,1150,1157,1181,1169) ","Live birds of prey",
196,2,"010632","010632",1171,2002,2050,5924622,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live psittaciformes incl. parrots, parrakeets, macaws and cockatoos",
196,2,"010639","010639",1171,2002,2050,5924623,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live birds (excluding birds of prey, psittaciformes, parrots, parrakeets, macaws, cockatoos, ostriches and emus)",
196,2,"010690","010690",1171,2002,2050,5924624,"Generic HS2002 to FCL (could also be mapped to 1083,1126,1140,1150,1157,1181,1169) ","Live animals (excluding mammals, reptiles, birds, insects, fish, crustaceans, molluscs and other aquatic invertebrates and cultures of micro-organisms, etc.)",
196,2,"020713","020713",1058,2000,2050,5924625,"Generic HS2002 to FCL (could also be mapped to 1059) ","Fresh or chilled cuts and edible offal of fowls of the species Gallus domesticus",
196,2,"020726","020726",1081,2000,2050,5924626,"Generic HS2002 to FCL unique six-digit match","Fresh or chilled cuts and edible offal of turkeys of the species domesticus",
196,2,"020732","020732",1069,2000,2050,5924627,"Generic HS2002 to FCL (could also be mapped to 1073) ","Fresh or chilled ducks, geese and guinea fowls of the species domesticus, not cut into pieces",
196,2,"020735","020735",1069,2000,2050,5924628,"Generic HS2002 to FCL (could also be mapped to 1073) ","Fresh or chilled cuts and edible offal of ducks, geese or guinea fowls of the species domesticus (excluding fatty livers)",
196,2,"020736","020736",1069,2000,2050,5924629,"Generic HS2002 to FCL (could also be mapped to 1073,1074,1075) ","Frozen cuts and edible offal of ducks, geese or guinea fowls of the species domesticus",
196,2,"020830","020830",1163,2001,2050,5924630,"Generic HS2002 to FCL unique six-digit match"," Fresh, chilled or frozen meat and edible offal of primates",
196,2,"020840","020840",1166,2001,2050,5924631,"Generic HS2002 to FCL unique six-digit match","Fresh, chilled or frozen meat and edible offal of whales, dolphins and porpoises (mammals of the order Cetacea), of manatees and dugongs (mammals of the order Sirenia) and of seals, sea lions and walruses (mammals of the suborder Pinnipedia)",
196,2,"020850","020850",1166,2001,2050,5924632,"Generic HS2002 to FCL unique six-digit match","Fresh, chilled or frozen meat and edible offal of reptiles e.g. snakes, turtles, crocodiles",
196,2,"040510","040510",886,2000,2050,5924633,"Generic HS2002 to FCL (could also be mapped to 952,983,1022) ","Butter (excluding dehydrated butter and ghee)",
196,2,"040520","040520",886,2000,2050,5924634,"Generic HS2002 to FCL (could also be mapped to 952,983,1022) ","Dairy spreads of a fat content, by weight, of >= 39% but < 80%",
196,2,"040590","040590",887,2000,2050,5924635,"Generic HS2002 to FCL (could also be mapped to 953, 1022) ","Fats and oils derived from milk, and dehydrated butter and ghee (excluding natural butter, recombined butter and whey butter)",
196,2,"110320","110320",16,2002,2050,5924636,"Generic HS2002 to FCL (could also be mapped to 38,48,58,72,80,84,90,95,98,104,111) ","Cereal pellets",
202,1,"02071411","02071411",1058,2015,2050,5924637,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: breast",
202,1,"02071413","02071413",1059,2015,2050,5924638,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: feet",
202,1,"02071415","02071415",1058,2015,2050,5924639,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: other",
202,1,"02071421","02071421",1059,2015,2050,5924640,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: livers",
202,1,"02071423","02071423",1059,2015,2050,5924641,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: feet",
202,1,"02071425","02071425",1059,2015,2050,5924642,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: heads",
202,1,"02071429","02071429",1059,2015,2050,5924643,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: other",
202,1,"02071493","02071493",1058,2015,2050,5924644,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: quarters",
202,1,"02071495","02071495",1058,2015,2050,5924645,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: wings",
202,1,"02071497","02071497",1058,2015,2050,5924646,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: breast",
202,1,"02071498","02071498",1058,2015,2050,5924647,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: drumsticks",
202,1,"02071499","02071499",1058,2015,2050,5924648,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: other",
202,2,"02071411","02071411",1058,2015,2050,5924649,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: breast",
202,2,"02071413","02071413",1059,2015,2050,5924650,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: feet",
202,2,"02071415","02071415",1058,2015,2050,5924651,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: other",
202,2,"02071421","02071421",1059,2015,2050,5924652,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: livers",
202,2,"02071423","02071423",1059,2015,2050,5924653,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: feet",
202,2,"02071425","02071425",1059,2015,2050,5924654,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: heads",
202,2,"02071429","02071429",1059,2015,2050,5924655,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: other",
202,2,"02071491","02071491",1058,2015,2050,5924656,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: half carcasses",
202,2,"02071493","02071493",1058,2015,2050,5924657,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: quarters",
202,2,"02071495","02071495",1058,2015,2050,5924658,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: wings",
202,2,"02071496","02071496",1058,2015,2050,5924659,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: breasts",
202,2,"02071497","02071497",1058,2015,2050,5924660,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: breast",
202,2,"02071498","02071498",1058,2015,2050,5924661,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: drumsticks",
202,2,"02071499","02071499",1058,2015,2050,5924662,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: other",
207,1,"01059990","01059990",1072,2005,2050,5924663,"Country TL description (could also be mapped to 1068, 1079) ","Live domestic ducks, geese, turkeys and guinea fowls, weighing > 185 g: Other (not for breeding or rearing)",
207,1,"01060030","01060030",1169,2008,2050,5924664,"Country TL description (WITS)","Turtles",
207,1,"01060040","01060040",1169,2000,2050,5924665,"Country TL description (WITS)","Dogs",
207,1,"01060050","01060050",1169,2002,2050,5924666,"Country TL description (WITS)","Cats",
207,1,"01060070","01060070",1169,2002,2050,5924667,"Country TL description (WITS)","Macaws and parrots",
207,1,"01060080","01060080",1169,2000,2050,5924668,"Country TL description (WITS)","Other birds",
207,1,"02023020","02023020",870,2002,2050,5924669,"Country TL description (WITS)","Sirloin",
207,1,"02062200","02062200",868,2003,2050,5924670,"Country TL description (WITS) (could also be mapped to 948)","Livers (bovine)",
207,1,"020714","020714",1058,2014,2050,5924671,"Generic HS2012 to FCL (could also be mapped to 1059) ","Cuts & edible offal of species Gallus domesticus, frozen",
207,1,"02072300","02072300",1069,2000,2050,5924672,"Generic HS1992 to FCL, Old SWS series (could also be mapped to 1073) ","Frozen ducks, geese and guinea fowls of the species domesticus, not cut into pieces",
207,1,"02075000","02075000",1059,2000,2050,5924673,"Generic HS1992 to FCL, Old SWS series (could also be mapped to 1074, 1075, 1081) ","Poultry livers, frozen",
207,1,"04060000","04060000",901,2000,2050,5924674,"Generic HS1996 to FCL, Old SWS series (could also be mapped to 904,905,907,955,984,1021) ","Cheese and curd",
207,1,"08011020","08011020",249,2000,2050,5924675,"Country TL description (could also be mapped to 250) ","Coconuts, fresh or dried",
207,1,"08071020","08071020",567,2000,2050,5924676,"Country TL description, Old SWS series (could also be mapped to 568) ","Melons and watermelons, fresh",
207,1,"08071030","08071030",567,2001,2050,5924677,"Country TL description, Old SWS series (could also be mapped to 568) ","Melons and watermelons, fresh",
207,1,"121299","121299",460,2014,2050,5924678,"Trademap description, Old SWS series (could also be mapped to 161) ","Fruit stones and kernels and other vegetable products, incl. unroasted chicory roots of the variety cichorium intybus sativum, of a kind used primarily for human consumption, n.e.s.",
207,1,"190190","190190",115,2014,2050,5924679,"Generic HS2012 to FCL, Old SWS series  (could also be mapped to 50) ","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905)",
207,1,"190590","190590",22,2014,2050,5924680,"Generic HS2012 to FCL, Old SWS series (could also be mapped to 110) ","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products (excluding crispbread, gingerbread and the like, sweet biscuits, waffles, wafers not mentioned, rusks, toasted bread and similar toasted products)",
207,1,"20092010","20092010",510,2007,2050,5924681,"Generic HS2007 to FCL (could also be mapped to 509) ","Grapefruit juice",
207,1,"20092090","20092090",510,2000,2050,5924682,"Generic HS2007 to FCL (could also be mapped to 509) ","Grapefruit juice, unfermented, not containing a",
207,1,"20093010","20093010",513,2000,2050,5924683,"Country TL description, Old SWS series  (could also be mapped to 514) ","Juice of any other single citrus fruit",
207,1,"20093020","20093020",513,2009,2050,5924684,"Country TL description, Old SWS series  (could also be mapped to 514) ","Juice of any other single citrus fruit",
207,1,"20093030","20093030",513,2000,2050,5924685,"Country TL description, Old SWS series  (could also be mapped to 514) ","Juice of any other single citrus fruit",
207,1,"20093040","20093040",513,2007,2050,5924686,"Country TL description, Old SWS series  (could also be mapped to 514) ","Juice of any other single citrus fruit",
207,1,"20093090","20093090",513,2000,2050,5924687,"Country TL description, Old SWS series  (could also be mapped to 514) ","Juice of any other single citrus fruit",
207,1,"20094020","20094020",576,2009,2050,5924688,"Country TL description","Pineapple juice",
207,1,"20094090","20094090",576,2000,2050,5924689,"Country TL description","Pineapple juice",
207,1,"20097010","20097010",518,2000,2050,5924690,"Country TL description  (could also be mapped to 519) ","Apple juice, unfermented, not containing added",
207,1,"20097020","20097020",518,2003,2050,5924691,"Country TL description  (could also be mapped to 519) ","Apple juice",
207,1,"20097090","20097090",518,2000,2050,5924692,"Country TL description  (could also be mapped to 519) ","Apple juice",
207,2,"01060030","01060030",1169,2000,2050,5924693,"Country TL description (WITS)","Turtles",
207,2,"01060040","01060040",1169,2000,2050,5924694,"Country TL description (WITS)","Dogs",
207,2,"01060050","01060050",1169,2000,2050,5924695,"Country TL description (WITS)","Cats",
207,2,"01060060","01060060",1169,2000,2050,5924696,"Country TL description (WITS)","Monkeys",
207,2,"01060070","01060070",1169,2000,2050,5924697,"Country TL description (WITS)","Macaws and parrots",
207,2,"01060080","01060080",1169,2000,2050,5924698,"Country TL description (WITS)","Other birds",
207,2,"020714","020714",1058,2014,2050,5924699,"Generic HS2012 to FCL (could also be mapped to 1059) ","Cuts & edible offal of species Gallus domesticus, frozen",
207,2,"02072300","02072300",1069,2000,2050,5924700,"Generic HS1992 to FCL, Old SWS series (could also be mapped to 1073) ","Frozen ducks, geese and guinea fowls of the species domesticus, not cut into pieces",
207,2,"02075000","02075000",1059,2005,2050,5924701,"Generic HS1992 to FCL, Old SWS series (could also be mapped to 1074, 1075, 1081) ","Poultry livers, frozen",
207,2,"02109030","02109030",1164,2000,2050,5924702,"Country TL description (WITS)","Edible flours and meals of meat or meat offal",
207,2,"04060000","04060000",901,2000,2050,5924703,"Generic HS1996 to FCL, Old SWS series (could also be mapped to 904,905,907,955,984,1021) ","Cheese and curd",
207,2,"15010010","15010010",1043,2000,2050,5924704,"Country TL description (could also be mapped to 1066) ","Lard, other pig fat and poultry fat, rendered",
207,2,"190190","190190",115,2014,2050,5924705,"Generic HS2012 to FCL, Old SWS series  (could also be mapped to 50) ","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905)",
207,2,"190590","190590",22,2014,2050,5924706,"Generic HS2012 to FCL, Old SWS series  (could also be mapped to 110) ","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products (excluding crispbread, gingerbread and the like, sweet biscuits, waffles, wafers not mentioned, rusks, toasted bread and similar toasted products)",
207,2,"20092090","20092090",510,2000,2050,5924707,"Country TL description (could also be mapped to 509) ","Grapefruit juice, unfermented, not containing a",
207,2,"20093090","20093090",513,2000,2050,5924708,"Country TL description, Old SWS series  (could also be mapped to 514) ","Juice of any other single citrus fruit",
207,2,"20094010","20094010",576,2003,2050,5924709,"Country TL description","Pineapple juice, unfermented, not containing ad",
207,2,"20094090","20094090",576,2004,2050,5924710,"Country TL description","Pineapple juice",
207,2,"20097010","20097010",518,2005,2050,5924711,"Country TL description  (could also be mapped to 519) ","Apple juice, unfermented, not containing added",
207,2,"20097090","20097090",518,2000,2050,5924712,"Country TL description  (could also be mapped to 519) ","Apple juice",
212,1,"010611","010611",1169,2009,2050,5924713,"Trademap description (could also be mapped to 1171)","live primates",
212,1,"01061110","01061110",1169,2006,2050,5924714,"Trademap description (could also be mapped to 1171)","live primates",
212,1,"01061190","01061190",1169,2006,2050,5924715,"Trademap description (could also be mapped to 1171)","live primates",
212,1,"010620","010620",1169,2009,2050,5924716,"Trademap description (could also be mapped to 1171)","Live reptiles",
212,1,"010632","010632",1169,2009,2050,5924717,"Trademap description (could also be mapped to 1171)","live psittaciformes",
212,1,"02062900","02062900",868,2006,2050,5924718,"Country TL description (WITS) (could also be mapped to 948)","Frozen edible bovine offal (excl. tongues and livers)",
212,1,"020727","020727",1080,2009,2050,5924719,"Trademap description (could also be mapped to 1081)","Frozen cuts and edible offal of Turkey",
212,1,"020900","020900",1037,2009,2050,5924720,"Trademap description (could also be mapped to 1065)","Pig fat, free of lean meat, and poultry fat, not rendered or otherwise extracted, fresh, chilled, frozen, salted, in brine, dried or smoked",
212,1,"040291","040291",894,2009,2050,5924721,"Generic HS 2007 to FCL mapping (could also be mapped to 895)","Milk and cream, concentrated but unsweetened (excluding in solid forms)",
212,1,"040299","040299",889,2009,2050,5924722,"Generic HS 2007 to FCL mapping (could also be mapped to 896)","Milk and cream, concentrated and sweetened (excluding in solid forms)",
212,1,"04029920","04029920",889,2005,2050,5924723,"Country TL description (WITS)","Milk and cream, concentrated and sweetened (excl. in solid forms): cream",
212,1,"070990","070990",463,2009,2050,5924724,"Trademap description","Fresh or chilled vegetables (excluding potatoes, tomatoes, vegetables of the Allium spp., cabbages of the genus Brassica, lettuces of the species Lactuca sativa and Cichorium, carrots, turnips, salad beetroot, salsify, celeriac, radishes and similar edible roots, cucumbers and gherkins, leguminous vegetables, asparagus, aubergines, mushrooms, truffles, fruits of the genus Capsicum or of the genus Pimenta, spinach, New Zealand spinach and orache spinach)",
212,1,"08082020","08082020",523,2006,2050,5924725,"Country TL description (WITS)","Quinces",
212,1,"110290","110290",111,2009,2050,5924726,"Trademap description","Cereal flours (excluding wheat, meslin and maize)",
212,1,"150100","150100",1043,2009,2050,5924727,"Country TL description (WITS) (could also be mapped to 1066)","pig fat (including lard) and poultry fat, other than that of heading 0209 or 1503",
212,1,"15020011","15020011",869,2005,2050,5924728,"Country TL description (WITS) (could also be mapped to 871, 949, 979, 1019)","Fats of bovine animals, sheep or goats, other than those of heading 1503: for use in the manufacture of soap",
212,1,"15020019","15020019",869,2006,2050,5924729,"Country TL description (WITS) (could also be mapped to 871, 949, 979, 1019)","Fats of bovine animals, sheep or goats, other than those of heading 1503: other (not used for soap manufacturing)",
212,1,"151211","151211",268,2009,2050,5924730,"Trademap description, Old SWS series (could also be mapped to 281)","Crude sunflower-seed or safflower oil",
212,1,"151219","151219",268,2009,2050,5924731,"Trademap description, Old SWS series (could also be mapped to 281)","Sunflower-seed or safflower oil and their fractions, whether or not refined, but not chemically modified (excluding crude)",
212,1,"151419","151419",271,2009,2050,5924732,"Trademap description","Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2% and its fractions, whether or not refined, but not chemically modified (excluding crude)",
212,1,"15141900","15141900",271,2005,2050,5924733,"Trademap description","Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2% and its fractions, whether or not refined, but not chemically modified (excluding crude)",
212,1,"15149100","15149100",271,2005,2050,5924734,"Trademap description","High erucic acid rape or colza oil fixed oil which has an erucic acid content of >= 2% and mustard oil, crude",
212,1,"151620","151620",1275,2009,2050,5924735,"Trademap description (could also be mapped to 1273)"," Vegetable fats and oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared",
212,1,"151790","151790",1243,2009,2050,5924736,"Trademap description (could also be mapped to 1241)"," Edible mixtures or preparations of animal or vegetable fats or oils and edible fractions of different fats or oils ",
212,1,"15219020","15219020",1183,2006,2050,5924737,"Country TL description (WITS)","Beeswax",
212,1,"17029019","17029019",167,2006,2050,5924738,"Generic HS 2002 to FCL mapping (could also be mapped to 175). Note that 8-digit code not found in country schedules","Other, including invert sugar and other sugar and sugar syrup blends containing in the dry state 50 % by weight of fructose",
212,1,"17029021","17029021",167,2005,2050,5924739,"Generic HS 2002 to FCL mapping (could also be mapped to 175). Note that 8-digit code not found in country schedules","Other, including invert sugar and other sugar and sugar syrup blends containing in the dry state 50 % by weight of fructose",
212,1,"190190","190190",115,2009,2050,5924740,"Trademap description, Old SWS series (could also be mapped to 50)","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905)",
212,1,"200811","200811",246,2009,2050,5924741,"Trademap description (could also be mapped to 247)","Groundnuts, prepared or preserved (excluding preserved with sugar)",
212,1,"200911","200911",492,2009,2050,5924742,"Trademap description","Frozen orange juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
212,1,"510219","510219",1218,2009,2050,5924743,"Trademap description (could also be mapped to 1030)","Fine animal hair, neither carded nor combed (excluding wool and hair of Kashmir cashmere goats)",
212,2,"01061190","01061190",1169,2005,2050,5924744,"Trademap description (could also be mapped to 1171)","live primates",
212,2,"01062090","01062090",1169,2005,2050,5924745,"Trademap description (could also be mapped to 1171)","Live reptiles",
212,2,"02061000","02061000",868,2005,2050,5924746,"Country TL description (WITS) (could also be mapped to 948)","Fresh or chilled edible offal of bovine animals",
212,2,"020629","020629",868,2009,2050,5924747,"Country TL description (WITS) (could also be mapped to 948)","Frozen edible bovine offal (excl. tongues and livers)",
212,2,"02062900","02062900",868,2006,2050,5924748,"Country TL description (WITS) (could also be mapped to 948)","Frozen edible bovine offal (excl. tongues and livers)",
212,2,"020680","020680",978,2009,2050,5924749,"Trademap description (could also be mapped to 1018, 1098, 1167)","Fresh or chilled edible offal of sheep, goats, horses, asses, mules and hinnies",
212,2,"020690","020690",978,2009,2050,5924750,"Trademap description (could also be mapped to 1018, 1098, 1167)","Frozen edible offal of sheep, goats, horses, asses, mules and hinnies",
212,2,"02069000","02069000",978,2006,2050,5924751,"Trademap description (could also be mapped to 1018, 1098, 1167)","Frozen edible offal of sheep, goats, horses, asses, mules and hinnies",
212,2,"02071490","02071490",1058,2005,2050,5924752,"Country TL description (WITS) (could also be mapped to 1059)","Frozen cuts and edible offal of fowls of the species Gallus domesticus: other (not livers)",
212,2,"02085090","02085090",1166,2005,2050,5924753,"Trademap six-digit description","Fresh, chilled or frozen meat and edible offals of reptiles",
212,2,"02089000","02089000",1166,2006,2050,5924754,"Trademap description (could also be mapped to 1089, 1127, 1128, 1151, 1158, 1159, 1163)","Fresh, chilled or frozen meat and edible offal of pigeons, game, reindeer and other animals (excluding bovine animals, swine, sheep, goats, horses, asses, mules, hinnies, poultry fowls of the species Gallus domesticus, ducks, geese, turkeys, guinea fowl, rabbits, hares, primates, whales, dolphins and porpoises mammals of the order Cetacea, manatees and dugongs mammals of the order Sirenia, seals, sea lions and walruses mammals of the suborder Pinnipedia and reptiles)",
212,2,"040291","040291",894,2009,2050,5924755,"Generic HS 2007 to FCL mapping (could also be mapped to 895)","Milk and cream, concentrated but unsweetened (excluding in solid forms)",
212,2,"040299","040299",889,2009,2050,5924756,"Generic HS 2007 to FCL mapping (could also be mapped to 896)","Milk and cream, concentrated and sweetened (excluding in solid forms)",
212,2,"070990","070990",463,2009,2050,5924757,"Trademap description","Fresh or chilled vegetables (excluding potatoes, tomatoes, vegetables of the Allium spp., cabbages of the genus Brassica, lettuces of the species Lactuca sativa and Cichorium, carrots, turnips, salad beetroot, salsify, celeriac, radishes and similar edible roots, cucumbers and gherkins, leguminous vegetables, asparagus, aubergines, mushrooms, truffles, fruits of the genus Capsicum or of the genus Pimenta, spinach, New Zealand spinach and orache spinach)",
212,2,"08045030","08045030",603,2006,2050,5924758,"Country TL description (WITS)","Mangosteens",
212,2,"08082020","08082020",523,2005,2050,5924759,"Country TL description (WITS)","Quinces",
212,2,"08103000","08103000",550,2006,2050,5924760,"Trademap description","Fresh black, white or red currant",
212,2,"11029000","11029000",111,2006,2050,5924761,"Trademap description","Cereal flours (excluding wheat, meslin and maize)",
212,2,"12079920","12079920",339,2006,2050,5924762,"Country TL description (WITS)","Oil seeds and oleaginous fruits: Niger and mafura seeds",
212,2,"12149020","12149020",205,2006,2050,5924763,"Country TL description (WITS)","Vetches",
212,2,"15010021","15010021",1043,2006,2050,5924764,"Country TL description (WITS) (could also be mapped to 1066)","pig fat (including lard) and poultry fat, other than that of heading 0209 or 1503: For use in the manufacture of soap",
212,2,"15010029","15010029",1043,2006,2050,5924765,"Country TL description (WITS) (could also be mapped to 1066)","pig fat (including lard) and poultry fat, other than that of heading 0209 or 1503: other (not used for soap manufacturing)",
212,2,"15020019","15020019",869,2006,2050,5924766,"Country TL description (WITS) (could also be mapped to 871, 949, 979, 1019)","Fats of bovine animals, sheep or goats, other than those of heading 1503: other (not used for soap manufacturing)",
212,2,"151211","151211",268,2009,2050,5924767,"Trademap description, Old SWS series (could also be mapped to 281)","Crude sunflower-seed or safflower oil",
212,2,"151219","151219",268,2009,2050,5924768,"Trademap description, Old SWS series (could also be mapped to 281)","Sunflower-seed or safflower oil and their fractions, whether or not refined, but not chemically modified (excluding crude)",
212,2,"151620","151620",1275,2009,2050,5924769,"Trademap description (could also be mapped to 1273)"," Vegetable fats and oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared",
212,2,"151790","151790",1243,2009,2050,5924770,"Trademap description (could also be mapped to 1241)"," Edible mixtures or preparations of animal or vegetable fats or oils and edible fractions of different fats or oils ",
212,2,"15220090","15220090",1222,2006,2050,5924771,"Country TL description (WITS) (could also be mapped to 1277)","Degras; residues resulting from the treatment of fatty substances or animal or vegetable waxes: other (i.e. not soapstock or oil-foot and dregs)",
212,2,"190190","190190",115,2009,2050,5924772,"Trademap description, Old SWS series (could also be mapped to 50)","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905)",
212,2,"200811","200811",246,2009,2050,5924773,"Trademap description (could also be mapped to 247)","Groundnuts, prepared or preserved (excluding preserved with sugar)",
212,2,"200911","200911",492,2009,2050,5924774,"Trademap description","Frozen orange juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
215,1,"01061300","01061300",1126,2015,2050,5924775,"Country TL description (WITS)","CAMELS",
215,1,"02074400","02074400",1069,2015,2050,5924776,"Country TL description (WITS), Old SWS series (could also be mapped to 1075)","MEAT OR OFFALS OF DUCKS",
215,1,"02075400","02075400",1073,2013,2050,5924777,"Trademap TL description (could also be mapped to 1074)","Fresh or chilled cuts and edible offal of domestic geese (excluding fatty livers)",
215,1,"02075500","02075500",1073,2015,2050,5924778,"Trademap TL description (could also be mapped to 1074)","Frozen cuts and edible offal of domestic geese",
215,1,"15029000","15029000",869,2013,2050,5924779,"Trademap TL description (could also be mapped to 871,949,979,1019)","Fats of bovine animals, sheep or goats,other than those of heading 15.03: Other",
215,1,"22030100","22030100",51,2015,2050,5924780,"Country TL description (WITS)","Beer made from malt: Stout and porter",
215,1,"22042200","22042200",564,2015,2050,5924781,"Generic HS 2017 to FCL","Wine of fresh grapes, including fortified wines; grape must other than that of heading 20.09: In containers holding more than 2 l but not more than 10 l,
",
215,2,"15029000","15029000",869,2014,2050,5924782,"Trademap TL description (could also be mapped to 871,949,979,1019)","Fats of bovine animals, sheep or goats,other than those of heading 15.03: Other",
219,1,"01011000","01011000",1096,2008,2050,5924783,"Trademap TL description (could also be mapped to 1107 or 1110)","Pure-bred breeding horses and asses",
219,1,"02074500","02074500",1069,2014,2050,5924784,"Trademap TL description, Old SWS series (could also be mapped to 1075)","Frozen cuts and edible offal of domestic ducks",
219,1,"02075500","02075500",1074,2014,2050,5924785,"Trademap TL description, Old SWS series (could also be mapped to 1073)","Frozen cuts and edible offal of domestic geese",
219,1,"02091000","02091000",1037,2014,2050,5924786,"Trademap TL description, Old SWS series (could also be mapped to 1040)","Pig fat, free of lean meat, not rendered or otherwise extracted, fresh, chilled, frozen, salted, in brine, dried or smoked",
219,1,"04014000","04014000",885,2014,2050,5924787,"Trademap TL description, Jellyfish (previously, anything above 6% fat was mapped to 885 cream)","Milk and cream of a fat content by weight of > 6% but <= 10%, not concentrated nor containing added sugar or other sweetening matter",
219,1,"04015000","04015000",885,2014,2050,5924788,"Trademap TL description, Jellyfish (previously, anything above 6% fat was mapped to 885 cream)","Milk and cream of a fat content by weight of > 10%, not concentrated nor containing added sugar or other sweetening matter",
219,1,"04079090","04079090",1062,2014,2050,5924789,"Trademap TL description, Old SWS series (could also be mapped to 1091)","Birds' eggs, in shell, fresh, preserved or cooked: Other: Other",
219,1,"07099900","07099900",463,2014,2050,5924790,"Trademap TL description","Other vegetables, fresh or chilled: Other: Other",
219,1,"11032000","11032000",111,2009,2050,5924791,"Trademap TL description, Old SWS series (could also be mapped to 16, 38, 58, 72)","Cereal groats, meal and pellets: pellets",
219,1,"15029000","15029000",869,2014,2050,5924792,"Trademap TL description (could be mapped to 869, 871, 949, 979, 1019. No time series in the Old SWS)","Fats of bovine animals, sheep or goats, other than those of heading 15.03: Other",
219,1,"17011300","17011300",162,2014,2050,5924793,"Trademap TL description (could also be mapped to 163)","Cane or beet sugar and chemically pure sucrose, in solid form: Raw sugar not containing added . . .",
219,1,"17011400","17011400",162,2014,2050,5924794,"Trademap TL description (could also be mapped to 163)","Cane or beet sugar and chemically pure sucrose, in solid form: Raw sugar not containing added . . .",
219,1,"41012000","41012000",921,2008,2050,5924795,"Trademap TL description, Old SWS series (could also be mapped to 920, 921, 922, 928, 929, 930, 958, 959, 1103, 1104, 1105)","Raw hides and skins of bovine (including buffalo) or equine animals (fresh, or salted, dried, limed, pickled or otherwise preserved, but not tanned, parchment-dressed or further prepared), whether or not dehaired or split: whole hides and skins, of a weight per skin not exceeding 8kg when simply dried, 10kg when dry- salted, or 16kg when fresh, wet-salted or otherwise preserved",
219,1,"410150","410150",921,2011,2050,5924796,"Trademap TL description, Old SWS series (could also be mapped to 920 or 1103)","Whole raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired or split, of a weight per skin > 16 kg, fresh, or salted, dried, limed, pickled or otherwise preserved (excluding tanned, parchment-dressed or further prepared)",
219,2,"08039000","08039000",486,2014,2050,5924797,"Trademap TL description, Old SWS series (could also be mapped to 604)","Bananas, including plantains, fresh or dried: Other",
220,1,"41032000","41032000",1216,2008,2050,5924798,"Generic HS 2007 to FCL, Old SWS series (could also be mapped to 1214, 1215)","Raw hides and skins of reptiles, fresh or salted, dried, limed, pickled or otherwise preserved (excluding tanned, parchment-dressed or further prepared)",
230,1,"010613","010613",1126,2015,2050,5924799,"Generic HS2012 to FCL (could also be mapped to 1157) ","Camels and other camelids (camelidae)",
230,1,"010649","010649",1171,2014,2050,5924800,"Generic HS2012 to FCL (could also be mapped to 1169) ","Other insects (excl bees)",
230,1,"020744","020744",1069,2014,2050,5924801,"Generic HS2012 to FCL (could also be mapped to 1075) ","Other meat or edible offals of ducks, fresh or chilled",
230,1,"020745","020745",1069,2014,2050,5924802,"Generic HS2012 to FCL (could also be mapped to 1075) ","Other meat or edible offals of ducks, frozen",
230,1,"020755","020755",1073,2014,2050,5924803,"Generic HS2012 to FCL (could also be mapped to 1074) ","Other meat or edible offals of geese, frozen",
230,1,"020910","020910",1037,2014,2050,5924804,"Generic HS2012 to FCL, Old SWS series (could also be mapped to 1040) ","Pig fat, free of lean meat, and poultry fat, not rendered or otherwise extracted, fresh, chilled, frozen, salted, in brine, dried or smoked",
230,1,"040140","040140",885,2014,2050,5924805,"Generic HS2012 to FCL, Jellyfish (previously, anything above 6% fat was mapped to 885 cream), (could also be mapped to 882,883,951,982,1020,1130) ","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight exceeding 6% but not exceeding 10%",
230,1,"040150","040150",885,2014,2050,5924806,"Generic HS2012 to FCL, Jellyfish (previously, anything above 6% fat was mapped to 885 cream), (could also be mapped to 882,883,951,982,1020,1130) ","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight exceeding 10%",
230,1,"070999","070999",463,2014,2050,5924807,"Generic HS2012 to FCL (could also be mapped to 378,430,446) ","Fresh or chilled vegetables n.e.s.",
230,1,"080390","080390",486,2014,2050,5924808,"Generic HS2012 to FCL (could also be mapped to 604) ","Fresh or dried bananas (excluding plantains)",
230,1,"170113","170113",162,2014,2050,5924809,"Generic HS2012 to FCL (could also be mapped to 163) ","Raw cane sugar, in solid form, not containing added flavouring or colouring matter, obtained without centrifugation, with sucrose content 69° to 93°, containing only natural anhedral microcrystals (see subheading note 2.)",
230,1,"170114","170114",162,2014,2050,5924810,"Generic HS2012 to FCL (could also be mapped to 163) ","Raw cane sugar, in solid form, not containing added flavouring or colouring matter (excluding cane sugar of 1701 13)",
230,1,"200989","200989",622,2014,2050,5924811,"Generic HS2012 to FCL (could also be mapped to 466,538,539,583) ","Juice of fruit or vegetables, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, and juice of citrus fruit, pineapples, tomatoes, grapes, incl. grape must, apples and cranberries)",
230,2,"020744","020744",1069,2014,2050,5924812,"Generic HS2012 to FCL (could also be mapped to 1075) ","Other meat or edible offals of ducks, fresh or chilled",
230,2,"020745","020745",1069,2014,2050,5924813,"Generic HS2012 to FCL (could also be mapped to 1075) ","Other meat or edible offals of ducks, frozen",
230,2,"020755","020755",1073,2014,2050,5924814,"Generic HS2012 to FCL (could also be mapped to 1074) ","Other meat or edible offals of geese, frozen",
230,2,"020910","020910",1037,2014,2050,5924815,"Generic HS2012 to FCL, Old SWS series (could also be mapped to 1040) ","Pig fat, free of lean meat, and poultry fat, not rendered or otherwise extracted, fresh, chilled, frozen, salted, in brine, dried or smoked",
230,2,"040140","040140",885,2014,2050,5924816,"Generic HS2012 to FCL, Jellyfish (previously, anything above 6% fat was mapped to 885 cream), (could also be mapped to 882,883,951,982,1020,1130) ","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight exceeding 6% but not exceeding 10%",
230,2,"040150","040150",885,2014,2050,5924817,"Generic HS2012 to FCL, Jellyfish (previously, anything above 6% fat was mapped to 885 cream), (could also be mapped to 882,883,951,982,1020,1130) ","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight exceeding 10%",
230,2,"040790","040790",1062,2014,2050,5924818,"Generic HS2012 to FCL (could also be mapped to 1091) ","Other birds eggs, in shell, preserved or cooked",
230,2,"070999","070999",463,2014,2050,5924819,"Generic HS2012 to FCL (could also be mapped to 378,430,446) ","Fresh or chilled vegetables n.e.s.",
230,2,"080390","080390",486,2014,2050,5924820,"Generic HS2012 to FCL (could also be mapped to 604) ","Fresh or dried bananas (excluding plantains)",
230,2,"150290","150290",869,2014,2050,5924821,"Generic HS2012 to FCL (could also be mapped to 871,949,979,1019) ","Fats of bovine animals, sheep or goats (excluding tallow, oleostearin and oleo-oil)",
230,2,"200989","200989",622,2014,2050,5924822,"Generic HS2012 to FCL (could also be mapped to 466,538,539,583) ","Juice of fruit or vegetables, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, and juice of citrus fruit, pineapples, tomatoes, grapes, incl. grape must, apples and cranberries)",
238,1,"02074500","02074500",1069,2014,2050,5924823,"Country TL description (WITS) (could also be mapped to 1075)","Meat and edible offal, of ducks",
238,1,"02084000","02084000",1166,2014,2050,5924824,"Country TL description (WITS) (could also be mapped to 1075)","Fresh, chilled or frozen meat and edible offal of whales",
238,1,"04015000","04015000",885,2014,2050,5924825,"Trademap TL description, Jellyfish (previously, anything above 6% fat was mapped to 885 cream)","Milk and cream, not concentrated nor containing added sugar or other sweetening matter",
238,1,"07099900","07099900",463,2014,2050,5924826,"Country TL description (WITS) (could also 378, 430, 446)","Other vegetables fresh",
238,1,"08039000","08039000",486,2014,2050,5924827,"Trademap TL description (could also be mapped to 604)","fresh or dried bananas (excluding plantains)",
238,1,"17011300","17011300",162,2014,2050,5924828,"Country TL description (WITS) (could also be mapped to 163)","Sugar raw-centrifugal",
238,1,"17011400","17011400",162,2014,2050,5924829,"Country TL description (WITS) (could also be mapped to 163)","Sugar raw-centrifugal",
238,2,"01061300","01061300",1126,2014,2050,5924830,"Country TL description (WITS) (could also be mapped to 1157)","Camels",
238,2,"02074500","02074500",1069,2015,2050,5924831,"Country TL description (WITS) (could also be mapped to 1167)","Meat and edible offal, of ducks",
238,2,"07099900","07099900",463,2014,2050,5924832,"Country TL description (WITS) (could also 378, 430, 446)","Other vegetables fresh",
238,2,"08039000","08039000",486,2014,2050,5924833,"Trademap TL description (could also be mapped to 604)","fresh or dried bananas (excluding plantains)",
299,1,"010210","010210",866,2014,2050,5924834,"Generic HS2007 to FCL (could also be mapped to 946)","Pure-bred breeding bovines",
299,1,"010290","010290",1171,2014,2050,5924835,"Trademap TL description","Live bovine animals (excluding cattle and buffalo)",
299,1,"010599","010599",1057,2014,2050,5924836,"Generic HS2012 to FCL (could also be mapped to 1068,1072) ","Live domestic ducks, geese, turkeys and guinea fowls, weighing > 185 g",
299,1,"010612","010612",1171,2014,2050,5924837,"Generic HS2012 to FCL (could also be mapped to 1169) ","Other live animals: mammals: whales, dolphins and porpoises",
299,1,"010619","010619",1171,2015,2050,5924838,"Generic HS2012 to FCL (could also be mapped to 1150,1169)","Live mammals (excluding primates, whales, dolphins and porpoises, manatees and dugongs, seals, sea lions and walruses, camels and other camelids, rabbits and hares, horses, asses, mules, hinnies, bovines, pigs, sheep and goats)",
299,1,"020110","020110",867,2015,2050,5924839,"Generic HS2012 to FCL (could also be mapped to 947) ","Carcases or half-carcases of bovine animals, fresh or chilled",
299,1,"020120","020120",867,2014,2050,5924840,"Generic HS2012 to FCL (could also be mapped to 947) ","Fresh or chilled bovine cuts, with bone in (excluding carcases and 1/2 carcases)",
299,1,"020130","020130",870,2014,2050,5924841,"Generic HS2012 to FCL (could also be mapped to 947) ","Fresh or chilled bovine meat, boneless",
299,1,"020210","020210",867,2015,2050,5924842,"Generic HS2012 to FCL (could also be mapped to 947) ","Frozen bovine carcases and half-carcases",
299,1,"020220","020220",867,2014,2050,5924843,"Generic HS2012 to FCL (could also be mapped to 947)","Frozen bovine cuts, with bone in (excluding carcases and half-carcases)",
299,1,"020230","020230",870,2014,2050,5924844,"Generic HS2012 to FCL (could also be mapped to 947) ","Frozen, boneless meat of bovine animals",
299,1,"020610","020610",868,2014,2050,5924845,"Generic HS2012 to FCL (could also be mapped to 948) ","Fresh or chilled edible offal of bovine animals",
299,1,"020621","020621",868,2014,2050,5924846,"Generic HS2012 to FCL (could also be mapped to 948) ","Frozen edible bovine tongues",
299,1,"020622","020622",868,2015,2050,5924847,"Generic HS2012 to FCL (could also be mapped to 7948)","Frozen edible bovine livers",
299,1,"020629","020629",868,2014,2050,5924848,"Generic HS2012 to FCL (could also be mapped to 948) ","Frozen edible bovine offal (excluding tongues and livers)",
299,1,"020690","020690",978,2014,2050,5924849,"Generic HS2012 to FCL (could also be mapped to 1018,1098,1167) ","Frozen edible offal of sheep, goats, horses, asses, mules and hinnies",
299,1,"020726","020726",1080,2014,2050,5924850,"Generic HS2012 to FCL (could also be mapped to 1081)","Fresh or chilled cuts and edible offal of turkeys of the species domesticus",
299,1,"020727","020727",1080,2014,2050,5924851,"Generic HS2012 to FCL (could also be mapped to 1081) ","Frozen cuts and edible offal of turkeys of the species domesticus",
299,1,"020734","020734",1074,2014,2050,5924852,"Generic HS2007 to FCL (could also be mapped to 1075)","Fresh or chilled edible fatty livers of ducks or geese of the species domesticus",
299,1,"040110","040110",888,2014,2050,5924853,"Generic HS2012 to FCL (could also be mapped to 954,985,1023) ","Milk and cream of a fat content by weight of <= 1%, not concentrated nor containing added sugar or other sweetening matter",
299,1,"040120","040120",882,2014,2050,5924854,"Generic HS2012 to FCL (could also be mapped to 883,908,951,982,1020,1130) ","Milk and cream of a fat content by weight of > 1% but <= 6%, not concentrated nor containing added sugar or other sweetening matter",
299,1,"040299","040299",896,2015,2050,5924855,"Generic HS2012 to FCL (could also be mapped to 889)","Milk and cream, concentrated and sweetened (excluding in solid forms)",
299,1,"040310","040310",891,2014,2050,5924856,"Generic HS2012 to FCL (could also be mapped to 892) ","Yogurt, whether or not flavoured or containing added sugar or other sweetening matter, fruits, nuts or cocoa",
299,1,"040390","040390",893,2014,2050,5924857,"Generic HS2012 to FCL (could also be mapped to 899)","Buttermilk, curdled milk and cream, kephir and other fermented or acidified milk and cream, whether or not concentrated or flavoured or containing added sugar or other sweetening matter, fruits, nuts or cocoa (excluding yogurt)",
299,1,"040410","040410",903,2014,2050,5924858,"Generic HS2012 to FCL (could also be mapped to 890,900)","Whey and modified whey, whether or not concentrated or containing added sugar or other sweetening matter",
299,1,"040510","040510",886,2014,2050,5924859,"Generic HS2012 to FCL (could also be mapped to 952,983,1022) ","Butter (excluding dehydrated butter and ghee)",
299,1,"040520","040520",886,2014,2050,5924860,"Generic HS2012 to FCL (could also be mapped to 952,983,1022)","Dairy spreads of a fat content, by weight, of >= 39% but < 80%",
299,1,"040590","040590",887,2014,2050,5924861,"Generic HS2012 to FCL (could also be mapped to 953,1022)","Fats and oils derived from milk, and dehydrated butter and ghee (excluding natural butter, recombined butter and whey butter)",
299,1,"040610","040610",901,2014,2050,5924862,"Generic HS2012 to FCL (could also be mapped to 904,905,955,984,1021)","Fresh cheese unripened or uncured cheese, incl. whey cheese, and curd",
299,1,"040700","040700",1062,2014,2050,5924863,"Generic HS2007 to FCL (could also be mapped to 1091)","Birds' eggs, in shell, fresh, preserved or cooked",
299,1,"060499","060499",1293,2014,2050,5924864,"Generic HS2007 to FCL unique six-digit match","Foliage, branches and other parts of plants, without flowers or flower buds, grasses, for bouquets or ornamental purposes, dried, dyed, bleached, impregnated or otherwise prepared",
299,1,"070310","070310",403,2014,2050,5924865,"Generic HS2012 to FCL (could also be mapped to 402)","Fresh or chilled onions and shallots",
299,1,"070610","070610",426,2014,2050,5924866,"Generic HS2012 to FCL, FCL descriptions (could also be mapped to 463)","Fresh or chilled carrots and turnips",
299,1,"070820","070820",414,2014,2050,5924867,"Generic HS2012 to FCL (could also be mapped to 423) ","Fresh or chilled beans Vigna spp., Phaseolus spp., shelled or unshelled",
299,1,"070990","070990",463,2014,2050,5924868,"Generic HS2012 to FCL (could also be mapped to 378,430,446) ","Fresh or chilled vegetables (excluding potatoes, tomatoes, vegetables of the Allium spp., cabbages of the genus Brassica, lettuces of the species Lactuca sativa and Cichorium, carrots, turnips, salad beetroot, salsify, celeriac, radishes and similar edible roots, cucumbers and gherkins, leguminous vegetables, asparagus, aubergines, mushrooms, truffles, fruits of the genus Capsicum or of the genus Pimenta, spinach, New Zealand spinach and orache spinach)",
299,1,"070999","070999",463,2015,2050,5924869,"Generic HS2012 to FCL (could also be mapped to 378,430,446) ","Fresh or chilled vegetables n.e.s.",
299,1,"071490","071490",149,2014,2050,5924870,"Generic HS2007 to FCL (could also be mapped to 135,136,137,151) ","Arrowroot, salep, Jerusalem artichokes and similar roots and tubers with high starch or inulin content, fresh, chilled, frozen or dried, whether or not sliced or in the form of pellets, and sago pith (excluding manioc cassava, sweet potatoes, yams, taro and yautia)",
299,1,"080240","080240",220,2014,2050,5924871,"Generic HS2007 to FCL unique six-digit match","Fresh or dried chestnuts (Castanea spp.), whether or not shelled or peeled",
299,1,"080250","080250",223,2014,2050,5924872,"Generic HS2007 to FCL unique six-digit match","Fresh or dried pistachios, whether or not shelled or peeled",
299,1,"080300","080300",486,2014,2050,5924873,"Generic HS2007 to FCL (could also be mapped to 489,604) ","Bananas, incl. plantains, fresh or dried",
299,1,"080420","080420",570,2014,2050,5924874,"Generic HS2012 to FCL (could also be mapped to 569) ","Fresh or dried figs",
299,1,"080450","080450",571,2014,2050,5924875,"Generic HS2012 to FCL (could also be mapped to 603,604)","Fresh or dried guavas, mangoes and mangosteens",
299,1,"080820","080820",521,2014,2050,5924876,"Generic HS2007 to FCL (could also be mapped to 523) ","Fresh pears and quinces",
299,1,"080920","080920",531,2014,2050,5924877,"Generic HS2007 to FCL (could also be mapped to 530,541) ","Fresh cherries",
299,1,"081090","081090",619,2014,2050,5924878,"Generic HS2012 to FCL (could also be mapped to 541,591)","Fresh tamarinds, cashew apples, jackfruit, lychees, sapodillo plums, passion fruit, carambola, pitahaya and other edible fruit (excluding nuts, bananas, dates, figs, pineapples, avocados, guavas, mangoes, mangosteens, papaws papayas, citrus fruit, grapes, melons, apples, pears quinces, apricots, cherries, peaches, plums, sloes, strawberries, raspberries, mulberries, blackberries, loganberries, cranberries, fruits of the genus Vaccinium, kiwifruit, durians, persimmons, black-, white- and redcurrants and gooseberries)",
299,1,"090190","090190",658,2014,2050,5924879,"Generic HS2012 to FCL (could also be mapped to 660) ","Coffee husks and skins; coffee substitutes containing coffee in any proportion",
299,1,"090420","090420",689,2014,2050,5924880,"Generic HS2007 to FCL unique six-digit match","Fruits of the genus Capsicum or of the genus Pimenta, dried or crushed or ground",
299,1,"090500","090500",692,2014,2050,5924881,"Generic HS2007 to FCL unique six-digit match","Vanilla",
299,1,"090700","090700",698,2014,2050,5924882,"Generic HS2007 to FCL unique six-digit match","Cloves, whole fruit, cloves and stems",
299,1,"090810","090810",702,2014,2050,5924883,"Generic HS2007 to FCL unique six-digit match","Nutmeg",
299,1,"090830","090830",702,2014,2050,5924884,"Generic HS2007 to FCL unique six-digit match","Cardamoms",
299,1,"090910","090910",711,2014,2050,5924885,"Generic HS2007 to FCL unique six-digit match","Seeds of anise or badian",
299,1,"090920","090920",711,2014,2050,5924886,"Generic HS2007 to FCL unique six-digit match","Coriander seeds",
299,1,"090930","090930",711,2014,2050,5924887,"Generic HS2007 to FCL unique six-digit match","Cumin seeds",
299,1,"100110","100110",15,2014,2050,5924888,"Generic HS2007 to FCL unique six-digit match","Durum wheat",
299,1,"100190","100190",15,2014,2050,5924889,"Generic HS2007 to FCL unique six-digit match","Wheat and meslin (excluding durum wheat)",
299,1,"100300","100300",44,2014,2050,5924890,"Generic HS2007 to FCL unique six-digit match","Barley",
299,1,"100400","100400",75,2014,2050,5924891,"Generic HS2007 to FCL unique six-digit match","Oats",
299,1,"100630","100630",31,2014,2050,5924892,"Generic HS2012 to FCL (could also be mapped to 29) ","Semi-milled or wholly milled rice, whether or not polished or glazed",
299,1,"100890","100890",108,2014,2050,5924893,"Generic HS2012 to FCL (could also be mapped to 103)","Cereals (excluding wheat and meslin, rye, barley, oats, maize, rice, grain sorghum, buckwheat, millet, canary seeds, fonio, quinoa and triticale)",
299,1,"110290","110290",111,2014,2050,5924894,"Generic HS2012 to FCL (could also be mapped to 38,48,72,80,84,90,95,98,104)","Cereal flours (excluding wheat, meslin and maize)",
299,1,"110319","110319",111,2014,2050,5924895,"Generic HS2012 to FCL (could also be mapped to 38,48,72,80,84,90,95,98,104)","Groats and meal of cereals (excluding wheat and maize)",
299,1,"110419","110419",113,2014,2050,5924896,"Generic HS2012 to FCL (could also be mapped to 45,46)","Rolled or flaked grains of cereals (excluding oats)",
299,1,"110429","110429",113,2014,2050,5924897,"Generic HS2012 to FCL (could also be mapped to 21,45,46) ","Grains of cereals, hulled, pearled, sliced, kibbled or otherwise worked (excluding rolled, flaked, flour, pellets, and oats and maize, and husked and semi- or wholly milled rice and broken rice)",
299,1,"110430","110430",19,2014,2050,5924898,"Generic HS2012 to FCL (could also be mapped to 57)","Germ of cereals, whole, rolled, flaked or ground",
299,1,"120100","120100",236,2014,2050,5924899,"Generic HS2007 to FCL unique six-digit match","Soya beans, whether or not broken",
299,1,"120210","120210",242,2014,2050,5924900,"Generic HS2007 to FCL unique six-digit match","Groundnuts in shell, not roasted or otherwise cooked",
299,1,"120220","120220",243,2014,2050,5924901,"Generic HS2007 to FCL unique six-digit match","Shelled groundnuts, whether or not broken (excluding roasted or otherwise cooked)",
299,1,"150200","150200",869,2014,2050,5924902,"Generic HS2007 to FCL (could also be mapped to 871,949,979,1019,1225) ","Fats of bovine animals, sheep or goats (excluding lard stearin, lard oil, oleostearin, oleooil and tallow oil, not emulsified or mixed or otherwise prepared)",
299,1,"150290","150290",869,2015,2050,5924903,"Generic HS2012 to FCL (could also be mapped to 871,949,979,1019) ","Fats of bovine animals, sheep or goats (excluding tallow, oleostearin and oleo-oil)",
299,1,"150600","150600",1168,2014,2050,5924904,"Generic HS2012 to FCL (could also be mapped to 1129,1160) ","Other animal fats and oils and their fractions, whether or not refined, but not chemically modified (excluding pig fat, poultry fat, fats of bovine animals, sheep and goats, fats of fish and other marine animals, lard stearin, lard oil, oloestearin, oleo-oil, tallow oil, wool grease and fatty substances derived therefrom)",
299,1,"151211","151211",268,2014,2050,5924905,"Generic HS2012 to FCL (could also be mapped to 281)","Crude sunflower-seed or safflower oil",
299,1,"151219","151219",268,2014,2050,5924906,"Generic HS2012 to FCL (could also be mapped to 281)","Sunflower-seed or safflower oil and their fractions, whether or not refined, but not chemically modified (excluding crude)",
299,1,"151419","151419",271,2014,2050,5924907,"Generic HS2012 to FCL (could also be mapped to 293) ","Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2% and its fractions, whether or not refined, but not chemically modified (excluding crude)",
299,1,"151499","151499",271,2014,2050,5924908,"Generic HS2012 to FCL (could also be mapped to 293)","High erucic acid rape or colza oil fixed oil which has an erucic acid content of >= 2%, and mustard oil, and fractions thereof, whether or not refined, but not chemically modified (excluding crude)",
299,1,"151590","151590",340,2014,2050,5924909,"Generic HS2012 to FCL (could also be mapped to 36,264,278,297,306,307,313,337) ","Fixed vegetable fats and oils and their fractions, whether or not refined, but not chemically modified (excluding soya-bean, groundnut, olive, palm, sunflower-seed, safflower, cotton-seed, coconut, palm kernel, babassu, rape, colza and mustard, linseed, maize, castor and sesame oil)",
299,1,"151620","151620",1275,2014,2050,5924910,"Generic HS2012 to FCL (could also be mapped to 1273)","Vegetable fats and oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared",
299,1,"151790","151790",1243,2014,2050,5924911,"Generic HS2012 to FCL (could also be mapped to 1241) ","Edible mixtures or preparations of animal or vegetable fats or oils and edible fractions of different fats or oils (excluding fats, oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared, mixtures of olive oils and their fractions, and solid margarine)",
299,1,"152190","152190",1183,2014,2050,5924912,"Generic HS2012 to FCL (could also be mapped to 1295)","Beeswax, other insect waxes and spermaceti, whether or not refined or coloured",
299,1,"160100","160100",874,2014,2050,5924913,"Generic HS2012 to FCL (could also be mapped to 1041)","Sausages and similar products, of meat, offal or blood; food preparations based on these products",
299,1,"170111","170111",162,2014,2050,5924914,"Generic HS2007 to FCL (could also be mapped to 163)","Raw cane sugar (excluding added flavouring or colouring)",
299,1,"170112","170112",162,2014,2050,5924915,"Generic HS2012 to FCL (could also be mapped to 163) ","Raw beet sugar (excluding added flavouring or colouring)",
299,1,"170114","170114",162,2015,2050,5924916,"Generic HS2012 to FCL (could also be mapped to 163)","Raw cane sugar, in solid form, not containing added flavouring or colouring matter (excluding cane sugar of 1701 13)",
299,1,"170290","170290",167,2014,2050,5924917,"Generic HS2012 to FCL (could also be mapped to 155,175)","Sugars in solid form, incl. invert sugar and chemically pure maltose, and sugar and sugar syrup blends containing in the dry state 50% by weight of fructose, not flavoured or coloured, artificial honey, whether or not mixed with natural honey and caramel (excluding cane or beet sugar, chemically pure sucrose, lactose, maple sugar, glucose, fructose, and syrups thereof)",
299,1,"190190","190190",115,2014,2050,5924918,"Generic HS2012 to FCL (could also be mapped to 50) ","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905)",
299,1,"190590","190590",22,2014,2050,5924919,"Generic HS2012 to FCL (could also be mapped to 110)","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products (excluding crispbread, gingerbread and the like, sweet biscuits, waffles, wafers not mentioned, rusks, toasted bread and similar toasted products)",
299,1,"200190","200190",262,2014,2050,5924920,"Generic HS2012 to FCL (could also be mapped to 471)","Vegetables, fruit, nuts and other edible parts of plants, prepared or preserved by vinegar or acetic acid (excluding cucumbers and gherkins)",
299,1,"200320","200320",451,2014,2050,5924921,"Generic HS2007 to FCL unique six-digit match","Truffles, prepared or preserved otherwise than by vinegar or acetic acid",
299,1,"200490","200490",475,2014,2050,5924922,"Generic HS2012 to FCL (could also be mapped to 447,262)","Vegetables and mixtures of vegetables, prepared or preserved otherwise than by vinegar or acetic acid, frozen (excluding preserved by sugar, and tomatoes, mushrooms, truffles and potatoes, unmixed)",
299,1,"200811","200811",246,2014,2050,5924923,"Generic HS2012 to FCL (could also be mapped to 247)","Groundnuts, prepared or preserved (excluding preserved with sugar)",
299,1,"200899","200899",623,2014,2050,5924924,"Generic HS2012 to FCL (could also be mapped to 466,538,539,593) ","Fruit and other edible parts of plants, prepared or preserved, whether or not containing added sugar or other sweetening matter or spirit (excluding prepared or preserved with vinegar, preserved with sugar but not laid in syrup, jams, fruit jellies, marmalades, fruit purée and pastes, obtained by cooking, and nuts, groundnuts and other seeds, pineapples, citrus fruits, pears, apricots, cherries, peaches, strawberries, palm hearts and cranberries)",
299,1,"200911","200911",492,2014,2050,5924925,"Generic HS2012 to FCL (could also be mapped to 491) ","Frozen orange juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
299,1,"200931","200931",513,2014,2050,5924926,"Generic HS2012 to FCL (could also be mapped to 496,498)","Single citrus fruit juice, unfermented, Brix value <= 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
299,1,"200950","200950",390,2014,2050,5924927,"Generic HS2012 to FCL (could also be mapped to 390)","Tomato juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
299,1,"200980","200980",622,2014,2050,5924928,"Generic HS2012 to FCL (could also be mapped to 466,538,539,593) ","Juice of fruit or vegetables, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, and juice of citrus fruit, pineapples, tomatoes, grapes, incl. grape must and apples)",
299,1,"200989","200989",622,2015,2050,5924929,"Generic HS2012 to FCL (could also be mapped to 466,538,539,583)","Juice of fruit or vegetables, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, and juice of citrus fruit, pineapples, tomatoes, grapes, incl. grape must, apples and cranberries)",
299,1,"200990","200990",622,2014,2050,5924930,"Generic HS2012 to FCL (could also be mapped to 466)","Mixtures of fruit juices, incl. grape must, and vegetable juices, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
299,1,"210390","210390",1232,2014,2050,5924931,"Generic HS2012 to FCL (could also be mapped to 240) ","Preparations for sauces and prepared sauces; mixed condiments and seasonings (excluding soya sauce, tomato ketchup and other tomato sauces, mustard, and mustard flour and meal)",
299,1,"210610","210610",1232,2014,2050,5924932,"Generic HS2012 to FCL (could also be mapped to 240,241)","Protein concentrates and textured protein substances",
299,1,"210690","210690",1232,2014,2050,5924933,"Generic HS2012 to FCL (could also be mapped to 674) ","Food preparations, n.e.s.",
299,1,"220300","220300",51,2014,2050,5924934,"Generic HS2012 to FCL (could also be mapped to 66,82,86) ","Beer made from malt",
299,1,"220600","220600",517,2014,2050,5924935,"Generic HS2012 to FCL (could also be mapped to 26,39)","Cider, perry, mead and other fermented beverages and mixtures of fermented beverages and non-alcoholic beverages, n.e.s. (excluding beer, wine or fresh grapes, grape must, vermouth and other wine of fresh grapes flavoured with plants or aromatic substances)",
299,1,"230990","230990",845,2014,2050,5924936,"Generic HS2012 to FCL (could also be mapped to 653,840,841,842,843,849,850,851,852,853,854,855,1259)","Preparations of a kind used in animal feeding (excluding dog or cat food put up for retail sale)",
299,1,"240310","240310",831,2014,2050,5924937,"Generic HS2007 to FCL unique six-digit match","Smoking tobacco, whether or not containing tobacco substitutes in any proportion",
299,1,"330129","330129",753,2014,2050,5924938,"Generic HS2012 to FCL (could also be mapped to 737)","Essential oils, whether or not terpeneless, incl. concretes and absolutes (excluding those of citrus fruit and mint)",
299,1,"410120","410120",920,2014,2050,5924939,"Generic HS2012 to FCL (could also be mapped to 921,922,928,929,930,958,959,1103,1104,1105)","Whole raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired, unsplit, of a weight per skin <= 8 kg when simply dried, <= 10 kg when dry-salted, or <= 16 kg when fresh, wet-salted or otherwise preserved (excluding tanned, parchment-dressed or further prepared)",
299,1,"510529","510529",1008,2015,2050,5924940,"Generic HS2012 to FCL (could also be mapped to 1010) ","Wool, combed (excluding that in fragments open tops)",
299,1,"530500","530500",809,2014,2050,5924941,"Generic HS2012 to FCL (could also be mapped to 821)","Coconut, abaca Manila hemp or Musa textilis Nee, ramie, agave and other vegetable textile fibres, n.e.s., raw or processed, but not spun; tow, noils and waste of such fibres, incl. yarn waste and garnetted stock",
299,2,"020120","020120",867,2015,2050,5924942,"Generic HS2012 to FCL (could also be mapped to 947) ","Fresh or chilled bovine cuts, with bone in (excluding carcases and 1/2 carcases)",
299,2,"020130","020130",870,2015,2050,5924943,"Generic HS2012 to FCL (could also be mapped to 947) ","Fresh or chilled bovine meat, boneless",
299,2,"020220","020220",867,2014,2050,5924944,"Generic HS2012 to FCL (could also be mapped to 947)","Frozen bovine cuts, with bone in (excluding carcases and half-carcases)",
299,2,"020727","020727",1080,2014,2050,5924945,"Generic HS2012 to FCL (could also be mapped to 1081) ","Frozen cuts and edible offal of turkeys of the species domesticus",
299,2,"040110","040110",888,2014,2050,5924946,"Generic HS2012 to FCL (could also be mapped to 954,985,1023) ","Milk and cream of a fat content by weight of <= 1%, not concentrated nor containing added sugar or other sweetening matter",
299,2,"040120","040120",882,2014,2050,5924947,"Generic HS2012 to FCL (could also be mapped to 883,908,951,982,1020,1130) ","Milk and cream of a fat content by weight of > 1% but <= 6%, not concentrated nor containing added sugar or other sweetening matter",
299,2,"040310","040310",891,2014,2050,5924948,"Generic HS2012 to FCL (could also be mapped to 892) ","Yogurt, whether or not flavoured or containing added sugar or other sweetening matter, fruits, nuts or cocoa",
299,2,"040390","040390",893,2014,2050,5924949,"Generic HS2012 to FCL (could also be mapped to 899)","Buttermilk, curdled milk and cream, kephir and other fermented or acidified milk and cream, whether or not concentrated or flavoured or containing added sugar or other sweetening matter, fruits, nuts or cocoa (excluding yogurt)",
299,2,"040510","040510",886,2014,2050,5924950,"Generic HS2012 to FCL (could also be mapped to 952,983,1022)","Butter (excluding dehydrated butter and ghee)",
299,2,"040590","040590",887,2014,2050,5924951,"Generic HS2012 to FCL (could also be mapped to 953,1022)","Fats and oils derived from milk, and dehydrated butter and ghee (excluding natural butter, recombined butter and whey butter)",
299,2,"040610","040610",901,2014,2050,5924952,"Generic HS2012 to FCL (could also be mapped to 904,905,955,984,1021)","Fresh cheese unripened or uncured cheese, incl. whey cheese, and curd",
299,2,"040700","040700",1062,2014,2050,5924953,"Generic HS2007 to FCL (could also be mapped to 1091)","Birds' eggs, in shell, fresh, preserved or cooked",
299,2,"060491","060491",1293,2014,2050,5924954,"Generic HS2007 to FCL unique six-digit match","Foliage, branches and other parts of plants, without flowers or flower buds, grasses, fresh, for bouquets or ornamental purposes",
299,2,"070310","070310",403,2014,2050,5924955,"Generic HS2012 to FCL (could also be mapped to 402)","Fresh or chilled onions and shallots",
299,2,"070610","070610",426,2014,2050,5924956,"Generic HS2012 to FCL, FCL descriptions (could also be mapped to 463)","Fresh or chilled carrots and turnips",
299,2,"070820","070820",414,2014,2050,5924957,"Generic HS2012 to FCL (could also be mapped to 423) ","Fresh or chilled beans Vigna spp., Phaseolus spp., shelled or unshelled",
299,2,"070990","070990",463,2014,2050,5924958,"Generic HS2007 to FCL (could also be mapped to 260,378,394,430,446)","Fresh or chilled vegetables (excluding potatoes, tomatoes, vegetables of the Allium spp., cabbages of the genus Brassica, lettuces of the species Lactuca sativa and Cichorium, carrots, turnips, salad beetroot, salsify, celeriac, radishes and similar edible roots, cucumbers and gherkins, leguminous vegetables, asparagus, aubergines, mushrooms, truffles, fruits of the genus Capsicum or of the genus Pimenta, spinach, New Zealand spinach and orache spinach)",
299,2,"070999","070999",463,2015,2050,5924959,"Generic HS2012 to FCL (could also be mapped to 378,430,446) ","Fresh or chilled vegetables n.e.s.",
299,2,"080300","080300",486,2014,2050,5924960,"Generic HS2007 to FCL (could also be mapped to 489,604) ","Bananas, incl. plantains, fresh or dried",
299,2,"080420","080420",570,2014,2050,5924961,"Generic HS2012 to FCL (could also be mapped to 569) ","Fresh or dried figs",
299,2,"080450","080450",571,2014,2050,5924962,"Generic HS2012 to FCL (could also be mapped to 603,604)","Fresh or dried guavas, mangoes and mangosteens",
299,2,"080820","080820",523,2014,2050,5924963,"Generic HS2007 to FCL unique six-digit match","Fresh pears and quinces",
299,2,"080920","080920",531,2014,2050,5924964,"Generic HS2007 to FCL (could also be mapped to 530,541) ","Fresh cherries",
299,2,"081090","081090",619,2014,2050,5924965,"Generic HS2012 to FCL (could also be mapped to 541,591) ","Fresh tamarinds, cashew apples, jackfruit, lychees, sapodillo plums, passion fruit, carambola, pitahaya and other edible fruit (excluding nuts, bananas, dates, figs, pineapples, avocados, guavas, mangoes, mangosteens, papaws papayas, citrus fruit, grapes, melons, apples, pears quinces, apricots, cherries, peaches, plums, sloes, strawberries, raspberries, mulberries, blackberries, loganberries, cranberries, fruits of the genus Vaccinium, kiwifruit, durians, persimmons, black-, white- and redcurrants and gooseberries)",
299,2,"090190","090190",658,2014,2050,5924966,"Generic HS2012 to FCL (could also be mapped to 660) ","Coffee husks and skins; coffee substitutes containing coffee in any proportion",
299,2,"090500","090500",692,2014,2050,5924967,"Generic HS2007 to FCL unique six-digit match","Vanilla",
299,2,"090700","090700",698,2014,2050,5924968,"Generic HS2007 to FCL unique six-digit match","Cloves, whole fruit, cloves and stems",
299,2,"090810","090810",702,2014,2050,5924969,"Generic HS2007 to FCL unique six-digit match","Nutmeg",
299,2,"090830","090830",702,2014,2050,5924970,"Generic HS2007 to FCL unique six-digit match","Cardamoms",
299,2,"090910","090910",711,2014,2050,5924971,"Generic HS2007 to FCL unique six-digit match","Seeds of anise or badian",
299,2,"090920","090920",711,2014,2050,5924972,"Generic HS2007 to FCL unique six-digit match","Coriander seeds",
299,2,"090930","090930",711,2014,2050,5924973,"Generic HS2007 to FCL unique six-digit match","Cumin seeds",
299,2,"090940","090940",711,2014,2050,5924974,"Generic HS2007 to FCL unique six-digit match","Caraway seeds",
299,2,"100110","100110",15,2014,2050,5924975,"Generic HS2007 to FCL unique six-digit match","Durum wheat",
299,2,"100300","100300",44,2014,2050,5924976,"Generic HS2007 to FCL unique six-digit match","Barley",
299,2,"100400","100400",75,2014,2050,5924977,"Generic HS2007 to FCL unique six-digit match","Oats",
299,2,"100630","100630",31,2014,2050,5924978,"Generic HS2012 to FCL (could also be mapped to 29) ","Semi-milled or wholly milled rice, whether or not polished or glazed",
299,2,"100890","100890",108,2014,2050,5924979,"Generic HS2012 to FCL (could also be mapped to 103)","Cereals (excluding wheat and meslin, rye, barley, oats, maize, rice, grain sorghum, buckwheat, millet, canary seeds, fonio, quinoa and triticale)",
299,2,"110419","110419",113,2014,2050,5924980,"Generic HS2012 to FCL (could also be mapped to 45,46)","Rolled or flaked grains of cereals (excluding oats)",
299,2,"110429","110429",113,2014,2050,5924981,"Generic HS2012 to FCL (could also be mapped to 21,45,46)","Grains of cereals, hulled, pearled, sliced, kibbled or otherwise worked (excluding rolled, flaked, flour, pellets, and oats and maize, and husked and semi- or wholly milled rice and broken rice)",
299,2,"120210","120210",242,2014,2050,5924982,"Generic HS2007 to FCL unique six-digit match","Groundnuts in shell, not roasted or otherwise cooked",
299,2,"120220","120220",243,2014,2050,5924983,"Generic HS2007 to FCL unique six-digit match","Shelled groundnuts, whether or not broken (excluding roasted or otherwise cooked)",
299,2,"120799","120799",399,2014,2050,5924984,"Generic HS2012 to FCL (could also be mapped to 263,275,277,305,311,312,336)","Oil seeds and oleaginous fruits, whether or not broken (excluding edible nuts, olives, soya beans, groundnuts, copra, linseed, rape or colza seeds, sunflower seeds, palm nuts and kernels, cotton, castor oil, sesamum, mustard, safflower, melon and poppy seeds)",
299,2,"151219","151219",268,2014,2050,5924985,"Generic HS2012 to FCL (could also be mapped to 281)","Sunflower-seed or safflower oil and their fractions, whether or not refined, but not chemically modified (excluding crude)",
299,2,"151419","151419",271,2014,2050,5924986,"Generic HS2012 to FCL (could also be mapped to 293)","Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2% and its fractions, whether or not refined, but not chemically modified (excluding crude)",
299,2,"151590","151590",340,2014,2050,5924987,"Generic HS2012 to FCL (could also be mapped to 36,264,278,297,306,307,313,337) ","Fixed vegetable fats and oils and their fractions, whether or not refined, but not chemically modified (excluding soya-bean, groundnut, olive, palm, sunflower-seed, safflower, cotton-seed, coconut, palm kernel, babassu, rape, colza and mustard, linseed, maize, castor and sesame oil)",
299,2,"151790","151790",1243,2015,2050,5924988,"Generic HS2012 to FCL (could also be mapped to 1241)","Edible mixtures or preparations of animal or vegetable fats or oils and edible fractions of different fats or oils (excluding fats, oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared, mixtures of olive oils and their fractions, and solid margarine)",
299,2,"152190","152190",1183,2014,2050,5924989,"Generic HS2012 to FCL (could also be mapped to 1295)","Beeswax, other insect waxes and spermaceti, whether or not refined or coloured",
299,2,"160100","160100",874,2014,2050,5924990,"Generic HS2012 to FCL (could also be mapped to 1041)","Sausages and similar products, of meat, offal or blood; food preparations based on these products",
299,2,"170290","170290",167,2014,2050,5924991,"Generic HS2012 to FCL (could also be mapped to 155,175)","Sugars in solid form, incl. invert sugar and chemically pure maltose, and sugar and sugar syrup blends containing in the dry state 50% by weight of fructose, not flavoured or coloured, artificial honey, whether or not mixed with natural honey and caramel (excluding cane or beet sugar, chemically pure sucrose, lactose, maple sugar, glucose, fructose, and syrups thereof)",
299,2,"190590","190590",22,2014,2050,5924992,"Generic HS2012 to FCL (could also be mapped to 110)","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products (excluding crispbread, gingerbread and the like, sweet biscuits, waffles, wafers not mentioned, rusks, toasted bread and similar toasted products)",
299,2,"200320","200320",451,2014,2050,5924993,"Generic HS2007 to FCL unique six-digit match","Truffles, prepared or preserved otherwise than by vinegar or acetic acid",
299,2,"200490","200490",475,2014,2050,5924994,"Generic HS2012 to FCL (could also be mapped to 447,262)","Vegetables and mixtures of vegetables, prepared or preserved otherwise than by vinegar or acetic acid, frozen (excluding preserved by sugar, and tomatoes, mushrooms, truffles and potatoes, unmixed)",
299,2,"200811","200811",246,2014,2050,5924995,"Generic HS2012 to FCL (could also be mapped to 247)","Groundnuts, prepared or preserved (excluding preserved with sugar)",
299,2,"200899","200899",623,2014,2050,5924996,"Generic HS2012 to FCL (could also be mapped to 584)","Fruit and other edible parts of plants, prepared or preserved, whether or not containing added sugar or other sweetening matter or spirit (excluding prepared or preserved with vinegar, preserved with sugar but not laid in syrup, jams, fruit jellies, marmalades, fruit purée and pastes, obtained by cooking, and nuts, groundnuts and other seeds, pineapples, citrus fruits, pears, apricots, cherries, peaches, strawberries, palm hearts and cranberries)",
299,2,"200931","200931",513,2014,2050,5924997,"Generic HS2012 to FCL (could also be mapped to 496,498)","Single citrus fruit juice, unfermented, Brix value <= 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
299,2,"200980","200980",622,2014,2050,5924998,"Generic HS2007 to FCL (could also be mapped to 466,538,539,583)","Juice of fruit or vegetables, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, and juice of citrus fruit, pineapples, tomatoes, grapes, incl. grape must and apples)",
299,2,"210390","210390",1232,2014,2050,5924999,"Generic HS2012 to FCL (could also be mapped to 163) ","Preparations for sauces and prepared sauces; mixed condiments and seasonings (excluding soya sauce, tomato ketchup and other tomato sauces, mustard, and mustard flour and meal)",
299,2,"210690","210690",1232,2014,2050,5925000,"Generic HS2012 to FCL (could also be mapped to 674) ","Food preparations, n.e.s.",
299,2,"220300","220300",51,2014,2050,5925001,"Generic HS2012 to FCL (could also be mapped to 66,82,86) ","Beer made from malt",
299,2,"220600","220600",517,2014,2050,5925002,"Generic HS2012 to FCL (could also be mapped to 26,39)","Cider, perry, mead and other fermented beverages and mixtures of fermented beverages and non-alcoholic beverages, n.e.s. (excluding beer, wine or fresh grapes, grape must, vermouth and other wine of fresh grapes flavoured with plants or aromatic substances)",
299,2,"230990","230990",845,2014,2050,5925003,"Generic HS2012 to FCL (could also be mapped to 653,840,841,842,843,849,850,851,852,853,854,855,1259)","Preparations of a kind used in animal feeding (excluding dog or cat food put up for retail sale)",
299,2,"330129","330129",753,2014,2050,5925004,"Generic HS2012 to FCL (could also be mapped to 948)","Essential oils, whether or not terpeneless, incl. concretes and absolutes (excluding those of citrus fruit and mint)",
299,2,"410120","410120",920,2014,2050,5925005,"Generic HS2012 to FCL (could also be mapped to 921,922,928,929,930,958,959,1103,1104,1105)","Whole raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired, unsplit, of a weight per skin <= 8 kg when simply dried, <= 10 kg when dry-salted, or <= 16 kg when fresh, wet-salted or otherwise preserved (excluding tanned, parchment-dressed or further prepared)",
7,1,"23070000","23070000",654,2007,2050,5924005,"Standard_HS12","FaoStatName: FOOD WASTES",
7,1,"23099000","23099000",654,2007,2050,5924008,"Country TL description (WITS), Old SWS series (could also be mapped to 653,840,841,842,843,845,849,850,851,852,853,854,855,1259)","Preparations of a kind used in animal feeding (excl. dog or cat food put up for retail sale)"
)

unmapped_codes <- select(unmapped_codes, -details, -tl_description)

unmapped_codes$recordnumb <- max(hsfclmap3$recordnumb) + 1:nrow(unmapped_codes)

hsfclmap3 <- bind_rows(unmapped_codes, hsfclmap3) %>%
  mutate(
         startyear = as.integer(startyear),
         endyear = as.integer(endyear)
         )

# END NEW MAPPING FOR REMAINING COUNTRIES



flog.info("HS->FCL mapping table preview:",
          rprt_glimpse0(hsfclmap3), capture = TRUE)

rprt(hsfclmap3, "hsfclmap", year)

hsfclmap <- hsfclmap3 %>%
  filter_(~startyear <= year &
            endyear >= year) %>%
  select_(~-startyear, ~-endyear)

# Workaround issue #123
hsfclmap <- hsfclmap %>%
  mutate_at(vars(ends_with("code")),
                 funs(num = as.numeric)) %>%
  mutate_(fromgtto = ~fromcode_num > tocode_num) %>%
  select(-ends_with("code_num"))

from_gt_to <- hsfclmap$recordnumb[hsfclmap$fromgtto]

if(length(from_gt_to) > 0)
  flog.warn(paste0("In following records of hsfclmap fromcode greater than tocode: ",
                 paste0(from_gt_to, collapse = ", ")))

hsfclmap <- hsfclmap %>%
  filter_(~!fromgtto) %>%
  select_(~-fromgtto)



stopifnot(nrow(hsfclmap) > 0)

flog.info("Rows in mapping table after filtering by year: %s",
          nrow(hsfclmap))

if(use_adjustments) {

  ##' - `adjustments`: Adjustment notes containing manually added conversion
  ##' factors to transform from non-standard units of measurement to standard
  ##' ones or to obtain quantities from traded values.

  ## Old precedure
  #data("adjustments", package = "hsfclmap", envir = environment())
  ## New procedure
  message(sprintf("[%s] Reading in adjustments", PID))

  adjustments <- tbl_df(ReadDatatable("adjustments"))
  colnames(adjustments) <- sapply(colnames(adjustments),
                                  function(x) gsub("adj_","",x))
  adj_cols_int <- c("year","flow","fcl","partner","reporter")
  adj_cols_dbl <- c("hs")
  adjustments <- adjustments %>%
    mutate_each_(funs(as.integer),adj_cols_int) %>%
    mutate_each_(funs(as.double),adj_cols_dbl)
}

##' - `unsdpartnersblocks`: UNSD Tariffline reporter and partner dimensions use
##' different list of geographic are codes. The partner dimesion is more
##' detailed than the reporter dimension. Since we can not split trade flows of
##' the reporter dimension, trade flows of the corresponding partner dimensions
##' have to be assigned the reporter dimension's geographic area code. For
##' example, the code 842 is used for the United States includes Virgin Islands
##' and Puerto Rico and thus the reported trade flows of those territories.
##' Analogous steps are taken for France, Italy, Norway, Switzerland and US
##' Minor Outlying Islands.

data("unsdpartnersblocks", package = "faoswsTrade", envir = environment())
#unsdpartnersblocks <- tbl_df(ReadDatatable("unsdpartnersblocks"))

##' - `fclunits`: For UNSD Tariffline units of measurement are converted to
##' meet FAO standards. According to FAO standard, all weights are reported in
##' tonnes, animals in heads or 1000 heads and for certain commodities,
##' only the value is provided.

data("fclunits", package = "faoswsTrade", envir = environment())
#fclunits <- tbl_df(ReadDatatable("fclunits"))

##' - `comtradeunits`: Translation of the `qunit` variable (supplementary
##' quantity units) in Tariffline data into intelligible unit of measurement,
##' which correspond to bthe standards of quantity recommended by the *World
##' Customs Organization* (WCO) (e.g., `qunit`=8 correspond to *kg*).
##' See: http://unstats.un.org/unsd/tradekb/Knowledgebase/UN-Comtrade-Reference-Tables

data("comtradeunits", package = "faoswsTrade", envir = environment())
#comtradeunits <- tbl_df(ReadDatatable("comtradeunits"))

##' - `EURconversionUSD`: Annual EUR/USD currency exchange rates table from SWS.

EURconversionUSD <- ReadDatatable("eur_conversion_usd")

# hs6fclmap ####

flog.trace("Extraction of HS6 mapping table", name = "dev")
flog.trace("Universal (all years) HS6 mapping table", name = "dev")
hs6fclmap_full <- extract_hs6fclmap(hsfclmap3, parallel = multicore)
flog.trace("Current year specific HS6 mapping table", name = "dev")
hs6fclmap_year <- extract_hs6fclmap(hsfclmap, parallel = multicore)
hs6fclmap <- bind_rows(hs6fclmap_full, hs6fclmap_year) %>%
  filter_(~fcl_links == 1L) %>%
  distinct()

rprt(hs6fclmap, "hs6fclmap")

# EUROSTAT DATA  -----------------
##' # Extract Eurostat Combined Nomenclature Data

##+ es-extract
##  Download ES data ===============

##' 1. Download raw data from SWS, filtering by `hs_chapters`.

flog.info(toupper("##### Eurostat trade data #####"))

if(!is.null(samplesize)) {
  esdata <- sample_n(esdata, samplesize)
  warning(sprintf("Eurostat data was sampled with size %d", samplesize))
}

# Fiter out HS codes which don't participate in futher processing
# Such solution drops all HS codes shorter than 6 digits.

esdata <- filterHS6FAOinterest(esdata)

##' 1. Add variables that will contain flags.

esdata <- generateFlagVars(data = esdata)

##' 1. Generate Observation Status X flag.
esdata <- esdata %>%
  setFlag3(!is.na(value),  type = 'status', flag = 'X', variable = 'value') %>%
  setFlag3(!is.na(weight), type = 'status', flag = 'X', variable = 'weight') %>%
  setFlag3(!is.na(qty),    type = 'status', flag = 'X', variable = 'quantity') %>%
  setFlag3(!is.na(value),  type = 'method', flag = 'h', variable = 'value') %>%
  setFlag3(!is.na(weight), type = 'method', flag = 'h', variable = 'weight') %>%
  setFlag3(!is.na(qty),    type = 'method', flag = 'h', variable = 'quantity')


##' 1. Remove code 252

esdata <- esdata %>%
  filter(partner != 252)

flog.info("Records after removing partners' 252 code: %s", nrow(esdata))

##' 1. Remove reporters with area codes that are not included in MDB commodity
##' mapping area list.

##+ es-treat-unmapped
esdata_not_area_in_fcl_mapping <- esdata %>%
  filter_(~!(reporter %in% unique(hsfclmap$area)))

rprt_writetable(esdata_not_area_in_fcl_mapping)

esdata <- esdata %>%
  filter_(~reporter %in% unique(hsfclmap$area))

flog.info("Records after removing areas absent in HS->FCL map: %s",
          nrow(esdata))

# ES trade data mapping to FCL ####
message(sprintf("[%s] Convert Eurostat HS to FCL", PID))

##' 1. Map HS to FCL.

esdatahs6links <- mapHS6toFCL(esdata, hs6fclmap)

esdatalinks <- mapHS2FCL(tradedata = esdata,
                         maptable = hsfclmap3,
                         hs6maptable = hs6fclmap,
                         year = year,
                         parallel = multicore)

esdata <- add_fcls_from_links(esdata,
                              hs6links = esdatahs6links,
                              links = esdatalinks)

flog.info("Records after HS-FCL mapping: %s",
          nrow(esdata))

rprt(esdata, "hs2fcl_fulldata", tradedataname = "esdata")

##' 1. Remove unmapped FCL codes.

esdata <- esdata %>%
  filter_(~!(is.na(fcl)))

flog.info("ES records after removing non-mapped HS codes: %s",
          nrow(esdata))

##' 1. Add FCL units.

esdata <- addFCLunits(tradedata = esdata, fclunits = fclunits)

##' 1. Specific ES conversions: some FCL codes are reported in Eurostat
##' with different supplementary units than those reported in FAOSTAT,
##' thus a conversion is done.

## specific supplementary unit conversion
es_spec_conv <- frame_data(
  ~fcl, ~conv,
  1057L, 0.001,
  1068L, 0.001,
  1072L, 0.001,
  1079L, 0.001,
  1083L, 0.001,
  1140L, 0.001,
  1181L, 1000
)

esdata <- esdata %>%
  left_join(es_spec_conv, by = 'fcl') %>%
  mutate_(qty = ~ifelse(is.na(conv), qty, qty*conv)) %>%
  setFlag3(!is.na(conv), type = 'method', flag = 'i', variable = 'quantity') %>%
  select_(~-conv)


# TARIFFLINE DATA ####
##' # Extract UNSD Tariffline Data

##+ tradeload

##' 1. Download raw data from SWS, filtering by `hs_chapters`.

if(!is.null(samplesize)) {
  tldata <- sample_n(tldata, samplesize)
  warning(sprintf("Tariffline data was sampled with size %d", samplesize))
}

# Convert qunit 6, 9, and 11 to 5 (mathematical conversion)
tldata <- as.data.table(tldata)
tldata[qunit ==  6, c('qty', 'qunit') := list(   qty*2, 5)]
tldata[qunit ==  9, c('qty', 'qunit') := list(qty*1000, 5)]
tldata[qunit == 11, c('qty', 'qunit') := list(  qty*12, 5)]
tldata <- tbl_df(tldata)

# tl-aggregate-multiple-rows ####

##' 1. Identical combinations of reporter / partner / commodity / flow / year / qunit
##' are aggregated.

flog.trace("TL: aggreation of similar flows", name = "dev")

tldata <- preAggregateMultipleTLRows(tldata)

##' 1. Add variables that will contain flags.
flog.trace("TL: add flag variables")
tldata <- generateFlagVars(data = tldata)

tldata <- tldata %>%
  setFlag3(nrows > 1, type = 'method', flag = 's', variable = 'all')

tldata <- filterHS6FAOinterest(tldata)

##' 1. Generate Observation Status X flag.
tldata <- tldata %>%
  setFlag3(!is.na(value),  type = 'status', flag = 'X', variable = 'value') %>%
  setFlag3(!is.na(weight), type = 'status', flag = 'X', variable = 'weight') %>%
  setFlag3(!is.na(qty),    type = 'status', flag = 'X', variable = 'quantity') %>%
  setFlag3(!is.na(value),  type = 'method', flag = 'h', variable = 'value') %>%
  setFlag3(!is.na(weight), type = 'method', flag = 'h', variable = 'weight') %>%
  setFlag3(!is.na(qty),    type = 'method', flag = 'h', variable = 'quantity')


##+ drop_reps_not_in_mdb ####

##' 1. Area codes not mapping to any FAO country code are removed.

# We drop reporters what are absent in MDB hsfcl map
# because in any case we can proceed their data

tldata_not_area_in_fcl_mapping <- tldata %>%
  filter_(~!(reporter %in% unique(hsfclmap$area)))

rprt_writetable(tldata_not_area_in_fcl_mapping)

flog.trace("TL: dropping reporters not found in the mapping table", name = "dev")
tldata <- tldata %>%
  filter_(~reporter %in% unique(hsfclmap$area))


##+ reexptoexp ####
flog.trace("TL: recoding reimport/reexport", name = "dev")
##' 1. Re-imports become imports and re-exports become exports.

# { "id": "1", "text": "Import" },
# { "id": "2", "text": "Export" },
# { "id": "4", "text": "re-Import" },
# { "id": "3", "text": "re-Export" }

tldata <- tldata %>%
  mutate_(flow = ~recode(flow, '4' = 1L, '3' = 2L))

# TF: Map HS to FCL ####
##+ tl_hs2fcl ####

tldatahs6links <- mapHS6toFCL(tldata, hs6fclmap)

tldatalinks <- mapHS2FCL(tradedata = tldata,
                         maptable = hsfclmap3,
                         hs6maptable = hs6fclmap,
                         year = year,
                         parallel = multicore)

tldata <- add_fcls_from_links(tldata,
                              hs6links = tldatahs6links,
                              links = tldatalinks)

rprt(tldata, "hs2fcl_fulldata", tradedataname = "tldata")

flog.trace("TL: dropping unmapped records", name = "dev")

tldata <- tldata %>%
  filter_(~!is.na(fcl))

flog.info("TL records after removing non-mapped HS codes: %s",
          nrow(tldata))

if(stop_after_mapping) stop("Stop after HS->FCL mapping")
#############Units of measurment in TL ####

##' Add FCL units. ####

flog.trace("TL: add FCL units", name = "dev")

tldata <- addFCLunits(tradedata = tldata, fclunits = fclunits)

tldata <- tldata %>%
  mutate_(qunit = ~as.integer(qunit)) %>%
  left_join(comtradeunits %>%
              select_(~qunit, ~wco),
            by = "qunit")

## Dataset with all matches between Comtrade and FAO units
ctfclunitsconv <- tldata %>%
  select_(~qunit, ~wco, ~fclunit) %>%
  distinct() %>%
  arrange_(~qunit) %>%
  as.data.table()

################ Conv. factor (TL) ################
flog.trace("TL: conversion factors", name = "dev")

##### Table for conv. factor

##' 1. General TL conversions: some FCL codes are reported in Tariffline
##' with different units than those reported in FAOSTAT, thus a conversion
##' is done.

ctfclunitsconv$conv <- 0
# Missing quantity
ctfclunitsconv[qunit == 1,                                conv :=   NA]
# Missing quantity
ctfclunitsconv[fclunit == "$ value only",                 conv :=   NA]
ctfclunitsconv[fclunit == "mt"         & wco == "l",      conv := .001]
ctfclunitsconv[fclunit == "heads"      & wco == "u" ,     conv :=    1]
ctfclunitsconv[fclunit == "1000 heads" & wco == "u" ,     conv := .001]
ctfclunitsconv[fclunit == "number"     & wco == "u"  ,    conv :=    1]
ctfclunitsconv[fclunit == "mt"         & wco == "kg"  ,   conv := .001]
ctfclunitsconv[fclunit == "mt"         & wco == "m³"   ,  conv :=    1]
ctfclunitsconv[fclunit == "mt"         & wco == "carat" , conv := 5e-6]


##### Add conv factor to the dataset

tldata <- tldata %>%
  left_join(ctfclunitsconv,
            by = c("qunit", "wco", "fclunit"))

##' 1. Specific TL conversions: some commodities need a specific conversion.

#### Commodity specific conversion

fcl_spec_mt_conv <- tldata %>%
  filter_(~fclunit == "mt" & is.na(weight) & conv == 0) %>%
  select_(~fcl, ~wco) %>%
  distinct

if(NROW(fcl_spec_mt_conv) > 0){

  conversion_factors_fcl <- tldata %>%
    filter(!is.na(weight) & !is.na(qty)) %>%
    mutate(qw = (weight/qty)/1000) %>%
    group_by(fcl, wco) %>%
    summarise(convspec = median(qw, na.rm = TRUE)) %>%
    ungroup()

  fcl_spec_mt_conv <- fcl_spec_mt_conv %>%
    left_join(conversion_factors_fcl, by = c("fcl", "wco"))

  fcl_spec_mt_conv$convspec[is.na(fcl_spec_mt_conv$convspec)] <- 0

  ### Add commodity specific conv.factors to dataset

  tldata <- tldata %>%
    left_join(fcl_spec_mt_conv,
              by = c("fcl", "wco"))
  ########## Conversion of units

  #### FCL specific conv

  tldata$qtyfcl <- tldata$qty * tldata$convspec

  #### Common conv
  # If no specific conv. factor, we apply general

  tldata$qtyfcl <- ifelse(is.na(tldata$convspec),
                          tldata$qty * tldata$conv,
                          tldata$qtyfcl)
} else {
  tldata$qtyfcl = NA
}



# FIX for UAE: animals weight given by Claudia and Katherine on 20170628
uae_specific_conversions <- frame_data(
~fcl, ~conv_uae, ~reporter,
 866,  1/350, 225L,
 976,  1/50, 225L,
 1016, 1/25, 225L,
 1057, (1/0.06)/1000, 225L,
 1096, 1/500, 225L,
 1126, 1/300, 225L
)

tldata <- tldata %>%
  left_join(uae_specific_conversions, by = c("reporter", "fcl")) %>%
  mutate(qtyfcl = ifelse(!is.na(conv_uae), weight * conv_uae, qtyfcl))

# XXX
# Flag on weight as qty (which underwent a change) will populate weight
tldata <- tldata %>%
  setFlag3(!is.na(conv_uae), type = 'method', flag = 'i', variable = 'weight')


# XXX modify flags

# / FIX for UAE: animals weight given by Claudia and Katherine



##' 1. If the `weight` variable is available and the final unit
##' of measurement is tonnes then `weight` is used as `quantity`

cond <- tldata$fclunit == 'mt' & !is.na(tldata$weight) & tldata$weight > 0

tldata$qtyfcl <- ifelse(cond, tldata$weight*0.001, tldata$qtyfcl)

# XXX
# Flag on weight as qty (which underwent a change) will populate weight
tldata <- tldata %>%
  setFlag3(cond, type = 'method', flag = 'i', variable = 'weight')







######### Value from USD to thousands of USD

if (dollars){
  esdata <- esdata %>%
    mutate(value = value * 1000) %>%
    setFlag3(value > 0, type = 'method', flag = 'i', variable = 'value')
} else { ## This means it is in k$
  tldata <- tldata %>%
    mutate(value = value / 1000) %>%
    setFlag3(value > 0, type = 'method', flag = 'i', variable = 'value')
}

##' 1. Aggregate UNSD Tariffline Data to FCL.

##+ tl_aggregate

# Replace weight (first quantity column) by newly produced qtyfcl column
# XXX "notes" are applied to weight that is transformed below from qtyfcl
flog.trace("TL: aggregate to FCL", name = "dev")
tldata <- tldata %>%
  select(-weight, -qty) %>%
  rename(weight = qtyfcl) # XXX weight should probably be renamed qty here

tldata_mid = tldata

##' # Combine Trade Data Sources

##' 1. Application of "adjustment notes" to both ES and TL data.

# TODO Check quantity/weight
# The notes should save the results in weight

# TODO (Christian) Check this (some ES partners are not TL partners):
# unique(esdata$partner)[!(unique(esdata$partner) %in% unique(tldata$partner))]

# We need to set the flags one by one as adjustments not necessarily
# (probably never?) adjust all the three variables at the same time
if (use_adjustments == TRUE) {
  flog.trace("Apply adjustments", name = "dev")
  esdata <- useAdjustments(tradedata = esdata, year = year, PID = PID,
                           adjustments = adjustments, parallel = multicore) %>%
    setFlag3(adj_value  == TRUE, type = 'method', flag = 'i', variable = 'value') %>%
    setFlag3(adj_weight == TRUE, type = 'method', flag = 'i', variable = 'weight') %>%
    setFlag3(adj_qty    == TRUE, type = 'method', flag = 'i', variable = 'quantity')

  tldata <- useAdjustments(tradedata = tldata, year = year,
                           adjustments = adjustments, parallel = multicore) %>%
    setFlag3(adj_value  == TRUE, type = 'method', flag = 'i', variable = 'value') %>%
    setFlag3(adj_weight == TRUE, type = 'method', flag = 'i', variable = 'weight') %>%
    setFlag3(adj_qty    == TRUE, type = 'method', flag = 'i', variable = 'quantity')
}

##+ es_convcur

##' 1. Convert currency of monetary values from EUR to USD using the
##' `EURconversionUSD` table.

esdata$value <- esdata$value * as.numeric(EURconversionUSD %>%
                                          filter(eusd_year == year) %>%
                                          select(eusd_exchangerate))

esdata <- esdata %>%
    setFlag3(value > 0, type = 'method', flag = 'i', variable = 'value')

###' 1. Assign 'weight' flags to 'qty' flags in TL XXX.
#
# NO: this isn't needed as below qty = weight and it has already its own flag
#
#tldata <- tldata %>%
#  mutate_each_(funs(swapFlags(., swap='\\1\\2\\2'), !is.na(weight)),
#               ~starts_with('flag_'))

##' 1. Assign 'qty' flags to 'weight' flags in ES but
##' only when 'fclunit' is different from 'mt'.

esdata <- esdata %>%
  mutate_each_(funs(swapFlags(., swap='\\1\\3\\3', fclunit != "mt")),
               ~starts_with('flag_'))

##' 1. Combine UNSD Tariffline and Eurostat Combined Nomenclature data sources
##' to single data set.
##'     - TL: assign `weight` to `qty`
##'     - ES: assign `weight` to `qty` if `fclunit` is `mt`, else keep `qty`

##+ combine_es_tl
flog.trace("Combine TL and ES data sets", name = "dev")
tradedata <- bind_rows(
  tldata %>%
    select(year, reporter, partner, flow,
            fcl, fclunit, hs,
            qty = weight, value,
            starts_with('flag_')),
  esdata %>%
    mutate(uniqqty = ifelse(fclunit == "mt", weight, qty)) %>%
    select(year, reporter, partner, flow,
            fcl, fclunit, hs,
            qty = uniqqty, value,
            starts_with('flag_'))
)

# XXX this is fine, but probably the name of the function should be changed
tradedata <- tradedata %>%
  mutate_each_(funs(swapFlags(., swap='\\1\\2')), ~starts_with('flag_'))

## Check for double counting f HS codes
#rprt_writetable(tradedata, subdir = 'details')

##' # Outlier Detection and Imputation
flog.trace("Outlier detection and imputation", name = "dev")
##+ calculate_median_uv

tradedata <- tradedata %>%
  mutate_(no_quant = ~near(qty, 0) | is.na(qty),
          no_value = ~near(value, 0) | is.na(value))

##' 1. Unit values are calculated for each observation at the HS level as ratio
##' of monetary value over quantity `value / qty`.

tradedata <- mutate_(tradedata,
                     uv = ~ifelse(no_quant | no_value, NA, value / qty))

## Round UV in order to avoid floating point number problems (see issue #54)
tradedata$uv <- round(tradedata$uv, 10)

##+ boxplot_uv

##' 1. Outlier detection by using the logarithm of the unit value.

if (detect_outliers) {
  tradedata <- detectOutliers(tradedata = tradedata, method = "boxplot",
                              parameters = list(out_coef = out_coef))
} else {
  tradedata$outlier <- FALSE
}

##+ impute_qty_uv

##' 1. Imputation of missing quantities and quantities categorized as outliers by
##' applying the method presented in the *Outlier Detection and Imputation* section.
##' The `flagTrade` variable is given a value of 1 if an imputation was performed.

## These flags are also assigned to monetary values. This may need to be revised
## (monetary values are not supposed to be modified).

tradedata <- computeMedianUnitValue(tradedata = tradedata)

tradedata <- doImputation(tradedata = tradedata)

flog.trace("Flag stuff", name = "dev")
# XXX using flagTrade for the moment, but should go away
tradedata <- tradedata %>%
    setFlag2(flagTrade > 0, type = 'status', flag = 'I', var = 'quantity') %>%
    setFlag2(flagTrade > 0, type = 'method', flag = 'e', var = 'quantity')

##' Separate flags.

###### TODO (Christian) Rethink/refactor
# separate flag_method and flag_status into 2 variables each one: _v and _q
flag_vars <- colnames(tradedata)[grep('flag_', colnames(tradedata))]
for (var in flag_vars) {
  tradedata <- separate_(tradedata, var, 1:2,
                         into = c('x', paste0(var, '_', c('v', 'q'))),
                         convert = TRUE) %>%
               select(-x)
}

##' 1. Aggregate values and quantities by FCL codes.

tradedata_flags <- tradedata %>%
  group_by_(~year, ~reporter, ~partner, ~flow, ~fcl) %>%
  summarise_each_(funs(sumFlags(flags = .)), vars = ~starts_with('flag_')) %>%
  ungroup()

# Aggregation by fcl
flog.trace("Aggregation by FCL", name = "dev")
tradedata <- tradedata %>%
  mutate_(nfcl = 1) %>%
  group_by_(~year, ~reporter, ~partner, ~flow, ~fcl, ~fclunit) %>%
  summarise_each_(funs(sum(., na.rm = TRUE)),
                  vars = c("qty", "value","flagTrade", "nfcl")) %>%
  ungroup()

flog.trace("Flags again", name = "dev")
tradedata <- left_join(tradedata,
                       tradedata_flags,
                       by = c('year', 'reporter', 'partner', 'flow', 'fcl'))

###### TODO (Christian) Rethink/refactor
# unite _v and _q into one variable
flag_vars <- sort(unique(sub('_[vq]$', '', colnames(tradedata)[grep('flag_', colnames(tradedata))])))
for (var in flag_vars) {
  var_v <- paste0(var, '_v')
  var_q <- paste0(var, '_q')

  tradedata[[var]] <- 100 + (tradedata[[var_v]]>0)*10 + (tradedata[[var_q]]>0)*1
}
tradedata <- tradedata[-grep('^flag_.*[vq]$', colnames(tradedata))]

##' 1. Se flags for aggregated values/quantities XXX.

tradedata <- tradedata %>%
  setFlag2(nfcl > 1,  type = 'method', flag = 's', variable = 'all')

##' 1. Map FCL codes to CPC.

# Adding CPC2 extended code
flog.trace("Add CPC item codes", name = "dev")
tradedata <- tradedata %>%
  mutate_(cpc = ~fcl2cpc(sprintf("%04d", fcl), version = "2.1"))

# Not resolve mapping fcl2cpc
no_mapping_fcl2cpc = tradedata %>%
  select_(~fcl, ~cpc) %>%
  filter_(~is.na(cpc)) %>%
  distinct_(~fcl) %>%
  select_(~fcl) %>%
  unlist()

##' 1. Map FAO area codes to M49.

# Converting back to M49 for the system
flog.trace("Convert FAO area codes to M49", name = "dev")
tradedata <- tradedata %>%
  mutate_(reporterM49 = ~fs2m49(as.character(reporter)),
          partnerM49 = ~fs2m49(as.character(partner)))

# Report of countries mapping to NA in M49
# 2011: fal 252: "Unspecified" in FAOSTAT area list
countries_not_mapping_M49 <- bind_rows(
  tradedata %>% select_(fc = ~reporter, m49 = ~reporterM49),
  tradedata %>% select_(fc = ~partner, m49 = ~partnerM49)) %>%
  distinct_() %>%
  filter_(~is.na(m49)) %>%
  select_(~fc) %>%
  unlist()

##+ mirror_estimation

##' # Mirror Trade Estimation

##' 1. Create a table with the list of reporters and partners
##' combined as areas and count the number of flows that the
##' areas declare as reporting countries. The partners that
##' never show up as reporters or the reporters that do not
##' report a flow will have a number of flows equal to zero
##' and will be mirrored.
flog.trace("Mirroring", name = "dev")

to_mirror <- flowsToMirror(tradedata)

##' 1. Swap the reporter and partner dimensions: the value previously appearing
##' as reporter country code becomes the partner country code (and vice versa).

##' 1. Invert the flow direction: an import becomes an export (and vice versa).

##' 1. Calculate monetary mirror value by adding (removing) a 12% mark-up on
##' imports (exports) to account for the difference between CIF and FOB prices.

## Mirroring for non reporting countries
tradedata <- mirrorNonReporters(tradedata = tradedata, mirror = to_mirror)

# Add an auxiliary variable "mirrored" that will be removed later
tradedata <- tradedata %>%
  left_join(
    to_mirror %>% mutate(mirrored = 1L),
    by = c('reporter' = 'area', 'flow')
  )

##' 1. Set flags XXX.
flog.trace("Flags XXX (for adults only?)", name = "dev")

tradedata <- tradedata %>%
  setFlag2(!is.na(mirrored), type = 'status', flag = 'E', var = 'all') %>%
  setFlag2(!is.na(mirrored), type = 'method', flag = 'i', var = 'value') %>%
  setFlag2(!is.na(mirrored), type = 'method', flag = 'c', var = 'quantity') %>%
  select(-mirrored)

##' ## Flag management

##' **Note**: work on this section is currently in progress.

################################################
# TODO Rethink/refactor: clean flags for fclunit != "$ value only"
################################################

##+ completed_trade_flow

###### TODO (Christian) Rethink/refactor
# separate flag_method and flag_status into 2 variables each one: _v and _q
flag_vars <- colnames(tradedata)[grep('flag_', colnames(tradedata))]
for (var in flag_vars) {
  tradedata <- separate_(tradedata, var, 1:2,
                         into = c('x', paste0(var, '_', c('v', 'q'))),
                         convert = TRUE) %>%
               select(-x)
}

##' # Output for SWS

##' 1. Filter observations with FCL code `1181` (bees).

##' 1. Filter observations with missing CPC codes.

##' 1. Rename dimensions to comply with SWS standard, e.g., `geographicAreaM49Reporter`

##' 1. Calculate unit value (US$ per quantity unit) at CPC level if the quantity is larger than zero

# Modified in order to have X in the table
flagWeightTable_status <- frame_data(
  ~flagObservationStatus, ~flagObservationWeights,
  'X',                   1.00,
  '',                    0.99,
  'T',                   0.80,
  'E',                   0.75,
  'I',                   0.50,
  'M',                   0.00
)

# There is no native "method" table
flagWeightTable_method <- frame_data(
  ~flagObservationStatus, ~flagObservationWeights,
  'h',                   1.00,
  'i',                   0.80,
  'e',                   0.60,
  'c',                   0.40,
  's',                   0.20
)

# XXX This piece of code is really slow. There should be a better way.
flog.trace("Cycle on status and method flags", name = "dev")
for (i in c('status', 'method')) {
  for (j in c('v', 'q')) {

    dummies <- tradedata %>%
      select(starts_with(paste0('flag_', i))) %>%
      select(ends_with(j))

    flags <- sub('.*_(.)_.$', '\\1', colnames(dummies))

    if (i == 'status') {
      flagWeightTable <- flagWeightTable_status
    } else {
      flagWeightTable <- flagWeightTable_method
    }

    var <- paste0('flag', toupper(i), '_', j)

    tradedata[[var]] <- apply(dummies, 1, function(x)
                              ifelse(sum(x)==0, NA,
                                     aggregateObservationFlag(flags[x==1])))
  }
}

flog.trace("Complete trade flow CPC", name = "dev")
complete_trade_flow_cpc <- tradedata %>%
  filter_(~fcl != 1181) %>% ## Subsetting out bees
  select_(~-fcl) %>%
  filter_(~!(is.na(cpc))) %>%
  transmute_(geographicAreaM49Reporter = ~reporterM49,
             geographicAreaM49Partner = ~partnerM49,
             flow = ~flow,
             timePointYears = ~year,
             flagObservationStatus_v = ~flagSTATUS_v,
             flagObservationStatus_q = ~flagSTATUS_q,
             flagMethod_v = ~flagMETHOD_v,
             flagMethod_q = ~flagMETHOD_q,
             measuredItemCPC = ~cpc,
             qty = ~qty,
             unit = ~fclunit,
             value = ~value) %>%
  ## unit of monetary values is "1000 $"
  mutate(uv = ifelse(qty > 0, value * 1000 / qty, NA))

##' 1. Transform dataset separating monetary values, quantities and unit values
##' in different rows.

##' 1. Convert monetary values, quantities and unit values to corresponding SWS
##' element codes. For example, a quantity import measured in metric tons is
##' assigned `5610`.

##+ convert_element

complete_trade_flow_cpc <- complete_trade_flow_cpc %>%
  tidyr::gather(measuredElementTrade, Value, -geographicAreaM49Reporter,
                -geographicAreaM49Partner, -measuredItemCPC,
                -timePointYears,
                -flagObservationStatus_v, -flagObservationStatus_q,
                -flagMethod_v, -flagMethod_q, -unit, -flow) %>%
  rowwise() %>%
  mutate_(measuredElementTrade =
            ~convertMeasuredElementTrade(measuredElementTrade,
                                         unit,
                                         flow)) %>%
  ungroup() %>%
  filter_(~measuredElementTrade != "999") %>%
  select_(~-flow,~-unit)

quantityElements <- c("5608", "5609", "5610", "5908", "5909", "5910")
uvElements       <- c("5638", "5639", "5630", "5938", "5939", "5930")

complete_trade_flow_cpc <- complete_trade_flow_cpc %>%
  mutate(flagObservationStatus = ifelse(measuredElementTrade %in% quantityElements,
                                        flagObservationStatus_q,
                                        flagObservationStatus_v),
         flagMethod = ifelse(measuredElementTrade %in% quantityElements,
                                        flagMethod_q,
                                        flagMethod_v)) %>%
  # The Status flag will be equal to the weakest flag between
  # the numerator and the denominator, in this case the denominator.
  mutate(flagObservationStatus = ifelse(measuredElementTrade %in% uvElements,
                                        flagObservationStatus_q,
                                        flagObservationStatus),
         flagMethod = ifelse(measuredElementTrade %in% uvElements,
                             'i',
                             flagMethod)) %>%
  select(-flagObservationStatus_v, -flagObservationStatus_q,
         -flagMethod_v, -flagMethod_q)

complete_trade_flow_cpc <- data.table::as.data.table(complete_trade_flow_cpc)

data.table::setcolorder(complete_trade_flow_cpc,
                        c("geographicAreaM49Reporter",
                          "geographicAreaM49Partner",
                          "measuredElementTrade",
                          "measuredItemCPC",
                          "timePointYears",
                          "Value",
                          "flagObservationStatus",
                          "flagMethod"))

# XXX Temporary workaround: some NAs are given flags and given
# that NAs cannot have flags the system refuses to save them.
# These NAs are unit values computed on a zero quantity. Setting
# Value to zero.
complete_trade_flow_cpc[is.na(Value), Value := 0]

# "official" status flag should be <BLANK> instead of X (this was a choice
# made after X was chosen as official flag). Thus, change X to <BLANK>.
complete_trade_flow_cpc[flagObservationStatus == 'X', flagObservationStatus := '']


flog.trace("[%s] Writing data to session/database", PID, name = "dev")
stats <- SaveData("trade",
                  "completed_tf_cpc_m49",
                  complete_trade_flow_cpc,
                  waitTimeout = 10800)

## remove value only

flog.trace("[%s] Session/database write completed!", PID, name = "dev")

sprintf(
  "Module completed in %1.2f minutes.
  Values inserted: %s
  appended: %s
  ignored: %s
  discarded: %s",
  difftime(Sys.time(), startTime, units = "min"),
  stats[["inserted"]],
  stats[["appended"]],
  stats[["ignored"]],
  stats[["discarded"]]
)

flog.info(
    "Module completed in %1.2f minutes.
  Values inserted: %s
  appended: %s
  ignored: %s
  discarded: %s",
    difftime(Sys.time(), startTime, units = "min"),
    stats[["inserted"]],
    stats[["appended"]],
    stats[["ignored"]],
    stats[["discarded"]], name = "dev"
  )

# Restore changed options
options(old_options)


