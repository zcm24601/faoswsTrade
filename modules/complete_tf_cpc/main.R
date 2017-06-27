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
38,1,"020754","020754",1073,2015,2050,5923193,"Generic HS 2012 to FCL (could also be mapped to 1074)","Meat and edible offal, of the poultry of heading 01.05, fresh, chilled, or frozen: Of geese, Other, fresh or chilled",
38,1,"150290","150290",869,2015,2050,5923194,"Generic HS 2012 to FCL (could also be mapped to 871, 949,979, or 1019, but 869 is the largest utilization)","Fats of bovine animals, sheep or goats (excluding tallow, oleostearin and oleo-oil)",
38,2,"040140","040140",885,2015,2050,5923195,"Trademap TL description, Jellyfish (previously, anything above 6% fat was mapped to 885 cream)","Milk and cream of a fat content by weight of > 6% but <= 10%, not concentrated nor containing added sugar or other sweetening matter",
47,1,"15020000","15020000",869,2009,2050,5923196,"Generic HS 2007 mapping, Old SWS series (could also be mapped to 871, 949, 979, 1019)","Fats of bovine animals, sheep or goats, other than those of heading 1503: other",
47,1,"23080000","23080000",652,2005,2050,5923197,"HS 2002","Vegetable materials and vegetable waste, vegetable residues and by-products, whether or not in the form of pellts, of a kind used in animal feeding, not elsewhere specified or included",
73,1,"150290","150290",869,2010,2050,5923198,"Trademap description, Old SWS series (could also be mapped to 871, 949, 979, 1019)","Fats of bovine animals, sheep or goats (excluding tallow, oleostearin and oleo-oil)",
73,1,"410190","410190",920,2009,2050,5923199,"Trademap TL description, Old SWS series (could also be mapped to 921, 1103)","Butts, bends, bellies and split raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired, fresh, or salted, dried, limed, pickled or otherwise preserved, and whole raw hides and skins of a weight per skin > 8 kg but < 16 kg when simply dried and > 10 kg but < 16 kg when dry-salted (excl. tanned, parchment-dressed or further prepared) (detailed label not available)",
73,1,"530590","530590",780,2008,2050,5923200,"Trademap description, FCL description, Old SWS series","Ramie and other vegetable textile fibres, n.e.s., raw or processed, but not spun; tow, noils and waste of such fibres, incl. yarn waste and garnetted stock",
73,2,"410190","410190",920,2009,2050,5923201,"Trademap TL description, Old SWS series (could also be mapped to 921, 1103)","Butts, bends, bellies and split raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired, fresh, or salted, dried, limed, pickled or otherwise preserved, and whole raw hides and skins of a weight per skin > 8 kg but < 16 kg when simply dried and > 10 kg but < 16 kg when dry-salted (excl. tanned, parchment-dressed or further prepared) (detailed label not available)",
74,1,"070990","070990",463,2005,2050,5923202,"Generic HS 2007 to FCL mapping, Old SWS series (could also be mapped to 260, 378, 394, 430, 446). Description from Trademap","Fresh or chilled vegetables (excluding potatoes, tomatoes, vegetables of the Allium spp., cabbages of the genus Brassica, lettuces of the species Lactuca sativa and Cichorium, carrots, turnips, salad beetroot, salsify, celeriac, radishes and similar edible roots, cucumbers and gherkins, leguminous vegetables, asparagus, aubergines, mushrooms, truffles, fruits of the genus Capsicum or of the genus Pimenta, spinach, New Zealand spinach and orache spinach)",
74,1,"160220","160220",878,2005,2050,5923203,"8-digit code does not exist , but six-digit code has two subcategories (10 foie gras and 90 other), but Old SWS series seems to indicate that bulk of imports come in under other liv preps rather than foie gras). Code from Trademap.","Preparations of liver of any animal (excluding sausages and similar products and finely homogenised preparations put up for retail sale as infant food or for dietetic purposes, in containers of a net weight of <= 250 g)",
74,1,"190190","190190",115,2005,2050,5923204,"Generic HS 2007 mapping, Old SWS series (could also be mapped to 50). Code from Trademap.","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905)",
74,1,"190590","190590",22,2005,2050,5923205,"8-digit code does not exist, but six-digit code could be mapped to 20, 22, 110. Old SWS series suggests bulk of trade is in 22. Code from Trademap.","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products (excluding crispbread, gingerbread and the like, sweet biscuits, waffles, wafers not mentioned, rusks, toasted bread and similar toasted products)",
74,1,"200911","200911",492,2005,2050,5923206,"Generic HS 2007 to FCL unique six-digit match, Trademap","Frozen orange juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
74,1,"200931","200931",513,2005,2050,5923207,"Generic HS 2007 to FCL mapping, Old SWS series (could also be mapped to 496, 498). Code from Trademap.","Single citrus fruit juice, unfermented, Brix value <= 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
74,1,"200939","200939",514,2005,2050,5923208,"Generic HS 2007 to FCL mapping, Old SWS series (could also be mapped to 499). Code from Trademap.","Single citrus fruit juice, unfermented, Brix value > 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
74,1,"200980","200980",622,2005,2050,5923209,"Generic HS 2007 to FCL mapping, Old SWS series (could also be mapped to 466,538,539,583). Code from Trademap.","Single citrus fruit juice, unfermented, Brix value > 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
74,1,"410120","410120",921,2005,2050,5923210,"Generic HS 2007 to FCL mapping (could also be mapped to 920, 922, 928, 929, 930, 958, 959, 1103, 1103, 1105)","Raw hides and skins of bovine (including buffalo) or equine animalswhether or not dehaired or split. Whole hides and skins, of a weight per skin not exceeding 8 kg when simply dried, 10 kg when dry-salted, or 16 kg when fresh, wet-salted or otherwise preserved",
74,2,"200911","200911",492,2005,2050,5923211,"Generic HS 2007 to FCL unique six-digit match, Trademap","Frozen orange juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
75,1,"382310","382310",1276,2001,2050,5923212,"Six-digit code not found in HS 1996, but all 3823s are mapped to this code","Industrial monocarboxylic fatty acids; acid oils from refining; industrial fatty alcohols",
83,1,"11031900","11031900",111,2006,2050,5923213,"Generic HS 2007 to FCL mapping (could also be mapped to 38,48,72,80,84,90,95,98,104)","Groats and meal of cereals (excluding wheat and maize)",
83,2,"01051900","01051900",1068,2007,2050,5923214,"Generic HS 2007 to FCL mapping (could also be mapped to 1072)","Live poultry, excl. gallus domestic, turkeys (i.e., can be ducks, geese, guinea fowls)",
86,1,"2009202","2009202",509,2005,2050,5923215,"Country TL description (Trademap & WITS)","Grapefruit juice, unfermented, whetehr or not containing added sugar or other sweetening matter (excluding containing spirit): Preparations for infant use, put up for retail sale",
86,1,"20093040","20093040",513,2008,2050,5923216,"Country TL description (WITS), Old SWS series (could also be mapped to 496, 498)","Preparations of the juice of any other single citrus fruit for infant use, put up for retail sale",
89,1,"02074410","02074410",1075,2015,2050,5923217,"Country TL description (WITS)","Únicamente Higados Los Demás Despojos : De Pato : Los Demás, Frescos O Refrigerados : En Pasta, Deshuesados Mecánicamente",
89,1,"02075410","02075410",1074,2015,2050,5923218,"Country TL description (WITS)","Únicamente Higados Los Demás Despojos : De Ganso : Los Demás, Frescos O Refrigerados : En Pasta, Deshuesados Mecánicamente",
90,1,"010690","010690",1169,2007,2050,5923219,"Generic HS 2007 to FCL (could also be mapped to 1171, 1181)","Other live animals (not mammals, reptiles, or birds)",
90,1,"151419","151419",271,2005,2050,5923220,"Generic HS 2002 to FCL, Old SWS series (could also be mapped to 293)","Low euracic acid rape or colza oil and its fractions: oil other than crude",
90,1,"200931","200931",513,2006,2050,5923221,"Generic HS 2002 to FCL, Old SWS series (could also be mapped to 496, 498)","Juice of any other single citrus fruit, of a brix value not exceeding 20",
90,2,"010619","010619",1169,2007,2050,5923222,"Generic HS 2007 to FCL (could also be mapped to 1126,1140,1150,1157,1171, 1181)","Other live mammals",
91,2,"0106304000","0106304000",1169,2009,2050,5923223,"Six-digit code not found in HS 2007, but 4-digit heading covers other live animals, most of which are non-food","Other live animals",
101,1,"1502909000","1502909000",869,2015,2050,5923224,"Trademap TL description, Old SWS series (could also be mapped to 871, 949, 979, 1019)","Unedible fats of bovine animals, sheep/ goats, oth than tallow, oth he",
108,1,"020830","020830",1166,2014,2050,5923225,"Country TL description (WITS), Old SWS series (could also be mapped to 1167)","Other meat and edible meat offal, fresh, chilled or frozen: Of primates",
112,1,"020860000","020860000",1127,2014,2050,5923226,"Trademap TL description (could also be mapped to 1128, 1158, or 1159)","Other meat and edible meat offal, fresh, chilled or frozen: Of camels and other camelids (Camelidae)",
112,2,"020621000","020621000",868,2014,2050,5923227,"Trademap TL description, Old SWS series (could also be mapped to 948)","Edible offal of bovine animals, swine, sheep, goats, horses, asses, mules or hinnies, fresh, chilled or frozen: Of bovine animals, frozen: Tongues",
114,1,"190190","190190",115,2010,2050,5923228,"Analysis of Trademap TL data of subcategories (although both 50 malt extracts and 115 food preps are covered under this six-digit code, the overwhelming majority of the imports come in under the subcode for 115 food preps and not 50 malt extracts)","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905)",
114,1,"190590","190590",22,2010,2050,5923229,"Generic HS2012 to FCL, Old SWS series (could also be mapped to 110)","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products (excluding crispbread, gingerbread and the like, sweet biscuits, waffles, wafers not mentioned, rusks, toasted bread and similar toasted products)",
121,1,"010613","010613",1126,2014,2050,5923230,"Trademap description (could also be mapped to 1157 other camelids)","Live camels and other camelids [Camelidae]",
129,1,"080521","080521",495,2015,2050,5923231,"Trademap TL description","Fresh or dried mandarins incl. tangerines and satsumas (excl. clementines)",
129,1,"080522","080522",495,2015,2050,5923232,"Trademap TL description","Fresh or dried clementines incl. monreales",
129,1,"080529","080529",512,2015,2050,5923233,"Trademap TL description","Fresh or dried wilkings and similar citrus hybrids",
129,1,"151591","151591",340,2015,2050,5923234,"Trademap TL description","Autres graisses et huiles vegetales; autres",
129,1,"220291","220291",633,2015,2050,5923235,"Trademap TL description","Non-alcoholic beer <= 0.5% vol alc",
129,1,"220299","220299",633,2015,2050,5923236,"Trademap TL description","Non-alcoholic beverages (excl. water, fruit or vegetable juices, milk and beer)",
129,1,"220422","220422",564,2015,2050,5923237,"Trademap TL description","Wine of fresh grapes, incl. fortified wines, and grape must whose fermentation has been arrested . . .",
129,2,"010649","010649",1169,2014,2050,5923238,"Trademap TL description (could also be mapped to 1171)","Live insects (excluding bees)",
129,2,"080521","080521",495,2015,2050,5923239,"Trademap TL description","Fresh or dried mandarins incl. tangerines and satsumas (excl. clementines)",
129,2,"080529","080529",512,2015,2050,5923240,"Trademap TL description","Fresh or dried wilkings and similar citrus hybrids",
129,2,"151599","151599",340,2015,2050,5923241,"Chose this code as nearest to Oil of vegetable Origin NES",NA,
129,2,"17011000","17011000",162,2001,2050,5923242,"Chose this code as nearest to Sugar Raw Centrifugal","Sugar Raw Centrifugal",
129,2,"52034600","52034600",768,2002,2050,5923243,"Heading is mapped to cotton carded combed, although the exact six-digit number could not be found in the domestic tariff schedule","Cotton: carded, combed",
132,1,"0105991010","0105991010",1068,2014,2050,5923244,"WITS","ducks",
132,1,"0105991011","0105991011",1068,2015,2050,5923245,"WITS","ducks",
132,1,"0105999099","0105999099",1057,2015,2050,5923246,"WITS","other",
132,1,"1901909016","1901909016",115,2015,2050,5923247,"Trademap TL description","Papadam",
132,1,"2008110010","2008110010",247,2015,2050,5923248,"Trademap TL description","Peanut Butter",
132,1,"2008110099","2008110099",246,2015,2050,5923249,"Trademap TL description","Ground Nuts Prepared Or Preserved",
132,1,"2009391000","2009391000",514,2015,2050,5923250,"Trademap TL description","Single Citrus Fruit Juice(Excl Grape & Orange,Brix Value >20 )Containing Spirit",
132,1,"2309901000","2309901000",841,2014,2050,5923251,"WITS","Chicken Food",
132,1,"2309909010","2309909010",1259,2014,2050,5923252,"WITS","Fish Food",
132,1,"2309909011","2309909011",841,2014,2050,5923253,"WITS","Poltry Food (Other Than Chicken Food)",
132,1,"2309909099","2309909099",1259,2014,2050,5923254,"WITS","Animal Food (Other Than Cat Food And Chicken Food), Nes",
137,1,"01061300","01061300",1259,2015,2050,5923255,"WITS","food waste prep",
137,1,"15029000","15029000",1259,2015,2050,5923256,"WITS","food waste prep",
146,1,"070999100","070999100",1259,2015,2050,5923257,"WITS","food waste prep",
146,1,"070999600","070999600",446,2015,2050,5923258,"Country TL description (WITS)","Fresh or chilled sweetcorn",
146,1,"080390100","080390100",486,2015,2050,5923259,"Country TL description (WITS)","Bananas, fresh (excl. plantains)",
146,1,"080390900","080390900",604,2015,2050,5923260,"Country TL description (WITS), FCL descriptions","Bananas, dried (excl. plantains)",
146,1,"081090200","081090200",603,2015,2050,5923261,"Country TL description (WITS), FCL descriptions","Fresh tamarinds, cashew apples, lychees, jackfruit, sapodillo plums, passion fruit, carambola and pitahaya",
146,1,"081090750","081090750",619,2015,2050,5923262,"Country TL description (WITS)","Fresh fruit, edible (excl. nuts, bananas, dates, figs, pineapples, avocados, guavas, mangoes, mangosteens, papaws papayas, tamarinds, cashew apples, jackfruit, lychees, sapodillo plums, passion fruit, carambola, pitahaya, citrus fruit, grapes, melons, apples, pears, quinces, apricots, cherries, peaches, plums, sloes, strawberries, raspberries, blackberries, mulberries, loganberries, black, white or red currants, gooseberries, cranberries, fruits of the genus Vaccinium, kiwifruit, durians and persimmons)",
146,1,"110290700","110290700",72,2015,2050,5923263,"Country TL description (WITS)","Rye flour",
146,1,"110319200","110319200",48,2015,2050,5923264,"Country TL description (WITS), Jellyfish mapping of EU countries and ostensible trading partners","Groats and meal of rye or barley",
146,1,"121299950","121299950",460,2015,2050,5923265,"Country TL description (WITS)","Fruit stones and kernels and other vegetable products, of a kind used primarily for human consumption, n.e.s.",
146,1,"150290900","150290900",869,2015,2050,5923266,"Country TL description (WITS), assigned based on largest utilization in Old SWS (could also be mapped to 871, 949, 979, 1019)","Fat of bovine animals, sheep or goats, (excl. for technical/industrial uses, and tallow, oleostearin and oleo-oil)",
146,1,"170114900","170114900",162,2015,2050,5923267,"Country TL description (WITS), Old SWS","Raw cane sugar, in solid form, not containing added flavouring or colouring matter (excl. for refining, and cane sugar of 1701.13)",
146,1,"200190970","200190970",471,2015,2050,5923268,"Country TL description (WITS)","Vegetables, fruit, nuts and other edible parts of plants, prepared or preserved by vinegar or acetic acid (excl. cucumbers and gherkins, mango chutney, fruit of the genus Capsicum other than sweet peppers or pimentos, sweetcorn, yams, sweet potatoes and similar edible parts of plants, containing >= 5% by weight of starch; mushrooms, palm hearts, olives, sweet peppers, guavas, mangoes, mangosteens, papaws papayas, tamarinds, cashew apples, lychees, jackfruit, sapodillo plums, passion fruit, carambola, pitahaya, coconuts, cashew nuts, brazil nuts, areca betel nuts, colanuts and macadamia nuts)",
146,2,"070999600","070999600",446,2015,2050,5923269,"Country TL description (WITS)","Fresh or chilled sweetcorn",
146,2,"081090750","081090750",619,2015,2050,5923270,"Country TL description (WITS)","Fresh fruit, edible (excl. nuts, bananas, dates, figs, pineapples, avocados, guavas, mangoes, mangosteens, papaws papayas, tamarinds, cashew apples, jackfruit, lychees, sapodillo plums, passion fruit, carambola, pitahaya, citrus fruit, grapes, melons, apples, pears, quinces, apricots, cherries, peaches, plums, sloes, strawberries, raspberries, blackberries, mulberries, loganberries, black, white or red currants, gooseberries, cranberries, fruits of the genus Vaccinium, kiwifruit, durians and persimmons)",
146,2,"110319200","110319200",48,2015,2050,5923271,"Country TL description (WITS), Jellyfish mapping of EU countries and ostensible trading partners","Groats and meal of rye or barley",
146,2,"121299950","121299950",460,2015,2050,5923272,"Country TL description (WITS)","Fruit stones and kernels and other vegetable products, of a kind used primarily for human consumption, n.e.s.",
146,2,"200190970","200190970",471,2015,2050,5923273,"Country TL description (WITS)","Vegetables, fruit, nuts and other edible parts of plants, prepared or preserved by vinegar or acetic acid (excl. cucumbers and gherkins, mango chutney, fruit of the genus Capsicum other than sweet peppers or pimentos, sweetcorn, yams, sweet potatoes and similar edible parts of plants, containing >= 5% by weight of starch; mushrooms, palm hearts, olives, sweet peppers, guavas, mangoes, mangosteens, papaws papayas, tamarinds, cashew apples, lychees, jackfruit, sapodillo plums, passion fruit, carambola, pitahaya, coconuts, cashew nuts, brazil nuts, areca betel nuts, colanuts and macadamia nuts)",
153,1,"01064900","01064900",1169,2014,2050,5923274,"Trademap TL description","Live insects (excluding bees): autres animaux vivants",
153,1,"02074411","02074411",1069,2014,2050,5923275,"Trademap TL description","Autres,frais ou refrigeres , de canard : cuisse",
153,1,"02074416","02074416",1069,2014,2050,5923276,"Trademap TL description","Autres , frais ou refrigeres de canard : magret",
153,1,"02074417","02074417",1075,2014,2050,5923277,"Trademap TL description","Autres , frais ou refrigeres de canard : foies",
153,1,"02074511","02074511",1069,2014,2050,5923278,"Trademap TL description","Autres , congeles de canard : cuisse",
153,1,"02074513","02074513",1069,2014,2050,5923279,"Trademap TL description","Autres, congeles de canard :pilon",
153,1,"02074516","02074516",1069,2014,2050,5923280,"Trademap TL description","Autres , congeles de canard : magret",
153,1,"02074517","02074517",1075,2014,2050,5923281,"Trademap TL description","Autres , congeles de canard: foies gras",
153,1,"02074518","02074518",1075,2014,2050,5923282,"Trademap TL description","Autres congeles de canard : foies : autres",
153,1,"02074519","02074519",1075,2014,2050,5923283,"Trademap TL description","Autres , congeles de canard : autres",
153,1,"02091000","02091000",1037,2014,2050,5923284,"Generic HS2012 to FCL, Old SWS series (could also be mapped to 1040)","Lard de porc sans parties maigres, frais, refrigeres, congeles, sales ou en saumure, sec",
153,1,"04014010","04014010",882,2014,2050,5923285,"Trademap TL description","Lait non conc.ni addit. de sucre ou aut.edulco.d'1teneur en pds de mat. grasses excedan",
153,1,"04014020","04014020",885,2014,2050,5923286,"Trademap TL description","Creme de lait non conc.ni addit. de sucre ou aut.edulco.d'1 teneur en pds de mat. gra.e",
153,1,"04015010","04015010",882,2014,2050,5923287,"Trademap TL description","Lait non conc.ni addit. de sucre ou aut.edulco.d'1teneur en pds de mat.gra.exc.10%",
153,1,"04015020","04015020",885,2014,2050,5923288,"Trademap TL description","Creme de lait non conc.ni addit.de sucre ou aut.edulco.d'1 ten.en pds de mat. gra.exc.1",
153,1,"04079000","04079000",1091,2014,2050,5923289,"Trademap TL description","Autres oeufs d'oiseaux,en coquilles, conserves ou cuits",
153,1,"07099913","07099913",394,2014,2050,5923290,"Trademap TL description","Courgette, frais ou refrigeres",
153,1,"07099917","07099917",463,2014,2050,5923291,"Trademap TL description","Alfa-alfa, frais ou refrigeres",
153,1,"07099918","07099918",463,2014,2050,5923292,"Trademap TL description","Persil, frais ou refrigeres",
153,1,"07099919","07099919",463,2015,2050,5923293,"Trademap TL description","Coriandre ou persil chinois, frais ou refrigeres",
153,1,"07099920","07099920",446,2014,2050,5923294,"Trademap TL description","Mais doux, frais ou refrigeres",
153,1,"07099990","07099990",463,2014,2050,5923295,"Trademap TL description","Autres, frais ou refrigeres",
153,1,"08039000","08039000",486,2014,2050,5923296,"Generic HS2012 to FCL, Old SWS series (could also be mapped to 604)","Fresh or dried bananas (excluding plantains): other",
153,1,"15029000","15029000",869,2014,2050,5923297,"Generic HS 2012 to FCL, Old SWS series (could also be mapped to 871, 949, 979, 1019)","Fat of bovine animals, sheep or goats, other than those of heading 1503: fats other than tallow",
153,1,"17011300","17011300",162,2014,2050,5923298,"Trademap TL description, Old SWS series (could also be mapped to 163)","Sucre de canne mentionne dans la note 2 de sous positions du present chapitre",
153,1,"17011400","17011400",162,2014,2050,5923299,"Trademap TL description, Old SWS series (could also be mapped to 163)","Autres sucres de canne",
153,1,"20098910","20098910",622,2014,2050,5923300,"Trademap TL description","Jus de noni (morinda citrifolia)",
153,1,"20098990","20098990",622,2014,2050,5923301,"Trademap TL description","Autres jus",
153,2,"02074416","02074416",1069,2014,2050,5923302,"Trademap TL description","Autres , frais ou refrigeres de canard : magret",
153,2,"02074417","02074417",1075,2014,2050,5923303,"Trademap TL description","Autres , frais ou refrigeres de canard : foies",
153,2,"02074419","02074419",1069,2014,2050,5923304,"Trademap TL description","Autres , frais ou refrigeres de canard : autres",
153,2,"04079000","04079000",1091,2014,2050,5923305,"Trademap TL description","Autres oeufs d'oiseaux,en coquilles, conserves ou cuits",
153,2,"07099913","07099913",394,2014,2050,5923306,"Trademap TL description","Courgette, frais ou refrigeres",
153,2,"07099914","07099914",394,2014,2050,5923307,"Trademap TL description","Squash, frais ou refrigeres",
153,2,"20098990","20098990",622,2014,2050,5923308,"Trademap TL description","Autres jus",
202,1,"02071411","02071411",1058,2015,2050,5923309,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: breast",
202,1,"02071413","02071413",1059,2015,2050,5923310,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: feet",
202,1,"02071415","02071415",1058,2015,2050,5923311,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: other",
202,1,"02071421","02071421",1059,2015,2050,5923312,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: livers",
202,1,"02071423","02071423",1059,2015,2050,5923313,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: feet",
202,1,"02071425","02071425",1059,2015,2050,5923314,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: heads",
202,1,"02071429","02071429",1059,2015,2050,5923315,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: other",
202,1,"02071493","02071493",1058,2015,2050,5923316,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: quarters",
202,1,"02071495","02071495",1058,2015,2050,5923317,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: wings",
202,1,"02071497","02071497",1058,2015,2050,5923318,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: breast",
202,1,"02071498","02071498",1058,2015,2050,5923319,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: drumsticks",
202,1,"02071499","02071499",1058,2015,2050,5923320,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: other",
202,2,"02071411","02071411",1058,2015,2050,5923321,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: breast",
202,2,"02071413","02071413",1059,2015,2050,5923322,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: feet",
202,2,"02071415","02071415",1058,2015,2050,5923323,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: other",
202,2,"02071421","02071421",1059,2015,2050,5923324,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: livers",
202,2,"02071423","02071423",1059,2015,2050,5923325,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: feet",
202,2,"02071425","02071425",1059,2015,2050,5923326,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: heads",
202,2,"02071429","02071429",1059,2015,2050,5923327,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: other",
202,2,"02071491","02071491",1058,2015,2050,5923328,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: half carcasses",
202,2,"02071493","02071493",1058,2015,2050,5923329,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: quarters",
202,2,"02071495","02071495",1058,2015,2050,5923330,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: wings",
202,2,"02071496","02071496",1058,2015,2050,5923331,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: breasts",
202,2,"02071497","02071497",1058,2015,2050,5923332,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: breast",
202,2,"02071498","02071498",1058,2015,2050,5923333,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: drumsticks",
202,2,"02071499","02071499",1058,2015,2050,5923334,"Trademap TL description","Fowls (gallus domesticus), cuts & offal, frozen: other",
215,1,"01061300","01061300",1126,2015,2050,5923335,"WITS","CAMELS",
215,1,"02074400","02074400",1069,2015,2050,5923336,"WITS, Old SWS series (could also be mapped to 1075)","MEAT OR OFFALS OF DUCKS",
215,1,"02075400","02075400",1073,2013,2050,5923337,"TFL (could also be mapped to 1074)","MEAT OF GEESE",
215,1,"02075500","02075500",1073,2015,2050,5923338,"WITS (could also be mapped to 1074)","MEAT OF GEESE",
215,1,"15029000","15029000",869,2013,2050,5923339,"TFL","FAT OF CATTLE",
215,1,"22030100","22030100",51,2015,2050,5923340,"Country TL description (WITS)","Beer made from malt: Stout and porter",
215,2,"15029000","15029000",869,2014,2050,5923341,"TFL","Fat of Bovine",
219,1,"01011000","01011000",1096,2008,2050,5923342,"ITC, WITS (could also be mapped to 1107 or 1110)","Pure-bred breeding horses and asses",
219,1,"02074500","02074500",1069,2014,2050,5923343,"ITC, Old SWS series (could also be mapped to 1075)","Frozen cuts and edible offal of domestic ducks",
219,1,"02075500","02075500",1074,2014,2050,5923344,"ITC, Old SWS series (could also be mapped to 1073)","Frozen cuts and edible offal of domestic geese",
219,1,"02091000","02091000",1037,2014,2050,5923345,"Old SWS series (ITC and WITS: only 02090000 available)","Pig fat, free of lean meat, and poultry fat, not rendered or otherwise extracted, fresh, chilled, . . .",
219,1,"04014000","04014000",885,2014,2050,5923346,"Trademap TL description, Jellyfish (previously, anything above 6% fat was mapped to 885 cream)","Milk and cream of a fat content by weight of > 6% but <= 10%, not concentrated nor containing added sugar or other sweetening matter",
219,1,"04015000","04015000",885,2014,2050,5923347,"Trademap TL description, Jellyfish (previously, anything above 6% fat was mapped to 885 cream)","Milk and cream of a fat content by weight of > 10%, not concentrated nor containing added sugar or other sweetening matter",
219,1,"04079090","04079090",1062,2014,2050,5923348,"ITC, Old SWS series (could also be mapped to 1091)","Birds' eggs, in shell, fresh, preserved or cooked: Other: Other",
219,1,"07099900","07099900",463,2014,2050,5923349,"ITC","Other vegetables, fresh or chilled: Other: Other",
219,1,"11032000","11032000",111,2009,2050,5923350,"TFL, ITC (based on the Old SWS series, could also be mapped to 16, 38, 58, 72)","Cereal groats, meal and pellets: pellets",
219,1,"15029000","15029000",869,2014,2050,5923351,"ITC (could be mapped to 869, 871, 949, 979, 1019. No time series in the Old SWS)","Fats of bovine animals, sheep or goats, other than those of heading 15.03: Other",
219,1,"17011300","17011300",162,2014,2050,5923352,"ITC (could also be mapped to 163)","Cane or beet sugar and chemically pure sucrose, in solid form: Raw sugar not containing added . . .",
219,1,"17011400","17011400",162,2014,2050,5923353,"ITC (could also be mapped to 163)","Cane or beet sugar and chemically pure sucrose, in solid form: Raw sugar not containing added . . .",
219,1,"41012000","41012000",921,2008,2050,5923354,"Old SWS series (could also be mapped to 920, 921, 922, 928, 929, 930, 958, 959, 1103, 1104, 1105)","Raw hides and skins of bovine (including buffalo) or equine animalswhether or not dehaired or split. Whole hides and skins, of a weight per skin not exceeding 8 kg when simply dried, 10 kg when dry-salted, or 16 kg when fresh, wet-salted or otherwise pre",
219,1,"410150","410150",921,2011,2050,5923355,"TFL, Old SWS series (could also be mapped to 920 or 1103)","Whole bovine (incl. buffalo)/equine hides & skins, weight >16kg",
219,2,"08039000","08039000",486,2014,2050,5923356,"Old SWS series (could also be mapped to 604)","Bananas, including plantains, fresh or dried: Other",
220,1,"41032000","41032000",1216,2008,2050,5923357,"Generic HS 2007 to FCL, Old SWS series (could also be mapped to 1214, 1215)","Other raw hides and skins (fresh or salted.): Of reptiles",
230,1,"010613","010613",1126,2015,2050,5923358,"Generic HS2012 to FCL (could also be mapped to 1157) ","Camels and other camelids (camelidae)",
230,1,"010649","010649",1171,2014,2050,5923359,"Generic HS2012 to FCL (could also be mapped to 1169) ","Other insects (excl bees)",
230,1,"020744","020744",1069,2014,2050,5923360,"Generic HS2012 to FCL (could also be mapped to 1075) ","Other meat or edible offals of ducks, fresh or chilled",
230,1,"020745","020745",1069,2014,2050,5923361,"Generic HS2012 to FCL (could also be mapped to 1075) ","Other meat or edible offals of ducks, frozen",
230,1,"020755","020755",1073,2014,2050,5923362,"Generic HS2012 to FCL (could also be mapped to 1074) ","Other meat or edible offals of geese, frozen",
230,1,"020910","020910",1037,2014,2050,5923363,"Generic HS2012 to FCL, Old SWS series (could also be mapped to 1040) ","Pig fat, free of lean meat, and poultry fat, not rendered or otherwise extracted, fresh, chilled, frozen, salted, in brine, dried or smoked",
230,1,"040140","040140",885,2014,2050,5923364,"Generic HS2012 to FCL, Jellyfish (previously, anything above 6% fat was mapped to 885 cream), (could also be mapped to 882,883,951,982,1020,1130) ","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight exceeding 6% but not exceeding 10%",
230,1,"040150","040150",885,2014,2050,5923365,"Generic HS2012 to FCL, Jellyfish (previously, anything above 6% fat was mapped to 885 cream), (could also be mapped to 882,883,951,982,1020,1130) ","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight exceeding 10%",
230,1,"070999","070999",463,2014,2050,5923366,"Generic HS2012 to FCL (could also be mapped to 378,430,446) ","Fresh or chilled vegetables n.e.s.",
230,1,"080390","080390",486,2014,2050,5923367,"Generic HS2012 to FCL (could also be mapped to 604) ","Fresh or dried bananas (excluding plantains)",
230,1,"170113","170113",162,2014,2050,5923368,"Generic HS2012 to FCL (could also be mapped to 163) ","Raw cane sugar, in solid form, not containing added flavouring or colouring matter, obtained without centrifugation, with sucrose content 69° to 93°, containing only natural anhedral microcrystals (see subheading note 2.)",
230,1,"170114","170114",162,2014,2050,5923369,"Generic HS2012 to FCL (could also be mapped to 163) ","Raw cane sugar, in solid form, not containing added flavouring or colouring matter (excluding cane sugar of 1701 13)",
230,1,"200989","200989",622,2014,2050,5923370,"Generic HS2012 to FCL (could also be mapped to 466,538,539,583) ","Juice of fruit or vegetables, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, and juice of citrus fruit, pineapples, tomatoes, grapes, incl. grape must, apples and cranberries)",
230,2,"020744","020744",1069,2014,2050,5923371,"Generic HS2012 to FCL (could also be mapped to 1075) ","Other meat or edible offals of ducks, fresh or chilled",
230,2,"020745","020745",1069,2014,2050,5923372,"Generic HS2012 to FCL (could also be mapped to 1075) ","Other meat or edible offals of ducks, frozen",
230,2,"020755","020755",1073,2014,2050,5923373,"Generic HS2012 to FCL (could also be mapped to 1074) ","Other meat or edible offals of geese, frozen",
230,2,"020910","020910",1037,2014,2050,5923374,"Generic HS2012 to FCL, Old SWS series (could also be mapped to 1040) ","Pig fat, free of lean meat, and poultry fat, not rendered or otherwise extracted, fresh, chilled, frozen, salted, in brine, dried or smoked",
230,2,"040140","040140",885,2014,2050,5923375,"Generic HS2012 to FCL, Jellyfish (previously, anything above 6% fat was mapped to 885 cream), (could also be mapped to 882,883,951,982,1020,1130) ","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight exceeding 6% but not exceeding 10%",
230,2,"040150","040150",885,2014,2050,5923376,"Generic HS2012 to FCL, Jellyfish (previously, anything above 6% fat was mapped to 885 cream), (could also be mapped to 882,883,951,982,1020,1130) ","Milk and cream, not concentrated nor containing added sugar or other sweetening matter: Of a fat content, by weight exceeding 10%",
230,2,"040790","040790",1062,2014,2050,5923377,"Generic HS2012 to FCL (could also be mapped to 1091) ","Other birds eggs, in shell, preserved or cooked",
230,2,"070999","070999",463,2014,2050,5923378,"Generic HS2012 to FCL (could also be mapped to 378,430,446) ","Fresh or chilled vegetables n.e.s.",
230,2,"080390","080390",486,2014,2050,5923379,"Generic HS2012 to FCL (could also be mapped to 604) ","Fresh or dried bananas (excluding plantains)",
230,2,"150290","150290",869,2014,2050,5923380,"Generic HS2012 to FCL (could also be mapped to 871,949,979,1019) ","Fats of bovine animals, sheep or goats (excluding tallow, oleostearin and oleo-oil)",
230,2,"200989","200989",622,2014,2050,5923381,"Generic HS2012 to FCL (could also be mapped to 466,538,539,583) ","Juice of fruit or vegetables, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, and juice of citrus fruit, pineapples, tomatoes, grapes, incl. grape must, apples and cranberries)",
238,1,"02074500","02074500",1069,2014,2050,5923382,"Country TL description (WITS) (could also be mapped to 1075)","Meat and edible offal, of ducks",
238,1,"02084000","02084000",1166,2014,2050,5923383,"Country TL description (WITS) (could also be mapped to 1075)","Fresh, chilled or frozen meat and edible offal of whales",
238,1,"04015000","04015000",885,2014,2050,5923384,"Trademap TL description, Jellyfish (previously, anything above 6% fat was mapped to 885 cream)","Milk and cream, not concentrated nor containing added sugar or other sweetening matter",
238,1,"07099900","07099900",463,2014,2050,5923385,"Country TL description (WITS) (could also 378, 430, 446)","Other vegetables fresh",
238,1,"08039000","08039000",486,2014,2050,5923386,"Trademap TL description (could also be mapped to 604)","fresh or dried bananas (excluding plantains)",
238,1,"17011300","17011300",162,2014,2050,5923387,"Country TL description (WITS) (could also be mapped to 163)","Sugar raw-centrifugal",
238,1,"17011400","17011400",162,2014,2050,5923388,"Country TL description (WITS) (could also be mapped to 163)","Sugar raw-centrifugal",
238,2,"01061300","01061300",1126,2014,2050,5923389,"Country TL description (WITS) (could also be mapped to 1157)","Camels",
238,2,"02074500","02074500",1069,2015,2050,5923390,"Country TL description (WITS) (could also be mapped to 1167)","Meat and edible offal, of ducks",
238,2,"07099900","07099900",463,2014,2050,5923391,"Country TL description (WITS) (could also 378, 430, 446)","Other vegetables fresh",
238,2,"08039000","08039000",486,2014,2050,5923392,"Trademap TL description (could also be mapped to 604)","fresh or dried bananas (excluding plantains)",
299,1,"010210","010210",866,2014,2050,5923393,"Generic HS2007 to FCL (could also be mapped to 946)","Pure-bred breeding bovines",
299,1,"010290","010290",1171,2014,2050,5923394,"Trademap TL description","Live bovine animals (excluding cattle and buffalo)",
299,1,"010599","010599",1057,2014,2050,5923395,"Generic HS2012 to FCL (could also be mapped to 1068,1072) ","Live domestic ducks, geese, turkeys and guinea fowls, weighing > 185 g",
299,1,"010612","010612",1171,2014,2050,5923396,"Generic HS2012 to FCL (could also be mapped to 1169) ","Other live animals: mammals: whales, dolphins and porpoises",
299,1,"010619","010619",1171,2015,2050,5923397,"Generic HS2012 to FCL (could also be mapped to 1150,1169)","Live mammals (excluding primates, whales, dolphins and porpoises, manatees and dugongs, seals, sea lions and walruses, camels and other camelids, rabbits and hares, horses, asses, mules, hinnies, bovines, pigs, sheep and goats)",
299,1,"020110","020110",867,2015,2050,5923398,"Generic HS2012 to FCL (could also be mapped to 947) ","Carcases or half-carcases of bovine animals, fresh or chilled",
299,1,"020120","020120",867,2014,2050,5923399,"Generic HS2012 to FCL (could also be mapped to 947) ","Fresh or chilled bovine cuts, with bone in (excluding carcases and 1/2 carcases)",
299,1,"020130","020130",870,2014,2050,5923400,"Generic HS2012 to FCL (could also be mapped to 947) ","Fresh or chilled bovine meat, boneless",
299,1,"020210","020210",867,2015,2050,5923401,"Generic HS2012 to FCL (could also be mapped to 947) ","Frozen bovine carcases and half-carcases",
299,1,"020220","020220",867,2014,2050,5923402,"Generic HS2012 to FCL (could also be mapped to 947)","Frozen bovine cuts, with bone in (excluding carcases and half-carcases)",
299,1,"020230","020230",870,2014,2050,5923403,"Generic HS2012 to FCL (could also be mapped to 947) ","Frozen, boneless meat of bovine animals",
299,1,"020610","020610",868,2014,2050,5923404,"Generic HS2012 to FCL (could also be mapped to 948) ","Fresh or chilled edible offal of bovine animals",
299,1,"020621","020621",868,2014,2050,5923405,"Generic HS2012 to FCL (could also be mapped to 948) ","Frozen edible bovine tongues",
299,1,"020622","020622",868,2015,2050,5923406,"Generic HS2012 to FCL (could also be mapped to 7948)","Frozen edible bovine livers",
299,1,"020629","020629",868,2014,2050,5923407,"Generic HS2012 to FCL (could also be mapped to 948) ","Frozen edible bovine offal (excluding tongues and livers)",
299,1,"020690","020690",978,2014,2050,5923408,"Generic HS2012 to FCL (could also be mapped to 1018,1098,1167) ","Frozen edible offal of sheep, goats, horses, asses, mules and hinnies",
299,1,"020726","020726",1080,2014,2050,5923409,"Generic HS2012 to FCL (could also be mapped to 1081)","Fresh or chilled cuts and edible offal of turkeys of the species domesticus",
299,1,"020727","020727",1080,2014,2050,5923410,"Generic HS2012 to FCL (could also be mapped to 1081) ","Frozen cuts and edible offal of turkeys of the species domesticus",
299,1,"020734","020734",1074,2014,2050,5923411,"Generic HS2007 to FCL (could also be mapped to 1075)","Fresh or chilled edible fatty livers of ducks or geese of the species domesticus",
299,1,"040110","040110",888,2014,2050,5923412,"Generic HS2012 to FCL (could also be mapped to 954,985,1023) ","Milk and cream of a fat content by weight of <= 1%, not concentrated nor containing added sugar or other sweetening matter",
299,1,"040120","040120",882,2014,2050,5923413,"Generic HS2012 to FCL (could also be mapped to 883,908,951,982,1020,1130) ","Milk and cream of a fat content by weight of > 1% but <= 6%, not concentrated nor containing added sugar or other sweetening matter",
299,1,"040299","040299",896,2015,2050,5923414,"Generic HS2012 to FCL (could also be mapped to 889)","Milk and cream, concentrated and sweetened (excluding in solid forms)",
299,1,"040310","040310",891,2014,2050,5923415,"Generic HS2012 to FCL (could also be mapped to 892) ","Yogurt, whether or not flavoured or containing added sugar or other sweetening matter, fruits, nuts or cocoa",
299,1,"040390","040390",893,2014,2050,5923416,"Generic HS2012 to FCL (could also be mapped to 899)","Buttermilk, curdled milk and cream, kephir and other fermented or acidified milk and cream, whether or not concentrated or flavoured or containing added sugar or other sweetening matter, fruits, nuts or cocoa (excluding yogurt)",
299,1,"040410","040410",903,2014,2050,5923417,"Generic HS2012 to FCL (could also be mapped to 890,900)","Whey and modified whey, whether or not concentrated or containing added sugar or other sweetening matter",
299,1,"040510","040510",886,2014,2050,5923418,"Generic HS2012 to FCL (could also be mapped to 952,983,1022) ","Butter (excluding dehydrated butter and ghee)",
299,1,"040520","040520",886,2014,2050,5923419,"Generic HS2012 to FCL (could also be mapped to 952,983,1022)","Dairy spreads of a fat content, by weight, of >= 39% but < 80%",
299,1,"040590","040590",887,2014,2050,5923420,"Generic HS2012 to FCL (could also be mapped to 953,1022)","Fats and oils derived from milk, and dehydrated butter and ghee (excluding natural butter, recombined butter and whey butter)",
299,1,"040610","040610",901,2014,2050,5923421,"Generic HS2012 to FCL (could also be mapped to 904,905,955,984,1021)","Fresh cheese unripened or uncured cheese, incl. whey cheese, and curd",
299,1,"040700","040700",1062,2014,2050,5923422,"Generic HS2007 to FCL (could also be mapped to 1091)","Birds' eggs, in shell, fresh, preserved or cooked",
299,1,"060499","060499",1293,2014,2050,5923423,"Generic HS2007 to FCL unique six-digit match","Foliage, branches and other parts of plants, without flowers or flower buds, grasses, for bouquets or ornamental purposes, dried, dyed, bleached, impregnated or otherwise prepared",
299,1,"070310","070310",403,2014,2050,5923424,"Generic HS2012 to FCL (could also be mapped to 402)","Fresh or chilled onions and shallots",
299,1,"070610","070610",426,2014,2050,5923425,"Generic HS2012 to FCL, FCL descriptions (could also be mapped to 463)","Fresh or chilled carrots and turnips",
299,1,"070820","070820",414,2014,2050,5923426,"Generic HS2012 to FCL (could also be mapped to 423) ","Fresh or chilled beans Vigna spp., Phaseolus spp., shelled or unshelled",
299,1,"070990","070990",463,2014,2050,5923427,"Generic HS2012 to FCL (could also be mapped to 378,430,446) ","Fresh or chilled vegetables (excluding potatoes, tomatoes, vegetables of the Allium spp., cabbages of the genus Brassica, lettuces of the species Lactuca sativa and Cichorium, carrots, turnips, salad beetroot, salsify, celeriac, radishes and similar edible roots, cucumbers and gherkins, leguminous vegetables, asparagus, aubergines, mushrooms, truffles, fruits of the genus Capsicum or of the genus Pimenta, spinach, New Zealand spinach and orache spinach)",
299,1,"070999","070999",463,2015,2050,5923428,"Generic HS2012 to FCL (could also be mapped to 378,430,446) ","Fresh or chilled vegetables n.e.s.",
299,1,"071490","071490",149,2014,2050,5923429,"Generic HS2007 to FCL (could also be mapped to 135,136,137,151) ","Arrowroot, salep, Jerusalem artichokes and similar roots and tubers with high starch or inulin content, fresh, chilled, frozen or dried, whether or not sliced or in the form of pellets, and sago pith (excluding manioc cassava, sweet potatoes, yams, taro and yautia)",
299,1,"080240","080240",220,2014,2050,5923430,"Generic HS2007 to FCL unique six-digit match","Fresh or dried chestnuts (Castanea spp.), whether or not shelled or peeled",
299,1,"080250","080250",223,2014,2050,5923431,"Generic HS2007 to FCL unique six-digit match","Fresh or dried pistachios, whether or not shelled or peeled",
299,1,"080300","080300",486,2014,2050,5923432,"Generic HS2007 to FCL (could also be mapped to 489,604) ","Bananas, incl. plantains, fresh or dried",
299,1,"080420","080420",570,2014,2050,5923433,"Generic HS2012 to FCL (could also be mapped to 569) ","Fresh or dried figs",
299,1,"080450","080450",571,2014,2050,5923434,"Generic HS2012 to FCL (could also be mapped to 603,604)","Fresh or dried guavas, mangoes and mangosteens",
299,1,"080820","080820",521,2014,2050,5923435,"Generic HS2007 to FCL (could also be mapped to 523) ","Fresh pears and quinces",
299,1,"080920","080920",531,2014,2050,5923436,"Generic HS2007 to FCL (could also be mapped to 530,541) ","Fresh cherries",
299,1,"081090","081090",619,2014,2050,5923437,"Generic HS2012 to FCL (could also be mapped to 541,591)","Fresh tamarinds, cashew apples, jackfruit, lychees, sapodillo plums, passion fruit, carambola, pitahaya and other edible fruit (excluding nuts, bananas, dates, figs, pineapples, avocados, guavas, mangoes, mangosteens, papaws papayas, citrus fruit, grapes, melons, apples, pears quinces, apricots, cherries, peaches, plums, sloes, strawberries, raspberries, mulberries, blackberries, loganberries, cranberries, fruits of the genus Vaccinium, kiwifruit, durians, persimmons, black-, white- and redcurrants and gooseberries)",
299,1,"090190","090190",658,2014,2050,5923438,"Generic HS2012 to FCL (could also be mapped to 660) ","Coffee husks and skins; coffee substitutes containing coffee in any proportion",
299,1,"090420","090420",689,2014,2050,5923439,"Generic HS2007 to FCL unique six-digit match","Fruits of the genus Capsicum or of the genus Pimenta, dried or crushed or ground",
299,1,"090500","090500",692,2014,2050,5923440,"Generic HS2007 to FCL unique six-digit match","Vanilla",
299,1,"090700","090700",698,2014,2050,5923441,"Generic HS2007 to FCL unique six-digit match","Cloves, whole fruit, cloves and stems",
299,1,"090810","090810",702,2014,2050,5923442,"Generic HS2007 to FCL unique six-digit match","Nutmeg",
299,1,"090830","090830",702,2014,2050,5923443,"Generic HS2007 to FCL unique six-digit match","Cardamoms",
299,1,"090910","090910",711,2014,2050,5923444,"Generic HS2007 to FCL unique six-digit match","Seeds of anise or badian",
299,1,"090920","090920",711,2014,2050,5923445,"Generic HS2007 to FCL unique six-digit match","Coriander seeds",
299,1,"090930","090930",711,2014,2050,5923446,"Generic HS2007 to FCL unique six-digit match","Cumin seeds",
299,1,"100110","100110",15,2014,2050,5923447,"Generic HS2007 to FCL unique six-digit match","Durum wheat",
299,1,"100190","100190",15,2014,2050,5923448,"Generic HS2007 to FCL unique six-digit match","Wheat and meslin (excluding durum wheat)",
299,1,"100300","100300",44,2014,2050,5923449,"Generic HS2007 to FCL unique six-digit match","Barley",
299,1,"100400","100400",75,2014,2050,5923450,"Generic HS2007 to FCL unique six-digit match","Oats",
299,1,"100630","100630",31,2014,2050,5923451,"Generic HS2012 to FCL (could also be mapped to 29) ","Semi-milled or wholly milled rice, whether or not polished or glazed",
299,1,"100890","100890",108,2014,2050,5923452,"Generic HS2012 to FCL (could also be mapped to 103)","Cereals (excluding wheat and meslin, rye, barley, oats, maize, rice, grain sorghum, buckwheat, millet, canary seeds, fonio, quinoa and triticale)",
299,1,"110290","110290",111,2014,2050,5923453,"Generic HS2012 to FCL (could also be mapped to 38,48,72,80,84,90,95,98,104)","Cereal flours (excluding wheat, meslin and maize)",
299,1,"110319","110319",111,2014,2050,5923454,"Generic HS2012 to FCL (could also be mapped to 38,48,72,80,84,90,95,98,104)","Groats and meal of cereals (excluding wheat and maize)",
299,1,"110419","110419",113,2014,2050,5923455,"Generic HS2012 to FCL (could also be mapped to 45,46)","Rolled or flaked grains of cereals (excluding oats)",
299,1,"110429","110429",113,2014,2050,5923456,"Generic HS2012 to FCL (could also be mapped to 21,45,46) ","Grains of cereals, hulled, pearled, sliced, kibbled or otherwise worked (excluding rolled, flaked, flour, pellets, and oats and maize, and husked and semi- or wholly milled rice and broken rice)",
299,1,"110430","110430",19,2014,2050,5923457,"Generic HS2012 to FCL (could also be mapped to 57)","Germ of cereals, whole, rolled, flaked or ground",
299,1,"120100","120100",236,2014,2050,5923458,"Generic HS2007 to FCL unique six-digit match","Soya beans, whether or not broken",
299,1,"120210","120210",242,2014,2050,5923459,"Generic HS2007 to FCL unique six-digit match","Groundnuts in shell, not roasted or otherwise cooked",
299,1,"120220","120220",243,2014,2050,5923460,"Generic HS2007 to FCL unique six-digit match","Shelled groundnuts, whether or not broken (excluding roasted or otherwise cooked)",
299,1,"150200","150200",869,2014,2050,5923461,"Generic HS2007 to FCL (could also be mapped to 871,949,979,1019,1225) ","Fats of bovine animals, sheep or goats (excluding lard stearin, lard oil, oleostearin, oleooil and tallow oil, not emulsified or mixed or otherwise prepared)",
299,1,"150290","150290",869,2015,2050,5923462,"Generic HS2012 to FCL (could also be mapped to 871,949,979,1019) ","Fats of bovine animals, sheep or goats (excluding tallow, oleostearin and oleo-oil)",
299,1,"150600","150600",1168,2014,2050,5923463,"Generic HS2012 to FCL (could also be mapped to 1129,1160) ","Other animal fats and oils and their fractions, whether or not refined, but not chemically modified (excluding pig fat, poultry fat, fats of bovine animals, sheep and goats, fats of fish and other marine animals, lard stearin, lard oil, oloestearin, oleo-oil, tallow oil, wool grease and fatty substances derived therefrom)",
299,1,"151211","151211",268,2014,2050,5923464,"Generic HS2012 to FCL (could also be mapped to 281)","Crude sunflower-seed or safflower oil",
299,1,"151219","151219",268,2014,2050,5923465,"Generic HS2012 to FCL (could also be mapped to 281)","Sunflower-seed or safflower oil and their fractions, whether or not refined, but not chemically modified (excluding crude)",
299,1,"151419","151419",271,2014,2050,5923466,"Generic HS2012 to FCL (could also be mapped to 293) ","Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2% and its fractions, whether or not refined, but not chemically modified (excluding crude)",
299,1,"151499","151499",271,2014,2050,5923467,"Generic HS2012 to FCL (could also be mapped to 293)","High erucic acid rape or colza oil fixed oil which has an erucic acid content of >= 2%, and mustard oil, and fractions thereof, whether or not refined, but not chemically modified (excluding crude)",
299,1,"151590","151590",340,2014,2050,5923468,"Generic HS2012 to FCL (could also be mapped to 36,264,278,297,306,307,313,337) ","Fixed vegetable fats and oils and their fractions, whether or not refined, but not chemically modified (excluding soya-bean, groundnut, olive, palm, sunflower-seed, safflower, cotton-seed, coconut, palm kernel, babassu, rape, colza and mustard, linseed, maize, castor and sesame oil)",
299,1,"151620","151620",1275,2014,2050,5923469,"Generic HS2012 to FCL (could also be mapped to 1273)","Vegetable fats and oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared",
299,1,"151790","151790",1243,2014,2050,5923470,"Generic HS2012 to FCL (could also be mapped to 1241) ","Edible mixtures or preparations of animal or vegetable fats or oils and edible fractions of different fats or oils (excluding fats, oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared, mixtures of olive oils and their fractions, and solid margarine)",
299,1,"152190","152190",1183,2014,2050,5923471,"Generic HS2012 to FCL (could also be mapped to 1295)","Beeswax, other insect waxes and spermaceti, whether or not refined or coloured",
299,1,"160100","160100",874,2014,2050,5923472,"Generic HS2012 to FCL (could also be mapped to 1041)","Sausages and similar products, of meat, offal or blood; food preparations based on these products",
299,1,"170111","170111",162,2014,2050,5923473,"Generic HS2007 to FCL (could also be mapped to 163)","Raw cane sugar (excluding added flavouring or colouring)",
299,1,"170112","170112",162,2014,2050,5923474,"Generic HS2012 to FCL (could also be mapped to 163) ","Raw beet sugar (excluding added flavouring or colouring)",
299,1,"170114","170114",162,2015,2050,5923475,"Generic HS2012 to FCL (could also be mapped to 163)","Raw cane sugar, in solid form, not containing added flavouring or colouring matter (excluding cane sugar of 1701 13)",
299,1,"170290","170290",167,2014,2050,5923476,"Generic HS2012 to FCL (could also be mapped to 155,175)","Sugars in solid form, incl. invert sugar and chemically pure maltose, and sugar and sugar syrup blends containing in the dry state 50% by weight of fructose, not flavoured or coloured, artificial honey, whether or not mixed with natural honey and caramel (excluding cane or beet sugar, chemically pure sucrose, lactose, maple sugar, glucose, fructose, and syrups thereof)",
299,1,"190190","190190",115,2014,2050,5923477,"Generic HS2012 to FCL (could also be mapped to 50) ","Malt extract; food preparations of flour, groats, meal, starch or malt extract, not containing cocoa or containing < 40% by weight of cocoa calculated on a totally defatted basis, n.e.s. and food preparations of milk, cream, butter milk, sour milk, sour cream, whey, yogurt, kephir or similar goods of heading 0401 to 0404, not containing cocoa or containing < 5% by weight of cocoa calculated on a totally defatted basis, n.e.s. (excluding for infant use, put up for retail sale, and mixes and doughs for the preparation of bakers' wares of heading 1905)",
299,1,"190590","190590",22,2014,2050,5923478,"Generic HS2012 to FCL (could also be mapped to 110)","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products (excluding crispbread, gingerbread and the like, sweet biscuits, waffles, wafers not mentioned, rusks, toasted bread and similar toasted products)",
299,1,"200190","200190",262,2014,2050,5923479,"Generic HS2012 to FCL (could also be mapped to 471)","Vegetables, fruit, nuts and other edible parts of plants, prepared or preserved by vinegar or acetic acid (excluding cucumbers and gherkins)",
299,1,"200320","200320",451,2014,2050,5923480,"Generic HS2007 to FCL unique six-digit match","Truffles, prepared or preserved otherwise than by vinegar or acetic acid",
299,1,"200490","200490",475,2014,2050,5923481,"Generic HS2012 to FCL (could also be mapped to 447,262)","Vegetables and mixtures of vegetables, prepared or preserved otherwise than by vinegar or acetic acid, frozen (excluding preserved by sugar, and tomatoes, mushrooms, truffles and potatoes, unmixed)",
299,1,"200811","200811",246,2014,2050,5923482,"Generic HS2012 to FCL (could also be mapped to 247)","Groundnuts, prepared or preserved (excluding preserved with sugar)",
299,1,"200899","200899",623,2014,2050,5923483,"Generic HS2012 to FCL (could also be mapped to 466,538,539,593) ","Fruit and other edible parts of plants, prepared or preserved, whether or not containing added sugar or other sweetening matter or spirit (excluding prepared or preserved with vinegar, preserved with sugar but not laid in syrup, jams, fruit jellies, marmalades, fruit purée and pastes, obtained by cooking, and nuts, groundnuts and other seeds, pineapples, citrus fruits, pears, apricots, cherries, peaches, strawberries, palm hearts and cranberries)",
299,1,"200911","200911",492,2014,2050,5923484,"Generic HS2012 to FCL (could also be mapped to 491) ","Frozen orange juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
299,1,"200931","200931",513,2014,2050,5923485,"Generic HS2012 to FCL (could also be mapped to 496,498)","Single citrus fruit juice, unfermented, Brix value <= 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
299,1,"200950","200950",390,2014,2050,5923486,"Generic HS2012 to FCL (could also be mapped to 390)","Tomato juice, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
299,1,"200980","200980",622,2014,2050,5923487,"Generic HS2012 to FCL (could also be mapped to 466,538,539,593) ","Juice of fruit or vegetables, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, and juice of citrus fruit, pineapples, tomatoes, grapes, incl. grape must and apples)",
299,1,"200989","200989",622,2015,2050,5923488,"Generic HS2012 to FCL (could also be mapped to 466,538,539,583)","Juice of fruit or vegetables, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, and juice of citrus fruit, pineapples, tomatoes, grapes, incl. grape must, apples and cranberries)",
299,1,"200990","200990",622,2014,2050,5923489,"Generic HS2012 to FCL (could also be mapped to 466)","Mixtures of fruit juices, incl. grape must, and vegetable juices, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit)",
299,1,"210390","210390",1232,2014,2050,5923490,"Generic HS2012 to FCL (could also be mapped to 240) ","Preparations for sauces and prepared sauces; mixed condiments and seasonings (excluding soya sauce, tomato ketchup and other tomato sauces, mustard, and mustard flour and meal)",
299,1,"210610","210610",1232,2014,2050,5923491,"Generic HS2012 to FCL (could also be mapped to 240,241)","Protein concentrates and textured protein substances",
299,1,"210690","210690",1232,2014,2050,5923492,"Generic HS2012 to FCL (could also be mapped to 674) ","Food preparations, n.e.s.",
299,1,"220300","220300",51,2014,2050,5923493,"Generic HS2012 to FCL (could also be mapped to 66,82,86) ","Beer made from malt",
299,1,"220600","220600",517,2014,2050,5923494,"Generic HS2012 to FCL (could also be mapped to 26,39)","Cider, perry, mead and other fermented beverages and mixtures of fermented beverages and non-alcoholic beverages, n.e.s. (excluding beer, wine or fresh grapes, grape must, vermouth and other wine of fresh grapes flavoured with plants or aromatic substances)",
299,1,"230990","230990",845,2014,2050,5923495,"Generic HS2012 to FCL (could also be mapped to 653,840,841,842,843,849,850,851,852,853,854,855,1259)","Preparations of a kind used in animal feeding (excluding dog or cat food put up for retail sale)",
299,1,"240310","240310",831,2014,2050,5923496,"Generic HS2007 to FCL unique six-digit match","Smoking tobacco, whether or not containing tobacco substitutes in any proportion",
299,1,"330129","330129",753,2014,2050,5923497,"Generic HS2012 to FCL (could also be mapped to 737)","Essential oils, whether or not terpeneless, incl. concretes and absolutes (excluding those of citrus fruit and mint)",
299,1,"410120","410120",920,2014,2050,5923498,"Generic HS2012 to FCL (could also be mapped to 921,922,928,929,930,958,959,1103,1104,1105)","Whole raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired, unsplit, of a weight per skin <= 8 kg when simply dried, <= 10 kg when dry-salted, or <= 16 kg when fresh, wet-salted or otherwise preserved (excluding tanned, parchment-dressed or further prepared)",
299,1,"510529","510529",1008,2015,2050,5923499,"Generic HS2012 to FCL (could also be mapped to 1010) ","Wool, combed (excluding that in fragments open tops)",
299,1,"530500","530500",809,2014,2050,5923500,"Generic HS2012 to FCL (could also be mapped to 821)","Coconut, abaca Manila hemp or Musa textilis Nee, ramie, agave and other vegetable textile fibres, n.e.s., raw or processed, but not spun; tow, noils and waste of such fibres, incl. yarn waste and garnetted stock",
299,2,"020120","020120",867,2015,2050,5923501,"Generic HS2012 to FCL (could also be mapped to 947) ","Fresh or chilled bovine cuts, with bone in (excluding carcases and 1/2 carcases)",
299,2,"020130","020130",870,2015,2050,5923502,"Generic HS2012 to FCL (could also be mapped to 947) ","Fresh or chilled bovine meat, boneless",
299,2,"020220","020220",867,2014,2050,5923503,"Generic HS2012 to FCL (could also be mapped to 947)","Frozen bovine cuts, with bone in (excluding carcases and half-carcases)",
299,2,"020727","020727",1080,2014,2050,5923504,"Generic HS2012 to FCL (could also be mapped to 1081) ","Frozen cuts and edible offal of turkeys of the species domesticus",
299,2,"040110","040110",888,2014,2050,5923505,"Generic HS2012 to FCL (could also be mapped to 954,985,1023) ","Milk and cream of a fat content by weight of <= 1%, not concentrated nor containing added sugar or other sweetening matter",
299,2,"040120","040120",882,2014,2050,5923506,"Generic HS2012 to FCL (could also be mapped to 883,908,951,982,1020,1130) ","Milk and cream of a fat content by weight of > 1% but <= 6%, not concentrated nor containing added sugar or other sweetening matter",
299,2,"040310","040310",891,2014,2050,5923507,"Generic HS2012 to FCL (could also be mapped to 892) ","Yogurt, whether or not flavoured or containing added sugar or other sweetening matter, fruits, nuts or cocoa",
299,2,"040390","040390",893,2014,2050,5923508,"Generic HS2012 to FCL (could also be mapped to 899)","Buttermilk, curdled milk and cream, kephir and other fermented or acidified milk and cream, whether or not concentrated or flavoured or containing added sugar or other sweetening matter, fruits, nuts or cocoa (excluding yogurt)",
299,2,"040510","040510",886,2014,2050,5923509,"Generic HS2012 to FCL (could also be mapped to 952,983,1022)","Butter (excluding dehydrated butter and ghee)",
299,2,"040590","040590",887,2014,2050,5923510,"Generic HS2012 to FCL (could also be mapped to 953,1022)","Fats and oils derived from milk, and dehydrated butter and ghee (excluding natural butter, recombined butter and whey butter)",
299,2,"040610","040610",901,2014,2050,5923511,"Generic HS2012 to FCL (could also be mapped to 904,905,955,984,1021)","Fresh cheese unripened or uncured cheese, incl. whey cheese, and curd",
299,2,"040700","040700",1062,2014,2050,5923512,"Generic HS2007 to FCL (could also be mapped to 1091)","Birds' eggs, in shell, fresh, preserved or cooked",
299,2,"060491","060491",1293,2014,2050,5923513,"Generic HS2007 to FCL unique six-digit match","Foliage, branches and other parts of plants, without flowers or flower buds, grasses, fresh, for bouquets or ornamental purposes",
299,2,"070310","070310",403,2014,2050,5923514,"Generic HS2012 to FCL (could also be mapped to 402)","Fresh or chilled onions and shallots",
299,2,"070610","070610",426,2014,2050,5923515,"Generic HS2012 to FCL, FCL descriptions (could also be mapped to 463)","Fresh or chilled carrots and turnips",
299,2,"070820","070820",414,2014,2050,5923516,"Generic HS2012 to FCL (could also be mapped to 423) ","Fresh or chilled beans Vigna spp., Phaseolus spp., shelled or unshelled",
299,2,"070990","070990",463,2014,2050,5923517,"Generic HS2007 to FCL (could also be mapped to 260,378,394,430,446)","Fresh or chilled vegetables (excluding potatoes, tomatoes, vegetables of the Allium spp., cabbages of the genus Brassica, lettuces of the species Lactuca sativa and Cichorium, carrots, turnips, salad beetroot, salsify, celeriac, radishes and similar edible roots, cucumbers and gherkins, leguminous vegetables, asparagus, aubergines, mushrooms, truffles, fruits of the genus Capsicum or of the genus Pimenta, spinach, New Zealand spinach and orache spinach)",
299,2,"070999","070999",463,2015,2050,5923518,"Generic HS2012 to FCL (could also be mapped to 378,430,446) ","Fresh or chilled vegetables n.e.s.",
299,2,"080300","080300",486,2014,2050,5923519,"Generic HS2007 to FCL (could also be mapped to 489,604) ","Bananas, incl. plantains, fresh or dried",
299,2,"080420","080420",570,2014,2050,5923520,"Generic HS2012 to FCL (could also be mapped to 569) ","Fresh or dried figs",
299,2,"080450","080450",571,2014,2050,5923521,"Generic HS2012 to FCL (could also be mapped to 603,604)","Fresh or dried guavas, mangoes and mangosteens",
299,2,"080820","080820",523,2014,2050,5923522,"Generic HS2007 to FCL unique six-digit match","Fresh pears and quinces",
299,2,"080920","080920",531,2014,2050,5923523,"Generic HS2007 to FCL (could also be mapped to 530,541) ","Fresh cherries",
299,2,"081090","081090",619,2014,2050,5923524,"Generic HS2012 to FCL (could also be mapped to 541,591) ","Fresh tamarinds, cashew apples, jackfruit, lychees, sapodillo plums, passion fruit, carambola, pitahaya and other edible fruit (excluding nuts, bananas, dates, figs, pineapples, avocados, guavas, mangoes, mangosteens, papaws papayas, citrus fruit, grapes, melons, apples, pears quinces, apricots, cherries, peaches, plums, sloes, strawberries, raspberries, mulberries, blackberries, loganberries, cranberries, fruits of the genus Vaccinium, kiwifruit, durians, persimmons, black-, white- and redcurrants and gooseberries)",
299,2,"090190","090190",658,2014,2050,5923525,"Generic HS2012 to FCL (could also be mapped to 660) ","Coffee husks and skins; coffee substitutes containing coffee in any proportion",
299,2,"090500","090500",692,2014,2050,5923526,"Generic HS2007 to FCL unique six-digit match","Vanilla",
299,2,"090700","090700",698,2014,2050,5923527,"Generic HS2007 to FCL unique six-digit match","Cloves, whole fruit, cloves and stems",
299,2,"090810","090810",702,2014,2050,5923528,"Generic HS2007 to FCL unique six-digit match","Nutmeg",
299,2,"090830","090830",702,2014,2050,5923529,"Generic HS2007 to FCL unique six-digit match","Cardamoms",
299,2,"090910","090910",711,2014,2050,5923530,"Generic HS2007 to FCL unique six-digit match","Seeds of anise or badian",
299,2,"090920","090920",711,2014,2050,5923531,"Generic HS2007 to FCL unique six-digit match","Coriander seeds",
299,2,"090930","090930",711,2014,2050,5923532,"Generic HS2007 to FCL unique six-digit match","Cumin seeds",
299,2,"090940","090940",711,2014,2050,5923533,"Generic HS2007 to FCL unique six-digit match","Caraway seeds",
299,2,"100110","100110",15,2014,2050,5923534,"Generic HS2007 to FCL unique six-digit match","Durum wheat",
299,2,"100300","100300",44,2014,2050,5923535,"Generic HS2007 to FCL unique six-digit match","Barley",
299,2,"100400","100400",75,2014,2050,5923536,"Generic HS2007 to FCL unique six-digit match","Oats",
299,2,"100630","100630",31,2014,2050,5923537,"Generic HS2012 to FCL (could also be mapped to 29) ","Semi-milled or wholly milled rice, whether or not polished or glazed",
299,2,"100890","100890",108,2014,2050,5923538,"Generic HS2012 to FCL (could also be mapped to 103)","Cereals (excluding wheat and meslin, rye, barley, oats, maize, rice, grain sorghum, buckwheat, millet, canary seeds, fonio, quinoa and triticale)",
299,2,"110419","110419",113,2014,2050,5923539,"Generic HS2012 to FCL (could also be mapped to 45,46)","Rolled or flaked grains of cereals (excluding oats)",
299,2,"110429","110429",113,2014,2050,5923540,"Generic HS2012 to FCL (could also be mapped to 21,45,46)","Grains of cereals, hulled, pearled, sliced, kibbled or otherwise worked (excluding rolled, flaked, flour, pellets, and oats and maize, and husked and semi- or wholly milled rice and broken rice)",
299,2,"120210","120210",242,2014,2050,5923541,"Generic HS2007 to FCL unique six-digit match","Groundnuts in shell, not roasted or otherwise cooked",
299,2,"120220","120220",243,2014,2050,5923542,"Generic HS2007 to FCL unique six-digit match","Shelled groundnuts, whether or not broken (excluding roasted or otherwise cooked)",
299,2,"120799","120799",399,2014,2050,5923543,"Generic HS2012 to FCL (could also be mapped to 263,275,277,305,311,312,336)","Oil seeds and oleaginous fruits, whether or not broken (excluding edible nuts, olives, soya beans, groundnuts, copra, linseed, rape or colza seeds, sunflower seeds, palm nuts and kernels, cotton, castor oil, sesamum, mustard, safflower, melon and poppy seeds)",
299,2,"151219","151219",268,2014,2050,5923544,"Generic HS2012 to FCL (could also be mapped to 281)","Sunflower-seed or safflower oil and their fractions, whether or not refined, but not chemically modified (excluding crude)",
299,2,"151419","151419",271,2014,2050,5923545,"Generic HS2012 to FCL (could also be mapped to 293)","Low erucic acid rape or colza oil fixed oil which has an erucic acid content of < 2% and its fractions, whether or not refined, but not chemically modified (excluding crude)",
299,2,"151590","151590",340,2014,2050,5923546,"Generic HS2012 to FCL (could also be mapped to 36,264,278,297,306,307,313,337) ","Fixed vegetable fats and oils and their fractions, whether or not refined, but not chemically modified (excluding soya-bean, groundnut, olive, palm, sunflower-seed, safflower, cotton-seed, coconut, palm kernel, babassu, rape, colza and mustard, linseed, maize, castor and sesame oil)",
299,2,"151790","151790",1243,2015,2050,5923547,"Generic HS2012 to FCL (could also be mapped to 1241)","Edible mixtures or preparations of animal or vegetable fats or oils and edible fractions of different fats or oils (excluding fats, oils and their fractions, partly or wholly hydrogenated, inter-esterified, re-esterified or elaidinised, whether or not refined, but not further prepared, mixtures of olive oils and their fractions, and solid margarine)",
299,2,"152190","152190",1183,2014,2050,5923548,"Generic HS2012 to FCL (could also be mapped to 1295)","Beeswax, other insect waxes and spermaceti, whether or not refined or coloured",
299,2,"160100","160100",874,2014,2050,5923549,"Generic HS2012 to FCL (could also be mapped to 1041)","Sausages and similar products, of meat, offal or blood; food preparations based on these products",
299,2,"170290","170290",167,2014,2050,5923550,"Generic HS2012 to FCL (could also be mapped to 155,175)","Sugars in solid form, incl. invert sugar and chemically pure maltose, and sugar and sugar syrup blends containing in the dry state 50% by weight of fructose, not flavoured or coloured, artificial honey, whether or not mixed with natural honey and caramel (excluding cane or beet sugar, chemically pure sucrose, lactose, maple sugar, glucose, fructose, and syrups thereof)",
299,2,"190590","190590",22,2014,2050,5923551,"Generic HS2012 to FCL (could also be mapped to 110)","Bread, pastry, cakes, biscuits and other bakers' wares, whether or not containing cocoa; communion wafers, empty cachets of a kind suitable for pharmaceutical use, sealing wafers, rice paper and similar products (excluding crispbread, gingerbread and the like, sweet biscuits, waffles, wafers not mentioned, rusks, toasted bread and similar toasted products)",
299,2,"200320","200320",451,2014,2050,5923552,"Generic HS2007 to FCL unique six-digit match","Truffles, prepared or preserved otherwise than by vinegar or acetic acid",
299,2,"200490","200490",475,2014,2050,5923553,"Generic HS2012 to FCL (could also be mapped to 447,262)","Vegetables and mixtures of vegetables, prepared or preserved otherwise than by vinegar or acetic acid, frozen (excluding preserved by sugar, and tomatoes, mushrooms, truffles and potatoes, unmixed)",
299,2,"200811","200811",246,2014,2050,5923554,"Generic HS2012 to FCL (could also be mapped to 247)","Groundnuts, prepared or preserved (excluding preserved with sugar)",
299,2,"200899","200899",623,2014,2050,5923555,"Generic HS2012 to FCL (could also be mapped to 584)","Fruit and other edible parts of plants, prepared or preserved, whether or not containing added sugar or other sweetening matter or spirit (excluding prepared or preserved with vinegar, preserved with sugar but not laid in syrup, jams, fruit jellies, marmalades, fruit purée and pastes, obtained by cooking, and nuts, groundnuts and other seeds, pineapples, citrus fruits, pears, apricots, cherries, peaches, strawberries, palm hearts and cranberries)",
299,2,"200931","200931",513,2014,2050,5923556,"Generic HS2012 to FCL (could also be mapped to 496,498)","Single citrus fruit juice, unfermented, Brix value <= 20 at 20°C, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, orange juice and grapefruit juice)",
299,2,"200980","200980",622,2014,2050,5923557,"Generic HS2007 to FCL (could also be mapped to 466,538,539,583)","Juice of fruit or vegetables, unfermented, whether or not containing added sugar or other sweetening matter (excluding containing spirit, mixtures, and juice of citrus fruit, pineapples, tomatoes, grapes, incl. grape must and apples)",
299,2,"210390","210390",1232,2014,2050,5923558,"Generic HS2012 to FCL (could also be mapped to 163) ","Preparations for sauces and prepared sauces; mixed condiments and seasonings (excluding soya sauce, tomato ketchup and other tomato sauces, mustard, and mustard flour and meal)",
299,2,"210690","210690",1232,2014,2050,5923559,"Generic HS2012 to FCL (could also be mapped to 674) ","Food preparations, n.e.s.",
299,2,"220300","220300",51,2014,2050,5923560,"Generic HS2012 to FCL (could also be mapped to 66,82,86) ","Beer made from malt",
299,2,"220600","220600",517,2014,2050,5923561,"Generic HS2012 to FCL (could also be mapped to 26,39)","Cider, perry, mead and other fermented beverages and mixtures of fermented beverages and non-alcoholic beverages, n.e.s. (excluding beer, wine or fresh grapes, grape must, vermouth and other wine of fresh grapes flavoured with plants or aromatic substances)",
299,2,"230990","230990",845,2014,2050,5923562,"Generic HS2012 to FCL (could also be mapped to 653,840,841,842,843,849,850,851,852,853,854,855,1259)","Preparations of a kind used in animal feeding (excluding dog or cat food put up for retail sale)",
299,2,"330129","330129",753,2014,2050,5923563,"Generic HS2012 to FCL (could also be mapped to 948)","Essential oils, whether or not terpeneless, incl. concretes and absolutes (excluding those of citrus fruit and mint)",
299,2,"410120","410120",920,2014,2050,5923564,"Generic HS2012 to FCL (could also be mapped to 921,922,928,929,930,958,959,1103,1104,1105)","Whole raw hides and skins of bovine incl. buffalo or equine animals, whether or not dehaired, unsplit, of a weight per skin <= 8 kg when simply dried, <= 10 kg when dry-salted, or <= 16 kg when fresh, wet-salted or otherwise preserved (excluding tanned, parchment-dressed or further prepared)"
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


