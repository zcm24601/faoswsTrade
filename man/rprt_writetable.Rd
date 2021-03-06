% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rprt_writetable.R
\name{rprt_writetable}
\alias{rprt_writetable}
\title{Writes full table for report collection. Table is saved as csv file and added
to rprt_data list.}
\usage{
rprt_writetable(dataset, prefix = NULL, subdir = NULL)
}
\arguments{
\item{dataset}{Data frame to save}

\item{prefix}{Optional string to be added to the name of output csv-file and
table in rprt_data list. NULL by default.}

\item{subdir}{Optional string specifying name of subdirectory in the report
directory}
}
\value{
Original data set invisibly.
}
\description{
Writes full table for report collection. Table is saved as csv file and added
to rprt_data list.
}
