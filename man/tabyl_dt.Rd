% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/html_output.R
\name{tabyl_dt}
\alias{tabyl_dt}
\title{tabyl_dt: Call table_dt after running janitor::tabyl() of data}
\usage{
tabyl_dt(data, var1, var2 = NULL, ...)
}
\arguments{
\item{data}{data.frame to be tabyled}

\item{var1}{First var to tabulate over}

\item{var2}{Second, optional, var to tabulate over}

\item{...}{Additional arguments passed to table_dt}
}
\value{
Tabyl output or table_dt output
}
\description{
tabyl_dt: Call table_dt after running janitor::tabyl() of data
}
