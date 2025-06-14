% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_table_unlabelled.R
\name{mean_calcs_unlabs}
\alias{mean_calcs_unlabs}
\title{calculate table for numeric variables in unlabelled data}
\usage{
mean_calcs_unlabs(dat, col_var, weight_var, flag_list)
}
\arguments{
\item{dat}{data}

\item{col_var}{Character string containing the variables to be used as
columns in the output table.}

\item{weight_var}{Character string containing the variable name of the
weight to be used.}

\item{flag_list}{named list of character vectors for numeric variables
As generated by \link[tveTableTool]{identify_var_type}}
}
\value{
table
}
\description{
calculate table for numeric variables in unlabelled data
}
\examples{
''
}
