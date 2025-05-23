% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Get_Red_Listing.R
\name{Get_Red_Listing}
\alias{Get_Red_Listing}
\title{Get the Varieties for Each combination of OCF and RCF Scales}
\usage{
Get_Red_Listing(OCF_df, RCF_df, quantiles = c(0.25, 0.5, 0.75), type = "log")
}
\arguments{
\item{OCF_df}{A data frame generated by the OCF function}

\item{RCF_df}{A data frame generated by the RCF function}

\item{quantiles}{Which quantiles you want to break the RCf and OCF distributions}

\item{type}{Normal scale or Logarithm scale. Default value is log scale.}
}
\value{
A tibble with a column called varieties which have the list of varieties for each combination of scales.
}
\description{
Get the Varieties for Each combination of OCF and RCF Scales
}
\examples{
data(varieties_data)
ocf_data <- OCF(dfr=varieties_data,
vname="variety_name",
hh="household_code",
community="community",
location="location")
rcf_data <- RCF(dfr=varieties_data,
vname="variety_name",
hh="household_code",
nsvarie="number_of_tubers",
community="community",
location="location")
combined_scales <- Get_Red_Listing(ocf_data, rcf_data)
red_listing_varieties <- combined_scales$varieties[[1]]
print(red_listing_varieties)
}
