% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vehicle_registrations.R
\name{all_registrations}
\alias{all_registrations}
\title{Total Vehicle Registrations by Electrification Level}
\usage{
all_registrations()
}
\value{
tibble of total vehicle registrations by electrification level by month and county
}
\description{
This function pulls and cleans data from the Washington State Registration Data.
Data is pulled from https://data.wa.gov/ via the Socrata API
}
\examples{
\dontrun{
total_vehicle_registrations <- all_registrations()}

}
