# Rainy Day Politics

Rainy Day Politics ([www.rainydaypolitics.com](www.rainydaypolitics.com)) is a no-code political analytics platform focused on Seattle and King County. Users can generate maps of precinct-level election results in recent local elections. This repository includes `app.R` and associated files in `data/` and `sql/` as well as infrastructure-as-code files in `terraform/` and `ansible/` used to create the underlying AWS infrastructure. Files containing inputs to Terraform and Ansible have been withheld for security reasons.

## Getting Started

In the left sidebar, select year, election, jurisdiction, position, candidate, and value (percentage or number of votes), then click the Run button. The map will appear on the right. Use controls in the upper left of the map to pan, zoom, or reset the map. Hover over a precinct to see the value for the precinct. Use links at the bottom of the left sidebar to access documentation, source code, and an [email address]('mailto:rainydaypoliticswebsite@gmail.com').

## Data Sources

All data, including [election results](https://kingcounty.gov/en/dept/elections/results) and [precinct geometries](https://kingcounty.gov/en/dept/elections/maps/precinct-and-district-data), were obtained from King County Elections.
