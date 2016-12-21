# Load packages
require(knitr)
require(markdown)

# Create slides
knit("online_retail_summary.Rmd")
system("pandoc -s -t slidy online_retail_summary.md -o online_retail_presentation.html")
