# library(beepr)
# library(here)
# library(rmarkdown)

# This script builds both the HTML and PDF versions of your CV
# Print the CV as PDF an overwrite the existing PDF

##  Knit the HTML Version ---

rmarkdown::render(
  here::here("index.Rmd"),
  params = list(pdf_mode = FALSE),
  output_file = here::here("docs", "index.html")
)

beepr::beep(1)

## Knit the PDF Version to Temporary HTML Location ---

rmarkdown::render(
  here::here("index.Rmd"),
  params = list(pdf_mode = TRUE),
  output_file = here::here("docs", "pdf", "index_pdf.html")
)

beepr::beep(1)
