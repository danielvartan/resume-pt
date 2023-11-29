# This script builds both the HTML and PDF versions of your CV

# Knit the HTML version
rmarkdown::render(
  "./index.Rmd",
  params = list(pdf_mode = FALSE),
  output_file = "./docs/index.html"
)

# Knit the PDF version to temporary html location
rmarkdown::render(
  "./index.Rmd",
  params = list(pdf_mode = TRUE),
  output_file = "./docs/pdf/index_pdf.html"
)
