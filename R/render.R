# This script builds both the HTML and PDF versions of your CV

# If you wanted to speed up rendering for googlesheets driven CVs you could use
# this script to cache a version of the CV_Printer class with data already
# loaded and load the cached version in the .Rmd instead of re-fetching it twice
# for the HTML and PDF rendering. This exercise is left to the reader.

# Knit the HTML version
rmarkdown::render("./index.Rmd",
                  params = list(pdf_mode = FALSE),
                  output_file = "./docs/index.html")

# Knit the PDF version to temporary html location
rmarkdown::render("./index.Rmd",
                  params = list(pdf_mode = TRUE),
                  output_file = "./docs/pdf/index_pdf.html")

# Convert to PDF using Pagedown
pagedown::chrome_print(input = "./docs/index_pdf.html",
                       output = "./docs/pdf/index.pdf")
