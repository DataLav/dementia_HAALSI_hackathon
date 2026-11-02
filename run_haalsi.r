

# HAALSI HACKATHON - RUN


# 1. Set your project folder
project_folder <- ""

# 2. Set the R Markdown file path
rmd_file <- file.path(
  project_folder,
  "HAALSI_hackathon_student.Rmd"
)

# 3. Check that the file exists
if (!file.exists(rmd_file)) {
  stop("ERROR: The R Markdown file was not found.")
}

# 4. Set the working directory to your project folder
setwd(project_folder)

# 5. Install rmarkdown if it is missing
if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  install.packages("rmarkdown", repos = "https://cloud.r-project.org")
}

# 6. Print progress information
cat("====================================\n")
cat("Starting HAALSI hackathon analysis\n")
cat("Project folder:", project_folder, "\n")
cat("Input file:", basename(rmd_file), "\n")
cat("====================================\n\n")

# 7. Render the R Markdown file to HTML
rmarkdown::render(
  input = rmd_file,
  output_format = "html_document",
  output_file = "HAALSI_hackathon_student.html",
  output_dir = project_folder,
  knit_root_dir = project_folder,
  envir = new.env(parent = globalenv()),
  clean = TRUE
)

# 8. Confirm the output files
cat("\n====================================\n")
cat("Analysis finished!\n")
cat("Check these files in your project folder:\n")
cat("- HAALSI_hackathon_student.html\n")
cat("- HAALSI_W1-3_Longitudinal_Dataset_long.rds\n")
cat("====================================\n")

# Open the HTML report in your default browser
html_file <- file.path(
  project_folder,
  "HAALSI_hackathon_student.html"
)

if (file.exists(html_file)) {
  browseURL(html_file)
}
