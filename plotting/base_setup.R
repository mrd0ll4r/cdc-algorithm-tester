library(pracma) # for fprintf

dir.create("val", showWarnings = FALSE, recursive = TRUE, mode = "0777")

now <- format(Sys.time(), tz="UTC", usetz=TRUE)
here <- as.character(Sys.info()["nodename"])

# Saves a string to a tex file, annotated with a timestamp and origin.
# value should be a tex-valid string, will be put verbose into the file at name.
# In particular, if math mode is required, value should probably look something like $<value here>$.
save_tex_value <- function(value="\textbf{missing value}", name) {
  output_file_name <- sprintf("val/%s.tex",name)
  
  fprintf("%% Generated %s on %s\n%s%%", now, here, value, file = output_file_name)
}
