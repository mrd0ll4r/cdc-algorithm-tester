library(pracma) # for fprintf

dir.create("val", showWarnings = FALSE, recursive = TRUE, mode = "0777")

now <- format(Sys.time(), tz="UTC", usetz=TRUE)
here <- as.character(Sys.info()["nodename"])

# Saves a string to a tex file, annotated with a timestamp and origin.
# value should be a tex-valid string, will be put verbose into the file at name.
# In particular, if math mode is required, value should probably look something like $<value here>$.
# If provided, an invisible comment is added.
save_tex_value <- function(name, value="\textbf{missing value}", comment=NA) {
  now = format(Sys.time(), tz="UTC", usetz=TRUE)
  here = as.character(Sys.info()["nodename"])
  output_file_name = sprintf("val/%s.tex",name)

  if (is.na(comment)) {
    fprintf("%% Generated %s on %s\n%s%%", now, here, value, file = output_file_name)
  } else {
    fprintf("%% Generated %s on %s\n%% Comment: %s\n%s%%", now, here, format(comment), value, file = output_file_name)
  }
}

# Prints a number in tex math-mode with separator marks for thousands etc.
tex_format_number <- function(n, scientific=FALSE) {
  if (scientific) {
    # TODO
  } else {
    sprintf("\\num{%s}",
            format(
              n,
              trim=TRUE,
              scientific=FALSE)
    )
  }
}

# Rounds and prints percentages in tex math-mode.
# If rounding would result in zero, the value is replaced with the smallest
# nonzero value respecting the rounding, and prefixed with a less-than sign.
tex_format_percentage <- function(n, digits=2, percentage_sign=TRUE) {
  tibble(n=n) %>%
    mutate(n=round(n,digits=digits)) %>%
    mutate(n_s=if_else(n==0,{
      d=10^(-digits)
      f=sprintf("< %%.%df",digits)
      s=sprintf(f,d)
    },{
      f=sprintf("%%.%df",digits)
      s=sprintf(f,n)
    })
    ) %>%
    mutate(s=if_else(rep(percentage_sign,length(n)),
                     sprintf("$%s \\, \\%%$",n_s),
                     sprintf("$%s$",n_s)
    )) %>% pull(s)
}
