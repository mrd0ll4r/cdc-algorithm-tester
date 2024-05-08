library(tikzDevice)
library(viridis)

tmp <- getOption("tikzSanitizeCharacters")
tmp2 <- getOption("tikzReplacementCharacters")
options(tikzSanitizeCharacters = append(tmp,c("\U00D7","\\char-215")))
options(tikzReplacementCharacters = append(tmp2,c("\\times","\\times")))
rm(tmp)
rm(tmp2)

options(tikzDefaultEngine = "pdftex")
options(tikzDocumentDeclaration = "\\documentclass[10pt]{article}")

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", gsub("\\+","",scales::scientific_format()(x))))
}
