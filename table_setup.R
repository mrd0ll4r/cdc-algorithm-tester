library(xtable)

dir.create("tab", showWarnings = FALSE, recursive = TRUE, mode = "0777")

options(xtable.floating=FALSE)
options(xtable.booktabs=TRUE)
options(xtable.auto=TRUE)
options(xtable.include.rownames = FALSE)
options(xtable.caption.placement = "top")
# This does not work because it thinks the option is unset.
#options(xtable.table.placement = NULL)
