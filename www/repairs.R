## repair libraries

unlink("renv/library/windows/R-4.5/x86_64-w64-mingw32/cluster", recursive = TRUE)
unlink("renv/library/windows/R-4.5/x86_64-w64-mingw32/foreign", recursive = TRUE)
unlink("renv/library/windows/R-4.5/x86_64-w64-mingw32/survival", recursive = TRUE)
unlink("renv/library/windows/R-4.5/x86_64-w64-mingw32/nlme", recursive = TRUE)
unlink("renv/library/windows/R-4.5/x86_64-w64-mingw32/MASS", recursive = TRUE)


renv::restore()


