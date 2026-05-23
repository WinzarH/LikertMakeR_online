# update renv


## check if all packages agree with the record
renv::status()


renv::activate()  # safe even if already active

# If you want to be explicit:
# remove.packages("LikertMakeR")
# 
# install.packages("LikertMakeR")  # installs the CRAN version


## Snapshot the environment to update renv.lock
renv::snapshot(prompt = FALSE)

renv::snapshot(force = TRUE)



renv::clean()

renv::diagnostics()

renv::refresh()

renv::repair()

## Quick local sanity check (optional but smart)
renv::restore(prompt = FALSE)

