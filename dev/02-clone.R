###########################
###########################
#### 03-clone.R

#### Aims
# 1) Clone a project.
# This script is designed to be used to set up an established project, shared
# via GitHub, on another machine. This includes:
# ... A) Creation of directory structure
# ... B) Installation of dependencies

#### Prerequisites
# 1) NA


###########################
###########################
#### Restore project dependencies

#### Option (1): Automatic restoration via renv
# Restore the project's dependencies (including dv) from the lockfile
renv::restore()

#### Option (2): Manual re-installation of packages

## B) Install CRAN dependencies (if necessary)
# This code requires modification if there are packages stored elsewhere
custom  <- ""
depends <- sort(unique(renv::dependencies()$Package))
cran    <- depends[!(depends %in% custom)]
renv::install(cran)


###########################
###########################
#### Rebuild project structure

#### Recreate high-level project structure
dv::use_template_proj(overwrite = FALSE)

#### Rebuild directory tree
tree <- here::here("data", "dv", "tree.rds")
if (file.exists(tree)) {
  tree <- readRDS(tree)
  dv::use_template_tree(tree = tree, recreate = TRUE)
}

#### Re-run project workflow
# Re-obtain raw dataset(s)
# Re-run data processing
# Re-run analysis


#### End of code.
###########################
###########################
