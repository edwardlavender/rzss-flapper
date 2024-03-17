Physiological responses to capture, handling and tagging in flapper
skate (*Dipturus intermedius*)
================
Georgina C. Cole<sup>1</sup>, Edward Lavender<sup>2,3,4\*</sup>, Adam
Naylor<sup>1,5</sup>, Simon J. Girling<sup>1</sup>, Dmitry
Aleynik<sup>6</sup>, Steffen J. Oppel<sup>7</sup>, Jane
Dodd<sup>8</sup>, James Thorburn<sup>3,9,10</sup>

[![Project Status: Inactive – The project has reached a stable, usable
state but is no longer being actively developed; support/maintenance
will be provided as time
allows.](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)

<sup>1</sup> Royal Zoological Society of Scotland, UK  
<sup>2</sup> Centre for Research into Ecological and Environmental
Modelling, University of St Andrews, UK  
<sup>3</sup> Scottish Oceans Institute, University of St Andrews, UK  
<sup>4</sup> Department of Systems Analysis, Integrated Assessment and
Modelling, Eawag, Swiss Federal Institute of Aquatic Science and
Technology, Switzerland  
<sup>5</sup> New Zealand Centre for Conservation Medicine, Auckland Zoo,
New Zealand  
<sup>6</sup> Scottish Association for Marine Science, UK  
<sup>7</sup> Swiss Ornithological Institute, Switzerland  
<sup>8</sup> Nature Scot, UK  
<sup>9</sup> School of Applied Sciences, Edinburgh Napier University,
UK  
<sup>10</sup> Centre for Conservation and Restoration Science, Edinburgh
Napier University, UK

<sup>\*</sup> This repository is maintained by Edward Lavender
(<edward.lavender@eawag.ch>).

# Introduction

This repository contains methods, written in
[R](https://www.r-project.org/) and organised as an
[RStudio](https://www.rstudio.com/)
[Project](https://r4ds.had.co.nz/workflow-projects.html) for Cole et
al. (in prep). Physiological responses to capture, handling and tagging
in flapper skate (*Dipturus intermedius*).

We studied the physiological responses of flapper skate following
catch-and-release angling and acoustic tagging in the Loch Sunart to the
Sound of Jura Marine Protected Area on the west coast of Scotland
(Figure 1). We recorded capture fight times and blood parameters
following capture (blood sample one: BS1) and following handling (blood
sample two: BS2). Using statistical modelling (as recorded in this
repository) we investigated capture fights and relationships between
blood parameters (for both BS1 and BS2) and aspects of the capture and
handing process (such as fight time), individual characteristics (such
as sex) and environmental conditions (such as temperature). This work
was conducted as part of the Movement Ecology of Flapper Skate project.

<img src="README.jpg"/>

*Figure 1. A flapper skate is released following capture and handing in
the Loch Sunart to the Sound of Jura Marine Protected Area on the west
coast of Scotland. Photograph courtesy of the Movement Ecology of
Flapper Skate project.*

# Description

## Dependencies

The project was built in [R](https://www.r-project.org/) (version 4.3.1)
in [RStudio](https://www.rstudio.com/) and implements local dependency
management using
[`renv`](https://rstudio.github.io/renv/articles/renv.html). This
manages the installation of the
[`dv`](https://github.com/edwardlavender/dv) package for project
management (from [GitHub](https://github.com/)), as well as other
packages from the [Comprehensive R Archive
Network](https://cran.r-project.org/). The first time the project is
opened, [`renv`](https://rstudio.github.io/renv/articles/renv.html) can
be used to regenerate the local project library, as described in
`renv.lock` (via `.Rprofile` and `renv/activate.R`).

## Directories

The project follows a standardised structure encouraged by the
[`dv`](https://github.com/edwardlavender/dv) package. The high-level
structure was generated via `dv::use_template_proj()`. The contents as
follows:

1.  **`renv/`** implements local dependency management.

2.  **`data-raw/`** contains ‘raw’ data:

    - `skate/` contains skate data:
      - `Skate acoustic tagging data .xlsx` contains raw data from
        capture and tagging, including the details of capture events,
        blood parameters and heart and respiratory rates during
        handling, with one individual per sheet. This file is used for
        the fight time, heart and respiratory rate analyses in this
        project.
      - `Skate data analysis shared.xlsx` summarises capture events and
        blood parameters in one place. (This document was collated
        by G. C. Cole.) The `Data all without formulas` sheet includes
        capture events, raw blood parameters and temperature-corrected
        blood parameter values. This sheet is used for the blood
        parameter analyses in this project.
    - `spatial/` contains spatial data:
      - `coast/` contains coastline data for the region, sourced from
        the [Database of Global Administrative
        Areas](https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_GBR_0_sp.rds);
      - `mesh/` contains mesh files for the West Scotland Coastal Ocean
        Modelling System (WeStCOMS), sourced from [Dmitry
        Aleynik](https://www.sams.ac.uk/people/researchers/aleynik-dr-dmitry/)
        at the Scottish Association for Marine Science (SAMS). These
        files are also available from the [SAMS Thredds
        Server](https://www.sams.ac.uk/facilities/thredds/);

3.  **`data/`** contains processed data and results:

    - `skate/` contains processed skate data:
      - `physio.rds` contains processed blood physiology data from
        `process_bloods.R`;
      - `vital.rds` contains processed vital signs data from
        `process_vitals.R`;
      - `fights.rds` contains processed capture fights data from
        `process_fights.R`;
    - `spatial/` contains processed spatial data:
      - `bathy/` contains one arc-second bathymetry data from
        [Digimap](https://digimap.edina.ac.uk) (higher resolution data
        from the [Ireland, Northern Ireland and Scotland Hydrographic
        surveys](https://doi.org/10.1017/S1755691015000146) are not
        available for all sampled locations);
      - `coast/` contains processed coastline data from
        `process_spatial.R`;
      - `mesh/` contains processed WeStCOMS mesh files from
        `process_spatial.R`;
    - `estimates/` contains model outputs from `analyse_*.R` scripts;
    - `inst/` contains [RStudio](https://www.rstudio.com/)
      [Project](https://r4ds.had.co.nz/workflow-projects.html)-management
      files generated by [`dv`](https://github.com/edwardlavender/dv):
      - `tree.rds` is a record of the project directory tree (as
        generated by `dv::use_template_tree()` in `dev/01-dev.R`, see
        below); <br/>

4.  **`R/`** contains `R` scripts for data processing and analysis:

    - `define_global_param.R` defines global parameters;
    - `define_helpers.R` defines helper functions;
    - `process_spatial.R` processes raw spatial data;
    - `process_bloods.R` processes raw blood parameter data;
    - `process_vitals.R` processes raw vital signs data;
    - `process_fights.R` processes raw capture fights data;
    - `analyse_fights.R` analyses capture fights;
    - `analyse_bloods.R` analyses BS1 and BS2;
    - `analyse_bloods_change.R` analyses the change in blood parameters
      during handling;
    - `analyse_bloods_synthesis.R` synthesises results across blood
      parameters;
    - `analyse_vitals.R` analyses heart and respiratory rates during
      handling; <br/>

5.  **`dev/`** contains project-management scripts:

    - `01-dev.R` and `02-clone.R` are standard
      [`dv`](https://github.com/edwardlavender/dv) scripts:
      - `01-dev.R` records project set up and development;
      - `02-clone.R` is used to clone the project (see ‘Instructions’);
        <br/>

6.  **`fig/`** contains figures.

7.  **`doc/`** contains supporting documents.

Note that the `data-raw/`, `data/` (except `data/inst/`), `fig/` and
`doc` directories are not currently provided in the online version of
this repository.

# Instructions

Follow the steps described below to clone the project and reproduce the
workflow.

1.  **Clone the project** via GitHub. Follow the instructions in
    `dev/02-clone.R` to install project packages and create directories:

    - **Packages.** Open `dev/02-clone.R` to use
      [`renv`](https://rstudio.github.io/renv/articles/renv.html) to
      regenerate the local project library. Packages can also be
      manually reinstalled via `02-clone.R`. (However, note that package
      versions may differ in this case.)
    - **Directories.** Rebuild the project directory tree, via
      `dv::use_template_proj()` and `dv::use_template_tree()`.

2.  **Obtain raw data** from the authors and/or links provided above

3.  **Define global parameters** via `define_global_param.R` and
    `define_helpers.R`.

4.  **Process raw data** via `process_spatial.R`, `process_bloods.R`,
    `process_vitals.R` and `process_fights.R`.

5.  **Implement data analysis** via `analyse_fights.R`,
    `analyse_bloods.R`, `analyse_bloods_change.R`,
    `analyse_bloods_synthesis.R` and `analyse_vitals.R`.

# Citation

Cole et al. (in prep). Physiological responses to capture, handling and
tagging in flapper skate (*Dipturus intermedius*).

------------------------------------------------------------------------
