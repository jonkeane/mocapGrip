# Mocap-Grip project at the [Center for Gesture, Sign, and Language at the University of Chicago](https://gslcenter.uchicago.edu/)

[![Travis-CI Build Status](https://travis-ci.org/jonkeane/mocapGrip.svg?branch=devel)](https://travis-ci.org/jonkeane/mocapGrip)  [![codecov](https://codecov.io/gh/jonkeane/mocapGrip/branch/devel/graph/badge.svg)](https://codecov.io/gh/jonkeane/mocapGrip)

This [R](https://www.r-project.org/) package contains all of the code used to process and analyze motion capture data from experiments that use a reach to grasp, size estimation, and gesture about objects (and actions taken on them) that are (sometimes) in a visual illusion paradigm.

The main functions are:
* synchronize motion capture and video data,
* setup annotation files to be annotated with [elan](https://tla.mpi.nl/tools/tla-tools/elan/),
* extract and check the annotations for errors,
* run basic analyses on the extracted data

If you do not yet have R installed on your computer I highly recommend using [RStudio](https://www.rstudio.com/products/rstudio/#Desktop). Once you have installed RStudio, you can install mocapGrip using the commands in the next section.

## Install

The easiest way to install the package is to use the [devtools](https://github.com/hadley/devtools) package. There are two different ways to install mocapGrip, you only need to do one of these. The preferred (and easiest) method is the first, but the second allows a bit more flexibility.

For both, you must install devtools, just type `install.packages("devtools")` at an R command prompt. (You can skip this step if you already have devtools installed.)

### 1. Install the most recent stable packaged release

To install mocapGrip copy and paste the following command into R:   
`devtools::install_url("https://github.com/jonkeane/mocapGrip/releases/download/v0.3.0/mocapGrip.tar.gz")`

### 2. Install the most recent source version

To install mocapGrip copy and paste the following command into R:     
`devtools::install_github("jonkeane/mocapGrip",  build_vignettes = TRUE)`

By default, this installation will not include [pyelan](https://github.com/jonkeane/pyelan). To install it, type `library(mocapGrip); installPyelan()` at an R command prompt. (This will have to be done once each time you install a new version of mocapGrip, but shouldn't need to be run more than that.)

> **Dependencies**

> Currently, there are only three dependencies (other than R package dependencies which will be installed by R in the `installation`). For the most part these should already be available, but if you run into errors about them, you might need to install them for mocapGrip to work properly.

> 1. a working modern (>=2.6) version of python. Some functions rely on the python module [pyelan](https://github.com/jonkeane/pyelan) for parsing elan files. This should be downloaded automatically in the `[package install location]/python/` directory as `pyelan`.

> 1. [pandoc](http://pandoc.org/) which is included in [RStudio](https://www.rstudio.com/), but can be [installed separately](http://pandoc.org/installing.html). *Pandoc is only required for making reports*   
> If you are running mocapGrip on the RCC server, you can use the command `module load pandoc` to load pandoc.

> 1. [ffmpeg](https://ffmpeg.org/) is needed to check the duration of the video files when using `makeElanFiles(...)` If it is not present there will be warnings about the duration of the videos and mocap not matching, but mocapGrip will work fine. *ffmpeg is only used when making blank elan files*>   
> If you are running mocapGrip on the RCC server, you can use the command `module load ffmpeg` to load ffmpeg.

## Usage

The package vignettes give step by step walkthroughs of how to use mocapGrip:

* *usingMocapGrip* is a broad overview of the package and includes step by step instructions for using it.  
  To view, after loading the package (with `library(mocapGrip)`), use the command:  
  `vignette("usingMocapGrip")`

* *modalMetadata* is a description of the modelMetadata object which is needed to specify different dataSets, models/analyses, and (some) narrative details for reports.  
   To view, after loading the package (with `library(mocapGrip)`), use the command:  
 `vignette("modelMetadata")`


## Best practices for scientific computing

As many best practices as possible were adopted in the development of this package, including unit testing, continuous integration, version control, semantic versioning, DRY principles, etc. See [Best Practices for Scientific Computing](http://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.1001745) for an introduction and discussion to many of these practices.

### Testing
This project uses unit testing and the package [testthat](https://github.com/hadley/testthat) to run those unit tests (found in the `tests/testthat/` directory). These tests (as well as general R package well-formedness) are also run externally on [Travis CI](https://travis-ci.org).

This project includes a decent amount of code from other projects that were not test-driven. Therefore not all functions are well tested.
