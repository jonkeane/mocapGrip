# Mocap-Grip project at the Center for Gesture, Sign, and Language at the University of Chicago

[![Travis-CI Build Status](https://travis-ci.org/jonkeane/mocapGrip.svg?branch=master)](https://travis-ci.org/jonkeane/mocapGrip)

This package contains all of the code used to process and analyze motion capture data from experiments that use a reach to grasp, size estimation, and gesture about objects (and actions taken on them) that are (sometimes) in a visual illusion paradigm.

The main functions are:
* synchronize motion capture and video data,
* setup annotation files to be annotated with [elan](https://tla.mpi.nl/tools/tla-tools/elan/), 
* extract and check the annotations for errors,
* run basic analyses on the extracted data (forthcoming) 

## Install

The easiest way to install the package is to use the [devtools](https://github.com/hadley/devtools) package. 

0. To install devtools, just type `install.packages("devtools")` at an R command prompt. 

1. Clone the mocapGrip repository. To do this, type `git clone --recursive https://github.com/jonkeane/mocapGrip.git` at the command prompt.

2. Launch R from the directory containing the git repository (or set your working directory to the directory that contains the git repository), and then type `library(devtools); install_local("./mocapGrip")` at the R command prompt.

## Dependencies (non-R)

Currently, the only dependency (other than R package dependencies which will be installed by R in the installation) is a working modern (>=2.6) version of python as well as [ffmpeg](https://ffmpeg.org/). Some functions rely on the python module [pyelan](https://github.com/jonkeane/pyelan) for parsing elan files. This should be downloaded automatically in the `[package install location]/python/` directory as `pyelan`.
 
## Usage
TBD

## Testing
This project uses unit testing and the package [testthat](https://github.com/hadley/testthat) to run those unit tests (found in the `tests/testthat/` directory). These tests (as well as general R package well-formedness) are also run externally on [Travis CI](https://travis-ci.org).

This project includes a decent amount of code from other projects that were not test-driven. Therefor not all functions are well tested. 
