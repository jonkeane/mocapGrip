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

First, to install devtools, just type `install.packages("devtools")` at an R command prompt. (You can skip this step if you already have devtools installed.)

To install mocapGrip type `library(devtools); install_github("jonkeane/mocapGrip")` at an R command prompt.

By default, this installation will not include [pyelan](https://github.com/jonkeane/pyelan). To install it, type `library(mocapGrip); installPyelan()` at an R command prompt. (This will have to be done once each time you install a new version of `mocapGrip`, but shouldn't need to be run more than that.)

## Dependencies (non-R)

Currently, the only dependency (other than R package dependencies which will be installed by R in the installation) is a working modern (>=2.6) version of python as well as [ffmpeg](https://ffmpeg.org/). Some functions rely on the python module [pyelan](https://github.com/jonkeane/pyelan) for parsing elan files. This should be downloaded automatically in the `[package install location]/python/` directory as `pyelan`.

## Usage
There are a number of functions (all of which are documented in the man folder for use with R's help system), but a basic workflow would look something like this:

### `makeElanFiles(...)`

 `makeElanFiles(...)`makes blank elan files and links the videos, audio, and motion capture data that is necessary to then annotate the videos. This command relies on the video, audio, and motion capture files being put into the folder structure that has already been established:

* `AUDIO` for audio files, one folder per subject (e.g. `./AUDIO/057GRI_057-SESSION_001-TRIAL_002.wav`)
* `Clipped Videos` for video files, one folder per subject (e.g. `./Clipped Video/057GRI_057-SESSION_001-TRIAL_002.mov`)
* `mocapData` for the motion capture data files, one subfolder called `GRIP`, and then one folder per subject (e.g. `./mocapData/GRIP/GRI_057/[phase space / obsis folder structure]`)

The script will then save files to the following two folders:

* `mocapCSVs` for clipped motion capture csvs, one folder per subject (e.g. `./mocapCSVs/GRI_057/GRI_057-SESSION_001-TRIAL_002.csv`)
* `elanFilesOut` for elan files the script will then make one folder per subject  (e.g. `./elanFilesOut/GRI_057/GRI_057-SESSION_001-TRIAL_002.eaf` and `./elanFilesOut/GRI_057/GRI_057-SESSION_001-TRIAL_002_tsconf.xml`), and put the created elan files as well as the time series configuration files (`\*.eaf` and `\*_tsconf.xml`)

An example of this command is: `makeElanFiles(files=c("./Clipped Video/059/GRI_059-SESSION_001-TRIAL_001.mov", "./Clipped Video/059/GRI_059-SESSION_001-TRIAL_002.mov"))` You can also use wildcards with this command: `makeElanFiles(files="./Clipped Video/0??/GRI_0??-SESSION_0??-TRIAL_0??.mov")` which will match any files that have any character in each of the positions with a `?`.

The empty elan files are then annotated according to annotation guidelines.

### `extractMocapDataFromAnnotations(...)`

`extractMocapDataFromAnnotations(...)` extracts completed annotations from the files specified, and checks to make sure that the format is correct. Currently it will give warnings if the checks it runs don't work, and will suggest possible fixes. Any file that has a warning will not have any annotations extract. You can supply it with a destination directory, which must already exist. An example of this command is: `extractMocapDataFromAnnotations(files=c("./elanFilesCompleted/GRI_059/GRI_059-SESSION_001-TRIAL_001.eaf", "./elanFilesCompleted/GRI_059/GRI_059-SESSION_001-TRIAL_002.eaf"), destDir="./extractedData/")` You can also use wildcards with this command: `extractMocapDataFromAnnotations(files="./elanFilesCompleted/GRI_0??/GRI_0??-SESSION_0??-TRIAL_0??.eaf", destDir="./extractedData/")` which will match any files that have any character in each of the positions with a `?`.

### `readExtractedMocapData(...)`
`readExtractedMocapData(...)` reads in the extracted motion capture data (that are written by the command `extractMocapDataFromAnnotations(...)` above). This function can extract as many or as few types of data for analysis as we want. Possible types include:

* `action` Extracts the maximum grip from the *grip* period of *action* trials
* `estimation` Extracts the mean and median grip from the *steady* period of *estimation* trials
* `release` Extracts the maximum grip from the *release* period of *action* trials
* `estMaxGrip` Extracts the maximum grip from the *grip* period of *estimation* trials

An example of this command is `readExtractedMocapData(path="./extractData", types = c("action", "estimation"))` which would extract `action` and `estimation` from all of the data that is in the folder `./extractedData`


## Testing
This project uses unit testing and the package [testthat](https://github.com/hadley/testthat) to run those unit tests (found in the `tests/testthat/` directory). These tests (as well as general R package well-formedness) are also run externally on [Travis CI](https://travis-ci.org).

This project includes a decent amount of code from other projects that were not test-driven. Therefore not all functions are well tested.
