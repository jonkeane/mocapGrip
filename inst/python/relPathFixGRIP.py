import sys, os, re, shutil, warnings
import pyelan.pyelan as pyelan

# This is a modification of the relPathFix.py in pyelan, that limist searches to specific directories for the GRIP project.

# python relPathFixGRIP.py [files]

# set to True if you need confirmation of files being found.
verbose = False

# grab eaf files from the command line
eafFiles = sys.argv[1:]
for eafFile in eafFiles:
    print("Staring to work on file: "+str(eafFile))
    # backup eaf file
    # is there a better way to backup old files? this will overwrite each other if used sequentially
    shutil.copyfile(eafFile, '.'.join([eafFile, "bak"]))
    eafPath = os.path.dirname(eafFile)

    # if  there is no path (just a filename was given), setup current directory as path.
    if eafPath == "":
        eafPath = "."

    # ignore errors issued by pyelan, the success of locating the files is checked later
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")

        # setup a pyelan tierSet object from the elan file
        fl = pyelan.tierSet(file = eafFile)

        # search for linked files in specific directories one after another. This works because the fixLinks function in pyelan does not try to search for a file if it's already reachable. In other words, each search finds a subset of the links, but the found links aren't overwriten by subsiquent searches.
        fl.fixLinks(searchDir = os.path.sep.join([eafPath,"../..","Clipped Video"]))
        fl.fixLinks(searchDir = os.path.sep.join([eafPath,"../..","AUDIO"]))
        fl.fixLinks(searchDir = os.path.sep.join([eafPath,"../..","mocapCSVs"]))
        fl.fixLinks(searchDir = os.path.sep.join([eafPath,]))

        # Iterate over the tsconf files that are linked, and find files linked there as well.
        for tsconf in filter(lambda s: re.match(".*tsconf.xml", s), fl.linkedFiles):
            print("Staring to work on tsconf file: "+os.path.basename(tsconf))
            # change TSCONFS to be in the EAF files' directory if they aren't here, this will be an issue.
            tsconf = os.path.sep.join([os.path.abspath(eafPath),os.path.basename(tsconf)])
            if os.path.isfile(tsconf):
                # if the file was found
                if verbose: print("The tsconf file "+tsconf+" was found.")
            else:
                # the file is not reachable, it will not be properly linked.
                print("The tsconf file "+tsconf+" can't be found. This probably means it wasn't copied appropriately.")


            # backup tsconf file
            # is there a better way to backup old files? this will overwrite each other if used sequentially
            shutil.copyfile(tsconf, '.'.join([tsconf, "bak"]))

            ts = pyelan.timeSeries(file = tsconf)
            ts.fixLinks(searchDir = os.path.sep.join([eafPath,"../..","mocapCSVs"]))

            # check if the source for the tsconf file is rachable.
            if os.path.isfile(ts.source):
                # if the file was found
                if verbose: print("The file "+ts.source+" was found.")
            else:
                # the file is not reachable, it will not be properly linked.
                print("The file "+ts.source+" can't be found. This file will not be found without manual searching.")


            tsOut = pyelan.timeSeries.timeSeriesOut(ts)
            tsOut[0].write(tsconf)
            print("Finished tsconf file: "+os.path.basename(tsconf))


    # Check if all media files are locatable.
    for mediaFile in fl.media:
        if os.path.isfile(mediaFile):
            # if the file was found
            if verbose: print("The file "+mediaFile+" was found.")
        else:
            # the file is not reachable, it will not be properly linked.
            print("The file "+mediaFile+" can't be found. This file will not be found without manual searching.")

    # Check if all linked files are locatable.
    for file in fl.linkedFiles:
        if os.path.isfile(file):
            # if the file was found
            if verbose: print("The file "+file+" was found.")
        else:
            # the file is not reachable, it will not be properly linked.
            print("The file "+file+" can't be found. This file will not be found without manual searching.")

    eafOut = pyelan.tierSet.elanOut(fl, dest=os.path.sep.join([eafPath,os.path.basename(eafFile)]))
    eafOut.write(eafFile)
    print("Finished file: "+str(eafFile)+"\n")
