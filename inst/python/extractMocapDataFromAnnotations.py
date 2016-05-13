import sys, os, re, warnings, csv, itertools, warnings
import pyelan.pyelan as pyelan

# to check and extract annotations:
# extratAnnotations.py [destDir] [eaffiles]

# changes the behavior of warnings so that they alert and do not print the code being warned about.
def custom_formatwarning(msg, *a):
    # ignore everything except the message
    return str(msg) + '\n'
warnings.formatwarning = custom_formatwarning

# def actionCheck(trialType, condition):
#     if trialType[0] != "ACTION":
#         warnings.warn("The first trial type is not ACTION in "+str(condition)+". In the file "+eafFile)
#     actionPeriods = ['EYESCLOSED', 'OBSERVE', 'GRIP', 'MOVEMENT', 'RELEASE']
#     if [period for period in trialType[2]] != actionPeriods:
#         warnings.warn("The periods for action do not contain "+str(actionPeriods)+" "+str(condition)+". In the file "+eafFile)

def gestureCheck(trialType, condition, typ, eafFile):
    # generate a regualr expression for checking. First check if there is a no gesture annotation, if there isn't move from there
    pattern = re.compile('(EYESCLOSED) *(NO GESTURE|.+)')
    match = pattern.match(' '.join(trialType[2]))

    # Check if eyes closed exists, if not warn for that
    if  not match or match.group(1) != "EYESCLOSED":
        warnings.warn("There is no EYESCLOSED period for condition "+str(typ)+" in file "+eafFile+" Condition: "+str(condition[0])+" Periods found: "+str(trialType[2]))
    else:
        if match.group(2) != "NO GESTURE":
            # if this is a gesture event
            pattern = re.compile('(PLANNING) *(GRIP)? *(MOVEMENT) *(OPEN|CLOSED|OPEN-CLOSED)? *(RELEASE)?')
            subAnnos = match.group(2)
            match = pattern.match(subAnnos)
            if not match:
                warnings.warn("There is not at least one PLANNING and one MOVEMENT periods for condition "+str(typ)+" in file "+eafFile+" Condition: "+str(condition[0])+" Periods found: "+str(trialType[2]))
            else:
                if not match.group(4):
                    warnings.warn("There is no OPEN, CLOSED, or OPEN-CLOSED annotation for condition "+str(typ)+" in file "+eafFile+" Condition: "+str(condition[0])+" Periods found: "+str(trialType[2]))



def estimationCheck(trialType, condition):
    # Checking the order does not currently work, because order has been shuffled.
    possEstPeriods = [['EYESCLOSED', 'OBSERVE', 'PREPARE', 'STEADY', 'TRANSITION', 'GRIP', 'MOVEMENT', 'RELEASE']]
    if trialType[2] not in possEstPeriods:
        warnings.warn("The periods for estimation are not right. \n Expected:"+str(possEstPeriods)+"\n Found:   "+str(trialType[2])+"\n In the condition "+str(condition)+" the file "+eafFile)

def actionCheck(trialType,condition):
    actionPeriods = ['EYESCLOSED', 'OBSERVE', 'GRIP', 'MOVEMENT', 'RELEASE']
    if trialType[2] !=  actionPeriods:
        warnings.warn("The periods for action are not right. \n Expected:"+str(actionPeriods)+"\n Found:   "+str(trialType[2])+"\n In the condition "+str(condition)+" the file "+eafFile)

def annoChecker(annos, eafFile, trialTypesPerTrial = 3):
    annoVals = [x[0] for x in annos]

    # annotations must match this pattern, there is a strict pattern and a less strict. The less strict should be fine given that each annotaiton is additionally tested later. This gives better warning messages.
    # pattern = re.compile('(\d+) +(ACTION|GESTURE|ESTIMATION) +(EYESCLOSED|OBSERVE|GRIP|MOVEMENT|RELEASE|PLANNING|PREPARE|STEADY|TRANSITION|UNCODABLE|NO GESTURE) *(CLOSED|OPEN|OPEN-CLOSED)?')
    pattern = re.compile('(\d+) +(\w+) +(\w+) *(\w+)?')

    # setup a list of lists that has the structure of the experiment
    # annoStruct = [[condition, [type, side, [periods]]]]
    annoStruct = [[None, [None, None, [None]]]]

    # parse the annotations into the list of lists
    for val in annoVals:
        match = pattern.match(val)
        try:
            condition = match.group(1)
            typ = match.group(2)
            period = match.group(3)
            gripType = match.group(4) # does gripType need to be checked? probably to make sure it coocurs with movement only

            # merge period and gripType if griptype is not None
            if gripType != None:
                period = ' '.join([period,gripType])

        except AttributeError:
            warnings.warn("Could not parse the annotation values for the annotation: "+val+" In the file "+eafFile+" This likely means there is a typo or other error in annotations.")



        if annoStruct[-1][0] == condition:
            if annoStruct[-1][-1][0] == typ:
                annoStruct[-1][-1][2].append(period)
            else:
                annoStruct[-1].append([typ, "sidex", [period]])
        else:
            annoStruct.append([condition, [typ, "sidex", [period]]])

    # remove the first element
    if annoStruct[0] == [None, [None, None, [None]]]:
        annoStruct.pop(0)

    # check if there are any duplicate conditions
    conditionNums = [trial[0] for trial in annoStruct]
    if len(conditionNums) > len(set(conditionNums)):
        warnings.warn("There are duplicate conditions numbers in the trials in the file "+eafFile+" This could be a typo. If there are actual multiple trials with the same condition numbers, all of the annotations for one of those trials must be deleted. The conditions that were found were: "+str(conditionNums))

    for condition in annoStruct:
        if  not re.match("[1234567890]", condition[0]):
            warnings.warn("The condition "+condition[0]+" in "+str(condition)+" does not match the possible condition list. In the file "+eafFile)

        # Check that the trial types are right and in the right order:
        trialTypes = ' '.join([tt[0] for tt in condition[1:]])

        if trialTypesPerTrial > 1:
            matches = re.match("(ACTION)? ?(GESTURE)? ?(ESTIMATION)?", trialTypes)
            if not matches.group(1):
                warnings.warn("There is no ACTION trial type for condition"+str(typ)+" in file "+eafFile+" Condition: "+str(condition[0])+" Trial types found: "+str(trialTypes))

            if not matches.group(2):
                warnings.warn("There is no GESTURE trial type for condition"+str(typ)+" in file "+eafFile+" Condition: "+str(condition[0])+" Trial types found: "+str(trialTypes))

            if not matches.group(3):
                warnings.warn("There is no ESTIMATION trial type for condition"+str(typ)+" in file "+eafFile+" Condition: "+str(condition[0])+" Trial types found: "+str(trialTypes))

        for trialType in condition[1:]:
            if trialType[0] == "ACTION":
                actionCheck(trialType = trialType, condition = condition)
            elif trialType[0] == "GESTURE":
                gestureCheck(trialType = trialType, condition = condition, typ = typ, eafFile = eafFile)
            elif trialType[0] == "ESTIMATION":
                estimationCheck(trialType = trialType, condition = condition)


# setup a warning catcher so that each file can be stopped form processing if there are warnings that pop up, but that they are displayed. from http://stackoverflow.com/questions/2324820/count-warnings-in-python-2-4
def setup_warning_catcher():
    """ Wrap warnings.showwarning with code that records warnings. """


    caught_warnings = []
    original_showwarning = warnings.showwarning

    def custom_showwarning(*args,  **kwargs):
        caught_warnings.append(args[0])
        return original_showwarning(*args, **kwargs)

    warnings.showwarning = custom_showwarning
    return caught_warnings


destDir = sys.argv[1]
# check that destdir exists:
if os.path.isdir(destDir) == False:
  raise Exception("The destination directory ("+str(destDir)+") does not exist. Please create it and try again.")

eafFiles = sys.argv[2:]
# eafFiles = ['../../elanFiles/GRI_016/GRI_016-SESSION_001-TRIAL_006.eaf']
for eafFile in eafFiles:
    eafPath = os.path.dirname(eafFile)
    basename = os.path.splitext(os.path.basename(eafFile))[0]

    print("Starting on file "+eafFile)

    # fix links, but supress warnings about those links for cleaner output
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")

        fl = pyelan.tierSet(file = eafFile)
        fl.fixLinks(searchDir = os.path.sep.join([destDir,"..","Clipped Video"]))
        fl.fixLinks(searchDir = os.path.sep.join([destDir,"..","AUDIO"]))
        fl.fixLinks(searchDir = os.path.sep.join([destDir,"..","elanFilesCompleted"]))

    # find time series files that are linked to the eaf.
    tsconfs = filter(lambda s: re.match(".*tsconf.xml", s), fl.linkedFiles)
    if len(tsconfs) > 1:
        warnings.warn("There's more than one tsconf file")

    # setup a list to store warnings about tsconf files in. This will allow us to throw warnings about serious errors with tsconf files, but not for simply not finding files (which are being ignored)
    tsconfWarnings = []

    for tsconf in tsconfs:
        # this should only be one file for this data
        # fix links, but supress warnings about those links for cleaner output
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")

            try:
                ts = pyelan.timeSeries(file = tsconf)
            except:
                # save a warning string to add to warnings after we are no longer ignoring them (see above)
                tsconfWarnings.append("The tsconf file is not properly configured. This is likely due to tracks not being selected. Please check the "+tsconf+" file.")
            else:
                ts.fixLinks(searchDir = os.path.sep.join([destDir,"..","mocapCSVs"]))

    # catch warnings to fail elegantly.
    caught_warnings_list = setup_warning_catcher()

    if len(tsconfWarnings) > 0:
        for tsconfWarning in tsconfWarnings:
            warnings.warn(tsconfWarning)

    # extract the annotations from the overlaps tear
    annos = []
    tier = [tier for tier in fl.tiers if tier.tierName == "OVERLAPS"]
    if len(tier) < 1:
        warnings.warn("No overlaps tier found.")
    elif len(tier) > 1:
        warnings.warn("More than one overlaps tier found.")
    tier = tier[0]

    for annotation in tier.annotations:
        annos.append((annotation.value, annotation.begin, annotation.end))

    # check if the annotations are of the correct form
    annoChecker(annos, eafFile, trialTypesPerTrial = 1)

    if len(annos) < 1:
        warnings.warn("No annotations on overlaps tier found.")

    # check warnings, if there are multiple, fail.
    if len(caught_warnings_list) > 0:
        if len(caught_warnings_list) == 1:
            print("No annotations were extracted: There was one warning with file "+eafFile+"\n")
        else:
            print("No annotations were extracted: There were "+str(len(caught_warnings_list))+" warnings with file "+eafFile+"\n")

    else:
        # if there were no warnings, write the extracted annotations
        print("Writing extracted annotations for "+eafFile+"\n")

        # relativize paths from the current to the csv
        currPath = os.path.abspath("./")
        relPathCSV = os.path.relpath(ts.source, start = currPath)
        csvfile = open(relPathCSV, 'r')
        reader = csv.DictReader(csvfile)
        csvData = []
        if ts.timeOrigin:
            offset = ts.timeOrigin/1000.
        else:
            offset = 0
        for row in reader:
            csvData.append((float(row['times'])-offset, (row['0-1'], row['mean-Y-0-1-2-3-4'])))




        colNames = [anno[0] for anno in annos]
        grip = []
        for name, minn, maxx, in annos:
            grip.append([x[1][0] for x in csvData if x[0] >= minn/1000. and x[0] <= maxx/1000.])

        # turn the list into rows, but add padding for mismatched lengths.
        gripRows = list(itertools.izip_longest(*grip))

        # create a subject directory if needed
        subj = basename.split("-")[0]
        if os.path.isdir(os.path.sep.join([destDir, subj])) == False :
            os.makedirs(os.path.sep.join([destDir, subj]))

        # write files
        csvfile = open(os.path.sep.join([destDir, subj,'.'.join([basename,"csv"])]), 'w')
        writer = csv.writer(csvfile)
        writer.writerow(colNames)
        for row in gripRows:
            writer.writerow(row)
