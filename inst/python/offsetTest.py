import sys, os, re, shutil
import pyelan.pyelan as pyelan


tsconf = '../../test/GRI_019-SESSION_001-TRIAL_008_tsconf.xml'
ts = pyelan.timeSeries(file = tsconf)
