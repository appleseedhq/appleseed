#!/usr/bin/python

import os
import shutil
import sys

if len(sys.argv) != 2:
    print "Wrong number of arguments"
    print "Usage compile_shaders [path to oslc]"
    sys.exit(0)

oslc_cmd = sys.argv[1]
include_dir = os.path.join(os.path.abspath(os.path.dirname(__file__)), "include")

for root, dirname, files in os.walk("."):
    for filename in files:
        if filename.endswith(".osl"):
            print "compiling shader: " + os.path.join(root, filename)

            dest_dir = os.path.join("../", root)

            if not os.path.exists(dest_dir):
                os.makedirs(dest_dir)

            saved_wd = os.getcwd()
            os.chdir(root)
            retcode = os.system(oslc_cmd + " -v -I" + include_dir + ' ' + filename)

            if retcode != 0:
                print "Stopping because of errors..."
                sys.exit(retcode)

            oso_filename = filename.replace(".osl", ".oso")
            dest_dir = os.path.join("..", dest_dir)

            if os.path.exists(os.path.join(dest_dir, oso_filename)):
                os.remove(os.path.join(dest_dir, oso_filename))

            shutil.move(oso_filename, dest_dir)
            os.chdir(saved_wd)

print "All shaders compiled!"
