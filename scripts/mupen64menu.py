#!/usr/bin/env python
#
# 23 Feb 2010
# Author: Torstein S. Skulbru - serrghi
# http://www.unyttig.info
#
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see .
#
# mupen64menu.py
# This program will generate a Openbox Pipemenu to list all n64 roms in a directory
# only requirement is mupen64plus.
#
# USAGE:
# Place the script in ~/.config/openbox/scripts/
# Make the script executable by: chmod +x mupen64menu.py
# Change the rom_dir to suit you
# Add the following in menu.xml (.config/openbox/menu.xml)
# somewhere inside  but before root-menu:
# 
# and then in the root-menu, add the menu where you want it to show up by adding the following:
# 
 
import os, random
 
rom_dir = '/home/serrghi/Documents/n64'
command = "mupen64plus"
 
def main():
    files = os.listdir(rom_dir)
    files.sort()
 
    print ""
 
    for f in files:
        fullpath = os.path.join(rom_dir, f)
        (path, fullname) = os.path.split(fullpath)
        (name, ext) = os.path.splitext(fullname)
 
        action = command + " \"%s\"" % fullpath
        print "  " % name
        print "    %s" % action
        print "  "
    print ""
 
if __name__ == "__main__":
    main()
