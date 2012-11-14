#!/usr/bin/python

#
# Copyright (c) 2012 Jonathan Topf
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#

#
# You will need PySide to run this script.
# Download it here: http://qt-project.org/wiki/PySideDownloads
#

import sys
from PySide import QtGui, QtCore
import os
import subprocess
import re

class RenderSequence_GUI(QtGui.QWidget):
    def __init__(self):
        super(RenderSequence_GUI, self).__init__()
        self.initUI()

    def initUI(self):
        main_layout = QtGui.QVBoxLayout()
        self.setLayout(main_layout)

        # appleseed cli layout
        binary_layout = QtGui.QHBoxLayout()
        binary_layout.addWidget(QtGui.QLabel("Directory of appleseed.cli:"))
        self.bin_dir = QtGui.QLineEdit('/projects/appleseed/sandbox/bin/Release')
        binary_layout.addWidget(self.bin_dir)
        main_layout.addLayout(binary_layout)

        # src layout
        src_layout = QtGui.QHBoxLayout()
        src_button = QtGui.QPushButton('Select File', self)
        self.src_text = QtGui.QLineEdit()
        self.sequence_check = QtGui.QCheckBox('Render Entire Sequence')
        src_layout.addWidget(src_button)
        src_layout.addWidget(self.src_text)
        src_layout.addWidget(self.sequence_check)
        main_layout.addLayout(src_layout)

        # dest layout
        dest_layout = QtGui.QHBoxLayout()
        dest_button = QtGui.QPushButton('Select Output Folder', self)
        self.dest_text = QtGui.QLineEdit()
        render_button = QtGui.QPushButton('Render', self)
        dest_layout.addWidget(dest_button)
        dest_layout.addWidget(self.dest_text)
        dest_layout.addWidget(render_button)
        main_layout.addLayout(dest_layout)

        # connections
        src_button.clicked.connect(self.getSrcDir)
        dest_button.clicked.connect(self.getDestDir)
        render_button.clicked.connect(self.export)

        self.setGeometry(300, 300, 600, 30)
        self.setWindowTitle('appleseed Render Sequence')
        self.show()

    def getDestDir(self):
        self.dest_text.setText(QtGui.QFileDialog.getExistingDirectory(caption='Choose Output Directory'))

    def getSrcDir(self):
        self.src_text.setText(QtGui.QFileDialog.getOpenFileName()[0]) 

    def export(self):
        cli_path = os.path.join(self.bin_dir.text(), 'appleseed.cli')
        src_file_path = self.src_text.text()
        dest_dir_path = self.dest_text.text()
        file_list = [src_file_path]

        if self.sequence_check.checkState():
            file_list = self.findSequence(src_file_path)

        for file in file_list:
            output_file_name = os.path.splitext(os.path.split(file)[1])[0] + '.png'
            output_file_path = os.path.join(dest_dir_path, output_file_name)
 
            print "Rendering {0} to {1}...".format(file, output_file_name)

            subprocess.call([cli_path, '-o', output_file_path, file, '-r 1 1'])

        print "Done."

    def findSequence(self, src_file):
        if os.path.exists(src_file):
            # get directory name and file name
            dir_name, file_name = os.path.split(src_file)

            # define regular expression that checks for sequences of numbers
            numeric_seq_regex = re.compile('\d+')

            # variables to store the position of the first digit of the number, last digit position, and the string that contains the number
            start_pos = 0
            end_pos = 0
            seq_start_number = 0

            # get the variables from the src file string 
            for num_seq in numeric_seq_regex.finditer(file_name):
                start_pos = num_seq.start()
                end_pos = num_seq.end()
                seq_start_number = int(num_seq.group())

            iter_file_name = file_name # holds the current file name in the loop looking for sequence files
            current_itter_pos = seq_start_number # holds current number that will be searched for in file name
            iter_files = [] # list of found file names

            while os.path.exists(os.path.join(dir_name, iter_file_name)):
                # add the file name that just passes the existence test
                iter_files.append(os.path.join(dir_name, iter_file_name))
                current_itter_pos += 1

                # split file name into character list
                iter_chars = list(iter_file_name)

                # create a new list of chars that contains the numeric portion of the file name
                iter_num_chars = list(str(current_itter_pos).zfill(end_pos - start_pos))

                # substitute the file number portion of the file name
                i = start_pos
                while i < end_pos:
                    iter_chars[i] = iter_num_chars[i - start_pos]
                    i += 1

                # join the char list and set the iter file name ready for existence checking
                iter_file_name = ''.join(iter_chars)

            # list the files that have been found
            print "Found the following files:"
            for file in iter_files:
                print file

        return iter_files

def main():
    app = QtGui.QApplication(sys.argv)
    ex = RenderSequence_GUI()
    sys.exit(app.exec_())

if __name__ == '__main__':
    main()
