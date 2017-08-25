
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2017 Gleb Mishchenko, The appleseedhq Organization
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

import appleseed.studio as studio
import Qt


def wrapinstance(addr, type):
    if Qt.__binding__ == 'PyQt4':
        import sip
        return sip.wrapinstance(addr, type)

    elif Qt.__binding__ == 'PySide':
        import shiboken
        return shiboken.wrapInstance(addr, type)

    else:
        raise Exception("No wrapinstance function defined for " + Qt.__binding__)


def find_or_create_menu(menu_name):
    ptr = studio.main_window()
    main_window = wrapinstance(long(ptr), Qt.QtWidgets.QMainWindow)
    menu_bar = main_window.menuBar()

    for menu in menu_bar.actions():
        cur_menu_name = menu.text().replace('&', '').lower()

        if cur_menu_name == menu_name.lower():
            return menu.menu()
    else:
        new_menu = Qt.QtWidgets.QAction(menu_name, main_window)
        new_menu.setMenu(Qt.QtWidgets.QMenu(main_window))

        menu_bar.addAction(new_menu)
        return new_menu.menu()
