import appleseed.studio as studio
from appleseed.studio.Qt import QtCore, QtGui, QtWidgets
from appleseed.studio.ui import wrapinstance

def find_or_create_menu(menu_name):
    ptr = studio.main_window()
    main_window = wrapinstance(long(ptr), QtWidgets.QMainWindow)
    menu_bar = main_window.menuBar()

    for menu in menu_bar.actions():
        cur_menu_name = menu.text().replace('&', '').lower()

        if cur_menu_name == menu_name.lower():
            return menu.menu()
    else:
        new_menu = QtWidgets.QAction(menu_name, main_window)
        new_menu.setMenu(QtWidgets.QMenu(main_window))

        menu_bar.addAction(new_menu)
        return new_menu.menu()