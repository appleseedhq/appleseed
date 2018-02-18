/********************************************************************************
** Form generated from reading UI file 'mainwindow.ui'
**
** Created by: Qt User Interface Compiler version 4.8.7
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MAINWINDOW_H
#define UI_MAINWINDOW_H

#include <QtCore/QLocale>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDockWidget>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QMainWindow>
#include <QtGui/QMenu>
#include <QtGui/QMenuBar>
#include <QtGui/QPushButton>
#include <QtGui/QScrollArea>
#include <QtGui/QStatusBar>
#include <QtGui/QTabWidget>
#include <QtGui/QToolBar>
#include <QtGui/QTreeWidget>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_MainWindow
{
public:
    QAction *action_file_new_project;
    QAction *action_file_open_project;
    QAction *action_file_open_builtin_project_cornellbox;
    QAction *action_file_save_project;
    QAction *action_file_save_project_as;
    QAction *action_file_exit;
    QAction *action_debug_tests;
    QAction *action_debug_benchmarks;
    QAction *action_debug_memory_map;
    QAction *action_debug_profiler;
    QAction *action_help_about;
    QAction *action_diagnostics_override_shading_no_override;
    QAction *action_navigation_inspect;
    QAction *action_navigation_fly;
    QAction *action_navigation_freeze;
    QAction *action_rendering_start_final_rendering;
    QAction *action_rendering_start_interactive_rendering;
    QAction *action_rendering_stop_rendering;
    QAction *action_rendering_rendering_settings;
    QAction *action_file_reload_project;
    QAction *action_tools_reload_settings;
    QAction *action_tools_save_settings;
    QAction *action_clear_open_recent_menu;
    QAction *action_tools_watch_file_changes;
    QAction *action_file_monitor_project;
    QAction *action_file_pack_project_as;
    QAction *action_file_close_project;
    QAction *action_tools_settings;
    QWidget *main_window_contents;
    QGridLayout *gridLayout_3;
    QTabWidget *tab_render_channels;
    QWidget *tab_render_channels_dummy;
    QGridLayout *gridLayout_8;
    QMenuBar *main_menubar;
    QMenu *menu_file;
    QMenu *menu_file_open_builtin_project;
    QMenu *menu_open_recent;
    QMenu *menu_rendering;
    QMenu *menu_diagnostics;
    QMenu *menu_diagnostics_override_shading;
    QMenu *menu_debug;
    QMenu *menu_tools;
    QMenu *menu_help;
    QMenu *menu_view;
    QToolBar *main_toolbar;
    QStatusBar *main_statusbar;
    QDockWidget *project_explorer;
    QWidget *project_explorer_contents;
    QGridLayout *gridLayout_2;
    QTreeWidget *treewidget_project_explorer_scene;
    QHBoxLayout *layout_filter;
    QLabel *label_filter;
    QLineEdit *lineedit_filter;
    QPushButton *pushbutton_clear_filter;
    QDockWidget *log;
    QWidget *log_contents;
    QGridLayout *gridLayout_9;
    QDockWidget *attribute_editor;
    QScrollArea *attribute_editor_scrollarea;
    QWidget *attribute_editor_scrollarea_contents;
    QDockWidget *python_console;
    QWidget *python_console_contents;
    QGridLayout *gridLayout_10;

    void setupUi(QMainWindow *MainWindow)
    {
        if (MainWindow->objectName().isEmpty())
            MainWindow->setObjectName(QString::fromUtf8("MainWindow"));
        MainWindow->resize(1280, 1024);
        MainWindow->setLocale(QLocale(QLocale::English, QLocale::UnitedStates));
        MainWindow->setToolButtonStyle(Qt::ToolButtonIconOnly);
        MainWindow->setAnimated(false);
        action_file_new_project = new QAction(MainWindow);
        action_file_new_project->setObjectName(QString::fromUtf8("action_file_new_project"));
        action_file_open_project = new QAction(MainWindow);
        action_file_open_project->setObjectName(QString::fromUtf8("action_file_open_project"));
        action_file_open_builtin_project_cornellbox = new QAction(MainWindow);
        action_file_open_builtin_project_cornellbox->setObjectName(QString::fromUtf8("action_file_open_builtin_project_cornellbox"));
        action_file_save_project = new QAction(MainWindow);
        action_file_save_project->setObjectName(QString::fromUtf8("action_file_save_project"));
        action_file_save_project_as = new QAction(MainWindow);
        action_file_save_project_as->setObjectName(QString::fromUtf8("action_file_save_project_as"));
        action_file_exit = new QAction(MainWindow);
        action_file_exit->setObjectName(QString::fromUtf8("action_file_exit"));
        action_debug_tests = new QAction(MainWindow);
        action_debug_tests->setObjectName(QString::fromUtf8("action_debug_tests"));
        action_debug_benchmarks = new QAction(MainWindow);
        action_debug_benchmarks->setObjectName(QString::fromUtf8("action_debug_benchmarks"));
        action_debug_memory_map = new QAction(MainWindow);
        action_debug_memory_map->setObjectName(QString::fromUtf8("action_debug_memory_map"));
        action_debug_profiler = new QAction(MainWindow);
        action_debug_profiler->setObjectName(QString::fromUtf8("action_debug_profiler"));
        action_help_about = new QAction(MainWindow);
        action_help_about->setObjectName(QString::fromUtf8("action_help_about"));
        action_diagnostics_override_shading_no_override = new QAction(MainWindow);
        action_diagnostics_override_shading_no_override->setObjectName(QString::fromUtf8("action_diagnostics_override_shading_no_override"));
        action_diagnostics_override_shading_no_override->setCheckable(true);
        action_diagnostics_override_shading_no_override->setChecked(true);
        action_navigation_inspect = new QAction(MainWindow);
        action_navigation_inspect->setObjectName(QString::fromUtf8("action_navigation_inspect"));
        action_navigation_fly = new QAction(MainWindow);
        action_navigation_fly->setObjectName(QString::fromUtf8("action_navigation_fly"));
        action_navigation_freeze = new QAction(MainWindow);
        action_navigation_freeze->setObjectName(QString::fromUtf8("action_navigation_freeze"));
        action_rendering_start_final_rendering = new QAction(MainWindow);
        action_rendering_start_final_rendering->setObjectName(QString::fromUtf8("action_rendering_start_final_rendering"));
        action_rendering_start_interactive_rendering = new QAction(MainWindow);
        action_rendering_start_interactive_rendering->setObjectName(QString::fromUtf8("action_rendering_start_interactive_rendering"));
        action_rendering_stop_rendering = new QAction(MainWindow);
        action_rendering_stop_rendering->setObjectName(QString::fromUtf8("action_rendering_stop_rendering"));
        action_rendering_rendering_settings = new QAction(MainWindow);
        action_rendering_rendering_settings->setObjectName(QString::fromUtf8("action_rendering_rendering_settings"));
        action_file_reload_project = new QAction(MainWindow);
        action_file_reload_project->setObjectName(QString::fromUtf8("action_file_reload_project"));
        action_tools_reload_settings = new QAction(MainWindow);
        action_tools_reload_settings->setObjectName(QString::fromUtf8("action_tools_reload_settings"));
        action_tools_save_settings = new QAction(MainWindow);
        action_tools_save_settings->setObjectName(QString::fromUtf8("action_tools_save_settings"));
        action_clear_open_recent_menu = new QAction(MainWindow);
        action_clear_open_recent_menu->setObjectName(QString::fromUtf8("action_clear_open_recent_menu"));
        action_tools_watch_file_changes = new QAction(MainWindow);
        action_tools_watch_file_changes->setObjectName(QString::fromUtf8("action_tools_watch_file_changes"));
        action_tools_watch_file_changes->setCheckable(true);
        action_file_monitor_project = new QAction(MainWindow);
        action_file_monitor_project->setObjectName(QString::fromUtf8("action_file_monitor_project"));
        action_file_monitor_project->setCheckable(true);
        action_file_pack_project_as = new QAction(MainWindow);
        action_file_pack_project_as->setObjectName(QString::fromUtf8("action_file_pack_project_as"));
        action_file_close_project = new QAction(MainWindow);
        action_file_close_project->setObjectName(QString::fromUtf8("action_file_close_project"));
        action_tools_settings = new QAction(MainWindow);
        action_tools_settings->setObjectName(QString::fromUtf8("action_tools_settings"));
        main_window_contents = new QWidget(MainWindow);
        main_window_contents->setObjectName(QString::fromUtf8("main_window_contents"));
        gridLayout_3 = new QGridLayout(main_window_contents);
        gridLayout_3->setSpacing(6);
        gridLayout_3->setContentsMargins(0, 0, 0, 0);
        gridLayout_3->setObjectName(QString::fromUtf8("gridLayout_3"));
        tab_render_channels = new QTabWidget(main_window_contents);
        tab_render_channels->setObjectName(QString::fromUtf8("tab_render_channels"));
        tab_render_channels_dummy = new QWidget();
        tab_render_channels_dummy->setObjectName(QString::fromUtf8("tab_render_channels_dummy"));
        gridLayout_8 = new QGridLayout(tab_render_channels_dummy);
        gridLayout_8->setSpacing(6);
        gridLayout_8->setContentsMargins(11, 11, 11, 11);
        gridLayout_8->setObjectName(QString::fromUtf8("gridLayout_8"));
        tab_render_channels->addTab(tab_render_channels_dummy, QString());

        gridLayout_3->addWidget(tab_render_channels, 0, 0, 1, 1);

        MainWindow->setCentralWidget(main_window_contents);
        main_menubar = new QMenuBar(MainWindow);
        main_menubar->setObjectName(QString::fromUtf8("main_menubar"));
        main_menubar->setGeometry(QRect(0, 0, 1280, 26));
        menu_file = new QMenu(main_menubar);
        menu_file->setObjectName(QString::fromUtf8("menu_file"));
        menu_file_open_builtin_project = new QMenu(menu_file);
        menu_file_open_builtin_project->setObjectName(QString::fromUtf8("menu_file_open_builtin_project"));
        menu_open_recent = new QMenu(menu_file);
        menu_open_recent->setObjectName(QString::fromUtf8("menu_open_recent"));
        menu_rendering = new QMenu(main_menubar);
        menu_rendering->setObjectName(QString::fromUtf8("menu_rendering"));
        menu_diagnostics = new QMenu(main_menubar);
        menu_diagnostics->setObjectName(QString::fromUtf8("menu_diagnostics"));
        menu_diagnostics_override_shading = new QMenu(menu_diagnostics);
        menu_diagnostics_override_shading->setObjectName(QString::fromUtf8("menu_diagnostics_override_shading"));
        menu_diagnostics_override_shading->setTearOffEnabled(true);
        menu_debug = new QMenu(main_menubar);
        menu_debug->setObjectName(QString::fromUtf8("menu_debug"));
        menu_tools = new QMenu(main_menubar);
        menu_tools->setObjectName(QString::fromUtf8("menu_tools"));
        menu_help = new QMenu(main_menubar);
        menu_help->setObjectName(QString::fromUtf8("menu_help"));
        menu_view = new QMenu(main_menubar);
        menu_view->setObjectName(QString::fromUtf8("menu_view"));
        MainWindow->setMenuBar(main_menubar);
        main_toolbar = new QToolBar(MainWindow);
        main_toolbar->setObjectName(QString::fromUtf8("main_toolbar"));
        main_toolbar->setIconSize(QSize(18, 18));
        MainWindow->addToolBar(Qt::TopToolBarArea, main_toolbar);
        main_statusbar = new QStatusBar(MainWindow);
        main_statusbar->setObjectName(QString::fromUtf8("main_statusbar"));
        MainWindow->setStatusBar(main_statusbar);
        project_explorer = new QDockWidget(MainWindow);
        project_explorer->setObjectName(QString::fromUtf8("project_explorer"));
        project_explorer->setMinimumSize(QSize(228, 187));
        project_explorer->setAutoFillBackground(false);
        project_explorer->setAllowedAreas(Qt::LeftDockWidgetArea|Qt::RightDockWidgetArea);
        project_explorer_contents = new QWidget();
        project_explorer_contents->setObjectName(QString::fromUtf8("project_explorer_contents"));
        gridLayout_2 = new QGridLayout(project_explorer_contents);
        gridLayout_2->setSpacing(6);
        gridLayout_2->setContentsMargins(11, 11, 11, 11);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        treewidget_project_explorer_scene = new QTreeWidget(project_explorer_contents);
        treewidget_project_explorer_scene->setObjectName(QString::fromUtf8("treewidget_project_explorer_scene"));
        treewidget_project_explorer_scene->setAlternatingRowColors(true);
        treewidget_project_explorer_scene->setSelectionMode(QAbstractItemView::ExtendedSelection);
        treewidget_project_explorer_scene->setUniformRowHeights(true);
        treewidget_project_explorer_scene->setAllColumnsShowFocus(true);
        treewidget_project_explorer_scene->setColumnCount(1);
        treewidget_project_explorer_scene->header()->setVisible(true);

        gridLayout_2->addWidget(treewidget_project_explorer_scene, 2, 0, 1, 1);

        layout_filter = new QHBoxLayout();
        layout_filter->setSpacing(6);
        layout_filter->setObjectName(QString::fromUtf8("layout_filter"));
        label_filter = new QLabel(project_explorer_contents);
        label_filter->setObjectName(QString::fromUtf8("label_filter"));

        layout_filter->addWidget(label_filter);

        lineedit_filter = new QLineEdit(project_explorer_contents);
        lineedit_filter->setObjectName(QString::fromUtf8("lineedit_filter"));

        layout_filter->addWidget(lineedit_filter);

        pushbutton_clear_filter = new QPushButton(project_explorer_contents);
        pushbutton_clear_filter->setObjectName(QString::fromUtf8("pushbutton_clear_filter"));

        layout_filter->addWidget(pushbutton_clear_filter);


        gridLayout_2->addLayout(layout_filter, 0, 0, 2, 1);

        project_explorer->setWidget(project_explorer_contents);
        MainWindow->addDockWidget(static_cast<Qt::DockWidgetArea>(1), project_explorer);
        log = new QDockWidget(MainWindow);
        log->setObjectName(QString::fromUtf8("log"));
        log_contents = new QWidget();
        log_contents->setObjectName(QString::fromUtf8("log_contents"));
        gridLayout_9 = new QGridLayout(log_contents);
        gridLayout_9->setSpacing(6);
        gridLayout_9->setContentsMargins(11, 11, 11, 11);
        gridLayout_9->setObjectName(QString::fromUtf8("gridLayout_9"));
        log->setWidget(log_contents);
        MainWindow->addDockWidget(static_cast<Qt::DockWidgetArea>(8), log);
        attribute_editor = new QDockWidget(MainWindow);
        attribute_editor->setObjectName(QString::fromUtf8("attribute_editor"));
        attribute_editor->setMinimumSize(QSize(200, 113));
        attribute_editor->setFloating(false);
        attribute_editor_scrollarea = new QScrollArea();
        attribute_editor_scrollarea->setObjectName(QString::fromUtf8("attribute_editor_scrollarea"));
        attribute_editor_scrollarea->setWidgetResizable(true);
        attribute_editor_scrollarea_contents = new QWidget();
        attribute_editor_scrollarea_contents->setObjectName(QString::fromUtf8("attribute_editor_scrollarea_contents"));
        attribute_editor_scrollarea_contents->setGeometry(QRect(0, 0, 198, 878));
        attribute_editor_scrollarea->setWidget(attribute_editor_scrollarea_contents);
        attribute_editor->setWidget(attribute_editor_scrollarea);
        MainWindow->addDockWidget(static_cast<Qt::DockWidgetArea>(2), attribute_editor);
        python_console = new QDockWidget(MainWindow);
        python_console->setObjectName(QString::fromUtf8("python_console"));
        python_console_contents = new QWidget();
        python_console_contents->setObjectName(QString::fromUtf8("python_console_contents"));
        gridLayout_10 = new QGridLayout(python_console_contents);
        gridLayout_10->setSpacing(6);
        gridLayout_10->setContentsMargins(11, 11, 11, 11);
        gridLayout_10->setObjectName(QString::fromUtf8("gridLayout_10"));
        python_console->setWidget(python_console_contents);
        MainWindow->addDockWidget(static_cast<Qt::DockWidgetArea>(8), python_console);

        main_menubar->addAction(menu_file->menuAction());
        main_menubar->addAction(menu_view->menuAction());
        main_menubar->addAction(menu_rendering->menuAction());
        main_menubar->addAction(menu_diagnostics->menuAction());
        main_menubar->addAction(menu_debug->menuAction());
        main_menubar->addAction(menu_tools->menuAction());
        main_menubar->addAction(menu_help->menuAction());
        menu_file->addAction(action_file_new_project);
        menu_file->addAction(action_file_open_project);
        menu_file->addAction(menu_open_recent->menuAction());
        menu_file->addAction(menu_file_open_builtin_project->menuAction());
        menu_file->addAction(action_file_reload_project);
        menu_file->addSeparator();
        menu_file->addAction(action_file_monitor_project);
        menu_file->addSeparator();
        menu_file->addAction(action_file_save_project);
        menu_file->addAction(action_file_save_project_as);
        menu_file->addAction(action_file_pack_project_as);
        menu_file->addSeparator();
        menu_file->addAction(action_file_close_project);
        menu_file->addSeparator();
        menu_file->addAction(action_file_exit);
        menu_file_open_builtin_project->addAction(action_file_open_builtin_project_cornellbox);
        menu_rendering->addAction(action_rendering_start_interactive_rendering);
        menu_rendering->addAction(action_rendering_start_final_rendering);
        menu_rendering->addAction(action_rendering_stop_rendering);
        menu_rendering->addSeparator();
        menu_rendering->addAction(action_rendering_rendering_settings);
        menu_diagnostics->addAction(menu_diagnostics_override_shading->menuAction());
        menu_diagnostics_override_shading->addAction(action_diagnostics_override_shading_no_override);
        menu_diagnostics_override_shading->addSeparator();
        menu_debug->addAction(action_debug_tests);
        menu_debug->addAction(action_debug_benchmarks);
        menu_debug->addSeparator();
        menu_debug->addAction(action_debug_profiler);
        menu_debug->addAction(action_debug_memory_map);
        menu_tools->addAction(action_tools_settings);
        menu_tools->addAction(action_tools_save_settings);
        menu_tools->addAction(action_tools_reload_settings);
        menu_help->addAction(action_help_about);

        retranslateUi(MainWindow);

        tab_render_channels->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(MainWindow);
    } // setupUi

    void retranslateUi(QMainWindow *MainWindow)
    {
        MainWindow->setWindowTitle(QApplication::translate("MainWindow", "appleseed.studio", 0, QApplication::UnicodeUTF8));
        action_file_new_project->setText(QApplication::translate("MainWindow", "&New Project", 0, QApplication::UnicodeUTF8));
        action_file_open_project->setText(QApplication::translate("MainWindow", "&Open Project...", 0, QApplication::UnicodeUTF8));
        action_file_open_builtin_project_cornellbox->setText(QApplication::translate("MainWindow", "&Cornell Box", 0, QApplication::UnicodeUTF8));
        action_file_save_project->setText(QApplication::translate("MainWindow", "&Save Project", 0, QApplication::UnicodeUTF8));
        action_file_save_project_as->setText(QApplication::translate("MainWindow", "Save Project &As...", 0, QApplication::UnicodeUTF8));
        action_file_exit->setText(QApplication::translate("MainWindow", "&Exit", 0, QApplication::UnicodeUTF8));
        action_debug_tests->setText(QApplication::translate("MainWindow", "&Tests...", 0, QApplication::UnicodeUTF8));
        action_debug_tests->setShortcut(QApplication::translate("MainWindow", "Ctrl+Shift+T", 0, QApplication::UnicodeUTF8));
        action_debug_benchmarks->setText(QApplication::translate("MainWindow", "&Benchmarks...", 0, QApplication::UnicodeUTF8));
        action_debug_benchmarks->setShortcut(QApplication::translate("MainWindow", "Ctrl+Shift+B", 0, QApplication::UnicodeUTF8));
        action_debug_memory_map->setText(QApplication::translate("MainWindow", "&Memory Map...", 0, QApplication::UnicodeUTF8));
        action_debug_memory_map->setShortcut(QApplication::translate("MainWindow", "Ctrl+Shift+M", 0, QApplication::UnicodeUTF8));
        action_debug_profiler->setText(QApplication::translate("MainWindow", "Profiler...", 0, QApplication::UnicodeUTF8));
        action_debug_profiler->setShortcut(QApplication::translate("MainWindow", "Ctrl+Shift+P", 0, QApplication::UnicodeUTF8));
        action_help_about->setText(QApplication::translate("MainWindow", "&About...", 0, QApplication::UnicodeUTF8));
        action_diagnostics_override_shading_no_override->setText(QApplication::translate("MainWindow", "&No Override", 0, QApplication::UnicodeUTF8));
        action_diagnostics_override_shading_no_override->setShortcut(QApplication::translate("MainWindow", "Ctrl+Shift+0", 0, QApplication::UnicodeUTF8));
        action_navigation_inspect->setText(QApplication::translate("MainWindow", "&Inspect", 0, QApplication::UnicodeUTF8));
        action_navigation_fly->setText(QApplication::translate("MainWindow", "&Fly", 0, QApplication::UnicodeUTF8));
        action_navigation_freeze->setText(QApplication::translate("MainWindow", "Free&ze", 0, QApplication::UnicodeUTF8));
        action_rendering_start_final_rendering->setText(QApplication::translate("MainWindow", "Start &Final Rendering", 0, QApplication::UnicodeUTF8));
        action_rendering_start_final_rendering->setShortcut(QApplication::translate("MainWindow", "F6", 0, QApplication::UnicodeUTF8));
        action_rendering_start_interactive_rendering->setText(QApplication::translate("MainWindow", "Start &Interactive Rendering", 0, QApplication::UnicodeUTF8));
        action_rendering_start_interactive_rendering->setShortcut(QApplication::translate("MainWindow", "F5", 0, QApplication::UnicodeUTF8));
        action_rendering_stop_rendering->setText(QApplication::translate("MainWindow", "S&top Rendering", 0, QApplication::UnicodeUTF8));
        action_rendering_stop_rendering->setShortcut(QApplication::translate("MainWindow", "Shift+F5", 0, QApplication::UnicodeUTF8));
        action_rendering_rendering_settings->setText(QApplication::translate("MainWindow", "&Rendering Settings...", 0, QApplication::UnicodeUTF8));
        action_rendering_rendering_settings->setShortcut(QApplication::translate("MainWindow", "F7", 0, QApplication::UnicodeUTF8));
        action_file_reload_project->setText(QApplication::translate("MainWindow", "&Reload Project", 0, QApplication::UnicodeUTF8));
        action_file_reload_project->setShortcut(QApplication::translate("MainWindow", "Ctrl+R", 0, QApplication::UnicodeUTF8));
        action_tools_reload_settings->setText(QApplication::translate("MainWindow", "&Reload Settings", 0, QApplication::UnicodeUTF8));
        action_tools_reload_settings->setShortcut(QApplication::translate("MainWindow", "Ctrl+Shift+R", 0, QApplication::UnicodeUTF8));
        action_tools_save_settings->setText(QApplication::translate("MainWindow", "S&ave Settings", 0, QApplication::UnicodeUTF8));
        action_tools_save_settings->setShortcut(QApplication::translate("MainWindow", "Ctrl+Shift+S", 0, QApplication::UnicodeUTF8));
        action_clear_open_recent_menu->setText(QApplication::translate("MainWindow", "Clear Menu", 0, QApplication::UnicodeUTF8));
        action_tools_watch_file_changes->setText(QApplication::translate("MainWindow", "Watch File Changes", 0, QApplication::UnicodeUTF8));
        action_file_monitor_project->setText(QApplication::translate("MainWindow", "&Monitor Project File", 0, QApplication::UnicodeUTF8));
        action_file_pack_project_as->setText(QApplication::translate("MainWindow", "&Pack Project As...", 0, QApplication::UnicodeUTF8));
        action_file_close_project->setText(QApplication::translate("MainWindow", "&Close Project", 0, QApplication::UnicodeUTF8));
        action_tools_settings->setText(QApplication::translate("MainWindow", "&Settings...", 0, QApplication::UnicodeUTF8));
        tab_render_channels->setTabText(tab_render_channels->indexOf(tab_render_channels_dummy), QApplication::translate("MainWindow", "This Text Should Not Be Visible", 0, QApplication::UnicodeUTF8));
        menu_file->setTitle(QApplication::translate("MainWindow", "&File", 0, QApplication::UnicodeUTF8));
        menu_file_open_builtin_project->setTitle(QApplication::translate("MainWindow", "Open &Built-in Project", 0, QApplication::UnicodeUTF8));
        menu_open_recent->setTitle(QApplication::translate("MainWindow", "Open Recen&t", 0, QApplication::UnicodeUTF8));
        menu_rendering->setTitle(QApplication::translate("MainWindow", "&Rendering", 0, QApplication::UnicodeUTF8));
        menu_diagnostics->setTitle(QApplication::translate("MainWindow", "&Diagnostics", 0, QApplication::UnicodeUTF8));
        menu_diagnostics_override_shading->setTitle(QApplication::translate("MainWindow", "&Override Shading", 0, QApplication::UnicodeUTF8));
        menu_debug->setTitle(QApplication::translate("MainWindow", "Debu&g", 0, QApplication::UnicodeUTF8));
        menu_tools->setTitle(QApplication::translate("MainWindow", "&Tools", 0, QApplication::UnicodeUTF8));
        menu_help->setTitle(QApplication::translate("MainWindow", "&Help", 0, QApplication::UnicodeUTF8));
        menu_view->setTitle(QApplication::translate("MainWindow", "&View", 0, QApplication::UnicodeUTF8));
        main_toolbar->setWindowTitle(QApplication::translate("MainWindow", "Toolbar", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_ACCESSIBILITY
        project_explorer->setAccessibleName(QString());
#endif // QT_NO_ACCESSIBILITY
        project_explorer->setWindowTitle(QApplication::translate("MainWindow", "Project Explorer", 0, QApplication::UnicodeUTF8));
        QTreeWidgetItem *___qtreewidgetitem = treewidget_project_explorer_scene->headerItem();
        ___qtreewidgetitem->setText(0, QApplication::translate("MainWindow", "Name", 0, QApplication::UnicodeUTF8));
        label_filter->setText(QApplication::translate("MainWindow", "Filter:", 0, QApplication::UnicodeUTF8));
        pushbutton_clear_filter->setText(QApplication::translate("MainWindow", "Clear", 0, QApplication::UnicodeUTF8));
        log->setWindowTitle(QApplication::translate("MainWindow", "Log", 0, QApplication::UnicodeUTF8));
        attribute_editor->setWindowTitle(QApplication::translate("MainWindow", "Attribute Editor", 0, QApplication::UnicodeUTF8));
        python_console->setWindowTitle(QApplication::translate("MainWindow", "Python Console", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class MainWindow: public Ui_MainWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAINWINDOW_H
