/****************************************************************************
** Meta object code from reading C++ file 'mainwindow.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../src/appleseed.studio/mainwindow/mainwindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'mainwindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__MainWindow[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
      42,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      38,   31,   30,   30, 0x05,

 // slots: signature, parameters, type, tag, flags
      94,   30,   30,   30, 0x08,
     113,   30,   30,   30, 0x08,
     133,   30,   30,   30, 0x08,
     152,   30,   30,   30, 0x08,
     188,   30,   30,   30, 0x08,
     227,   30,   30,   30, 0x08,
     269,  249,   30,   30, 0x08,
     310,   30,   30,   30, 0x08,
     330,   30,   30,   30, 0x08,
     353,   30,   30,   30, 0x08,
     376,   30,   30,   30, 0x08,
     397,   30,   30,   30, 0x08,
     429,  421,   30,   30, 0x08,
     480,  471,   30,   30, 0x08,
     515,   30,   30,   30, 0x08,
     536,   30,   30,   30, 0x08,
     557,   30,   30,   30, 0x08,
     579,   30,   30,   30, 0x08,
     614,   30,   30,   30, 0x08,
     677,  643,   30,   30, 0x08,
     725,   30,   30,   30, 0x08,
     746,   30,   30,   30, 0x08,
     768,   30,   30,   30, 0x08,
     798,   30,   30,   30, 0x08,
     826,   30,   30,   30, 0x08,
     858,  853,   30,   30, 0x08,
     894,  888,   30,   30, 0x08,
     934,   30,   30,   30, 0x08,
     952,   30,   30,   30, 0x08,
     973,   30,   30,   30, 0x08,
     999,   30,   30,   30, 0x08,
    1018,   30,   30,   30, 0x08,
    1044, 1036,   30,   30, 0x08,
    1078,   30,   30,   30, 0x08,
    1098,   30,   30,   30, 0x08,
    1120,   30,   30,   30, 0x08,
    1138,   30,   30,   30, 0x08,
    1166,   30,   30,   30, 0x08,
    1204,   30,   30,   30, 0x08,
    1228,   30,   30,   30, 0x08,
    1257,   30,   30,   30, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__MainWindow[] = {
    "appleseed::studio::MainWindow\0\0values\0"
    "signal_refresh_attribute_editor(foundation::Dictionary)\0"
    "slot_new_project()\0slot_open_project()\0"
    "slot_open_recent()\0"
    "slot_clear_open_recent_files_menu()\0"
    "slot_open_cornellbox_builtin_project()\0"
    "slot_reload_project()\0filepath,successful\0"
    "slot_open_project_complete(QString,bool)\0"
    "slot_save_project()\0slot_save_project_as()\0"
    "slot_pack_project_as()\0slot_close_project()\0"
    "slot_project_modified()\0checked\0"
    "slot_toggle_project_file_monitoring(bool)\0"
    "filepath\0slot_project_file_changed(QString)\0"
    "slot_load_settings()\0slot_save_settings()\0"
    "slot_apply_settings()\0"
    "slot_start_interactive_rendering()\0"
    "slot_start_final_rendering()\0"
    "filepath,configuration,successful\0"
    "slot_start_rendering_once(QString,QString,bool)\0"
    "slot_rendering_end()\0slot_camera_changed()\0"
    "slot_clear_shading_override()\0"
    "slot_set_shading_override()\0"
    "slot_clear_render_region()\0rect\0"
    "slot_set_render_region(QRect)\0point\0"
    "slot_render_widget_context_menu(QPoint)\0"
    "slot_save_frame()\0slot_save_all_aovs()\0"
    "slot_quicksave_all_aovs()\0slot_clear_frame()\0"
    "slot_reset_zoom()\0pattern\0"
    "slot_filter_text_changed(QString)\0"
    "slot_clear_filter()\0slot_frame_modified()\0"
    "slot_fullscreen()\0slot_show_settings_window()\0"
    "slot_show_rendering_settings_window()\0"
    "slot_show_test_window()\0"
    "slot_show_benchmark_window()\0"
    "slot_show_about_window()\0"
};

void appleseed::studio::MainWindow::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        MainWindow *_t = static_cast<MainWindow *>(_o);
        switch (_id) {
        case 0: _t->signal_refresh_attribute_editor((*reinterpret_cast< const foundation::Dictionary(*)>(_a[1]))); break;
        case 1: _t->slot_new_project(); break;
        case 2: _t->slot_open_project(); break;
        case 3: _t->slot_open_recent(); break;
        case 4: _t->slot_clear_open_recent_files_menu(); break;
        case 5: _t->slot_open_cornellbox_builtin_project(); break;
        case 6: _t->slot_reload_project(); break;
        case 7: _t->slot_open_project_complete((*reinterpret_cast< const QString(*)>(_a[1])),(*reinterpret_cast< const bool(*)>(_a[2]))); break;
        case 8: _t->slot_save_project(); break;
        case 9: _t->slot_save_project_as(); break;
        case 10: _t->slot_pack_project_as(); break;
        case 11: _t->slot_close_project(); break;
        case 12: _t->slot_project_modified(); break;
        case 13: _t->slot_toggle_project_file_monitoring((*reinterpret_cast< const bool(*)>(_a[1]))); break;
        case 14: _t->slot_project_file_changed((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 15: _t->slot_load_settings(); break;
        case 16: _t->slot_save_settings(); break;
        case 17: _t->slot_apply_settings(); break;
        case 18: _t->slot_start_interactive_rendering(); break;
        case 19: _t->slot_start_final_rendering(); break;
        case 20: _t->slot_start_rendering_once((*reinterpret_cast< const QString(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2])),(*reinterpret_cast< const bool(*)>(_a[3]))); break;
        case 21: _t->slot_rendering_end(); break;
        case 22: _t->slot_camera_changed(); break;
        case 23: _t->slot_clear_shading_override(); break;
        case 24: _t->slot_set_shading_override(); break;
        case 25: _t->slot_clear_render_region(); break;
        case 26: _t->slot_set_render_region((*reinterpret_cast< const QRect(*)>(_a[1]))); break;
        case 27: _t->slot_render_widget_context_menu((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 28: _t->slot_save_frame(); break;
        case 29: _t->slot_save_all_aovs(); break;
        case 30: _t->slot_quicksave_all_aovs(); break;
        case 31: _t->slot_clear_frame(); break;
        case 32: _t->slot_reset_zoom(); break;
        case 33: _t->slot_filter_text_changed((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 34: _t->slot_clear_filter(); break;
        case 35: _t->slot_frame_modified(); break;
        case 36: _t->slot_fullscreen(); break;
        case 37: _t->slot_show_settings_window(); break;
        case 38: _t->slot_show_rendering_settings_window(); break;
        case 39: _t->slot_show_test_window(); break;
        case 40: _t->slot_show_benchmark_window(); break;
        case 41: _t->slot_show_about_window(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::MainWindow::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::MainWindow::staticMetaObject = {
    { &QMainWindow::staticMetaObject, qt_meta_stringdata_appleseed__studio__MainWindow,
      qt_meta_data_appleseed__studio__MainWindow, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::MainWindow::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::MainWindow::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::MainWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__MainWindow))
        return static_cast<void*>(const_cast< MainWindow*>(this));
    return QMainWindow::qt_metacast(_clname);
}

int appleseed::studio::MainWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QMainWindow::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 42)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 42;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::MainWindow::signal_refresh_attribute_editor(const foundation::Dictionary & _t1)const
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(const_cast< appleseed::studio::MainWindow *>(this), &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
