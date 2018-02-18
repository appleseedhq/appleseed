/****************************************************************************
** Meta object code from reading C++ file 'renderingsettingswindow.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../src/appleseed.studio/mainwindow/renderingsettingswindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'renderingsettingswindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__RenderingSettingsWindow[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      44,   43,   43,   43, 0x05,

 // slots: signature, parameters, type, tag, flags
      71,   43,   43,   43, 0x08,
     131,  112,   43,   43, 0x08,
     173,   43,   43,   43, 0x08,
     209,   43,   43,   43, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__RenderingSettingsWindow[] = {
    "appleseed::studio::RenderingSettingsWindow\0"
    "\0signal_settings_modified()\0"
    "slot_open_configuration_manager_window()\0"
    "configuration_name\0"
    "slot_change_active_configuration(QString)\0"
    "slot_save_configuration_and_close()\0"
    "slot_restore_configuration_and_close()\0"
};

void appleseed::studio::RenderingSettingsWindow::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        RenderingSettingsWindow *_t = static_cast<RenderingSettingsWindow *>(_o);
        switch (_id) {
        case 0: _t->signal_settings_modified(); break;
        case 1: _t->slot_open_configuration_manager_window(); break;
        case 2: _t->slot_change_active_configuration((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 3: _t->slot_save_configuration_and_close(); break;
        case 4: _t->slot_restore_configuration_and_close(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::RenderingSettingsWindow::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::RenderingSettingsWindow::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_appleseed__studio__RenderingSettingsWindow,
      qt_meta_data_appleseed__studio__RenderingSettingsWindow, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::RenderingSettingsWindow::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::RenderingSettingsWindow::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::RenderingSettingsWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__RenderingSettingsWindow))
        return static_cast<void*>(const_cast< RenderingSettingsWindow*>(this));
    return QWidget::qt_metacast(_clname);
}

int appleseed::studio::RenderingSettingsWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 5)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 5;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::RenderingSettingsWindow::signal_settings_modified()const
{
    QMetaObject::activate(const_cast< appleseed::studio::RenderingSettingsWindow *>(this), &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
