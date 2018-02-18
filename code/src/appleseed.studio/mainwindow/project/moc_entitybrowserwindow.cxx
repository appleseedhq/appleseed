/****************************************************************************
** Meta object code from reading C++ file 'entitybrowserwindow.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/entitybrowserwindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'entitybrowserwindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__EntityBrowserWindow[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       7,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      61,   40,   39,   39, 0x05,

 // slots: signature, parameters, type, tag, flags
     104,   94,   39,   39, 0x08,
     134,   39,   39,   39, 0x08,
     172,  164,   39,   39, 0x08,
     210,   39,   39,   39, 0x08,
     224,   39,   39,   39, 0x08,
     252,  244,   39,   39, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__EntityBrowserWindow[] = {
    "appleseed::studio::EntityBrowserWindow\0"
    "\0page_name,item_value\0"
    "signal_accepted(QString,QString)\0"
    "tab_index\0slot_current_tab_changed(int)\0"
    "slot_item_selection_changed()\0current\0"
    "slot_item_activated(QListWidgetItem*)\0"
    "slot_accept()\0slot_clear_filter()\0"
    "pattern\0slot_filter_text_changed(QString)\0"
};

void appleseed::studio::EntityBrowserWindow::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        EntityBrowserWindow *_t = static_cast<EntityBrowserWindow *>(_o);
        switch (_id) {
        case 0: _t->signal_accepted((*reinterpret_cast< QString(*)>(_a[1])),(*reinterpret_cast< QString(*)>(_a[2]))); break;
        case 1: _t->slot_current_tab_changed((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 2: _t->slot_item_selection_changed(); break;
        case 3: _t->slot_item_activated((*reinterpret_cast< QListWidgetItem*(*)>(_a[1]))); break;
        case 4: _t->slot_accept(); break;
        case 5: _t->slot_clear_filter(); break;
        case 6: _t->slot_filter_text_changed((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::EntityBrowserWindow::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::EntityBrowserWindow::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_appleseed__studio__EntityBrowserWindow,
      qt_meta_data_appleseed__studio__EntityBrowserWindow, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::EntityBrowserWindow::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::EntityBrowserWindow::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::EntityBrowserWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__EntityBrowserWindow))
        return static_cast<void*>(const_cast< EntityBrowserWindow*>(this));
    return QWidget::qt_metacast(_clname);
}

int appleseed::studio::EntityBrowserWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 7)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 7;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::EntityBrowserWindow::signal_accepted(QString _t1, QString _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
