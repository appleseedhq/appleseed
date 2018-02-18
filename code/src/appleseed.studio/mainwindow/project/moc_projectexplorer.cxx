/****************************************************************************
** Meta object code from reading C++ file 'projectexplorer.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/projectexplorer.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'projectexplorer.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__ProjectExplorer[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       2,       // signalCount

 // signals: signature, parameters, type, tag, flags
      36,   35,   35,   35, 0x05,
      62,   35,   35,   35, 0x05,

 // slots: signature, parameters, type, tag, flags
      92,   86,   35,   35, 0x08,
     118,   35,   35,   35, 0x08,
     160,  148,   35,   35, 0x08,
     197,   35,   35,   35, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__ProjectExplorer[] = {
    "appleseed::studio::ProjectExplorer\0\0"
    "signal_project_modified()\0"
    "signal_frame_modified()\0point\0"
    "slot_context_menu(QPoint)\0"
    "slot_item_selection_changed()\0item,column\0"
    "slot_edit_item(QTreeWidgetItem*,int)\0"
    "slot_delete_items()\0"
};

void appleseed::studio::ProjectExplorer::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        ProjectExplorer *_t = static_cast<ProjectExplorer *>(_o);
        switch (_id) {
        case 0: _t->signal_project_modified(); break;
        case 1: _t->signal_frame_modified(); break;
        case 2: _t->slot_context_menu((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 3: _t->slot_item_selection_changed(); break;
        case 4: _t->slot_edit_item((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 5: _t->slot_delete_items(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::ProjectExplorer::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::ProjectExplorer::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__ProjectExplorer,
      qt_meta_data_appleseed__studio__ProjectExplorer, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::ProjectExplorer::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::ProjectExplorer::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::ProjectExplorer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__ProjectExplorer))
        return static_cast<void*>(const_cast< ProjectExplorer*>(this));
    if (!strcmp(_clname, "foundation::NonCopyable"))
        return static_cast< foundation::NonCopyable*>(const_cast< ProjectExplorer*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::ProjectExplorer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 6)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::ProjectExplorer::signal_project_modified()const
{
    QMetaObject::activate(const_cast< appleseed::studio::ProjectExplorer *>(this), &staticMetaObject, 0, 0);
}

// SIGNAL 1
void appleseed::studio::ProjectExplorer::signal_frame_modified()const
{
    QMetaObject::activate(const_cast< appleseed::studio::ProjectExplorer *>(this), &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
