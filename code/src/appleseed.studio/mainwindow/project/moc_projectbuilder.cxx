/****************************************************************************
** Meta object code from reading C++ file 'projectbuilder.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/projectbuilder.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'projectbuilder.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__ProjectBuilder[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       2,       // signalCount

 // signals: signature, parameters, type, tag, flags
      35,   34,   34,   34, 0x05,
      61,   34,   34,   34, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__ProjectBuilder[] = {
    "appleseed::studio::ProjectBuilder\0\0"
    "signal_project_modified()\0"
    "signal_frame_modified()\0"
};

void appleseed::studio::ProjectBuilder::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        ProjectBuilder *_t = static_cast<ProjectBuilder *>(_o);
        switch (_id) {
        case 0: _t->signal_project_modified(); break;
        case 1: _t->signal_frame_modified(); break;
        default: ;
        }
    }
    Q_UNUSED(_a);
}

const QMetaObjectExtraData appleseed::studio::ProjectBuilder::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::ProjectBuilder::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__ProjectBuilder,
      qt_meta_data_appleseed__studio__ProjectBuilder, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::ProjectBuilder::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::ProjectBuilder::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::ProjectBuilder::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__ProjectBuilder))
        return static_cast<void*>(const_cast< ProjectBuilder*>(this));
    if (!strcmp(_clname, "foundation::NonCopyable"))
        return static_cast< foundation::NonCopyable*>(const_cast< ProjectBuilder*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::ProjectBuilder::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 2)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::ProjectBuilder::signal_project_modified()const
{
    QMetaObject::activate(const_cast< appleseed::studio::ProjectBuilder *>(this), &staticMetaObject, 0, 0);
}

// SIGNAL 1
void appleseed::studio::ProjectBuilder::signal_frame_modified()const
{
    QMetaObject::activate(const_cast< appleseed::studio::ProjectBuilder *>(this), &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
