/****************************************************************************
** Meta object code from reading C++ file 'qttestlistener.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/debug/tests/qttestlistener.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'qttestlistener.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__QtTestListener[] = {

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
      40,   35,   34,   34, 0x05,
     109,   83,   34,   34, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__QtTestListener[] = {
    "appleseed::studio::QtTestListener\0\0"
    "item\0signal_add_top_level_item(TestOutputItem*)\0"
    "passed_count,failed_count\0"
    "signal_update(int,int)\0"
};

void appleseed::studio::QtTestListener::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        QtTestListener *_t = static_cast<QtTestListener *>(_o);
        switch (_id) {
        case 0: _t->signal_add_top_level_item((*reinterpret_cast< TestOutputItem*(*)>(_a[1]))); break;
        case 1: _t->signal_update((*reinterpret_cast< const int(*)>(_a[1])),(*reinterpret_cast< const int(*)>(_a[2]))); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::QtTestListener::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::QtTestListener::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__QtTestListener,
      qt_meta_data_appleseed__studio__QtTestListener, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::QtTestListener::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::QtTestListener::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::QtTestListener::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__QtTestListener))
        return static_cast<void*>(const_cast< QtTestListener*>(this));
    if (!strcmp(_clname, "foundation::TestListenerBase"))
        return static_cast< foundation::TestListenerBase*>(const_cast< QtTestListener*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::QtTestListener::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
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
void appleseed::studio::QtTestListener::signal_add_top_level_item(TestOutputItem * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void appleseed::studio::QtTestListener::signal_update(const int _t1, const int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
