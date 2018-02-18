/****************************************************************************
** Meta object code from reading C++ file 'testresultwidgetdecorator.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/debug/tests/testresultwidgetdecorator.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'testresultwidgetdecorator.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__TestResultWidgetDecorator[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      72,   46,   45,   45, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__TestResultWidgetDecorator[] = {
    "appleseed::studio::TestResultWidgetDecorator\0"
    "\0passed_count,failed_count\0"
    "slot_update(int,int)\0"
};

void appleseed::studio::TestResultWidgetDecorator::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        TestResultWidgetDecorator *_t = static_cast<TestResultWidgetDecorator *>(_o);
        switch (_id) {
        case 0: _t->slot_update((*reinterpret_cast< const int(*)>(_a[1])),(*reinterpret_cast< const int(*)>(_a[2]))); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::TestResultWidgetDecorator::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::TestResultWidgetDecorator::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__TestResultWidgetDecorator,
      qt_meta_data_appleseed__studio__TestResultWidgetDecorator, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::TestResultWidgetDecorator::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::TestResultWidgetDecorator::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::TestResultWidgetDecorator::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__TestResultWidgetDecorator))
        return static_cast<void*>(const_cast< TestResultWidgetDecorator*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::TestResultWidgetDecorator::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 1)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
