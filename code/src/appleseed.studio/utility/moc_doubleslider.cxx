/****************************************************************************
** Meta object code from reading C++ file 'doubleslider.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../src/appleseed.studio/utility/doubleslider.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'doubleslider.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__DoubleSlider[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       2,       // signalCount

 // signals: signature, parameters, type, tag, flags
      39,   33,   32,   32, 0x05,
      68,   60,   32,   32, 0x05,

 // slots: signature, parameters, type, tag, flags
      96,   33,   32,   32, 0x0a,
     110,   33,   32,   32, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__DoubleSlider[] = {
    "appleseed::studio::DoubleSlider\0\0value\0"
    "valueChanged(double)\0min,max\0"
    "rangeChanged(double,double)\0setValue(int)\0"
    "setValue(double)\0"
};

void appleseed::studio::DoubleSlider::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        DoubleSlider *_t = static_cast<DoubleSlider *>(_o);
        switch (_id) {
        case 0: _t->valueChanged((*reinterpret_cast< const double(*)>(_a[1]))); break;
        case 1: _t->rangeChanged((*reinterpret_cast< const double(*)>(_a[1])),(*reinterpret_cast< const double(*)>(_a[2]))); break;
        case 2: _t->setValue((*reinterpret_cast< const int(*)>(_a[1]))); break;
        case 3: _t->setValue((*reinterpret_cast< const double(*)>(_a[1]))); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::DoubleSlider::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::DoubleSlider::staticMetaObject = {
    { &QSlider::staticMetaObject, qt_meta_stringdata_appleseed__studio__DoubleSlider,
      qt_meta_data_appleseed__studio__DoubleSlider, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::DoubleSlider::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::DoubleSlider::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::DoubleSlider::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__DoubleSlider))
        return static_cast<void*>(const_cast< DoubleSlider*>(this));
    return QSlider::qt_metacast(_clname);
}

int appleseed::studio::DoubleSlider::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QSlider::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 4)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 4;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::DoubleSlider::valueChanged(const double _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void appleseed::studio::DoubleSlider::rangeChanged(const double _t1, const double _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
