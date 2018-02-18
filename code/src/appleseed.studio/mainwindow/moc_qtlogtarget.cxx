/****************************************************************************
** Meta object code from reading C++ file 'qtlogtarget.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../src/appleseed.studio/mainwindow/qtlogtarget.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'qtlogtarget.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__QtLogTarget[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      43,   32,   31,   31, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__QtLogTarget[] = {
    "appleseed::studio::QtLogTarget\0\0"
    "color,text\0signal_append_item(QColor,QString)\0"
};

void appleseed::studio::QtLogTarget::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        QtLogTarget *_t = static_cast<QtLogTarget *>(_o);
        switch (_id) {
        case 0: _t->signal_append_item((*reinterpret_cast< const QColor(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::QtLogTarget::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::QtLogTarget::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__QtLogTarget,
      qt_meta_data_appleseed__studio__QtLogTarget, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::QtLogTarget::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::QtLogTarget::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::QtLogTarget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__QtLogTarget))
        return static_cast<void*>(const_cast< QtLogTarget*>(this));
    if (!strcmp(_clname, "foundation::ILogTarget"))
        return static_cast< foundation::ILogTarget*>(const_cast< QtLogTarget*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::QtLogTarget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
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

// SIGNAL 0
void appleseed::studio::QtLogTarget::signal_append_item(const QColor & _t1, const QString & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
