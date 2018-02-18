/****************************************************************************
** Meta object code from reading C++ file 'objectinstanceitem.cpp'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'objectinstanceitem.cpp' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__EnrichAndForwardAcceptedSignal[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      77,   51,   50,   50, 0x05,

 // slots: signature, parameters, type, tag, flags
     140,  119,   50,   50, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__EnrichAndForwardAcceptedSignal[] = {
    "appleseed::studio::EnrichAndForwardAcceptedSignal\0"
    "\0page_name,item_value,data\0"
    "signal_accepted(QString,QString,QVariant)\0"
    "page_name,item_value\0"
    "slot_accepted(QString,QString)\0"
};

void appleseed::studio::EnrichAndForwardAcceptedSignal::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        EnrichAndForwardAcceptedSignal *_t = static_cast<EnrichAndForwardAcceptedSignal *>(_o);
        switch (_id) {
        case 0: _t->signal_accepted((*reinterpret_cast< QString(*)>(_a[1])),(*reinterpret_cast< QString(*)>(_a[2])),(*reinterpret_cast< QVariant(*)>(_a[3]))); break;
        case 1: _t->slot_accepted((*reinterpret_cast< QString(*)>(_a[1])),(*reinterpret_cast< QString(*)>(_a[2]))); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::EnrichAndForwardAcceptedSignal::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::EnrichAndForwardAcceptedSignal::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__EnrichAndForwardAcceptedSignal,
      qt_meta_data_appleseed__studio__EnrichAndForwardAcceptedSignal, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::EnrichAndForwardAcceptedSignal::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::EnrichAndForwardAcceptedSignal::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::EnrichAndForwardAcceptedSignal::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__EnrichAndForwardAcceptedSignal))
        return static_cast<void*>(const_cast< EnrichAndForwardAcceptedSignal*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::EnrichAndForwardAcceptedSignal::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
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
void appleseed::studio::EnrichAndForwardAcceptedSignal::signal_accepted(QString _t1, QString _t2, QVariant _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
