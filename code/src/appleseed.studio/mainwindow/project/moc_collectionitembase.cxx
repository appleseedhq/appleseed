/****************************************************************************
** Meta object code from reading C++ file 'collectionitembase.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/collectionitembase.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'collectionitembase.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__CollectionItemBaseSlots[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      44,   43,   43,   43, 0x09,
      65,   58,   43,   43, 0x09,
     109,   58,   43,   43, 0x09,
     154,   58,   43,   43, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__CollectionItemBaseSlots[] = {
    "appleseed::studio::CollectionItemBaseSlots\0"
    "\0slot_create()\0values\0"
    "slot_create_applied(foundation::Dictionary)\0"
    "slot_create_accepted(foundation::Dictionary)\0"
    "slot_create_canceled(foundation::Dictionary)\0"
};

void appleseed::studio::CollectionItemBaseSlots::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        CollectionItemBaseSlots *_t = static_cast<CollectionItemBaseSlots *>(_o);
        switch (_id) {
        case 0: _t->slot_create(); break;
        case 1: _t->slot_create_applied((*reinterpret_cast< foundation::Dictionary(*)>(_a[1]))); break;
        case 2: _t->slot_create_accepted((*reinterpret_cast< foundation::Dictionary(*)>(_a[1]))); break;
        case 3: _t->slot_create_canceled((*reinterpret_cast< foundation::Dictionary(*)>(_a[1]))); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::CollectionItemBaseSlots::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::CollectionItemBaseSlots::staticMetaObject = {
    { &ItemBase::staticMetaObject, qt_meta_stringdata_appleseed__studio__CollectionItemBaseSlots,
      qt_meta_data_appleseed__studio__CollectionItemBaseSlots, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::CollectionItemBaseSlots::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::CollectionItemBaseSlots::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::CollectionItemBaseSlots::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__CollectionItemBaseSlots))
        return static_cast<void*>(const_cast< CollectionItemBaseSlots*>(this));
    return ItemBase::qt_metacast(_clname);
}

int appleseed::studio::CollectionItemBaseSlots::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ItemBase::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 4)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 4;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
