/****************************************************************************
** Meta object code from reading C++ file 'itembase.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/itembase.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'itembase.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__ItemBase[] = {

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
      46,   29,   28,   28, 0x0a,
      74,   28,   28,   28, 0x2a,
      86,   28,   28,   28, 0x0a,
     105,   28,   28,   28, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__ItemBase[] = {
    "appleseed::studio::ItemBase\0\0"
    "attribute_editor\0slot_edit(AttributeEditor*)\0"
    "slot_edit()\0slot_instantiate()\0"
    "slot_delete_multiple()\0"
};

void appleseed::studio::ItemBase::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        ItemBase *_t = static_cast<ItemBase *>(_o);
        switch (_id) {
        case 0: _t->slot_edit((*reinterpret_cast< AttributeEditor*(*)>(_a[1]))); break;
        case 1: _t->slot_edit(); break;
        case 2: _t->slot_instantiate(); break;
        case 3: _t->slot_delete_multiple(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::ItemBase::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::ItemBase::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__ItemBase,
      qt_meta_data_appleseed__studio__ItemBase, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::ItemBase::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::ItemBase::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::ItemBase::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__ItemBase))
        return static_cast<void*>(const_cast< ItemBase*>(this));
    if (!strcmp(_clname, "QTreeWidgetItem"))
        return static_cast< QTreeWidgetItem*>(const_cast< ItemBase*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::ItemBase::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
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
