/****************************************************************************
** Meta object code from reading C++ file 'materialitem.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/materialitem.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'materialitem.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__MaterialItem[] = {

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
      33,   32,   32,   32, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__MaterialItem[] = {
    "appleseed::studio::MaterialItem\0\0"
    "slot_export()\0"
};

void appleseed::studio::MaterialItem::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        MaterialItem *_t = static_cast<MaterialItem *>(_o);
        switch (_id) {
        case 0: _t->slot_export(); break;
        default: ;
        }
    }
    Q_UNUSED(_a);
}

const QMetaObjectExtraData appleseed::studio::MaterialItem::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::MaterialItem::staticMetaObject = {
    { &FixedModelEntityItem<renderer::Material,renderer::Assembly,MaterialCollectionItem>::staticMetaObject, qt_meta_stringdata_appleseed__studio__MaterialItem,
      qt_meta_data_appleseed__studio__MaterialItem, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::MaterialItem::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::MaterialItem::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::MaterialItem::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__MaterialItem))
        return static_cast<void*>(const_cast< MaterialItem*>(this));
    typedef FixedModelEntityItem<renderer::Material,renderer::Assembly,MaterialCollectionItem> QMocSuperClass;
    return QMocSuperClass::qt_metacast(_clname);
}

int appleseed::studio::MaterialItem::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    typedef FixedModelEntityItem<renderer::Material,renderer::Assembly,MaterialCollectionItem> QMocSuperClass;
    _id = QMocSuperClass::qt_metacall(_c, _id, _a);
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
