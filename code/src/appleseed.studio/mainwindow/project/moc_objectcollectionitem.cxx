/****************************************************************************
** Meta object code from reading C++ file 'objectcollectionitem.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/objectcollectionitem.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'objectcollectionitem.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__ObjectCollectionItem[] = {

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
      41,   40,   40,   40, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__ObjectCollectionItem[] = {
    "appleseed::studio::ObjectCollectionItem\0"
    "\0slot_import_objects()\0"
};

void appleseed::studio::ObjectCollectionItem::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        ObjectCollectionItem *_t = static_cast<ObjectCollectionItem *>(_o);
        switch (_id) {
        case 0: _t->slot_import_objects(); break;
        default: ;
        }
    }
    Q_UNUSED(_a);
}

const QMetaObjectExtraData appleseed::studio::ObjectCollectionItem::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::ObjectCollectionItem::staticMetaObject = {
    { &CollectionItemBase<renderer::Object>::staticMetaObject, qt_meta_stringdata_appleseed__studio__ObjectCollectionItem,
      qt_meta_data_appleseed__studio__ObjectCollectionItem, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::ObjectCollectionItem::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::ObjectCollectionItem::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::ObjectCollectionItem::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__ObjectCollectionItem))
        return static_cast<void*>(const_cast< ObjectCollectionItem*>(this));
    typedef CollectionItemBase<renderer::Object> QMocSuperClass;
    return QMocSuperClass::qt_metacast(_clname);
}

int appleseed::studio::ObjectCollectionItem::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    typedef CollectionItemBase<renderer::Object> QMocSuperClass;
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
