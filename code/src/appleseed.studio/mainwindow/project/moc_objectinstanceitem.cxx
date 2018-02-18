/****************************************************************************
** Meta object code from reading C++ file 'objectinstanceitem.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/objectinstanceitem.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'objectinstanceitem.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__ObjectInstanceItem[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      39,   38,   38,   38, 0x08,
      73,   38,   38,   38, 0x08,
     112,   38,   38,   38, 0x08,
     162,  135,   38,   38, 0x08,
     218,   38,   38,   38, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__ObjectInstanceItem[] = {
    "appleseed::studio::ObjectInstanceItem\0"
    "\0slot_assign_new_disney_material()\0"
    "slot_open_material_assignment_editor()\0"
    "slot_assign_material()\0"
    "page_name,entity_name,data\0"
    "slot_assign_material_accepted(QString,QString,QVariant)\0"
    "slot_clear_material()\0"
};

void appleseed::studio::ObjectInstanceItem::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        ObjectInstanceItem *_t = static_cast<ObjectInstanceItem *>(_o);
        switch (_id) {
        case 0: _t->slot_assign_new_disney_material(); break;
        case 1: _t->slot_open_material_assignment_editor(); break;
        case 2: _t->slot_assign_material(); break;
        case 3: _t->slot_assign_material_accepted((*reinterpret_cast< QString(*)>(_a[1])),(*reinterpret_cast< QString(*)>(_a[2])),(*reinterpret_cast< QVariant(*)>(_a[3]))); break;
        case 4: _t->slot_clear_material(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::ObjectInstanceItem::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::ObjectInstanceItem::staticMetaObject = {
    { &SingleModelEntityItem<renderer::ObjectInstance,renderer::Assembly,ObjectInstanceCollectionItem>::staticMetaObject, qt_meta_stringdata_appleseed__studio__ObjectInstanceItem,
      qt_meta_data_appleseed__studio__ObjectInstanceItem, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::ObjectInstanceItem::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::ObjectInstanceItem::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::ObjectInstanceItem::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__ObjectInstanceItem))
        return static_cast<void*>(const_cast< ObjectInstanceItem*>(this));
    typedef SingleModelEntityItem<renderer::ObjectInstance,renderer::Assembly,ObjectInstanceCollectionItem> QMocSuperClass;
    return QMocSuperClass::qt_metacast(_clname);
}

int appleseed::studio::ObjectInstanceItem::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    typedef SingleModelEntityItem<renderer::ObjectInstance,renderer::Assembly,ObjectInstanceCollectionItem> QMocSuperClass;
    _id = QMocSuperClass::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 5)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 5;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
