/****************************************************************************
** Meta object code from reading C++ file 'disneymaterialcustomui.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/disneymaterialcustomui.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'disneymaterialcustomui.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__DisneyMaterialCustomUI[] = {

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
      43,   42,   42,   42, 0x08,
      73,   60,   42,   42, 0x08,
     102,   60,   42,   42, 0x08,
     133,   60,   42,   42, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__DisneyMaterialCustomUI[] = {
    "appleseed::studio::DisneyMaterialCustomUI\0"
    "\0slot_add_layer()\0layer_widget\0"
    "slot_move_layer_up(QWidget*)\0"
    "slot_move_layer_down(QWidget*)\0"
    "slot_delete_layer(QWidget*)\0"
};

void appleseed::studio::DisneyMaterialCustomUI::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        DisneyMaterialCustomUI *_t = static_cast<DisneyMaterialCustomUI *>(_o);
        switch (_id) {
        case 0: _t->slot_add_layer(); break;
        case 1: _t->slot_move_layer_up((*reinterpret_cast< QWidget*(*)>(_a[1]))); break;
        case 2: _t->slot_move_layer_down((*reinterpret_cast< QWidget*(*)>(_a[1]))); break;
        case 3: _t->slot_delete_layer((*reinterpret_cast< QWidget*(*)>(_a[1]))); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::DisneyMaterialCustomUI::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::DisneyMaterialCustomUI::staticMetaObject = {
    { &CustomEntityUI::staticMetaObject, qt_meta_stringdata_appleseed__studio__DisneyMaterialCustomUI,
      qt_meta_data_appleseed__studio__DisneyMaterialCustomUI, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::DisneyMaterialCustomUI::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::DisneyMaterialCustomUI::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::DisneyMaterialCustomUI::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__DisneyMaterialCustomUI))
        return static_cast<void*>(const_cast< DisneyMaterialCustomUI*>(this));
    return CustomEntityUI::qt_metacast(_clname);
}

int appleseed::studio::DisneyMaterialCustomUI::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = CustomEntityUI::qt_metacall(_c, _id, _a);
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
