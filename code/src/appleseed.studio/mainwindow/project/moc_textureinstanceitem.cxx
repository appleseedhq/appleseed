/****************************************************************************
** Meta object code from reading C++ file 'textureinstanceitem.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/textureinstanceitem.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'textureinstanceitem.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__TextureInstanceItem[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__TextureInstanceItem[] = {
    "appleseed::studio::TextureInstanceItem\0"
};

void appleseed::studio::TextureInstanceItem::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    Q_UNUSED(_o);
    Q_UNUSED(_id);
    Q_UNUSED(_c);
    Q_UNUSED(_a);
}

const QMetaObjectExtraData appleseed::studio::TextureInstanceItem::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::TextureInstanceItem::staticMetaObject = {
    { &SingleModelEntityItem<renderer::TextureInstance,renderer::BaseGroup,TextureInstanceCollectionItem>::staticMetaObject, qt_meta_stringdata_appleseed__studio__TextureInstanceItem,
      qt_meta_data_appleseed__studio__TextureInstanceItem, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::TextureInstanceItem::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::TextureInstanceItem::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::TextureInstanceItem::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__TextureInstanceItem))
        return static_cast<void*>(const_cast< TextureInstanceItem*>(this));
    typedef SingleModelEntityItem<renderer::TextureInstance,renderer::BaseGroup,TextureInstanceCollectionItem> QMocSuperClass;
    return QMocSuperClass::qt_metacast(_clname);
}

int appleseed::studio::TextureInstanceItem::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    typedef SingleModelEntityItem<renderer::TextureInstance,renderer::BaseGroup,TextureInstanceCollectionItem> QMocSuperClass;
    _id = QMocSuperClass::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
QT_END_MOC_NAMESPACE
