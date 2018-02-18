/****************************************************************************
** Meta object code from reading C++ file 'scenepickinghandler.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/rendering/scenepickinghandler.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'scenepickinghandler.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__ScenePickingHandler[] = {

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
      47,   40,   39,   39, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__ScenePickingHandler[] = {
    "appleseed::studio::ScenePickingHandler\0"
    "\0result\0"
    "signal_entity_picked(renderer::ScenePicker::PickingResult)\0"
};

void appleseed::studio::ScenePickingHandler::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        ScenePickingHandler *_t = static_cast<ScenePickingHandler *>(_o);
        switch (_id) {
        case 0: _t->signal_entity_picked((*reinterpret_cast< renderer::ScenePicker::PickingResult(*)>(_a[1]))); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::ScenePickingHandler::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::ScenePickingHandler::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__ScenePickingHandler,
      qt_meta_data_appleseed__studio__ScenePickingHandler, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::ScenePickingHandler::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::ScenePickingHandler::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::ScenePickingHandler::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__ScenePickingHandler))
        return static_cast<void*>(const_cast< ScenePickingHandler*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::ScenePickingHandler::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
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
void appleseed::studio::ScenePickingHandler::signal_entity_picked(renderer::ScenePicker::PickingResult _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
