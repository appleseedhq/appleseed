/****************************************************************************
** Meta object code from reading C++ file 'cameracontroller.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/rendering/cameracontroller.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'cameracontroller.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__CameraController[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       3,       // signalCount

 // signals: signature, parameters, type, tag, flags
      37,   36,   36,   36, 0x05,
      66,   36,   36,   36, 0x05,
      90,   36,   36,   36, 0x05,

 // slots: signature, parameters, type, tag, flags
     124,  117,   36,   36, 0x0a,
     181,   36,   36,   36, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__CameraController[] = {
    "appleseed::studio::CameraController\0"
    "\0signal_camera_change_begin()\0"
    "signal_camera_changed()\0"
    "signal_camera_change_end()\0result\0"
    "slot_entity_picked(renderer::ScenePicker::PickingResult)\0"
    "slot_frame_modified()\0"
};

void appleseed::studio::CameraController::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        CameraController *_t = static_cast<CameraController *>(_o);
        switch (_id) {
        case 0: _t->signal_camera_change_begin(); break;
        case 1: _t->signal_camera_changed(); break;
        case 2: _t->signal_camera_change_end(); break;
        case 3: _t->slot_entity_picked((*reinterpret_cast< renderer::ScenePicker::PickingResult(*)>(_a[1]))); break;
        case 4: _t->slot_frame_modified(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::CameraController::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::CameraController::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__CameraController,
      qt_meta_data_appleseed__studio__CameraController, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::CameraController::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::CameraController::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::CameraController::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__CameraController))
        return static_cast<void*>(const_cast< CameraController*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::CameraController::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 5)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 5;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::CameraController::signal_camera_change_begin()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void appleseed::studio::CameraController::signal_camera_changed()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void appleseed::studio::CameraController::signal_camera_change_end()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}
QT_END_MOC_NAMESPACE
