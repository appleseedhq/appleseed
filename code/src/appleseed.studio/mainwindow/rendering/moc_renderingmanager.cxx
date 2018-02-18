/****************************************************************************
** Meta object code from reading C++ file 'renderingmanager.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/rendering/renderingmanager.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'renderingmanager.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__RenderingManager[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
      14,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       2,       // signalCount

 // signals: signature, parameters, type, tag, flags
      37,   36,   36,   36, 0x05,
      61,   36,   36,   36, 0x05,

 // slots: signature, parameters, type, tag, flags
      84,   36,   36,   36, 0x0a,
     107,   36,   36,   36, 0x0a,
     132,   36,   36,   36, 0x0a,
     162,   36,   36,   36, 0x08,
     185,   36,   36,   36, 0x08,
     206,   36,   36,   36, 0x08,
     230,   36,   36,   36, 0x08,
     249,   36,   36,   36, 0x08,
     266,   36,   36,   36, 0x08,
     293,   36,   36,   36, 0x08,
     315,   36,   36,   36, 0x08,
     340,   36,   36,   36, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__RenderingManager[] = {
    "appleseed::studio::RenderingManager\0"
    "\0signal_camera_changed()\0"
    "signal_rendering_end()\0slot_abort_rendering()\0"
    "slot_restart_rendering()\0"
    "slot_reinitialize_rendering()\0"
    "slot_rendering_begin()\0slot_rendering_end()\0"
    "slot_rendering_failed()\0slot_frame_begin()\0"
    "slot_frame_end()\0slot_camera_change_begin()\0"
    "slot_camera_changed()\0slot_camera_change_end()\0"
    "slot_master_renderer_thread_finished()\0"
};

void appleseed::studio::RenderingManager::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        RenderingManager *_t = static_cast<RenderingManager *>(_o);
        switch (_id) {
        case 0: _t->signal_camera_changed(); break;
        case 1: _t->signal_rendering_end(); break;
        case 2: _t->slot_abort_rendering(); break;
        case 3: _t->slot_restart_rendering(); break;
        case 4: _t->slot_reinitialize_rendering(); break;
        case 5: _t->slot_rendering_begin(); break;
        case 6: _t->slot_rendering_end(); break;
        case 7: _t->slot_rendering_failed(); break;
        case 8: _t->slot_frame_begin(); break;
        case 9: _t->slot_frame_end(); break;
        case 10: _t->slot_camera_change_begin(); break;
        case 11: _t->slot_camera_changed(); break;
        case 12: _t->slot_camera_change_end(); break;
        case 13: _t->slot_master_renderer_thread_finished(); break;
        default: ;
        }
    }
    Q_UNUSED(_a);
}

const QMetaObjectExtraData appleseed::studio::RenderingManager::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::RenderingManager::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__RenderingManager,
      qt_meta_data_appleseed__studio__RenderingManager, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::RenderingManager::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::RenderingManager::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::RenderingManager::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__RenderingManager))
        return static_cast<void*>(const_cast< RenderingManager*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::RenderingManager::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 14)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 14;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::RenderingManager::signal_camera_changed()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void appleseed::studio::RenderingManager::signal_rendering_end()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
