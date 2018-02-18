/****************************************************************************
** Meta object code from reading C++ file 'qtrenderercontroller.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/rendering/qtrenderercontroller.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'qtrenderercontroller.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__QtRendererController[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       5,       // signalCount

 // signals: signature, parameters, type, tag, flags
      41,   40,   40,   40, 0x05,
      66,   40,   40,   40, 0x05,
      93,   40,   40,   40, 0x05,
     118,   40,   40,   40, 0x05,
     139,   40,   40,   40, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__QtRendererController[] = {
    "appleseed::studio::QtRendererController\0"
    "\0signal_rendering_begin()\0"
    "signal_rendering_success()\0"
    "signal_rendering_abort()\0signal_frame_begin()\0"
    "signal_frame_end()\0"
};

void appleseed::studio::QtRendererController::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        QtRendererController *_t = static_cast<QtRendererController *>(_o);
        switch (_id) {
        case 0: _t->signal_rendering_begin(); break;
        case 1: _t->signal_rendering_success(); break;
        case 2: _t->signal_rendering_abort(); break;
        case 3: _t->signal_frame_begin(); break;
        case 4: _t->signal_frame_end(); break;
        default: ;
        }
    }
    Q_UNUSED(_a);
}

const QMetaObjectExtraData appleseed::studio::QtRendererController::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::QtRendererController::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__QtRendererController,
      qt_meta_data_appleseed__studio__QtRendererController, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::QtRendererController::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::QtRendererController::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::QtRendererController::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__QtRendererController))
        return static_cast<void*>(const_cast< QtRendererController*>(this));
    if (!strcmp(_clname, "renderer::DefaultRendererController"))
        return static_cast< renderer::DefaultRendererController*>(const_cast< QtRendererController*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::QtRendererController::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
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
void appleseed::studio::QtRendererController::signal_rendering_begin()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void appleseed::studio::QtRendererController::signal_rendering_success()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void appleseed::studio::QtRendererController::signal_rendering_abort()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}

// SIGNAL 3
void appleseed::studio::QtRendererController::signal_frame_begin()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}

// SIGNAL 4
void appleseed::studio::QtRendererController::signal_frame_end()
{
    QMetaObject::activate(this, &staticMetaObject, 4, 0);
}
QT_END_MOC_NAMESPACE
