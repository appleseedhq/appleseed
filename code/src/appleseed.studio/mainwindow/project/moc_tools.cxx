/****************************************************************************
** Meta object code from reading C++ file 'tools.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/tools.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'tools.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__LineEditSliderAdaptor[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      48,   42,   41,   41, 0x0a,
      78,   42,   41,   41, 0x0a,
     109,   41,   41,   41, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__LineEditSliderAdaptor[] = {
    "appleseed::studio::LineEditSliderAdaptor\0"
    "\0value\0slot_set_line_edit_value(int)\0"
    "slot_set_slider_value(QString)\0"
    "slot_apply_line_edit_value()\0"
};

void appleseed::studio::LineEditSliderAdaptor::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        LineEditSliderAdaptor *_t = static_cast<LineEditSliderAdaptor *>(_o);
        switch (_id) {
        case 0: _t->slot_set_line_edit_value((*reinterpret_cast< const int(*)>(_a[1]))); break;
        case 1: _t->slot_set_slider_value((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 2: _t->slot_apply_line_edit_value(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::LineEditSliderAdaptor::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::LineEditSliderAdaptor::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__LineEditSliderAdaptor,
      qt_meta_data_appleseed__studio__LineEditSliderAdaptor, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::LineEditSliderAdaptor::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::LineEditSliderAdaptor::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::LineEditSliderAdaptor::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__LineEditSliderAdaptor))
        return static_cast<void*>(const_cast< LineEditSliderAdaptor*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::LineEditSliderAdaptor::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 3)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 3;
    }
    return _id;
}
static const uint qt_meta_data_appleseed__studio__LineEditDoubleSliderAdaptor[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      54,   48,   47,   47, 0x0a,
      87,   48,   47,   47, 0x0a,
     118,   47,   47,   47, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__LineEditDoubleSliderAdaptor[] = {
    "appleseed::studio::LineEditDoubleSliderAdaptor\0"
    "\0value\0slot_set_line_edit_value(double)\0"
    "slot_set_slider_value(QString)\0"
    "slot_apply_line_edit_value()\0"
};

void appleseed::studio::LineEditDoubleSliderAdaptor::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        LineEditDoubleSliderAdaptor *_t = static_cast<LineEditDoubleSliderAdaptor *>(_o);
        switch (_id) {
        case 0: _t->slot_set_line_edit_value((*reinterpret_cast< const double(*)>(_a[1]))); break;
        case 1: _t->slot_set_slider_value((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 2: _t->slot_apply_line_edit_value(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::LineEditDoubleSliderAdaptor::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::LineEditDoubleSliderAdaptor::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__LineEditDoubleSliderAdaptor,
      qt_meta_data_appleseed__studio__LineEditDoubleSliderAdaptor, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::LineEditDoubleSliderAdaptor::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::LineEditDoubleSliderAdaptor::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::LineEditDoubleSliderAdaptor::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__LineEditDoubleSliderAdaptor))
        return static_cast<void*>(const_cast< LineEditDoubleSliderAdaptor*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::LineEditDoubleSliderAdaptor::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 3)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 3;
    }
    return _id;
}
static const uint qt_meta_data_appleseed__studio__ForwardColorChangedSignal[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       2,       // signalCount

 // signals: signature, parameters, type, tag, flags
      64,   46,   45,   45, 0x05,
     101,   46,   45,   45, 0x05,

 // slots: signature, parameters, type, tag, flags
     142,  136,   45,   45, 0x0a,
     169,   45,   45,   45, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__ForwardColorChangedSignal[] = {
    "appleseed::studio::ForwardColorChangedSignal\0"
    "\0widget_name,color\0"
    "signal_color_changed(QString,QColor)\0"
    "signal_color_reset(QString,QColor)\0"
    "color\0slot_color_changed(QColor)\0"
    "slot_color_reset()\0"
};

void appleseed::studio::ForwardColorChangedSignal::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        ForwardColorChangedSignal *_t = static_cast<ForwardColorChangedSignal *>(_o);
        switch (_id) {
        case 0: _t->signal_color_changed((*reinterpret_cast< const QString(*)>(_a[1])),(*reinterpret_cast< const QColor(*)>(_a[2]))); break;
        case 1: _t->signal_color_reset((*reinterpret_cast< const QString(*)>(_a[1])),(*reinterpret_cast< const QColor(*)>(_a[2]))); break;
        case 2: _t->slot_color_changed((*reinterpret_cast< const QColor(*)>(_a[1]))); break;
        case 3: _t->slot_color_reset(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::ForwardColorChangedSignal::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::ForwardColorChangedSignal::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__ForwardColorChangedSignal,
      qt_meta_data_appleseed__studio__ForwardColorChangedSignal, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::ForwardColorChangedSignal::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::ForwardColorChangedSignal::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::ForwardColorChangedSignal::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__ForwardColorChangedSignal))
        return static_cast<void*>(const_cast< ForwardColorChangedSignal*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::ForwardColorChangedSignal::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
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

// SIGNAL 0
void appleseed::studio::ForwardColorChangedSignal::signal_color_changed(const QString & _t1, const QColor & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void appleseed::studio::ForwardColorChangedSignal::signal_color_reset(const QString & _t1, const QColor & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
