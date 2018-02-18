/****************************************************************************
** Meta object code from reading C++ file 'entityinputwidget.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/entityinputwidget.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'entityinputwidget.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__EntityInputWidget[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       2,       // signalCount

 // signals: signature, parameters, type, tag, flags
      38,   37,   37,   37, 0x05,
      67,   37,   37,   37, 0x05,

 // slots: signature, parameters, type, tag, flags
      90,   84,   37,   37, 0x0a,
     109,   37,   37,   37, 0x08,
     126,   37,   37,   37, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__EntityInputWidget[] = {
    "appleseed::studio::EntityInputWidget\0"
    "\0signal_bind_button_clicked()\0"
    "signal_changed()\0value\0set_value(QString)\0"
    "slot_set_value()\0slot_unbind()\0"
};

void appleseed::studio::EntityInputWidget::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        EntityInputWidget *_t = static_cast<EntityInputWidget *>(_o);
        switch (_id) {
        case 0: _t->signal_bind_button_clicked(); break;
        case 1: _t->signal_changed(); break;
        case 2: _t->set_value((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 3: _t->slot_set_value(); break;
        case 4: _t->slot_unbind(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::EntityInputWidget::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::EntityInputWidget::staticMetaObject = {
    { &QStackedWidget::staticMetaObject, qt_meta_stringdata_appleseed__studio__EntityInputWidget,
      qt_meta_data_appleseed__studio__EntityInputWidget, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::EntityInputWidget::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::EntityInputWidget::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::EntityInputWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__EntityInputWidget))
        return static_cast<void*>(const_cast< EntityInputWidget*>(this));
    return QStackedWidget::qt_metacast(_clname);
}

int appleseed::studio::EntityInputWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QStackedWidget::qt_metacall(_c, _id, _a);
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
void appleseed::studio::EntityInputWidget::signal_bind_button_clicked()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void appleseed::studio::EntityInputWidget::signal_changed()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
static const uint qt_meta_data_appleseed__studio__ColorMapInputWidget[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       2,       // signalCount

 // signals: signature, parameters, type, tag, flags
      40,   39,   39,   39, 0x05,
      69,   39,   39,   39, 0x05,

 // slots: signature, parameters, type, tag, flags
      92,   86,   39,   39, 0x0a,
     111,   39,   39,   39, 0x08,
     128,   39,   39,   39, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__ColorMapInputWidget[] = {
    "appleseed::studio::ColorMapInputWidget\0"
    "\0signal_bind_button_clicked()\0"
    "signal_changed()\0value\0set_value(QString)\0"
    "slot_set_value()\0slot_unbind()\0"
};

void appleseed::studio::ColorMapInputWidget::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        ColorMapInputWidget *_t = static_cast<ColorMapInputWidget *>(_o);
        switch (_id) {
        case 0: _t->signal_bind_button_clicked(); break;
        case 1: _t->signal_changed(); break;
        case 2: _t->set_value((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 3: _t->slot_set_value(); break;
        case 4: _t->slot_unbind(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::ColorMapInputWidget::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::ColorMapInputWidget::staticMetaObject = {
    { &QStackedWidget::staticMetaObject, qt_meta_stringdata_appleseed__studio__ColorMapInputWidget,
      qt_meta_data_appleseed__studio__ColorMapInputWidget, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::ColorMapInputWidget::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::ColorMapInputWidget::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::ColorMapInputWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__ColorMapInputWidget))
        return static_cast<void*>(const_cast< ColorMapInputWidget*>(this));
    return QStackedWidget::qt_metacast(_clname);
}

int appleseed::studio::ColorMapInputWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QStackedWidget::qt_metacall(_c, _id, _a);
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
void appleseed::studio::ColorMapInputWidget::signal_bind_button_clicked()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void appleseed::studio::ColorMapInputWidget::signal_changed()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
