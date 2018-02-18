/****************************************************************************
** Meta object code from reading C++ file 'disneymateriallayerui.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/disneymateriallayerui.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'disneymateriallayerui.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__DisneyMaterialLayerUI[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
      13,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       4,       // signalCount

 // signals: signature, parameters, type, tag, flags
      55,   42,   41,   41, 0x05,
      86,   42,   41,   41, 0x05,
     119,   42,   41,   41, 0x05,
     149,   41,   41,   41, 0x05,

 // slots: signature, parameters, type, tag, flags
     164,   41,   41,   41, 0x08,
     189,   41,   41,   41, 0x08,
     210,   41,   41,   41, 0x08,
     233,   41,   41,   41, 0x08,
     265,  253,   41,   41, 0x08,
     315,  297,   41,   41, 0x08,
     350,  253,   41,   41, 0x08,
     381,  253,   41,   41, 0x08,
     441,  418,   41,   41, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__DisneyMaterialLayerUI[] = {
    "appleseed::studio::DisneyMaterialLayerUI\0"
    "\0layer_widget\0signal_move_layer_up(QWidget*)\0"
    "signal_move_layer_down(QWidget*)\0"
    "signal_delete_layer(QWidget*)\0"
    "signal_apply()\0slot_fold_unfold_layer()\0"
    "slot_move_layer_up()\0slot_move_layer_down()\0"
    "slot_delete_layer()\0widget_name\0"
    "slot_open_color_picker(QString)\0"
    "widget_name,color\0slot_color_changed(QString,QColor)\0"
    "slot_open_file_picker(QString)\0"
    "slot_open_expression_editor(QString)\0"
    "widget_name,expression\0"
    "slot_expression_changed(QString,QString)\0"
};

void appleseed::studio::DisneyMaterialLayerUI::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        DisneyMaterialLayerUI *_t = static_cast<DisneyMaterialLayerUI *>(_o);
        switch (_id) {
        case 0: _t->signal_move_layer_up((*reinterpret_cast< QWidget*(*)>(_a[1]))); break;
        case 1: _t->signal_move_layer_down((*reinterpret_cast< QWidget*(*)>(_a[1]))); break;
        case 2: _t->signal_delete_layer((*reinterpret_cast< QWidget*(*)>(_a[1]))); break;
        case 3: _t->signal_apply(); break;
        case 4: _t->slot_fold_unfold_layer(); break;
        case 5: _t->slot_move_layer_up(); break;
        case 6: _t->slot_move_layer_down(); break;
        case 7: _t->slot_delete_layer(); break;
        case 8: _t->slot_open_color_picker((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 9: _t->slot_color_changed((*reinterpret_cast< const QString(*)>(_a[1])),(*reinterpret_cast< const QColor(*)>(_a[2]))); break;
        case 10: _t->slot_open_file_picker((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 11: _t->slot_open_expression_editor((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 12: _t->slot_expression_changed((*reinterpret_cast< const QString(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::DisneyMaterialLayerUI::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::DisneyMaterialLayerUI::staticMetaObject = {
    { &QFrame::staticMetaObject, qt_meta_stringdata_appleseed__studio__DisneyMaterialLayerUI,
      qt_meta_data_appleseed__studio__DisneyMaterialLayerUI, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::DisneyMaterialLayerUI::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::DisneyMaterialLayerUI::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::DisneyMaterialLayerUI::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__DisneyMaterialLayerUI))
        return static_cast<void*>(const_cast< DisneyMaterialLayerUI*>(this));
    return QFrame::qt_metacast(_clname);
}

int appleseed::studio::DisneyMaterialLayerUI::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 13)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 13;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::DisneyMaterialLayerUI::signal_move_layer_up(QWidget * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void appleseed::studio::DisneyMaterialLayerUI::signal_move_layer_down(QWidget * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void appleseed::studio::DisneyMaterialLayerUI::signal_delete_layer(QWidget * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void appleseed::studio::DisneyMaterialLayerUI::signal_apply()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}
QT_END_MOC_NAMESPACE
