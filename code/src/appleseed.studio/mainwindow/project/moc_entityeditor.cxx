/****************************************************************************
** Meta object code from reading C++ file 'entityeditor.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/entityeditor.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'entityeditor.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__EntityEditor[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       8,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      40,   33,   32,   32, 0x05,

 // slots: signature, parameters, type, tag, flags
      79,   32,   32,   32, 0x08,
     111,   99,   32,   32, 0x08,
     179,  145,   32,   32, 0x08,
     231,   99,   32,   32, 0x08,
     281,  263,   32,   32, 0x08,
     316,   99,   32,   32, 0x08,
     347,   32,   32,   32, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__EntityEditor[] = {
    "appleseed::studio::EntityEditor\0\0"
    "values\0signal_applied(foundation::Dictionary)\0"
    "slot_rebuild_form()\0widget_name\0"
    "slot_open_entity_browser(QString)\0"
    "widget_name,page_name,entity_name\0"
    "slot_entity_browser_accept(QString,QString,QString)\0"
    "slot_open_color_picker(QString)\0"
    "widget_name,color\0slot_color_changed(QString,QColor)\0"
    "slot_open_file_picker(QString)\0"
    "slot_apply()\0"
};

void appleseed::studio::EntityEditor::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        EntityEditor *_t = static_cast<EntityEditor *>(_o);
        switch (_id) {
        case 0: _t->signal_applied((*reinterpret_cast< foundation::Dictionary(*)>(_a[1]))); break;
        case 1: _t->slot_rebuild_form(); break;
        case 2: _t->slot_open_entity_browser((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 3: _t->slot_entity_browser_accept((*reinterpret_cast< QString(*)>(_a[1])),(*reinterpret_cast< QString(*)>(_a[2])),(*reinterpret_cast< QString(*)>(_a[3]))); break;
        case 4: _t->slot_open_color_picker((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 5: _t->slot_color_changed((*reinterpret_cast< const QString(*)>(_a[1])),(*reinterpret_cast< const QColor(*)>(_a[2]))); break;
        case 6: _t->slot_open_file_picker((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 7: _t->slot_apply(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::EntityEditor::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::EntityEditor::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_appleseed__studio__EntityEditor,
      qt_meta_data_appleseed__studio__EntityEditor, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::EntityEditor::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::EntityEditor::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::EntityEditor::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__EntityEditor))
        return static_cast<void*>(const_cast< EntityEditor*>(this));
    return QObject::qt_metacast(_clname);
}

int appleseed::studio::EntityEditor::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 8)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 8;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::EntityEditor::signal_applied(foundation::Dictionary _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
