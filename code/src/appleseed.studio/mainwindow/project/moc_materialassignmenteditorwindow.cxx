/****************************************************************************
** Meta object code from reading C++ file 'materialassignmenteditorwindow.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/materialassignmenteditorwindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'materialassignmenteditorwindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__MaterialAssignmentEditorWindow[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       7,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      58,   51,   50,   50, 0x05,

 // slots: signature, parameters, type, tag, flags
     104,   98,   50,   50, 0x08,
     140,   50,   50,   50, 0x08,
     199,  167,   50,   50, 0x08,
     254,   50,   50,   50, 0x08,
     267,   50,   50,   50, 0x08,
     281,   50,   50,   50, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__MaterialAssignmentEditorWindow[] = {
    "appleseed::studio::MaterialAssignmentEditorWindow\0"
    "\0values\0signal_accepted(foundation::Dictionary)\0"
    "index\0slot_change_back_material_mode(int)\0"
    "slot_open_entity_browser()\0"
    "line_edit,page_name,entity_name\0"
    "slot_entity_browser_accept(QLineEdit*,QString,QString)\0"
    "slot_apply()\0slot_accept()\0slot_cancel()\0"
};

void appleseed::studio::MaterialAssignmentEditorWindow::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        MaterialAssignmentEditorWindow *_t = static_cast<MaterialAssignmentEditorWindow *>(_o);
        switch (_id) {
        case 0: _t->signal_accepted((*reinterpret_cast< foundation::Dictionary(*)>(_a[1]))); break;
        case 1: _t->slot_change_back_material_mode((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 2: _t->slot_open_entity_browser(); break;
        case 3: _t->slot_entity_browser_accept((*reinterpret_cast< QLineEdit*(*)>(_a[1])),(*reinterpret_cast< QString(*)>(_a[2])),(*reinterpret_cast< QString(*)>(_a[3]))); break;
        case 4: _t->slot_apply(); break;
        case 5: _t->slot_accept(); break;
        case 6: _t->slot_cancel(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::MaterialAssignmentEditorWindow::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::MaterialAssignmentEditorWindow::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_appleseed__studio__MaterialAssignmentEditorWindow,
      qt_meta_data_appleseed__studio__MaterialAssignmentEditorWindow, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::MaterialAssignmentEditorWindow::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::MaterialAssignmentEditorWindow::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::MaterialAssignmentEditorWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__MaterialAssignmentEditorWindow))
        return static_cast<void*>(const_cast< MaterialAssignmentEditorWindow*>(this));
    return QWidget::qt_metacast(_clname);
}

int appleseed::studio::MaterialAssignmentEditorWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 7)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 7;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::MaterialAssignmentEditorWindow::signal_accepted(foundation::Dictionary _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
