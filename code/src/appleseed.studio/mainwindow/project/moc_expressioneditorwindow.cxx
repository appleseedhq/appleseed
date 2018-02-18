/****************************************************************************
** Meta object code from reading C++ file 'expressioneditorwindow.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/project/expressioneditorwindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'expressioneditorwindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__ExpressionEditorWindow[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
      10,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       2,       // signalCount

 // signals: signature, parameters, type, tag, flags
      66,   43,   42,   42, 0x05,
     109,   42,   42,   42, 0x05,

 // slots: signature, parameters, type, tag, flags
     132,   42,   42,   42, 0x0a,
     146,   42,   42,   42, 0x0a,
     159,   42,   42,   42, 0x0a,
     173,   42,   42,   42, 0x0a,
     197,   42,   42,   42, 0x0a,
     216,   42,   42,   42, 0x0a,
     235,   42,   42,   42, 0x0a,
     256,   42,   42,   42, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__ExpressionEditorWindow[] = {
    "appleseed::studio::ExpressionEditorWindow\0"
    "\0widget_name,expression\0"
    "signal_expression_applied(QString,QString)\0"
    "signal_editor_closed()\0slot_accept()\0"
    "slot_apply()\0slot_cancel()\0"
    "slot_clear_expression()\0slot_save_script()\0"
    "slot_load_script()\0slot_show_examples()\0"
    "slot_show_help()\0"
};

void appleseed::studio::ExpressionEditorWindow::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        ExpressionEditorWindow *_t = static_cast<ExpressionEditorWindow *>(_o);
        switch (_id) {
        case 0: _t->signal_expression_applied((*reinterpret_cast< const QString(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        case 1: _t->signal_editor_closed(); break;
        case 2: _t->slot_accept(); break;
        case 3: _t->slot_apply(); break;
        case 4: _t->slot_cancel(); break;
        case 5: _t->slot_clear_expression(); break;
        case 6: _t->slot_save_script(); break;
        case 7: _t->slot_load_script(); break;
        case 8: _t->slot_show_examples(); break;
        case 9: _t->slot_show_help(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::ExpressionEditorWindow::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::ExpressionEditorWindow::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_appleseed__studio__ExpressionEditorWindow,
      qt_meta_data_appleseed__studio__ExpressionEditorWindow, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::ExpressionEditorWindow::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::ExpressionEditorWindow::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::ExpressionEditorWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__ExpressionEditorWindow))
        return static_cast<void*>(const_cast< ExpressionEditorWindow*>(this));
    return QWidget::qt_metacast(_clname);
}

int appleseed::studio::ExpressionEditorWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 10)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 10;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::ExpressionEditorWindow::signal_expression_applied(const QString & _t1, const QString & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void appleseed::studio::ExpressionEditorWindow::signal_editor_closed()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
