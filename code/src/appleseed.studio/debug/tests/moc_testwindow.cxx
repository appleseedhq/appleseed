/****************************************************************************
** Meta object code from reading C++ file 'testwindow.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/debug/tests/testwindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'testwindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__TestWindow[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
      10,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      43,   31,   30,   30, 0x08,
     103,   30,   30,   30, 0x08,
     137,   30,   30,   30, 0x08,
     162,   30,   30,   30, 0x08,
     185,   30,   30,   30, 0x08,
     212,   30,   30,   30, 0x08,
     237,   30,   30,   30, 0x08,
     280,  254,   30,   30, 0x08,
     322,   30,   30,   30, 0x08,
     351,   30,   30,   30, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__TestWindow[] = {
    "appleseed::studio::TestWindow\0\0"
    "item,column\0"
    "slot_on_test_item_check_state_changed(QTreeWidgetItem*,int)\0"
    "slot_filter_text_changed(QString)\0"
    "slot_clear_filter_text()\0"
    "slot_check_all_tests()\0"
    "slot_check_visible_tests()\0"
    "slot_uncheck_all_tests()\0slot_run_tests()\0"
    "passed_count,failed_count\0"
    "slot_on_tests_execution_complete(int,int)\0"
    "slot_clear_output_treeview()\0"
    "slot_filter_output_treeview()\0"
};

void appleseed::studio::TestWindow::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        TestWindow *_t = static_cast<TestWindow *>(_o);
        switch (_id) {
        case 0: _t->slot_on_test_item_check_state_changed((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 1: _t->slot_filter_text_changed((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 2: _t->slot_clear_filter_text(); break;
        case 3: _t->slot_check_all_tests(); break;
        case 4: _t->slot_check_visible_tests(); break;
        case 5: _t->slot_uncheck_all_tests(); break;
        case 6: _t->slot_run_tests(); break;
        case 7: _t->slot_on_tests_execution_complete((*reinterpret_cast< const int(*)>(_a[1])),(*reinterpret_cast< const int(*)>(_a[2]))); break;
        case 8: _t->slot_clear_output_treeview(); break;
        case 9: _t->slot_filter_output_treeview(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::TestWindow::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::TestWindow::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_appleseed__studio__TestWindow,
      qt_meta_data_appleseed__studio__TestWindow, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::TestWindow::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::TestWindow::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::TestWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__TestWindow))
        return static_cast<void*>(const_cast< TestWindow*>(this));
    return QWidget::qt_metacast(_clname);
}

int appleseed::studio::TestWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
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
QT_END_MOC_NAMESPACE
