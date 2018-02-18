/****************************************************************************
** Meta object code from reading C++ file 'benchmarkwindow.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/debug/benchmarks/benchmarkwindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'benchmarkwindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__BenchmarkWindow[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      36,   35,   35,   35, 0x08,
      58,   35,   35,   35, 0x08,
      98,   35,   35,   35, 0x08,
     126,  120,   35,   35, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__BenchmarkWindow[] = {
    "appleseed::studio::BenchmarkWindow\0\0"
    "slot_run_benchmarks()\0"
    "slot_on_benchmarks_execution_complete()\0"
    "slot_rebuild_charts()\0state\0"
    "slot_on_equidistant_checkbox_state_changed(int)\0"
};

void appleseed::studio::BenchmarkWindow::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        BenchmarkWindow *_t = static_cast<BenchmarkWindow *>(_o);
        switch (_id) {
        case 0: _t->slot_run_benchmarks(); break;
        case 1: _t->slot_on_benchmarks_execution_complete(); break;
        case 2: _t->slot_rebuild_charts(); break;
        case 3: _t->slot_on_equidistant_checkbox_state_changed((*reinterpret_cast< int(*)>(_a[1]))); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::BenchmarkWindow::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::BenchmarkWindow::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_appleseed__studio__BenchmarkWindow,
      qt_meta_data_appleseed__studio__BenchmarkWindow, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::BenchmarkWindow::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::BenchmarkWindow::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::BenchmarkWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__BenchmarkWindow))
        return static_cast<void*>(const_cast< BenchmarkWindow*>(this));
    return QWidget::qt_metacast(_clname);
}

int appleseed::studio::BenchmarkWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 4)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 4;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
