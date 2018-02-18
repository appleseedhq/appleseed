/****************************************************************************
** Meta object code from reading C++ file 'benchmarkrunnerthread.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/debug/benchmarks/benchmarkrunnerthread.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'benchmarkrunnerthread.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__BenchmarkRunnerThread[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       2,       // signalCount

 // signals: signature, parameters, type, tag, flags
      42,   41,   41,   41, 0x05,
      80,   41,   41,   41, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__BenchmarkRunnerThread[] = {
    "appleseed::studio::BenchmarkRunnerThread\0"
    "\0signal_cannot_create_benchmark_file()\0"
    "signal_finished()\0"
};

void appleseed::studio::BenchmarkRunnerThread::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        BenchmarkRunnerThread *_t = static_cast<BenchmarkRunnerThread *>(_o);
        switch (_id) {
        case 0: _t->signal_cannot_create_benchmark_file(); break;
        case 1: _t->signal_finished(); break;
        default: ;
        }
    }
    Q_UNUSED(_a);
}

const QMetaObjectExtraData appleseed::studio::BenchmarkRunnerThread::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::BenchmarkRunnerThread::staticMetaObject = {
    { &QThread::staticMetaObject, qt_meta_stringdata_appleseed__studio__BenchmarkRunnerThread,
      qt_meta_data_appleseed__studio__BenchmarkRunnerThread, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::BenchmarkRunnerThread::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::BenchmarkRunnerThread::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::BenchmarkRunnerThread::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__BenchmarkRunnerThread))
        return static_cast<void*>(const_cast< BenchmarkRunnerThread*>(this));
    return QThread::qt_metacast(_clname);
}

int appleseed::studio::BenchmarkRunnerThread::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QThread::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 2)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::BenchmarkRunnerThread::signal_cannot_create_benchmark_file()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void appleseed::studio::BenchmarkRunnerThread::signal_finished()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
