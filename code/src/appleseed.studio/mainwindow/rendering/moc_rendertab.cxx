/****************************************************************************
** Meta object code from reading C++ file 'rendertab.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../../../src/appleseed.studio/mainwindow/rendering/rendertab.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'rendertab.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__RenderTab[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
      15,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
      11,       // signalCount

 // signals: signature, parameters, type, tag, flags
      30,   29,   29,   29, 0x05,
      53,   29,   29,   29, 0x05,
      86,   81,   29,   29, 0x05,
     118,   29,   29,   29, 0x05,
     153,  147,   29,   29, 0x05,
     195,   29,   29,   29, 0x05,
     215,   29,   29,   29, 0x05,
     236,   29,   29,   29, 0x05,
     265,   29,   29,   29, 0x05,
     289,   29,   29,   29, 0x05,
     316,   29,   29,   29, 0x05,

 // slots: signature, parameters, type, tag, flags
     375,  147,   29,   29, 0x08,
     423,  415,   29,   29, 0x08,
     455,   81,   29,   29, 0x08,
     485,  415,   29,   29, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__RenderTab[] = {
    "appleseed::studio::RenderTab\0\0"
    "signal_save_all_aovs()\0"
    "signal_quicksave_all_aovs()\0rect\0"
    "signal_set_render_region(QRect)\0"
    "signal_clear_render_region()\0point\0"
    "signal_render_widget_context_menu(QPoint)\0"
    "signal_reset_zoom()\0signal_clear_frame()\0"
    "signal_camera_change_begin()\0"
    "signal_camera_changed()\0"
    "signal_camera_change_end()\0"
    "signal_entity_picked(renderer::ScenePicker::PickingResult)\0"
    "slot_render_widget_context_menu(QPoint)\0"
    "checked\0slot_toggle_render_region(bool)\0"
    "slot_set_render_region(QRect)\0"
    "slot_toggle_pixel_inspector(bool)\0"
};

void appleseed::studio::RenderTab::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        RenderTab *_t = static_cast<RenderTab *>(_o);
        switch (_id) {
        case 0: _t->signal_save_all_aovs(); break;
        case 1: _t->signal_quicksave_all_aovs(); break;
        case 2: _t->signal_set_render_region((*reinterpret_cast< const QRect(*)>(_a[1]))); break;
        case 3: _t->signal_clear_render_region(); break;
        case 4: _t->signal_render_widget_context_menu((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 5: _t->signal_reset_zoom(); break;
        case 6: _t->signal_clear_frame(); break;
        case 7: _t->signal_camera_change_begin(); break;
        case 8: _t->signal_camera_changed(); break;
        case 9: _t->signal_camera_change_end(); break;
        case 10: _t->signal_entity_picked((*reinterpret_cast< renderer::ScenePicker::PickingResult(*)>(_a[1]))); break;
        case 11: _t->slot_render_widget_context_menu((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 12: _t->slot_toggle_render_region((*reinterpret_cast< const bool(*)>(_a[1]))); break;
        case 13: _t->slot_set_render_region((*reinterpret_cast< const QRect(*)>(_a[1]))); break;
        case 14: _t->slot_toggle_pixel_inspector((*reinterpret_cast< const bool(*)>(_a[1]))); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::RenderTab::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::RenderTab::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_appleseed__studio__RenderTab,
      qt_meta_data_appleseed__studio__RenderTab, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::RenderTab::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::RenderTab::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::RenderTab::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__RenderTab))
        return static_cast<void*>(const_cast< RenderTab*>(this));
    return QWidget::qt_metacast(_clname);
}

int appleseed::studio::RenderTab::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 15)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 15;
    }
    return _id;
}

// SIGNAL 0
void appleseed::studio::RenderTab::signal_save_all_aovs()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void appleseed::studio::RenderTab::signal_quicksave_all_aovs()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void appleseed::studio::RenderTab::signal_set_render_region(const QRect & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void appleseed::studio::RenderTab::signal_clear_render_region()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}

// SIGNAL 4
void appleseed::studio::RenderTab::signal_render_widget_context_menu(const QPoint & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}

// SIGNAL 5
void appleseed::studio::RenderTab::signal_reset_zoom()
{
    QMetaObject::activate(this, &staticMetaObject, 5, 0);
}

// SIGNAL 6
void appleseed::studio::RenderTab::signal_clear_frame()
{
    QMetaObject::activate(this, &staticMetaObject, 6, 0);
}

// SIGNAL 7
void appleseed::studio::RenderTab::signal_camera_change_begin()
{
    QMetaObject::activate(this, &staticMetaObject, 7, 0);
}

// SIGNAL 8
void appleseed::studio::RenderTab::signal_camera_changed()
{
    QMetaObject::activate(this, &staticMetaObject, 8, 0);
}

// SIGNAL 9
void appleseed::studio::RenderTab::signal_camera_change_end()
{
    QMetaObject::activate(this, &staticMetaObject, 9, 0);
}

// SIGNAL 10
void appleseed::studio::RenderTab::signal_entity_picked(renderer::ScenePicker::PickingResult _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 10, _a);
}
QT_END_MOC_NAMESPACE
