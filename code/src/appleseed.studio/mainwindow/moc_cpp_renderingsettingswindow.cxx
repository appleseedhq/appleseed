/****************************************************************************
** Meta object code from reading C++ file 'renderingsettingswindow.cpp'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.7)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'renderingsettingswindow.cpp' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.7. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_appleseed__studio__GeneralSettingsPanel[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__GeneralSettingsPanel[] = {
    "appleseed::studio::GeneralSettingsPanel\0"
};

void appleseed::studio::GeneralSettingsPanel::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    Q_UNUSED(_o);
    Q_UNUSED(_id);
    Q_UNUSED(_c);
    Q_UNUSED(_a);
}

const QMetaObjectExtraData appleseed::studio::GeneralSettingsPanel::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::GeneralSettingsPanel::staticMetaObject = {
    { &RenderSettingsPanel::staticMetaObject, qt_meta_stringdata_appleseed__studio__GeneralSettingsPanel,
      qt_meta_data_appleseed__studio__GeneralSettingsPanel, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::GeneralSettingsPanel::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::GeneralSettingsPanel::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::GeneralSettingsPanel::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__GeneralSettingsPanel))
        return static_cast<void*>(const_cast< GeneralSettingsPanel*>(this));
    return RenderSettingsPanel::qt_metacast(_clname);
}

int appleseed::studio::GeneralSettingsPanel::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = RenderSettingsPanel::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
static const uint qt_meta_data_appleseed__studio__ImagePlaneSamplingPanel[] = {

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
      50,   44,   43,   43, 0x08,
      95,   88,   43,   43, 0x08,
     148,  140,   43,   43, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_appleseed__studio__ImagePlaneSamplingPanel[] = {
    "appleseed::studio::ImagePlaneSamplingPanel\0"
    "\0index\0slot_changed_image_plane_sampler(int)\0"
    "passes\0slot_changed_image_plane_sampler_passes(int)\0"
    "samples\0slot_changed_uniform_sampler_samples(int)\0"
};

void appleseed::studio::ImagePlaneSamplingPanel::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        ImagePlaneSamplingPanel *_t = static_cast<ImagePlaneSamplingPanel *>(_o);
        switch (_id) {
        case 0: _t->slot_changed_image_plane_sampler((*reinterpret_cast< const int(*)>(_a[1]))); break;
        case 1: _t->slot_changed_image_plane_sampler_passes((*reinterpret_cast< const int(*)>(_a[1]))); break;
        case 2: _t->slot_changed_uniform_sampler_samples((*reinterpret_cast< const int(*)>(_a[1]))); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData appleseed::studio::ImagePlaneSamplingPanel::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject appleseed::studio::ImagePlaneSamplingPanel::staticMetaObject = {
    { &RenderSettingsPanel::staticMetaObject, qt_meta_stringdata_appleseed__studio__ImagePlaneSamplingPanel,
      qt_meta_data_appleseed__studio__ImagePlaneSamplingPanel, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &appleseed::studio::ImagePlaneSamplingPanel::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *appleseed::studio::ImagePlaneSamplingPanel::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *appleseed::studio::ImagePlaneSamplingPanel::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_appleseed__studio__ImagePlaneSamplingPanel))
        return static_cast<void*>(const_cast< ImagePlaneSamplingPanel*>(this));
    return RenderSettingsPanel::qt_metacast(_clname);
}

int appleseed::studio::ImagePlaneSamplingPanel::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = RenderSettingsPanel::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 3)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
