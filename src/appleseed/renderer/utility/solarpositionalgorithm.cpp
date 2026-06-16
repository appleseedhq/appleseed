
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Joao Marcos Costa, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

// Interface header. 
#include "solarpositionalgorithm.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/uid.h"

// appleseed.renderer headers.
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/basegroup.h"
#include "renderer/modeling/entity/onframebeginrecorder.h"
#include "renderer/utility/paramarray.h"

// Standard headers.
#include <math.h>

using namespace foundation;

namespace renderer
{

    namespace
    {
        const UniqueID g_class_uid = new_guid();
    }

    double get_julian_century(double jd)
    {
        return (jd - 2451545.0) / 36525.0;
    }

    double get_julian_day(int year, int month, int day, double decimal_day)
    {
        decimal_day += day;

        if (month < 3)
        {
            month += 12;
            year--;
        }

        double julian_day = std::floor(365.25 * (year + 4716.0)) +
            std::floor(30.6001 * (month + 1)) + decimal_day - 1524.5;

        // Test whether to change to gregorian caledar calculation.
        if (julian_day > 2299160.0) {
            double A = std::floor(year / 100);
            julian_day += (2 - A + std::floor(A / 4));
        }

        return julian_day;
    }

    struct SunPositioner::Impl
    {
        // Input time.
        int     year;
        int     month;
        int     day;
        int     hour;
        int     minute;
        int     second;
        int     timezone;

        // Input location.
        double longitude;       // Observer longitude (negative west of Greenwich)
        double latitude;        // Observer latitude (negative south of equator)
        double elevation;       // Observer elevation [meters]
        double azm_rotation;    // Surface azimuth rotation.

        double zenith;          // Topocentric zenith angle [degrees]
        double azimuth;         // Topocentric azimuth angle (eastward from north) [for navigators and solar radiation]

        double solar_noon;
        double sunrise;         // Local sunrise time (+/- 30 seconds).
        double sunset;          // Local sunset time (+/- 30 seconds).
    };

    // Constructor
    SunPositioner::SunPositioner(const char* name, const ParamArray& params)
        : Entity(g_class_uid, params),
        impl(new Impl())
    {
    }

    inline double SunPositioner::mean_obliquity_ecliptic(const double jc) const
    {
        return 23.0 + (26.0 + (21.448 - jc * (46.815 + jc * (0.00059 - jc * 0.001813))) / 60.0) / 60.0;
    }

    inline double SunPositioner::obliquity_correction(const double jc) const
    {
        return mean_obliquity_ecliptic(jc) + 0.00256 * std::cos(deg_to_rad(125.04 - 1934.136 * jc));
    }

    inline double SunPositioner::mean_sun_anomaly(const double jc) const
    {
        return 357.52911 + jc * (35999.05029 - 0.0001537 * jc);
    }

    inline double SunPositioner::sun_eqc_of_center(const double jc, const double msa) const
    {
        return std::sin(deg_to_rad(msa))* (1.914602 - jc * (0.004817 + 0.000014 * jc)) +
            std::sin(deg_to_rad(2 * msa)) * (0.019993 - 0.000101 * jc) +
            std::sin(deg_to_rad(3 * msa)) * 0.000289;
    }

    inline double SunPositioner::mean_longitude_sun(const double jc) const
    {
        return std::fmod(280.46646 + jc * (36000.76983 + jc * 0.0003032), 360);
    }

    inline double SunPositioner::sun_true_longitude(const double jc, const double mean_long_sun, const double mean_sun_ano) const
    {
        return mean_long_sun + sun_eqc_of_center(jc, mean_sun_ano);
    }

    inline double SunPositioner::apparent_longitude_of_sun(const double jc, const double mean_long_sun, const double mean_sun_ano) const
    {
        return sun_true_longitude(jc, mean_long_sun, mean_sun_ano) - 0.00569 - 0.00478 * std::sin(deg_to_rad(125.04 - 1934.136 * jc));
    }

    inline double SunPositioner::sun_declination(const double obl_cor, const double app_lon_sun) const
    {
        return std::asin(std::sin(deg_to_rad(obl_cor)) * std::sin(deg_to_rad(app_lon_sun)));
    }

    inline double SunPositioner::equation_of_time(const double jc, const double obl_cor, const double mean_long_sun, const double mean_sun_ano) const
    {
        const double eccent_earth_orbit = 0.016708634 - jc * (0.000042037 + 0.0000001267 * jc);
        const double y = std::tan(deg_to_rad(obl_cor / 2.0)) *
            std::tan(deg_to_rad(obl_cor / 2.0));

        double eq_of_time = y * std::sin(2 * deg_to_rad(mean_long_sun));
        eq_of_time -= 2 * eccent_earth_orbit * std::sin(deg_to_rad(mean_sun_ano));
        eq_of_time += 4 * eccent_earth_orbit * y * std::sin(deg_to_rad(mean_sun_ano)) * std::cos(2 * deg_to_rad(mean_long_sun));
        eq_of_time -= 0.5 * y * y * std::sin(4 * deg_to_rad(mean_long_sun));
        eq_of_time -= 1.25 * eccent_earth_orbit * eccent_earth_orbit * std::sin(2 * deg_to_rad(mean_sun_ano));

       return 4 * rad_to_deg(eq_of_time);
    }

    void SunPositioner::compute_sun_position()
    {
        const double decimal_day = (impl->hour - impl->timezone + (impl->minute + (impl->second) / 60.0) / 60.0) / 24.0;
        const double jc = get_julian_century(get_julian_day(impl->year, impl->month, impl->day, decimal_day));
        const double obl_cor = obliquity_correction(jc);
        const double mean_long_sun = mean_longitude_sun(jc);
        const double mean_sun_ano = mean_sun_anomaly(jc);
        const double app_lon_sun = apparent_longitude_of_sun(jc, mean_long_sun, mean_sun_ano);
        const double sun_dcli = sun_declination(obl_cor, app_lon_sun);
        const double solar_time =
            std::fmod((decimal_day * 1440 + equation_of_time(jc, obl_cor, mean_long_sun, mean_sun_ano) + 4 * impl->longitude), 1440);
        double hour_angle = solar_time / 4;
        hour_angle += (hour_angle < 0) ? 180 : -180;

        // Compute zenith and azimuth.
        const double sin_lat = std::sin(deg_to_rad(impl->latitude));
        const double cos_lat = std::cos(deg_to_rad(impl->latitude));
        const double sin_sun_dcli = std::sin(sun_dcli);
        const double cos_sun_dcli = std::cos(sun_dcli);

        impl->zenith = std::acos(sin_lat * sin_sun_dcli + cos_lat * cos_sun_dcli * std::cos(deg_to_rad(hour_angle)));

        const double sin_zenith = std::sin(impl->zenith);
        const double cos_zenith = std::cos(impl->zenith);

        impl->azimuth = rad_to_deg(std::acos(((sin_lat * cos_zenith) - sin_sun_dcli) / (cos_lat * sin_zenith)));

        if (hour_angle > 0)
            impl->azimuth += 180;
        else
            impl->azimuth = 540 - impl->azimuth;

        impl->zenith = rad_to_deg(impl->zenith);
        impl->azimuth = std::fmod(impl->azimuth, 360);
    }

    void SunPositioner::fetch_data()
    {
        impl->year = m_params.get_optional<int>("year", 2020);
        impl->month = m_params.get_optional<int>("month", 1);
        impl->day = m_params.get_optional<int>("day", 1);
        impl->hour = m_params.get_optional<int>("hour", 12);
        impl->minute = m_params.get_optional<int>("minute", 0);
        impl->second = m_params.get_optional<int>("second", 0);
        impl->timezone = m_params.get_optional<int>("timezone", 0);

        impl->azm_rotation = m_params.get_optional<float>("north", 0);
        impl->longitude = m_params.get_optional<float>("longitude", 0);
        impl->latitude = m_params.get_optional<float>("latitude", 0);
    }

    float SunPositioner::get_zenith() const
    {
        return static_cast<float>(impl->zenith);
    }

    float SunPositioner::get_azimuth() const
    {
        return static_cast<float>(impl->azimuth);
    }

    float SunPositioner::get_solar_noon() const
    {
        return static_cast<float>(impl->solar_noon);
    }

    float SunPositioner::get_sunrise() const
    {
        return static_cast<float>(impl->sunrise);
    }

    float SunPositioner::get_sunset() const
    {
        return static_cast<float>(impl->sunset);
    }

    void SunPositioner::release()
    {
        delete this;
    }

    const char* SunPositionerFactory::get_model()
    {
        return nullptr;
    }

    DictionaryArray SunPositionerFactory::get_input_metadata()
    {
        DictionaryArray metadata;

        metadata.push_back(
            Dictionary()
                .insert("name", "hour")
                .insert("label", "hour")
                .insert("type", "integer")
                .insert("min",
                    Dictionary()
                        .insert("value", "0")
                        .insert("type", "hard"))
                .insert("max",
                    Dictionary()
                        .insert("value", "24")
                        .insert("type", "hard"))
                .insert("use", "optional")
                .insert("default", "12")
                .insert("help", "..."));

        metadata.push_back(
            Dictionary()
                .insert("name", "minute")
                .insert("label", "minute")
                .insert("type", "integer")
                .insert("min",
                    Dictionary()
                        .insert("value", "0")
                        .insert("type", "hard"))
                .insert("max",
                    Dictionary()
                        .insert("value", "59")
                        .insert("type", "hard"))
                .insert("use", "optional")
                .insert("default", "0")
                .insert("help", "..."));

        metadata.push_back(
            Dictionary()
                .insert("name", "second")
                .insert("label", "second")
                .insert("type", "integer")
                .insert("min",
                    Dictionary()
                        .insert("value", "0")
                        .insert("type", "hard"))
                .insert("max",
                    Dictionary()
                        .insert("value", "59")
                        .insert("type", "hard"))
                .insert("use", "optional")
                .insert("default", "0")
                .insert("help", "..."));

        metadata.push_back(
            Dictionary()
                .insert("name", "year")
                .insert("label", "year")
                .insert("type", "integer")
                .insert("min",
                    Dictionary()
                        .insert("value", "-2000")
                        .insert("type", "soft"))
                .insert("max",
                    Dictionary()
                        .insert("value", "3000")
                        .insert("type", "hard"))
                .insert("use", "optional")
                .insert("default", "2020")
                .insert("help", "..."));

        metadata.push_back(
            Dictionary()
                .insert("name", "month")
                .insert("label", "month ")
                .insert("type", "integer")
                .insert("min",
                    Dictionary()
                        .insert("value", "1")
                        .insert("type", "hard"))
                .insert("max",
                    Dictionary()
                        .insert("value", "12")
                        .insert("type", "hard"))
                .insert("use", "optional")
                .insert("default", "1")
                .insert("help", "..."));

        metadata.push_back(
            Dictionary()
                .insert("name", "day")
                .insert("label", "day")
                .insert("type", "integer")
                .insert("min",
                    Dictionary()
                        .insert("value", "1")
                        .insert("type", "hard"))
                .insert("max",
                    Dictionary()
                        .insert("value", "31")
                        .insert("type", "hard"))
                .insert("use", "optional")
                .insert("default", "1")
                .insert("help", "..."));

        metadata.push_back(
            Dictionary()
                .insert("name", "timezone")
                .insert("label", "timezone")
                .insert("type", "integer")
                .insert("min",
                    Dictionary()
                        .insert("value", "-14")
                        .insert("type", "hard"))
                .insert("max",
                    Dictionary()
                        .insert("value", "13")
                        .insert("type", "hard"))
                .insert("use", "optional")
                .insert("default", "0")
                .insert("help", "UTC time zone"));

        metadata.push_back(
            Dictionary()
                .insert("name", "north")
                .insert("label", "north")
                .insert("type", "numeric")
                .insert("min",
                    Dictionary()
                        .insert("value", "-180")
                        .insert("type", "hard"))
                .insert("max",
                    Dictionary()
                        .insert("value", "180")
                        .insert("type", "hard"))
                .insert("use", "optional")
                .insert("default", "0")
                .insert("help", "..."));

        metadata.push_back(
            Dictionary()
            .insert("name", "longitude")
            .insert("label", "longitude")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-180.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "180.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "0.0")
            .insert("help", "..."));

        metadata.push_back(
            Dictionary()
                .insert("name", "latitude")
                .insert("label", "latitude")
                .insert("type", "numeric")
                .insert("min",
                    Dictionary()
                        .insert("value", "-90.0")
                        .insert("type", "hard"))
                .insert("max",
                    Dictionary()
                        .insert("value", "90.0")
                        .insert("type", "hard"))
                .insert("use", "optional")
                .insert("default", "0")
                .insert("help", "..."));

        return metadata;
    }

    auto_release_ptr<SunPositioner> SunPositionerFactory::create(const char* name, const ParamArray& param)
    {
        return foundation::auto_release_ptr<SunPositioner>(new SunPositioner(name, param));
    }

}   // namespace renderer
