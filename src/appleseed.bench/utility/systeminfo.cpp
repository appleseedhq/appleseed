
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Francois Beaune, The appleseedhq Organization
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
#include "systeminfo.h"

// Qt headers.
#include <QProcessEnvironment>
#include <QRegularExpression>
#include <QRegularExpressionMatch>
#include <QString>
#include <qvariant.h>

namespace appleseed {
namespace bench {

//
// SystemInfo class implementation.
//

// ------------------------------------------------------------------------------------------------
// Windows.
// ------------------------------------------------------------------------------------------------

#if defined _WIN32

struct SystemInfo::Impl
{
    QString m_cpu_id;
    int m_cpu_core_count;
    int m_enabled_cpu_core_count;
    int m_cpu_thread_count;

    QString m_os_id;

    Impl()
    {
        const QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
        const QString wmic_path = compute_wmic_path(env);

        const QString cpu_info = get_wmic_info(wmic_path, "cpu");
        m_cpu_id = extract_cpu_id(cpu_info);
        m_cpu_core_count = extract_cpu_core_count(cpu_info);
        m_enabled_cpu_core_count = extract_enabled_cpu_core_count(cpu_info);
        m_cpu_thread_count = extract_cpu_thread_count(cpu_info);

        // `NumberOfEnabledCore` is not available before Windows 10.
        if (m_enabled_cpu_core_count == 0)
            m_enabled_cpu_core_count = m_cpu_core_count;

        const QString os_info = get_wmic_info(wmic_path, "os");
        m_os_id = extract_os_id(os_info);
    }

    QString get_cpu_id() const
    {
        return m_cpu_id;
    }

    int get_cpu_core_count() const
    {
        return m_cpu_core_count;
    }

    int get_enabled_cpu_core_count() const
    {
        return m_enabled_cpu_core_count;
    }

    int get_cpu_thread_count() const
    {
        return m_cpu_thread_count;
    }

    QString get_os_id() const
    {
        return m_os_id;
    }

    QString compute_wmic_path(const QProcessEnvironment& env) const
    {
        const QString system_root = env.value("SystemRoot", "C:\\Windows");
        return QString("%1\\System32\\wbem\\WMIC.exe").arg(system_root);
    }

    QString get_wmic_info(const QString& wmic_path, const QString& category) const
    {
        QString wmic_output;
        return run(QString("%1 %2 get /format: list").arg(wmic_path).arg(category), wmic_output) ? wmic_output : QString();
    }

    QString extract_cpu_id(const QString& cpu_info) const
    {
        return extract_value<QString>("^Name=([^\r\n]+)", cpu_info, "Unavailable");
    }

    int extract_cpu_core_count(const QString& cpu_info) const
    {
        return extract_value<int>("^NumberOfCores=(\\d+)", cpu_info, 0);
    }

    int extract_enabled_cpu_core_count(const QString& cpu_info) const
    {
        return extract_value<int>("^NumberOfEnabledCore=(\\d+)", cpu_info, 0);
    }

    int extract_cpu_thread_count(const QString& cpu_info) const
    {
        return extract_value<int>("^NumberOfLogicalProcessors=(\\d+)", cpu_info, 0);
    }

    QString extract_os_id(const QString& os_info) const
    {
        const QString caption = extract_os_caption(os_info);
        const QString version = extract_os_version(os_info);
        return QString("%1 (%2)").arg(caption, version);
    }

    QString extract_os_caption(const QString& os_info) const
    {
        return extract_value<QString>("^Caption=([^\r\n]+)", os_info, "Unavailable");
    }

    QString extract_os_version(const QString& os_info) const
    {
        return extract_value<QString>("^Version=([^\r\n]+)", os_info, "Unavailable");
    }

    template <typename T>
    static T extract_value(const QString& pattern, const QString& subject, const T& default_value = T())
    {
        const QRegularExpression regex(pattern, QRegularExpression::MultilineOption);
        const QRegularExpressionMatch match = regex.match(subject);
        return match.hasMatch() ? QVariant(match.captured(1)).value<T>() : default_value;
    }

    static bool run(const QString& cmd, QString& output)
    {
        static constexpr int CmdTimeout = 10000;  // in milliseconds

        QProcess process;
        process.start(cmd);

        if (process.waitForFinished(CmdTimeout) &&
            process.exitStatus() == QProcess::NormalExit &&
            process.exitCode() == 0)
        {
            output = process.readAllStandardOutput();
            return true;
        }
        else return false;
    }
};

// ------------------------------------------------------------------------------------------------
// Unsupported system.
// ------------------------------------------------------------------------------------------------

#else

struct SystemInfo::Impl
{
    QString get_cpu_id() const
    {
        return "Unavailable";
    }

    int get_cpu_core_count() const
    {
        return 0;
    }

    int get_enabled_cpu_core_count() const
    {
        return 0;
    }

    int get_cpu_thread_count() const
    {
        return 0;
    }

    QString get_os_id() const
    {
        return "Unavailable";
    }
};

#endif

// ------------------------------------------------------------------------------------------------
// Common code.
// ------------------------------------------------------------------------------------------------

QString SystemInfo::cleanup_cpu_model_string(const QString& s)
{
    QString result = s;
    result.replace("(TM)", "", Qt::CaseInsensitive);
    result.replace("(R)", "", Qt::CaseInsensitive);
    result.replace(" @ ", " at ");
    return result;
}

SystemInfo::SystemInfo() : impl(new Impl())
{
}

SystemInfo::~SystemInfo() = default;

QString SystemInfo::get_cpu_id() const
{
    return impl->get_cpu_id();
}

int SystemInfo::get_cpu_core_count() const
{
    return impl->get_cpu_core_count();
}

int SystemInfo::get_enabled_cpu_core_count() const
{
    return impl->get_enabled_cpu_core_count();
}

int SystemInfo::get_cpu_thread_count() const
{
    return impl->get_cpu_thread_count();
}

QString SystemInfo::get_os_id() const
{
    return impl->get_os_id();
}

}   // namespace bench
}   // namespace appleseed
