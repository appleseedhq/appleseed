
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
#include "backendapi.h"

// appleseed.bench headers.
#include "utility/encryptionkey.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// appleseed.qtcommon headers.
#include "utility/encryption.h"

// Qt headers.
#include <QByteArray>
#include <QFile>
#include <QIODevice>
#include <QJsonObject>
#include <QJsonValue>
#include <QNetworkRequest>
#include <QTextStream>
#include <QUrl>
#include <QVariant>

// Standard headers.
#include <cassert>
#include <cstdint>
#include <stdexcept>

using namespace foundation;

namespace appleseed {
namespace bench {

//
// Setup:
//
//   1. Retrieve from the backend API's configuration page:
//        - the API URL (append %1?apiKey=%2 to it)
//        - the API key (in the form of a Base64 string)
//
//   2. Generate a new encryption key:
//        a. Go to https://www.random.org/cgi-bin/randbyte?nbytes=16&format=h
//        b. Copy the hexadecimal key
//        c. Go to https://base64.guru/converter/encode/hex
//        d. Convert the hexadecimal key to a Base64 one and copy it
//
//   3. Encode the API URL in Base64:
//        a. Go to https://base64.guru/converter/encode/text
//        b. Convert the API URL to Base64 and copy it
//
//   4. Run CMake a first time:
//        cmake
//            -DAPPLESEED_BENCH_BACKEND_API_URL=<unencrypted API URL in Base64>
//            -DAPPLESEED_BENCH_BACKEND_API_KEY=<unencrypted API key in Base64>
//            -DAPPLESEED_BENCH_ENCRYPTION_KEY=<encryption key in Base64>
//            ..
//
//   5. Encrypt the API URL and API key:
//        a. In appleseed.bench's main.cpp, uncomment the lines
//             const QString encrypted_api_url = get_encrypted_api_url();
//             const QString encrypted_api_key = get_encrypted_api_key();
//        b. Set a breakpoint on the next line
//        c. Build appleseed.bench in Debug mode and run it (make sure you have Qt Visual Studio Tools installed)
//        d. Capture and copy the values of the `encrypted_api_url` and `encrypted_api_key` variables
//        e. Comment again the two lines uncommented on step a and remove the breakpoint
//
//   6. Run CMake a second time to replace the API URL and API key by their encrypted versions:
//        cmake
//            -DAPPLESEED_BENCH_BACKEND_API_URL=<encrypted API URL in Base64>
//            -DAPPLESEED_BENCH_BACKEND_API_KEY=<encrypted API key in Base64>
//            ..
//
//   7. Convert the encryption key to C++ code:
//        a. In appleseed.bench's main.cpp, uncomment the line
//             generate_encryption_key_cpp_file();
//        b. Build appleseed.bench and run it
//        c. Make sure the file utility/encryptionkey.cpp has been generated or overwritten
//        d. Comment again the line uncommented on step a
//        e. Build appleseed.bench and run it
//        f. Check that you can view rankings
//

#ifndef APPLESEED_SHIP
#ifdef APPLESEED_BENCH_ENCRYPTION_KEY

void generate_encryption_key_cpp_file()
{
    // Assume that the working directory is sandbox/.
    QFile file("../src/appleseed.bench/utility/encryptionkey.cpp");

    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        throw std::runtime_error("Failed to open encryption_key.h for writing");

    const QByteArray encryption_key = QByteArray::fromBase64(APPLESEED_TO_STRING_EVAL(APPLESEED_BENCH_ENCRYPTION_KEY));

    QTextStream stream(&file);

    stream << "// This file is auto-generated. Do not modify it.\n";
    stream << "\n";
    stream << "// Interface header.\n";
    stream << "#include \"encryptionkey.h\"\n";
    stream << "\n";
    stream << "// Qt headers.\n";
    stream << "#include <QByteArray>\n";
    stream << "\n";
    stream << "namespace appleseed {\n";
    stream << "namespace bench {\n";
    stream << "\n";
    stream << "QByteArray get_encryption_key()\n";
    stream << "{\n";
    stream << "    QByteArray key;\n";
    stream << QString("    key.resize(%1);\n").arg(encryption_key.size());

    for (int i = 0, e = encryption_key.size(); i < e; ++i)
        stream << QString("    key[%1] = %2;\n").arg(i).arg(static_cast<int>(encryption_key[i]));

    stream << "    return key;\n";
    stream << "}\n";
    stream << "\n";
    stream << "}   // namespace bench\n";
    stream << "}   // namespace appleseed\n";
}

QByteArray encrypt(const QByteArray& input)
{
    const QByteArray encryption_key = QByteArray::fromBase64(APPLESEED_TO_STRING_EVAL(APPLESEED_BENCH_ENCRYPTION_KEY));

    // Abort (don't just assert) if the key is too short.
    if (encryption_key.size() < 16)
        throw std::runtime_error("Encryption key must be 16-byte long (24-character long in Base64)");

    return qtcommon::encrypt(input, encryption_key);
}

QString get_encrypted_api_url()
{
    const QByteArray api_url = QByteArray::fromBase64(APPLESEED_TO_STRING_EVAL(APPLESEED_BENCH_BACKEND_API_URL));
    return encrypt(api_url).toBase64();
}

QString get_encrypted_api_key()
{
    const QByteArray api_key = QByteArray::fromBase64(APPLESEED_TO_STRING_EVAL(APPLESEED_BENCH_BACKEND_API_KEY));
    return encrypt(api_key).toBase64();
}

#endif
#endif

QByteArray decrypt(const QByteArray& input)
{
    const QByteArray encryption_key = get_encryption_key();
    assert(encryption_key.size() == 16);

    return qtcommon::decrypt(input, encryption_key);
}

QString get_decrypted_api_url()
{
    const QByteArray encrypted_api_url = QByteArray::fromBase64(APPLESEED_TO_STRING_EVAL(APPLESEED_BENCH_BACKEND_API_URL));
    return decrypt(encrypted_api_url);
}

QString get_decrypted_api_key()
{
    const QByteArray encrypted_api_key = QByteArray::fromBase64(APPLESEED_TO_STRING_EVAL(APPLESEED_BENCH_BACKEND_API_KEY));
    return decrypt(encrypted_api_key).toBase64();
}

QNetworkRequest make_api_request(const QString& route)
{
    assert(!route.isEmpty() && route[0] == '/');

    const QUrl url(
        get_decrypted_api_url()
            .arg(route)
            .arg(QString(QUrl::toPercentEncoding(get_decrypted_api_key()))));

    return QNetworkRequest(url);
}

QString filter_api_key(const QString& s)
{
    QString result = s;
    result.replace(get_decrypted_api_key(), QString(3, 'X'));
    return result;
}

namespace
{
    QString get_string(const QJsonObject& json_obj, const QString& key)
    {
        const QJsonValue json_value = json_obj[key];
        if (!json_value.isString())
            throw ExceptionJsonValue();
        return json_value.toString();
    }

    int get_int(const QJsonObject& json_obj, const QString& key)
    {
        const QJsonValue json_value = json_obj[key];
        bool ok = false;
        const int value = json_value.toVariant().toInt(&ok);
        if (!ok)
            throw ExceptionJsonValue();
        return value;
    }
}

BenchmarkResult BenchmarkResult::from_json_object(const QJsonObject& json_obj)
{
    BenchmarkResult result;
    result.m_id = get_string(json_obj, "_id");
    result.m_submission_datetime_utc = QDateTime::fromString(get_string(json_obj, "submissionDateTimeUtc"));
    result.m_benchmark_version = get_int(json_obj, "benchmarkVersion");
    result.m_benchmark_scene_id = get_string(json_obj, "benchmarkSceneId");
    result.m_cpu_model = get_string(json_obj, "cpuModel");
    result.m_enabled_cpu_core_count = get_int(json_obj, "cpuCoreCount");
    result.m_cpu_thread_count = get_int(json_obj, "cpuThreadCount");
    result.m_render_thread_count = get_int(json_obj, "renderThreadCount");
    result.m_render_time = get_int(json_obj, "renderTime");
    return result;
}

QJsonObject BenchmarkResult::to_json_object() const
{
    QJsonObject json_obj;
    // _id is ignored, it will be set automatically by MongoDB.
    json_obj["submissionDateTimeUtc"] = m_submission_datetime_utc.toString(Qt::ISODate);
    json_obj["benchmarkVersion"] = m_benchmark_version;
    json_obj["benchmarkSceneId"] = m_benchmark_scene_id;
    json_obj["cpuModel"] = m_cpu_model;
    json_obj["cpuCoreCount"] = m_enabled_cpu_core_count;
    json_obj["cpuThreadCount"] = m_cpu_thread_count;
    json_obj["renderThreadCount"] = m_render_thread_count;
    json_obj["renderTime"] = m_render_time;
    return json_obj;
}

}   // namespace bench
}   // namespace appleseed
