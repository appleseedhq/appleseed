
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Francois Beaune, The appleseedhq Organization
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

// appleseed.qtcommon headers.
#include "utility/encryption.h"
#include "utility/interop.h"

// appleseed.foundation headers.
#include "foundation/utility/test.h"

// Qt headers.
#include <QByteArray>
#include <QString>

using namespace appleseed::qtcommon;

void force_linking_qtcommon_utility_encryption_tests() {}

TEST_SUITE(QtCommon_Utility_Encryption)
{
    const QByteArray Key = QByteArrayLiteral("\xff\x99\xfa\x8e\xc1\x6d\x82\xfc\xc0\x62\x8d\xe9\x2e\xe7\x01\x32");

    TEST_CASE(Encrypt_Decrypt_Roundtrip_EmptyString)
    {
        const QByteArray encrypted = encrypt(QString("").toUtf8(), Key);
        const QString decrypted = QString(decrypt(encrypted, Key));

        EXPECT_EQ("", decrypted);
    }

    TEST_CASE(Encrypt_Decrypt_Roundtrip_NonEmptyString)
    {
        const QByteArray encrypted = encrypt(QString("Hello World").toUtf8(), Key);
        const QString decrypted = QString(decrypt(encrypted, Key));

        EXPECT_EQ("Hello World", decrypted);
    }

    TEST_CASE(Encrypt_Decrypt_Roundtrip_EightCharacterLongString)
    {
        const QByteArray encrypted = encrypt(QString("abcdefgh").toUtf8(), Key);
        const QString decrypted = QString(decrypt(encrypted, Key));

        EXPECT_EQ("abcdefgh", decrypted);
    }
}
