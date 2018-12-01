
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

#pragma once

// appleseed.foundation headers.
#include "foundation/utility/benchmark/benchmarksuite.h"
#include "foundation/utility/benchmark/benchmarksuiterepository.h"
#include "foundation/utility/benchmark/ibenchmarkcase.h"
#include "foundation/utility/benchmark/ibenchmarkcasefactory.h"

namespace foundation
{

//
// Define a benchmark suite.
//

#define BENCHMARK_SUITE(Name)                                                               \
    namespace BenchmarkSuite##Name                                                          \
    {                                                                                       \
        struct BenchmarkSuite##Name                                                         \
          : public foundation::BenchmarkSuite                                               \
        {                                                                                   \
            BenchmarkSuite##Name()                                                          \
              : foundation::BenchmarkSuite(#Name)                                           \
            {                                                                               \
            }                                                                               \
        };                                                                                  \
                                                                                            \
        foundation::BenchmarkSuite& current_benchmark_suite__()                             \
        {                                                                                   \
            static BenchmarkSuite##Name bs;                                                 \
            return bs;                                                                      \
        }                                                                                   \
                                                                                            \
        struct RegisterBenchmarkSuite##Name                                                 \
        {                                                                                   \
            RegisterBenchmarkSuite##Name()                                                  \
            {                                                                               \
                using namespace foundation;                                                 \
                BenchmarkSuite& suite = current_benchmark_suite__();                        \
                BenchmarkSuiteRepository::instance().register_suite(&suite);                \
            }                                                                               \
        };                                                                                  \
                                                                                            \
        static RegisterBenchmarkSuite##Name RegisterBenchmarkSuite##Name##_instance__;      \
    }                                                                                       \
                                                                                            \
    namespace BenchmarkSuite##Name


//
// Define a benchmark case without fixture.
//

#define BENCHMARK_CASE(Name)                                                                \
    struct BenchmarkCase##Name                                                              \
      : public foundation::IBenchmarkCase                                                   \
    {                                                                                       \
        virtual const char* get_name() const                                                \
        {                                                                                   \
            return #Name;                                                                   \
        }                                                                                   \
                                                                                            \
        virtual void run();                                                                 \
    };                                                                                      \
                                                                                            \
    struct BenchmarkCase##Name##Factory                                                     \
      : public foundation::IBenchmarkCaseFactory                                            \
    {                                                                                       \
        virtual const char* get_name() const                                                \
        {                                                                                   \
            return #Name;                                                                   \
        }                                                                                   \
                                                                                            \
        virtual foundation::IBenchmarkCase* create()                                        \
        {                                                                                   \
            return new BenchmarkCase##Name();                                               \
        }                                                                                   \
    };                                                                                      \
                                                                                            \
    struct RegisterBenchmarkCase##Name                                                      \
    {                                                                                       \
        RegisterBenchmarkCase##Name()                                                       \
        {                                                                                   \
            using namespace foundation;                                                     \
            static BenchmarkCase##Name##Factory factory;                                    \
            current_benchmark_suite__().register_case(&factory);                            \
        }                                                                                   \
    };                                                                                      \
                                                                                            \
    static RegisterBenchmarkCase##Name RegisterBenchmarkCase##Name##_instance__;            \
                                                                                            \
    void BenchmarkCase##Name::run()


//
// Define a benchmark case with fixture.
//

#define BENCHMARK_CASE_F(Name, FixtureName)                                                 \
    struct BenchmarkCase##Name                                                              \
      : public foundation::IBenchmarkCase                                                   \
      , public FixtureName                                                                  \
    {                                                                                       \
        virtual const char* get_name() const                                                \
        {                                                                                   \
            return #Name;                                                                   \
        }                                                                                   \
                                                                                            \
        virtual void run();                                                                 \
    };                                                                                      \
                                                                                            \
    struct BenchmarkCase##Name##Factory                                                     \
      : public foundation::IBenchmarkCaseFactory                                            \
    {                                                                                       \
        virtual const char* get_name() const                                                \
        {                                                                                   \
            return #Name;                                                                   \
        }                                                                                   \
                                                                                            \
        virtual foundation::IBenchmarkCase* create()                                        \
        {                                                                                   \
            return new BenchmarkCase##Name();                                               \
        }                                                                                   \
    };                                                                                      \
                                                                                            \
    struct RegisterBenchmarkCase##Name                                                      \
    {                                                                                       \
        RegisterBenchmarkCase##Name()                                                       \
        {                                                                                   \
            using namespace foundation;                                                     \
            static BenchmarkCase##Name##Factory factory;                                    \
            current_benchmark_suite__().register_case(&factory);                            \
        }                                                                                   \
    };                                                                                      \
                                                                                            \
    static RegisterBenchmarkCase##Name RegisterBenchmarkCase##Name##_instance__;            \
                                                                                            \
    void BenchmarkCase##Name::run()


//
// Forward-declare a benchmark case.
//

#define DECLARE_BENCHMARK_CASE(SuiteName, CaseName)                                         \
    namespace BenchmarkSuite##SuiteName { struct BenchmarkCase##CaseName; }


//
// Declare that a benchmark case has access to the internals of a class.
//

#define GRANT_ACCESS_TO_BENCHMARK_CASE(SuiteName, CaseName)                                 \
    friend struct BenchmarkSuite##SuiteName::BenchmarkCase##CaseName

}   // namespace foundation
