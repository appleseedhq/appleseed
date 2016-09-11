
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/math/knn.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/rng/xorshift.h"
#include "foundation/math/vector.h"
#include "foundation/platform/timers.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/benchmark.h"
#include "foundation/utility/bufferedfile.h"
#include "foundation/utility/log.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <memory>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

BENCHMARK_SUITE(Foundation_Math_Knn_Answer)
{
    template <size_t EntryCount>
    struct Fixture
    {
        Xorshift            m_rng;
        knn::Answer<float>  m_answer;

        Fixture()
          : m_answer(EntryCount)
        {
            for (size_t i = 0; i < EntryCount; ++i)
            {
                const float distance = rand_float1(m_rng);
                m_answer.array_insert(0, distance);
            }

            m_answer.make_heap();
        }

        void insert_into_heap()
        {
            for (size_t i = 0; i < EntryCount; ++i)
            {
                const float distance = rand_float1(m_rng);
                m_answer.heap_insert(0, distance);
            }
        }
    };

    BENCHMARK_CASE_F(InsertIntoHeap_K5, Fixture<5>)         { insert_into_heap(); }
    BENCHMARK_CASE_F(InsertIntoHeap_K20, Fixture<20>)       { insert_into_heap(); }
    BENCHMARK_CASE_F(InsertIntoHeap_K100, Fixture<100>)     { insert_into_heap(); }
    BENCHMARK_CASE_F(InsertIntoHeap_K500, Fixture<500>)     { insert_into_heap(); }

    BENCHMARK_CASE_F(Sort_K5, Fixture<5>)                   { m_answer.sort(); }
    BENCHMARK_CASE_F(Sort_K20, Fixture<20>)                 { m_answer.sort(); }
    BENCHMARK_CASE_F(Sort_K100, Fixture<100>)               { m_answer.sort(); }
    BENCHMARK_CASE_F(Sort_K500, Fixture<500>)               { m_answer.sort(); }
}

BENCHMARK_SUITE(Foundation_Math_Knn_Query)
{
    namespace
    {
        bool load_points_from_disk(const char* filename, vector<Vector3f>& points)
        {
            assert(filename);
            assert(points.empty());

            BufferedFile file(filename, BufferedFile::BinaryType, BufferedFile::ReadMode);

            if (!file.is_open())
                return false;

            uint32 point_count;
            if (file.read(point_count) != sizeof(uint32))
                return false;

            if (point_count > 0)
            {
                points.resize(point_count);

                const size_t bytes = point_count * sizeof(Vector3f);

                if (file.read(&points[0], bytes) != bytes)
                    return false;
            }

            return true;
        }
    }

    const size_t QueryCount = 10;

    template <size_t AnswerSize>
    class FixtureBase
    {
      protected:
        vector<Vector3f>    m_points;
        vector<Vector3f>    m_query_points;

        FixtureBase(const string& name)
          : m_answer(AnswerSize)
          , m_accumulator(0)
        {
            configure_logger(name);
        }

#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS

        ~FixtureBase()
        {
            LOG_DEBUG(
                m_logger, "%s",
                StatisticsVector::make(
                    "query statistics",
                    m_query_stats.get_statistics()).to_string().c_str());
        }

#endif

        void prepare()
        {
            if (!m_points.empty())
            {
                build_tree();
                find_query_points();
            }
        }

        void run_queries()
        {
            const size_t query_point_count = m_query_points.size();

            knn::Query3f query(m_tree, m_answer);

            for (size_t i = 0; i < query_point_count; ++i)
            {
                query.run(
                    m_query_points[i]
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
                    , m_query_stats
#endif
                    );

                m_accumulator += m_answer.size();
            }
        }

      private:
        Logger                              m_logger;
        auto_release_ptr<FileLogTarget>     m_log_target;

        knn::Tree3f                         m_tree;
        knn::Answer<float>                  m_answer;
        size_t                              m_accumulator;

#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
        knn::QueryStatistics                m_query_stats;
#endif

        void configure_logger(const string& name)
        {
            m_log_target.reset(create_file_log_target());
            m_log_target->open(("unit benchmarks/outputs/test_knn_" + name + "_stats.txt").c_str());
            m_logger.add_target(m_log_target.get());
        }

        void build_tree()
        {
            knn::Builder3f builder(m_tree);
            builder.build<DefaultWallclockTimer>(&m_points[0], m_points.size());

            LOG_DEBUG(
                m_logger, "%s",
                StatisticsVector::make(
                    "tree statistics",
                    knn::TreeStatistics<knn::Tree3f>(m_tree)).to_string().c_str());
        }

        void find_query_points()
        {
            knn::Answer<float> answer(4);
            knn::Query3f query(m_tree, answer);

            const int32 point_count = static_cast<int32>(m_points.size());
            MersenneTwister rng;

            m_query_points.reserve(QueryCount);

            for (size_t i = 0; i < QueryCount; ++i)
            {
                // Choose a random point from the original point cloud.
                const size_t seed_point_index = rand_int1(rng, 0, point_count - 1);
                const Vector3f& seed_point = m_points[seed_point_index];

                // Find its neighboring points.
                query.run(seed_point);

                // Let the barycenter of these points be a query point.
                Vector3f query_point(0.0f);
                for (size_t j = 0; j < answer.size(); ++j)
                    query_point += m_points[m_tree.remap(answer.get(j).m_index)];
                query_point /= static_cast<float>(answer.size());
                m_query_points.push_back(query_point);
            }
        }
    };

    template <size_t AnswerSize>
    struct ParticlesFixture
      : public FixtureBase<AnswerSize>
    {
        typedef FixtureBase<AnswerSize> FixtureBaseType;

        ParticlesFixture()
          : FixtureBaseType("particles_k" + to_string(AnswerSize))
        {
            load_points_from_disk("unit benchmarks/inputs/test_knn_particles.bin", FixtureBaseType::m_points);
            FixtureBaseType::prepare();
        }
    };

    template <size_t AnswerSize>
    struct PhotonMapFixture
      : public FixtureBase<AnswerSize>
    {
        typedef FixtureBase<AnswerSize> FixtureBaseType;

        PhotonMapFixture()
          : FixtureBaseType("photons_k" + to_string(AnswerSize))
        {
            load_points_from_disk("unit benchmarks/inputs/test_knn_photons.bin", FixtureBaseType::m_points);
            FixtureBaseType::prepare();
        }
    };

    BENCHMARK_CASE_F(Particles_K1, ParticlesFixture<1>)      { run_queries(); }
    BENCHMARK_CASE_F(Particles_K5, ParticlesFixture<5>)      { run_queries(); }
    BENCHMARK_CASE_F(Particles_K20, ParticlesFixture<20>)    { run_queries(); }
    BENCHMARK_CASE_F(Particles_K100, ParticlesFixture<100>)  { run_queries(); }
    BENCHMARK_CASE_F(Particles_K500, ParticlesFixture<500>)  { run_queries(); }

    BENCHMARK_CASE_F(PhotonMap_K1, PhotonMapFixture<1>)      { run_queries(); }
    BENCHMARK_CASE_F(PhotonMap_K5, PhotonMapFixture<5>)      { run_queries(); }
    BENCHMARK_CASE_F(PhotonMap_K20, PhotonMapFixture<20>)    { run_queries(); }
    BENCHMARK_CASE_F(PhotonMap_K100, PhotonMapFixture<100>)  { run_queries(); }
    BENCHMARK_CASE_F(PhotonMap_K500, PhotonMapFixture<500>)  { run_queries(); }
}
