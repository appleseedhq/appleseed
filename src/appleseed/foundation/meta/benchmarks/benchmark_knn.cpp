
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

// appleseed.foundation headers.
#include "foundation/log/log.h"
#include "foundation/math/aabb.h"
#include "foundation/math/knn.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/rng/xorshift32.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/timers.h"
#include "foundation/string/string.h"
#include "foundation/utility/benchmark.h"
#include "foundation/utility/bufferedfile.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

using namespace foundation;

BENCHMARK_SUITE(Foundation_Math_Knn_Answer)
{
    template <std::size_t EntryCount>
    struct Fixture
    {
        Xorshift32          m_rng;
        knn::Answer<float>  m_answer;

        Fixture()
          : m_answer(EntryCount)
        {
            for (std::size_t i = 0; i < EntryCount; ++i)
            {
                const float distance = rand_float1(m_rng);
                m_answer.array_insert(0, distance);
            }

            m_answer.make_heap();
        }

        void insert_into_heap()
        {
            for (std::size_t i = 0; i < EntryCount; ++i)
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

namespace
{
    class FixtureBase
    {
      protected:
        std::vector<Vector3f>   m_points;
        AABB3f                  m_bbox;
        knn::Tree3f             m_tree;
        std::vector<Vector3f>   m_query_points;

        FixtureBase(const std::string& benchmark_name, const std::string& dataset_filepath)
        {
            configure_logger(benchmark_name);
            load_points_from_disk(dataset_filepath);
            compute_bbox();
            build_tree();
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

        void establish_query_points_in_cloud(const std::size_t query_point_count)
        {
            if (!m_points.empty())
            {
                assert(m_query_points.empty());

                knn::Answer<float> answer(4);
                const knn::Query3f query(m_tree, answer);

                const auto point_count = static_cast<std::int32_t>(m_points.size());
                MersenneTwister rng;

                m_query_points.reserve(query_point_count);

                for (std::size_t i = 0; i < query_point_count; ++i)
                {
                    // Choose a random point from the original point cloud.
                    const std::size_t seed_point_index = rand_int1(rng, 0, point_count - 1);
                    const Vector3f& seed_point = m_points[seed_point_index];

                    // Find its neighboring points.
                    query.run(seed_point);

                    // Let the barycenter of these points be a query point.
                    Vector3f query_point(0.0f);
                    for (std::size_t j = 0; j < answer.size(); ++j)
                        query_point += m_points[m_tree.remap(answer.get(j).m_index)];
                    query_point /= static_cast<float>(answer.size());
                    m_query_points.push_back(query_point);
                }
            }
        }

        void establish_random_query_points(const std::size_t query_point_count)
        {
            if (!m_points.empty())
            {
                assert(m_query_points.empty());

                AABB3f bbox = m_bbox;
                bbox.grow(0.5f * bbox.extent());

                MersenneTwister rng;

                m_query_points.reserve(query_point_count);

                for (std::size_t i = 0; i < query_point_count; ++i)
                    m_query_points.push_back(lerp(bbox.min, bbox.max, rand_vector1<Vector3f>(rng)));
            }
        }

      private:
        Logger                              m_logger;
        auto_release_ptr<FileLogTarget>     m_log_target;

#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
        knn::QueryStatistics                m_query_stats;
#endif

        void configure_logger(const std::string& benchmark_name)
        {
            m_log_target.reset(create_file_log_target());
            m_log_target->open(("unit benchmarks/outputs/test_knn_" + benchmark_name + "_stats.txt").c_str());
            m_logger.add_target(m_log_target.get());
        }

        void load_points_from_disk(const std::string& dataset_filepath)
        {
            assert(m_points.empty());

            BufferedFile file(dataset_filepath.c_str(), BufferedFile::BinaryType, BufferedFile::ReadMode);

            if (!file.is_open())
                return;

            std::uint32_t point_count;
            if (file.read(point_count) != sizeof(std::uint32_t))
                return;

            if (point_count > 0)
            {
                m_points.resize(point_count);

                const std::size_t bytes = point_count * sizeof(Vector3f);

                if (file.read(&m_points[0], bytes) != bytes)
                {
                    m_points.clear();
                    return;
                }
            }
        }

        void compute_bbox()
        {
            m_bbox.invalidate();

            for (const Vector3f& point : m_points)
                m_bbox.insert(point);
        }

        void build_tree()
        {
            if (!m_points.empty())
            {
                knn::Builder3f builder(m_tree);
                builder.build<DefaultWallclockTimer>(&m_points[0], m_points.size());

                LOG_DEBUG(
                    m_logger, "%s",
                    StatisticsVector::make(
                        "tree statistics",
                        knn::TreeStatistics<knn::Tree3f>(m_tree)).to_string().c_str());
            }
        }
    };
}

BENCHMARK_SUITE(Foundation_Math_Knn_Query)
{
    template <std::size_t AnswerSize>
    class Fixture
      : public FixtureBase
    {
      public:
        explicit Fixture(const std::string& benchmark_name, const std::string& dataset_filepath)
          : FixtureBase(benchmark_name, dataset_filepath)
          , m_answer(AnswerSize)
        {
        }

        void run_queries()
        {
            const knn::Query3f query(m_tree, m_answer);

            for (const Vector3f& query_point : m_query_points)
            {
                query.run(
                    query_point
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
                    , m_query_stats
#endif
                    );

                m_accumulator += m_answer.size();
            }
        }

      private:
        knn::Answer<float>      m_answer;
        std::size_t             m_accumulator = 0;
    };

    const std::size_t QueryPointCount = 100;

    template <std::size_t AnswerSize>
    struct ParticlesQueryPointsInCloudFixture
      : public Fixture<AnswerSize>
    {
        ParticlesQueryPointsInCloudFixture()
          : Fixture<AnswerSize>("particles_k" + to_string(AnswerSize), "unit benchmarks/inputs/test_knn_particles.bin")
        {
            Fixture<AnswerSize>::establish_query_points_in_cloud(QueryPointCount);
        }
    };

    template <std::size_t AnswerSize>
    struct PhotonMapQueryPointsInCloudFixture
      : public Fixture<AnswerSize>
    {
        PhotonMapQueryPointsInCloudFixture()
          : Fixture<AnswerSize>("photons_k" + to_string(AnswerSize), "unit benchmarks/inputs/test_knn_photons.bin")
        {
            Fixture<AnswerSize>::establish_query_points_in_cloud(QueryPointCount);
        }
    };

    template <std::size_t AnswerSize>
    struct ParticlesRandomQueryPointsFixture
      : public Fixture<AnswerSize>
    {
        ParticlesRandomQueryPointsFixture()
          : Fixture<AnswerSize>("particles_k" + to_string(AnswerSize), "unit benchmarks/inputs/test_knn_particles.bin")
        {
            Fixture<AnswerSize>::establish_random_query_points(QueryPointCount);
        }
    };

    template <std::size_t AnswerSize>
    struct PhotonMapRandomQueryPointsFixture
      : public Fixture<AnswerSize>
    {
        PhotonMapRandomQueryPointsFixture()
          : Fixture<AnswerSize>("photons_k" + to_string(AnswerSize), "unit benchmarks/inputs/test_knn_photons.bin")
        {
            Fixture<AnswerSize>::establish_random_query_points(QueryPointCount);
        }
    };

    BENCHMARK_CASE_F(Particles_QueryPointsInCloud_K1, ParticlesQueryPointsInCloudFixture<1>)        { run_queries(); }
    BENCHMARK_CASE_F(Particles_QueryPointsInCloud_K5, ParticlesQueryPointsInCloudFixture<5>)        { run_queries(); }
    BENCHMARK_CASE_F(Particles_QueryPointsInCloud_K20, ParticlesQueryPointsInCloudFixture<20>)      { run_queries(); }
    BENCHMARK_CASE_F(Particles_QueryPointsInCloud_K100, ParticlesQueryPointsInCloudFixture<100>)    { run_queries(); }
    BENCHMARK_CASE_F(Particles_QueryPointsInCloud_K500, ParticlesQueryPointsInCloudFixture<500>)    { run_queries(); }

    BENCHMARK_CASE_F(PhotonMap_QueryPointsInCloud_K1, PhotonMapQueryPointsInCloudFixture<1>)        { run_queries(); }
    BENCHMARK_CASE_F(PhotonMap_QueryPointsInCloud_K5, PhotonMapQueryPointsInCloudFixture<5>)        { run_queries(); }
    BENCHMARK_CASE_F(PhotonMap_QueryPointsInCloud_K20, PhotonMapQueryPointsInCloudFixture<20>)      { run_queries(); }
    BENCHMARK_CASE_F(PhotonMap_QueryPointsInCloud_K100, PhotonMapQueryPointsInCloudFixture<100>)    { run_queries(); }
    BENCHMARK_CASE_F(PhotonMap_QueryPointsInCloud_K500, PhotonMapQueryPointsInCloudFixture<500>)    { run_queries(); }

    BENCHMARK_CASE_F(Particles_RandomQueryPoints_K1, ParticlesRandomQueryPointsFixture<1>)          { run_queries(); }
    BENCHMARK_CASE_F(Particles_RandomQueryPoints_K5, ParticlesRandomQueryPointsFixture<5>)          { run_queries(); }
    BENCHMARK_CASE_F(Particles_RandomQueryPoints_K20, ParticlesRandomQueryPointsFixture<20>)        { run_queries(); }
    BENCHMARK_CASE_F(Particles_RandomQueryPoints_K100, ParticlesRandomQueryPointsFixture<100>)      { run_queries(); }
    BENCHMARK_CASE_F(Particles_RandomQueryPoints_K500, ParticlesRandomQueryPointsFixture<500>)      { run_queries(); }

    BENCHMARK_CASE_F(PhotonMap_RandomQueryPoints_K1, PhotonMapRandomQueryPointsFixture<1>)          { run_queries(); }
    BENCHMARK_CASE_F(PhotonMap_RandomQueryPoints_K5, PhotonMapRandomQueryPointsFixture<5>)          { run_queries(); }
    BENCHMARK_CASE_F(PhotonMap_RandomQueryPoints_K20, PhotonMapRandomQueryPointsFixture<20>)        { run_queries(); }
    BENCHMARK_CASE_F(PhotonMap_RandomQueryPoints_K100, PhotonMapRandomQueryPointsFixture<100>)      { run_queries(); }
    BENCHMARK_CASE_F(PhotonMap_RandomQueryPoints_K500, PhotonMapRandomQueryPointsFixture<500>)      { run_queries(); }
}

BENCHMARK_SUITE(Foundation_Math_Knn_AnyQuery)
{
    class Fixture
      : public FixtureBase
    {
      public:
        explicit Fixture(const std::string& benchmark_name, const std::string& dataset_filepath)
          : FixtureBase(benchmark_name, dataset_filepath)
        {
        }

        void run_queries()
        {
            const knn::AnyQuery3f query(m_tree);
            const float query_max_square_distance = m_bbox.square_diameter() * square(0.2f);

            for (const Vector3f& query_point : m_query_points)
            {
                if (query.run(
                        query_point,
                        query_max_square_distance
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
                        , m_query_stats
#endif
                        ))
                    ++m_accumulator;
            }
        }

      private:
        std::size_t m_accumulator = 0;
    };

    const std::size_t QueryPointCount = 100;

    struct ParticlesFixture
      : public Fixture
    {
        ParticlesFixture()
          : Fixture("particles_any", "unit benchmarks/inputs/test_knn_particles.bin")
        {
            establish_random_query_points(QueryPointCount);
        }
    };

    struct PhotonMapFixture
      : public Fixture
    {
        PhotonMapFixture()
          : Fixture("photons_any", "unit benchmarks/inputs/test_knn_photons.bin")
        {
            establish_random_query_points(QueryPointCount);
        }
    };

    BENCHMARK_CASE_F(Particles_RandomQueryPoints, ParticlesFixture) { run_queries(); }
    BENCHMARK_CASE_F(PhotonMap_RandomQueryPoints, PhotonMapFixture) { run_queries(); }
}
