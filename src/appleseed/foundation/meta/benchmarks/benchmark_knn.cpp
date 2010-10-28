
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "foundation/math/rng.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/benchmark.h"
#include "foundation/utility/bufferedfile.h"

// STANN headers.
#pragma warning (push)
#pragma warning (disable : 4800)
#include "sfcnn.hpp"
#pragma warning (pop)

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdio>
#include <memory>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

BENCHMARK_SUITE(Foundation_Math_Knn)
{
    const size_t QueryCount = 10;
    const size_t AnswerSize = 5;

    class FixtureBase
    {
      protected:
        vector<Vector3f>    m_points;
        vector<Vector3f>    m_query_points;
        size_t              m_accumulator;

        FixtureBase()
          : m_accumulator(0)
        {
        }

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
            size_t answer[AnswerSize];
            knn::Query3f query(m_tree, answer, AnswerSize);

            for (size_t i = 0; i < QueryCount; ++i)
                m_accumulator += query.run(m_query_points[i]);
        }

      private:
        knn::Tree3f         m_tree;

        void build_tree()
        {
            knn::Builder3f builder(m_tree, AnswerSize);
            builder.build(&m_points[0], m_points.size());
        }

        void find_query_points()
        {
            const int32 point_count = static_cast<int32>(m_points.size());
            MersenneTwister rng;

            const size_t AnswerSize = 4;
            size_t answer[AnswerSize];
            knn::Query3f query(m_tree, answer, AnswerSize);

            m_query_points.reserve(QueryCount);

            for (size_t i = 0; i < QueryCount; ++i)
            {
                // Choose a random point from the original point cloud.
                const size_t seed_point_index = rand_int1(rng, 0, point_count - 1);
                const Vector3f& seed_point = m_points[seed_point_index];

                // Find its neighboring points.
                const size_t found = query.run(seed_point);
                assert(found == AnswerSize);

                // Let the barycenter of these points be a query point.
                Vector3f query_point(0.0f);
                for (size_t j = 0; j < found; ++j)
                    query_point += m_points[answer[j]];
                query_point /= static_cast<float>(found);
                m_query_points.push_back(query_point);
            }
        }
    };

    class FixtureBaseSTANN
      : public FixtureBase
    {
      protected:
        void prepare()
        {
            FixtureBase::prepare();

            if (!m_points.empty())
                m_stann_tree.reset(new STANNTree(&m_points[0], m_points.size()));
        }

        void run_queries()
        {
            for (size_t i = 0; i < QueryCount; ++i)
                m_stann_tree->ksearch(m_query_points[i], AnswerSize, m_answer);
        }

      private:
        typedef sfcnn<foundation::Vector3f, 3, float> STANNTree;

        auto_ptr<STANNTree>         m_stann_tree;
        vector<unsigned long int>   m_answer;
    };

    template <typename FixtureBaseType>
    struct ParticlesFixture
      : public FixtureBaseType
    {
        ParticlesFixture()
        {
            load_points("data/test_knn_points.txt");
            prepare();
        }

        void load_points(const char* filename)
        {
            FILE* file = fopen(filename, "rt");

            if (file == 0)
                return;

            size_t point_count;
            fscanf(file, FMT_SIZE_T, &point_count);

            m_points.resize(point_count);

            for (size_t i = 0; i < point_count; ++i)
            {
                Vector3f& p = m_points[i];
                fscanf(file, "%f %f %f", &p.x, &p.y, &p.z);
            }

            fclose(file);
        }
    };

    template <typename FixtureBaseType>
    struct PhotonMapFixture
      : public FixtureBaseType
    {
        PhotonMapFixture()
        {
            load_toxic_photon_map_v1("data/test_knn_gally_gpm.bin");
            prepare();
        }

        void load_toxic_photon_map_v1(const char* filename)
        {
            BufferedFile file(filename, BufferedFile::BinaryType, BufferedFile::ReadMode);

            if (!file.is_open())
                return;

            const string FileSignature = "toxic photon map file version 1";
            const size_t FileSignatureLength = FileSignature.size();

            string sig(FileSignatureLength, 0);

            if (file.read(&sig[0], FileSignatureLength) != FileSignatureLength)
                return;

            if (sig != FileSignature)
                return;

            uint32 stored_photon_count;

            if (file.read(stored_photon_count) != sizeof(uint32))
                return;

            swap_bytes(&stored_photon_count);

            m_points.resize(stored_photon_count);

            for (uint32 i = 0; i < stored_photon_count; ++i)
            {
                Vector3f& p = m_points[i];

                if (file.read(p) != sizeof(Vector3f))
                    return;

                swap_bytes(&p.x);
                swap_bytes(&p.y);
                swap_bytes(&p.z);

                file.seek(29, BufferedFile::SeekFromCurrent);
            }
        }

        template <typename T>
        static void swap_bytes(T* ptr)
        {
            assert(ptr);
            assert(sizeof(T) % 2 == 0);

            uint8* bytes = reinterpret_cast<uint8*>(ptr);

            reverse(bytes, bytes + sizeof(T));
        }
    };

    BENCHMARK_CASE_WITH_FIXTURE(QueryParticles, ParticlesFixture<FixtureBase>)
    {
        run_queries();
    }

    BENCHMARK_CASE_WITH_FIXTURE(QueryParticles_STANN, ParticlesFixture<FixtureBaseSTANN>)
    {
        run_queries();
    }

    BENCHMARK_CASE_WITH_FIXTURE(QueryPhotonMap, PhotonMapFixture<FixtureBase>)
    {
        run_queries();
    }

    BENCHMARK_CASE_WITH_FIXTURE(QueryPhotonMap_STANN, PhotonMapFixture<FixtureBaseSTANN>)
    {
        run_queries();
    }
}
