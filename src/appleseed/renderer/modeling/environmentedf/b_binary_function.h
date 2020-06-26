/**
 * Copyright (c) 2016 Eric Bruneton
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef MATH_BINARY_FUNCTION_H_
#define MATH_BINARY_FUNCTION_H_

#include <algorithm>
#include <cassert>
#include <cmath>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#include "b_vector.h"

namespace dimensional {

    // A function from [0:1]x[0:1] to values of type T represented by its values at
    // NXxNY uniformly distributed samples (i + 0.5) / NX, (j + 0.5) / NY, and
    // bilinearly interpolated in between.
    template<unsigned int NX, unsigned int NY, class T>
    class BinaryFunction {
    public:
        BinaryFunction() { value_.reset(new T[NX * NY]); }

        explicit BinaryFunction(const T& constant_value) {
            value_.reset(new T[NX * NY]);
            for (unsigned int i = 0; i < NX * NY; ++i) {
                value_[i] = constant_value;
            }
        }

        BinaryFunction(const BinaryFunction& rhs) = delete;
        BinaryFunction& operator=(const BinaryFunction& rhs) = delete;

        BinaryFunction& operator+=(const BinaryFunction& rhs) {
            for (unsigned int i = 0; i < NX * NY; ++i) {
                value_[i] += rhs.value_[i];
            }
            return *this;
        }

        inline unsigned int size_x() const { return NX; }
        inline unsigned int size_y() const { return NY; }

        virtual inline const T& Get(int i, int j) const {
            assert(i >= 0 && i < static_cast<int>(NX) &&
                j >= 0 && j < static_cast<int>(NY));
            return value_[i + j * NX];
        }

        inline T& Get(int i, int j) {
            assert(i >= 0 && i < static_cast<int>(NX) &&
                j >= 0 && j < static_cast<int>(NY));
            return value_[i + j * NX];
        }

        inline void Set(int i, int j, T value) {
            assert(i >= 0 && i < static_cast<int>(NX) &&
                j >= 0 && j < static_cast<int>(NY));
            value_[i + j * NX] = value;
        }

        void Set(const T& constant_value) {
            for (unsigned int i = 0; i < NX * NY; ++i) {
                value_[i] = constant_value;
            }
        }

        void Set(const BinaryFunction& rhs) {
            for (unsigned int i = 0; i < NX * NY; ++i) {
                value_[i] = rhs.value_[i];
            }
        }

        const T operator()(double x, double y) const {
            double u = x * NX - 0.5;
            double v = y * NY - 0.5;
            int i = std::floor(u);
            int j = std::floor(v);
            u -= i;
            v -= j;
            int i0 = std::max(0, std::min(static_cast<int>(NX) - 1, i));
            int i1 = std::max(0, std::min(static_cast<int>(NX) - 1, i + 1));
            int j0 = std::max(0, std::min(static_cast<int>(NY) - 1, j));
            int j1 = std::max(0, std::min(static_cast<int>(NY) - 1, j + 1));
            return Get(i0, j0) * ((1.0 - u) * (1.0 - v)) +
                Get(i1, j0) * (u * (1.0 - v)) +
                Get(i0, j1) * ((1.0 - u) * v) +
                Get(i1, j1) * (u * v);
        }

        void Load(const std::string& filename) {
            std::ifstream file(filename, std::ifstream::binary | std::ifstream::in);
            file.read(reinterpret_cast<char*>(value_.get()), NX * NY * sizeof(T));
            file.close();
        }

        void Save(const std::string& filename) const {
            std::ofstream file(filename, std::ofstream::binary);
            file.write(
                reinterpret_cast<const char*>(value_.get()), NX * NY * sizeof(T));
            file.close();
        }

    protected:
        std::unique_ptr<T[]> value_;
    };

    template<unsigned int NX, unsigned int NY, typename T>
    T texture(const BinaryFunction<NX, NY, T>& table, const vec2& uv) {
        return table(uv.x(), uv.y());
    }

}  // namespace dimensional

#endif  // MATH_BINARY_FUNCTION_H_
