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

#ifndef MATH_VECTOR_H_
#define MATH_VECTOR_H_

#include "b_scalar.h"

namespace dimensional {

    template<typename T>
    struct Vector2 {
        T x, y;
        inline constexpr Vector2() {}
        inline constexpr Vector2(T x, T y) : x(x), y(y) {}
    };

    template<int U1, int U2, int U3, int U4, int U5,
        int V1, int V2, int V3, int V4, int V5>
        inline Vector2<Scalar<U1 - V1, U2 - V2, U3 - V3, U4 - V4, U5 - V5>> operator/(
            const Vector2<Scalar<U1, U2, U3, U4, U5>>& lhs,
            const Vector2<Scalar<V1, V2, V3, V4, V5>>& rhs) {
        return Vector2<Scalar<U1 - V1, U2 - V2, U3 - V3, U4 - V4, U5 - V5>>(
            lhs.x / rhs.x, lhs.y / rhs.y);
    }

    template<typename T>
    struct Vector3 {
        T x, y, z;
        inline constexpr Vector3() {}
        inline constexpr Vector3(T x, T y, T z) : x(x), y(y), z(z) {}
        inline Vector3 operator+(const Vector3& rhs) const {
            return Vector3(x + rhs.x, y + rhs.y, z + rhs.z);
        }
        inline Vector3& operator+=(const Vector3& rhs) {
            x += rhs.x;
            y += rhs.y;
            z += rhs.z;
            return *this;
        }
        inline Vector3 operator-(const Vector3& rhs) const {
            return Vector3(x - rhs.x, y - rhs.y, z - rhs.z);
        }
        inline Vector3 operator-() const {
            return Vector3(-x, -y, -z);
        }
        inline Vector3 operator*(const double rhs) const {
            return Vector3(x * rhs, y * rhs, z * rhs);
        }
    };

    template<int U1, int U2, int U3, int U4, int U5,
        int V1, int V2, int V3, int V4, int V5>
        inline Vector3<Scalar<U1 + V1, U2 + V2, U3 + V3, U4 + V4, U5 + V5>> operator*(
            const Vector3<Scalar<U1, U2, U3, U4, U5>>& lhs,
            const Scalar<V1, V2, V3, V4, V5> rhs) {
        return Vector3<Scalar<U1 + V1, U2 + V2, U3 + V3, U4 + V4, U5 + V5>>(
            lhs.x * rhs, lhs.y * rhs, lhs.z * rhs);
    }

    template<int U1, int U2, int U3, int U4, int U5,
        int V1, int V2, int V3, int V4, int V5>
        inline Vector3<Scalar<U1 - V1, U2 - V2, U3 - V3, U4 - V4, U5 - V5>> operator/(
            const Vector3<Scalar<U1, U2, U3, U4, U5>>& lhs,
            const Scalar<V1, V2, V3, V4, V5>& rhs) {
        return Vector3<Scalar<U1 - V1, U2 - V2, U3 - V3, U4 - V4, U5 - V5>>(
            lhs.x / rhs, lhs.y / rhs, lhs.z / rhs);
    }

    template<int U1, int U2, int U3, int U4, int U5,
        int V1, int V2, int V3, int V4, int V5>
        inline Scalar<U1 + V1, U2 + V2, U3 + V3, U4 + V4, U5 + V5> dot(
            const Vector3<Scalar<U1, U2, U3, U4, U5>>& u,
            const Vector3<Scalar<V1, V2, V3, V4, V5>>& v) {
        return u.x * v.x + u.y * v.y + u.z * v.z;
    }

    template<typename T>
    T length(const Vector3<T>& v) {
        return sqrt(dot(v, v));
    }

    template<typename T>
    Vector3<Number> normalize(const Vector3<T>& v) {
        T length = sqrt(dot(v, v));
        return Vector3<Number>(v.x / length, v.y / length, v.z / length);
    }

    template<typename T>
    struct Vector4 {
        T x, y, z, w;
        inline constexpr Vector4() {}
        inline constexpr Vector4(T x, T y, T z, T w) : x(x), y(y), z(z), w(w) {}
    };

    template<int U1, int U2, int U3, int U4, int U5,
        int V1, int V2, int V3, int V4, int V5>
        inline Vector4<Scalar<U1 - V1, U2 - V2, U3 - V3, U4 - V4, U5 - V5>> operator/(
            const Vector4<Scalar<U1, U2, U3, U4, U5>>& lhs,
            const Vector4<Scalar<V1, V2, V3, V4, V5>>& rhs) {
        return Vector4<Scalar<U1 - V1, U2 - V2, U3 - V3, U4 - V4, U5 - V5>>(
            lhs.x / rhs.x, lhs.y / rhs.y, lhs.z / rhs.z, lhs.w / rhs.w);
    }

    typedef Vector2<Number> vec2;
    typedef Vector3<Number> vec3;
    typedef Vector4<Number> vec4;

}  // namespace dimensional

#endif  // MATH_VECTOR_H_
