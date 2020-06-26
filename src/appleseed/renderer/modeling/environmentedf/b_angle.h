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

#ifndef MATH_ANGLE_H_
#define MATH_ANGLE_H_

#include <cmath>

#include "b_scalar.h"

namespace dimensional {

    // An angle, represented with its own class to make sure it is properly used
    // (e.g. that degrees are not used when radians are expected or vice versa).
    // NOTES:
    // - We intentionally do not provide a constructor from a literal value in order
    // to force the use of an explicit unit (e.g. "Angle a = 3.0 * deg;").
    // - Likewise, we intentionally do not provide a noarg accessor, in order to
    // force the use of an explicit unit when accessing the angle value (e.g.
    // "double value = a.to(rad);").
    class Angle {
    public:
        Angle() {}
        static constexpr Angle Unit() { return Angle(1.0); }

        inline constexpr double to(const Angle unit) const {
            return value_ / unit.value_;
        }

        inline Angle& operator=(const Angle rhs) {
            value_ = rhs.value_;
            return *this;
        }

        inline constexpr Angle operator+(const Angle rhs) const {
            return Angle(value_ + rhs.value_);
        }

        inline constexpr Angle operator-(const Angle rhs) const {
            return Angle(value_ - rhs.value_);
        }

        inline constexpr Angle operator-() const {
            return Angle(-value_);
        }

        inline constexpr Angle operator*(const Number rhs) const {
            return Angle(value_ * rhs());
        }

        inline constexpr Angle operator/(const Number rhs) const {
            return Angle(value_ / rhs());
        }

        inline constexpr Number operator/(const Angle rhs) const {
            return value_ / rhs.value_;
        }

        inline bool operator==(const Angle rhs) const {
            return value_ == rhs.value_;
        }

        inline constexpr bool operator<(const Angle rhs) const {
            return value_ < rhs.value_;
        }

        inline constexpr bool operator<=(const Angle rhs) const {
            return value_ <= rhs.value_;
        }

        inline constexpr bool operator>(const Angle rhs) const {
            return value_ > rhs.value_;
        }

        inline constexpr bool operator>=(const Angle rhs) const {
            return value_ >= rhs.value_;
        }

    private:
        explicit constexpr Angle(double value) : value_(value) {}

        double value_;

        friend constexpr Angle operator*(const Number lhs, const Angle rhs);
        friend Number cos(Angle);
        friend Number sin(Angle);
        friend Number tan(Angle);
    };

    inline constexpr Angle operator*(const Number lhs, const Angle rhs) {
        return Angle(lhs() * rhs.value_);
    }

    constexpr double PI = 3.14159265358979323846;

    constexpr Angle rad = Angle::Unit();
    constexpr Angle pi = PI * rad;
    constexpr Angle deg = pi / 180.0;

    constexpr inline Angle acos(const Number cosine) {
        return std::acos(cosine()) * rad;
    }
    constexpr inline Angle asin(const Number sine) {
        return std::asin(sine()) * rad;
    }
    inline Angle atan(const Number x) { return std::atan(x()) * rad; }
    inline Angle atan2(const Number y, const Number x) {
        return std::atan2(y(), x()) * rad;
    }
    inline Number cos(const Angle angle) { return std::cos(angle.value_); }
    inline Number sin(const Angle angle) { return std::sin(angle.value_); }
    inline Number tan(const Angle angle) { return std::tan(angle.value_); }

}  // namespace dimensional

#endif  // MATH_ANGLE_H_
