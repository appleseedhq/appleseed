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

#ifndef MATH_SCALAR_H_
#define MATH_SCALAR_H_

#include <algorithm>
#include <cmath>

namespace dimensional {

    // Internal class used to differentiate two Scalar constructors below.
    struct Double {
        const double value;
        explicit inline constexpr Double(double value) : value(value) {}
    };

    // A scalar value with a dimension combining up to five base units (length,
    // mass, time, etc). The template parameters are the exponents of the base units
    // in the scalar value dimension. For instance, if U1 corresponds to length and
    // U3 to time, a speed is a Scalar<1, 0, -1, 0, 0> (length divided by time), an
    // area is a Scalar<2, 0, 0, 0, 0>, etc. The semantics of the base units is not
    // defined by this template so that it can be used with various unit systems.
    // NOTES:
    // - We intentionally do not provide a constructor from a literal value, except
    // for dimensionless scalars, in order to force the use of an explicit unit
    // (e.g. "Length l = 3.0 * m;").
    // - Likewise, we intentionally do not provide a noarg accessor, except for
    // dimensionless scalars, in order to force the use of an explicit unit when
    // accessing a value (e.g. "double value = l.to(km);").
    template<int U1, int U2, int U3, int U4, int U5>
    class Scalar {
    public:
        Scalar() {}
        inline constexpr Scalar(double value) : value_(value) {
            static_assert(
                U1 == 0 && U2 == 0 && U3 == 0 && U4 == 0 && U5 == 0,
                "This constructor is valid only for dimensionless values.");
        }
        static constexpr Scalar Unit() { return Scalar(Double(1.0)); }

        inline constexpr double operator()() const {
            static_assert(
                U1 == 0 && U2 == 0 && U3 == 0 && U4 == 0 && U5 == 0,
                "This operator is valid onlyfor dimensionless values.");
            return value_;
        }

        inline constexpr double to(const Scalar<U1, U2, U3, U4, U5> unit) const {
            return value_ / unit.value_;
        }

        inline Scalar& operator=(const Scalar rhs) {
            value_ = rhs.value_;
            return *this;
        }

        inline Scalar& operator+=(const Scalar rhs) {
            value_ += rhs.value_;
            return *this;
        }

        inline constexpr Scalar operator+(const Scalar rhs) const {
            return Scalar(Double(value_ + rhs.value_));
        }

        inline constexpr Scalar operator-() const {
            return Scalar(Double(-value_));
        }

        inline constexpr Scalar operator-(const Scalar rhs) const {
            return Scalar(Double(value_ - rhs.value_));
        }

        inline constexpr Scalar operator*(double rhs) const {
            return Scalar(Double(value_ * rhs));
        }

        template<int V1, int V2, int V3, int V4, int V5>
        inline constexpr
            Scalar<U1 + V1, U2 + V2, U3 + V3, U4 + V4, U5 + V5> operator*(
                const Scalar<V1, V2, V3, V4, V5> rhs) const {
            return Scalar<U1 + V1, U2 + V2, U3 + V3, U4 + V4, U5 + V5>(
                Double(value_ * rhs.value_));
        }

        inline constexpr Scalar operator/(double rhs) const {
            return Scalar(Double(value_ / rhs));
        }

        template<int V1, int V2, int V3, int V4, int V5>
        inline constexpr
            Scalar<U1 - V1, U2 - V2, U3 - V3, U4 - V4, U5 - V5> operator/(
                const Scalar<V1, V2, V3, V4, V5> rhs) const {
            return Scalar<U1 - V1, U2 - V2, U3 - V3, U4 - V4, U5 - V5>(
                Double(value_ / rhs.value_));
        }

        inline bool operator==(const Scalar<U1, U2, U3, U4, U5> rhs) const {
            return value_ == rhs.value_;
        }

        inline bool operator<(const Scalar<U1, U2, U3, U4, U5> rhs) const {
            return value_ < rhs.value_;
        }

        inline bool operator<=(const Scalar<U1, U2, U3, U4, U5> rhs) const {
            return value_ <= rhs.value_;
        }

        inline bool operator>(const Scalar<U1, U2, U3, U4, U5> rhs) const {
            return value_ > rhs.value_;
        }

        inline bool operator>=(const Scalar<U1, U2, U3, U4, U5> rhs) const {
            return value_ >= rhs.value_;
        }

    private:
        // Private constructor, which can construct a value of any dimension from
        // a double literal. In order to diffentiate this constructor from the
        // implicit constructor above, we use the auxilliary internal class "Double".
        explicit inline constexpr Scalar(const Double value) : value_(value.value) {}

        double value_;

        template<int V1, int V2, int V3, int V4, int V5>
        friend class Scalar;

        friend constexpr Scalar<0, 0, 0, 0, 0> operator+(
            double lhs, const Scalar<0, 0, 0, 0, 0> rhs);

        friend constexpr Scalar<0, 0, 0, 0, 0> operator+(
            const Scalar<0, 0, 0, 0, 0> lhs, double rhs);

        friend constexpr Scalar<0, 0, 0, 0, 0> operator-(
            double lhs, const Scalar<0, 0, 0, 0, 0> rhs);

        friend constexpr Scalar<0, 0, 0, 0, 0> operator-(
            const Scalar<0, 0, 0, 0, 0> lhs, double rhs);

        template<int V1, int V2, int V3, int V4, int V5>
        friend constexpr Scalar<V1, V2, V3, V4, V5> operator*(
            double lhs, const Scalar<V1, V2, V3, V4, V5> rhs);

        template<int V1, int V2, int V3, int V4, int V5>
        friend constexpr Scalar<-V1, -V2, -V3, -V4, -V5> operator/(
            double lhs, const Scalar<V1, V2, V3, V4, V5> rhs);

        friend bool operator<(const Scalar<0, 0, 0, 0, 0> lhs, double rhs);
        friend bool operator<(double lhs, const Scalar<0, 0, 0, 0, 0> rhs);
        friend bool operator<=(const Scalar<0, 0, 0, 0, 0> lhs, double rhs);
        friend bool operator<=(double lhs, const Scalar<0, 0, 0, 0, 0> rhs);
        friend bool operator>(const Scalar<0, 0, 0, 0, 0> lhs, double rhs);
        friend bool operator>(double lhs, const Scalar<0, 0, 0, 0, 0> rhs);
        friend bool operator>=(const Scalar<0, 0, 0, 0, 0> lhs, double rhs);
        friend bool operator>=(double lhs, const Scalar<0, 0, 0, 0, 0> rhs);

        friend constexpr Scalar<0, 0, 0, 0, 0> exp(const Scalar<0, 0, 0, 0, 0> x);
        friend Scalar<0, 0, 0, 0, 0> floor(const Scalar<0, 0, 0, 0, 0> x);
        friend constexpr Scalar<0, 0, 0, 0, 0> log(const Scalar<0, 0, 0, 0, 0> x);
        friend Scalar<0, 0, 0, 0, 0> max(const Scalar<0, 0, 0, 0, 0> x, double y);
        friend Scalar<0, 0, 0, 0, 0> max(double x, const Scalar<0, 0, 0, 0, 0> y);
        friend Scalar<0, 0, 0, 0, 0> min(const Scalar<0, 0, 0, 0, 0> x, double y);
        friend Scalar<0, 0, 0, 0, 0> min(double x, const Scalar<0, 0, 0, 0, 0> y);
        friend Scalar<0, 0, 0, 0, 0> pow(const Scalar<0, 0, 0, 0, 0> x,
            const Scalar<0, 0, 0, 0, 0> y);
        friend Scalar<0, 0, 0, 0, 0> pow(double x, const Scalar<0, 0, 0, 0, 0> y);
        friend Scalar<0, 0, 0, 0, 0> pow(const Scalar<0, 0, 0, 0, 0> x, double y);
        friend Scalar<0, 0, 0, 0, 0> sqrt(const Scalar<0, 0, 0, 0, 0> x);
        template<int V1, int V2, int V3, int V4, int V5>
        friend constexpr Scalar<V1 / 2, V2 / 2, V3 / 2, V4 / 2, V5 / 2> sqrt(
            const Scalar<V1, V2, V3, V4, V5> x);
    };

    typedef Scalar<0, 0, 0, 0, 0> Dimensionless;
    typedef Scalar<0, 0, 0, 0, 0> Number;

    inline constexpr Number operator+(double lhs, const Number rhs) {
        return lhs + rhs.value_;
    }

    inline constexpr Number operator+(const Number lhs, double rhs) {
        return lhs.value_ + rhs;
    }

    inline constexpr Number operator-(double lhs, const Number rhs) {
        return lhs - rhs.value_;
    }

    inline constexpr Number operator-(const Number lhs, double rhs) {
        return lhs.value_ - rhs;
    }

    inline bool operator<(const Number lhs, double rhs) {
        return lhs.value_ < rhs;
    }

    inline bool operator<(double lhs, const Number rhs) {
        return lhs < rhs.value_;
    }

    inline bool operator<=(const Number lhs, double rhs) {
        return lhs.value_ <= rhs;
    }

    inline bool operator<=(double lhs, const Number rhs) {
        return lhs <= rhs.value_;
    }

    inline bool operator>(const Number lhs, double rhs) {
        return lhs.value_ > rhs;
    }

    inline bool operator>(double lhs, const Number rhs) {
        return lhs > rhs.value_;
    }

    inline bool operator>=(const Number lhs, double rhs) {
        return lhs.value_ >= rhs;
    }

    inline bool operator>=(double lhs, const Number rhs) {
        return lhs >= rhs.value_;
    }

    template<int U1, int U2, int U3, int U4, int U5>
    inline constexpr Scalar<U1, U2, U3, U4, U5> operator*(
        double lhs, const Scalar<U1, U2, U3, U4, U5> rhs) {
        return Scalar<U1, U2, U3, U4, U5>(Double(lhs * rhs.value_));
    }

    template<int U1, int U2, int U3, int U4, int U5>
    inline constexpr Scalar<-U1, -U2, -U3, -U4, -U5> operator/(
        double lhs, const Scalar<U1, U2, U3, U4, U5> rhs) {
        return Scalar<-U1, -U2, -U3, -U4, -U5>(Double(lhs / rhs.value_));
    }

    inline constexpr Number exp(const Number x) { return std::exp(x()); }
    inline Number floor(const Number x) { return std::floor(x()); }
    inline constexpr Number log(const Number x) { return std::log(x()); }
    inline Number max(const Number x, double y) { return std::max(x(), y); }
    inline Number max(double x, const Number y) { return std::max(x, y()); }
    inline Number min(const Number x, double y) { return std::min(x(), y); }
    inline Number min(double x, const Number y) { return std::min(x, y()); }
    inline Number pow(const Number x, const Number y) { return std::pow(x(), y()); }
    inline Number pow(const Number x, double y) { return std::pow(x(), y); }
    inline Number pow(double x, const Number y) { return std::pow(x, y()); }
    inline Number sqrt(const Number x) { return std::sqrt(x()); }

    template<int U1, int U2, int U3, int U4, int U5>
    constexpr Scalar<U1 / 2, U2 / 2, U3 / 2, U4 / 2, U5 / 2> sqrt(
        const Scalar<U1, U2, U3, U4, U5> x) {
        return Scalar<U1 / 2, U2 / 2, U3 / 2, U4 / 2, U5 / 2>(
            Double(std::sqrt(x.value_)));
    }

    template<typename T>
    T mod(T x, T y) { return x - y * floor(x / y); }
    inline double mod(double x, double y) { return x - y * std::floor(x / y); }
    inline Number mod(Number x, double y) { return x - y * floor(x / y); }
    inline Number mod(double x, Number y) { return x - y * floor(x / y); }

    template<typename T>
    T lerp(T a, T b, double x) { return a * (1.0 - x) + b * x; }

    template<typename T>
    T clamp(T x, T min, T max) { return std::max(min, std::min(max, x)); }

    template<typename T>
    T smoothstep(T edge0, T edge1, T x) {
        x = clamp((x - edge0) / (edge1 - edge0), T(0.0), T(1.0));
        return x * x * (3.0 - 2.0 * x);
    }

}  // namespace dimensional

#endif  // MATH_SCALAR_H_
