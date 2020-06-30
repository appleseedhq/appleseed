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

#ifndef MATH_SCALAR_FUNCTION_H_
#define MATH_SCALAR_FUNCTION_H_

#include <algorithm>
#include <cassert>
#include <vector>

namespace dimensional {

    // A function from scalars of type Scalar<U1, U2, U3, U4, U5> to scalars of type
    // Scalar<V1, V2, V3, V4, V5>, represented with N uniformly distributed samples
    // between MIN and MAX in the input domain, and linearly interpolated in
    // between.
    template<
        unsigned int N, int MIN, int MAX>
        class ScalarFunction {
        public:
            constexpr static unsigned int SIZE = N;

            ScalarFunction() {}

            explicit ScalarFunction(const double& constant_value) {
                for (unsigned int i = 0; i < N; ++i) {
                    value_[i] = constant_value;
                }
            }

            ScalarFunction(const std::vector<float>& sampled_values, double unit) {
                for (unsigned int i = 0; i < N; ++i) {
                    value_[i] = (i < sampled_values.size() ? sampled_values[i] : 0.0) * unit;
                }
            }

            ScalarFunction(const ScalarFunction& rhs) {
                for (unsigned int i = 0; i < N; ++i) {
                    value_[i] = rhs.value_[i];
                }
            }

            // Creates a new function from the given values, sampled at the given sampling
            // points. The given vectors must have the same size n and sampling_points
            // must be sorted in increasing order. The input function is supposed constant
            // outside the sampling interval (i.e. for x < sampling_points[0] and for
            // x > sampling_points[n - 1]) and is linearly interpolated between samples.
            ScalarFunction(const std::vector<double>& sampling_points, const std::vector<double>& sampled_values) {
                Init(sampling_points, sampled_values);
            }

            // Creates a new function from the given values, uniformly sampled from
            // 'min_input' and 'max_input' (inclusive). The input function is supposed
            // constant outside the sampling interval (i.e. for x < min_input and for
            // x > max_input) and is linearly interpolated between samples.
            ScalarFunction(double min_input, double max_input, const std::vector<double> values) {
                std::vector<double> sampling_points;
                for (unsigned int i = 0; i < values.size(); ++i) {
                    sampling_points.push_back(
                        lerp(min_input, max_input, i / (values.size() - 1.0)));
                }
                Init(sampling_points, values);
            }

            std::vector<double> to(double unit) {
                std::vector<double> result;
                for (unsigned int i = 0; i < N; ++i) {
                    result.push_back(value_[i] * unit);
                }
                return result;
            }

            ScalarFunction& operator=(const ScalarFunction& rhs) {
                for (unsigned int i = 0; i < N; ++i) {
                    value_[i] = rhs.value_[i];
                }
                return *this;
            }

            ScalarFunction& operator+=(const ScalarFunction& rhs) {
                for (unsigned int i = 0; i < N; ++i) {
                    value_[i] += rhs.value_[i];
                }
                return *this;
            }

            ScalarFunction operator+(const ScalarFunction& rhs) const {
                ScalarFunction result;
                for (unsigned int i = 0; i < N; ++i) {
                    result.value_[i] = value_[i] + rhs.value_[i];
                }
                return result;
            }

            ScalarFunction operator-() const {
                ScalarFunction result;
                for (unsigned int i = 0; i < N; ++i) {
                    result.value_[i] = -value_[i];
                }
                return result;
            }

            ScalarFunction operator-(const ScalarFunction& rhs) const {
                ScalarFunction result;
                for (unsigned int i = 0; i < N; ++i) {
                    result.value_[i] = value_[i] - rhs.value_[i];
                }
                return result;
            }

            ScalarFunction operator*(double rhs) const {
                ScalarFunction result;
                for (unsigned int i = 0; i < N; ++i) {
                    result.value_[i] = value_[i] * rhs;
                }
                return result;
            }

            template<int W1, int W2, int W3, int W4, int W5>
            ScalarFunction<N, MIN, MAX>
                operator*(double rhs) const {
                ScalarFunction<N, MIN, MAX> result;
                for (unsigned int i = 0; i < N; ++i) {
                    result[i] = value_[i] * rhs;
                }
                return result;
            }

            ScalarFunction<N, MIN, MAX>
                operator*(const ScalarFunction<N, MIN, MAX>& rhs) const {
                ScalarFunction<N, MIN, MAX> result;
                for (unsigned int i = 0; i < N; ++i) {
                    result[i] = value_[i] * rhs[i];
                }
                return result;
            }

            ScalarFunction<N, MIN, MAX>
                operator/(const ScalarFunction<N, MIN, MAX>& rhs) const {
                ScalarFunction<N, MIN, MAX> result;
                for (unsigned int i = 0; i < N; ++i) {
                    result[i] = value_[i] / rhs[i];
                }
                return result;
            }

            inline unsigned int size() const {
                return N;
            }

            inline const double operator[](int index) const {
                return value_[index];
            }

            inline double& operator[](int index) {
                return value_[index];
            }

            inline double GetSample(int sample_index) const {
                double u = static_cast<double>(sample_index) / N;
                return lerp(MIN, MAX, u);
            }

            const double operator()(double x) const {
                double u = (x.to(double::Unit()) - MIN) / (MAX - MIN) * N;
                int i = std::floor(u);
                u -= i;
                int i0 = std::max(0, std::min(static_cast<int>(N) - 1, i));
                int i1 = std::max(0, std::min(static_cast<int>(N) - 1, i + 1));
                return value_[i0] * (1.0 - u) + value_[i1] * u;
            }

        private:
            void Init(const std::vector<double>& sampling_points,
                const std::vector<double>& sampled_values) {
                assert(sampling_points.size() == sampled_values.size());
                for (unsigned int i = 0; i < N; ++i) {
                    double x = GetSample(i);
                    // Find the two nearest sampling points around 'x' using binary search and
                    // return the corresponding sampled values, linearly interpolated.
                    int min_index = 0;
                    int max_index = sampling_points.size() - 1;
                    if (x <= sampling_points[min_index]) {
                        value_[i] = sampled_values[min_index];
                        continue;
                    }
                    if (x >= sampling_points[max_index]) {
                        value_[i] = sampled_values[max_index];
                        continue;
                    }
                    while (max_index - min_index > 1) {
                        int mid_index = (min_index + max_index) / 2;
                        if (x < sampling_points[mid_index]) {
                            max_index = mid_index;
                        }
                        else {
                            min_index = mid_index;
                        }
                    }
                    double u = (x - sampling_points[min_index]) / (sampling_points[max_index] - sampling_points[min_index]);
                    value_[i] = sampled_values[min_index] * (1.0 - u) + sampled_values[max_index] * u;
                }
            }

            double value_[N];
    };

    template<unsigned int N, int MIN, int MAX>
        ScalarFunction<N, MIN, MAX>
        min(const ScalarFunction<N, MIN, MAX>& a, const ScalarFunction<N, MIN, MAX>& b) {
        ScalarFunction<N, MIN, MAX> result;
        for (unsigned int i = 0; i < result.size(); ++i) {
            result[i] = a[i] < b[i] ? a[i] : b[i];
        }
        return result;
    }

    template<unsigned int N, int MIN, int MAX>
        ScalarFunction<N, MIN, MAX>
        exp(const ScalarFunction<N, MIN, MAX>& f) {
        ScalarFunction<N, MIN, MAX> result;
        for (unsigned int i = 0; i < result.size(); ++i) {
            result[i] = std::exp(f[i]);
        }
        return result;
    }

    // Returns the integral of the given function over its range,
    // i.e. integral of function * dx from MIN to MAX.
    template<unsigned int N, int MIN, int MAX>
        double Integral(const ScalarFunction<N, MIN, MAX>& f) {
        double sum = 0.0;
        for (unsigned int i = 0; i < f.size(); ++i) {
            sum = sum + f[i];
        }
        return sum * ((MAX - MIN) / N);
    }

    // Returns the integral of the given function over a subset of its range,
    // i.e. integral of function * dx from min_value to max_value, using the
    // specified number of samples.
    template<unsigned int N, int MIN, int MAX>
        double Integral(const ScalarFunction<N, MIN, MAX>& f, double min_value, double max_value, int number_of_samples) {
        double dx = (max_value - min_value) / number_of_samples;
        double x = min_value;
        double sum = 0.0;
        for (int i = 0; i < number_of_samples; ++i) {
            sum = sum + f(x);
            x = x + dx;
        }
        return sum * dx;
    }

    // Returns the integral of the given function over a subset of its range,
    // i.e. integral of function * dx from min_value to max_value.
    template<unsigned int N, int MIN, int MAX>
        double Integral(const ScalarFunction<N, MIN, MAX>& f, double min_value, double max_value) {
        double sum = 0.0;
        for (unsigned int i = 0; i < f.size(); ++i) {
            double dx = std::min(f.GetSample(i + 1), max_value) - std::max(f.GetSample(i), min_value);
            sum = sum + f[i] * std::max(dx, 0.0);
        }
        return sum;
    }

}  // namespace dimensional

#endif  // MATH_SCALAR_FUNCTION_H_
