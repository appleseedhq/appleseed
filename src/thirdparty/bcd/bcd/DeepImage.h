// This file comes from the original BCD implementation,
// with minor changes to remove dependencies, unused code
// and re-formatting. Original license follows:

// This file is part of the reference implementation for the paper
//   Bayesian Collaborative Denoising for Monte-Carlo Rendering
//   Malik Boughida and Tamy Boubekeur.
//   Computer Graphics Forum (Proc. EGSR 2017), vol. 36, no. 4, p. 137-153, 2017
//
// All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE.txt file.

#ifndef DEEP_IMAGE_H
#define DEEP_IMAGE_H

// Standard headers.
#include <algorithm>
#include <cassert>
#include <vector>

namespace bcd
{

class PixelPosition
{
  public:
    PixelPosition();
    PixelPosition(int i_line, int i_column);
    PixelPosition(const PixelPosition& i_rPos);
    PixelPosition& operator=(const PixelPosition& i_rPos);

  public:
    int m_line;
    int m_column;

  public:
    void get(int& o_rLine, int& o_rColumn) const;
    PixelPosition operator+(const PixelPosition& i_rPix) const;
    PixelPosition operator-(const PixelPosition& i_rPix) const;
    bool operator==(const PixelPosition& i_rPix) const;
    bool operator!=(const PixelPosition& i_rPix) const;
};

typedef PixelPosition PixelVector;
typedef PixelPosition PixelWindowSize;
typedef PixelPosition PatchSize;
typedef PixelPosition ImageWindowSize;

class PixelWindow
{
  public:
    class iterator
    {
    public:
      iterator();
      iterator(PixelPosition i_centralPixel, int i_radius);
      iterator(int i_bufferWidth,
               int i_bufferHeight,
               PixelPosition i_centralPixel,
               int i_radius,
               int i_border = 0);
      iterator(PixelPosition i_minCorner,
               PixelPosition i_maxCorner,
               PixelPosition i_currentPixel);

    public:
      void reset(PixelPosition i_centralPixel, int i_radius);
      void reset(int i_bufferWidth,
                 int i_bufferHeight,
                 PixelPosition i_centralPixel,
                 int i_radius,
                 int i_border = 0);
      PixelWindowSize getSize() const;
      const PixelPosition& operator*() const;
      iterator& operator++();
      bool hasEnded() const;
      bool operator!=(const iterator& i_rIt) const;

    private:
      PixelPosition m_minCorner;
      PixelPosition m_maxCorner;
      PixelPosition m_currentPixel;
    };

  public:
    PixelWindow();
    PixelWindow(int i_bufferWidth,
                int i_bufferHeight,
                PixelPosition i_centralPixel,
                int i_radius,
                int i_border = 0);

  public:
    void reset(int i_bufferWidth,
               int i_bufferHeight,
               PixelPosition i_centralPixel,
               int i_radius,
               int i_border = 0);

    PixelWindowSize getSize() const;

    iterator begin() const;
    iterator end() const;

  private:
    int m_width;
    int m_height;
    PixelPosition m_minCorner;
    PixelPosition m_maxCorner;
};

typedef PixelWindow PixelPatch;
typedef PixelWindow::iterator PixWinIt;
typedef PixelPatch::iterator PixPatchIt;

//  Template class for storing a 2D buffer of any vector type (any number of
//  scalars per pixel)
/// but seen as a 3D buffer of scalars
///
/// TODO : detailed desciption
template<typename scalar = float>
class DeepImage
{
  public:
    class iterator
    {
      public:
        iterator();
        iterator(scalar* i_pPixelDataPtr, int i_nbOfScalarsInPixelData);
        scalar* operator*() const;
        iterator& operator++();
        scalar& operator[](int i_dimensionIndex) const;
        bool operator!=(const iterator& i_rIt) const;

      private:
        scalar* m_pCurrentPixelDataPtr;
        int m_nbOfScalarsInPixelData;
    };

    class const_iterator
    {
      public:
        const_iterator();
        const_iterator(const scalar* i_pPixelDataPtr, int i_nbOfScalarsInPixelData);
        const scalar* operator*() const;
        const_iterator& operator++();
        const scalar& operator[](int i_dimensionIndex) const;
        bool operator!=(const const_iterator& i_rIt) const;

      private:
        const scalar* m_pCurrentPixelDataPtr;
        int m_nbOfScalarsInPixelData;
    };

    DeepImage();

    DeepImage(
      int i_width,
      int i_height,
      int i_depth);

    DeepImage(const DeepImage<scalar>& i_tImage);

    DeepImage(DeepImage<scalar>&& i_tImage);

    DeepImage& operator=(const DeepImage<scalar>& i_tImage);

    DeepImage& operator=(DeepImage<scalar>&& i_tImage);

    void resize(int i_width, int i_height, int i_depth);

    //  Copies data from a pointer to the buffer.
    void copyDataFrom(const scalar* i_pData);

    //  Copies data from the buffer to a pointer.
    void copyDataTo(scalar* i_pData) const;

    int getWidth() const;  // Returns the width of the 3D buffer
    int getHeight() const; // Returns the height of the 3D buffer
    int getDepth() const;  // Returns the depth of the 3D buffer
    int getSize() const;   // Returns the number of scalars stored in the buffer

    scalar* getDataPtr();
    const scalar* getDataPtr() const;

    //  Returns a clamped position with the width and height of the image
    PixelPosition clamp(const PixelPosition& pos) const;

    //  Returns the 1D storage index in the buffer from 3 coordinates
    int glueIndices(int i_line, int i_column, int i_dimensionIndex) const;

    //  Returns the 1D storage index in the buffer from 3 coordinates
    static int glueIndices(
        int i_width,
        int i_height,
        int i_depth,
        int i_line,
        int i_column,
        int i_dimensionIndex);

    //  Returns the 3 coordinates from the 1D storage index in the buffer
    void splitIndex(
        int& o_rLine,
        int& o_rColumn,
        int& o_rDimensionIndex,
        int  i_buffer1DIndex) const;

    //  Returns the 3 coordinates from the 1D storage index in the buffer
    static void splitIndex(
        int& o_rLine,
        int& o_rColumn,
        int& o_rDimensionIndex,
        int i_buffer1DIndex,
        int i_width,
        int i_height,
        int i_depth);

    const scalar& get(int i_line, int i_column, int i_dimensionIndex) const;

    scalar& get(int i_line, int i_column, int i_dimensionIndex);

    const scalar& get(PixelPosition i_pixel, int i_dimensionIndex) const;

    scalar& get(PixelPosition i_pixel, int i_dimensionIndex);

    const scalar& get(int i_buffer1DIndex) const;

    scalar& get(int i_buffer1DIndex);

    const scalar getValue(PixelPosition i_pixel, int i_dimensionIndex) const;

    scalar getValue(PixelPosition i_pixel, int i_dimensionIndex);

    void set(int i_line, int i_column, int i_dimensionIndex, scalar i_value);

    void set(PixelPosition i_pixel, int i_dimensionIndex, scalar i_value);

    void set(int i_buffer1DIndex, scalar i_value);

    void set(int i_line, int i_column, const scalar* i_pVectorValue);

    void set(PixelPosition i_pixel, const scalar* i_pVectorValue);

    void isotropicalScale(scalar i_scaleFactor);

    void anisotropicalScale(const scalar* i_scaleFactors);

    void fill(scalar f);

    bool isEmpty() const;

    void clearAndFreeMemory();

    iterator begin();
    iterator end();
    const_iterator begin() const;
    const_iterator end() const;

    DeepImage<scalar>& operator+=(const DeepImage& i_rImage);
    DeepImage<scalar>& operator-=(const DeepImage& i_rImage);

  private:
    int m_width;
    int m_height;
    int m_depth;
    int m_widthTimesDepth;
    std::vector<scalar> m_data;
};

typedef DeepImage<float> Deepimf;
typedef Deepimf::iterator ImfIt;
typedef Deepimf::const_iterator ImfConstIt;

template<typename scalar = float>
class ImageWindow
{
  public:
    class iterator
    {
      public:
        iterator();
        iterator(DeepImage<scalar>& i_rImage,
                 PixelPosition i_centralPixel,
                 int i_radius,
                 int i_border = 0);
        iterator(int i_width,
                 int i_height,
                 int i_depth,
                 PixelPosition i_minCorner,
                 PixelPosition i_maxCorner,
                 PixelPosition i_currentPixel,
                 scalar* i_pCurrentDataPointer);

        void reset(DeepImage<scalar>& i_rImage,
                   PixelPosition i_centralPixel,
                   int i_radius,
                   int i_border = 0);

        ImageWindowSize getSize() const;
        scalar* operator*() const;
        iterator& operator++();
        bool hasEnded() const;
        scalar& operator[](int i_dimensionIndex) const;
        bool operator!=(const iterator& i_rIt) const;

      private:
        int m_width;
        int m_height;
        int m_depth;
        PixelPosition m_minCorner;
        PixelPosition m_maxCorner;
        PixelPosition m_currentPixel;
        scalar* m_pCurrentDataPointer;
    };

    ImageWindow();
    ImageWindow(DeepImage<scalar>& i_rImage,
                PixelPosition i_centralPixel,
                int i_radius,
                int i_border = 0);

    void reset(DeepImage<scalar>& i_rImage,
               PixelPosition i_centralPixel,
               int i_radius,
               int i_border = 0);

    ImageWindowSize getSize() const;

    iterator begin() const;
    iterator end() const;

  private:
    int m_width;
    int m_height;
    int m_depth;
    PixelPosition m_minCorner;
    PixelPosition m_maxCorner;
    scalar* m_pMinCornerDataPointer;
};

typedef ImageWindow<float> Win;
typedef Win Patch;

typedef Win::iterator WinIt;
typedef WinIt PatchIt;

template<typename scalar = float>
class ConstImageWindow
{
  public:
    class iterator
    {
    public:
      iterator();
      iterator(const DeepImage<scalar>& i_rImage,
               PixelPosition i_centralPixel,
               int i_radius,
               int i_border = 0);
      iterator(int i_width,
               int i_height,
               int i_depth,
               PixelPosition i_minCorner,
               PixelPosition i_maxCorner,
               PixelPosition i_currentPixel,
               const scalar* i_pCurrentDataPointer);

    public:
      void reset(const DeepImage<scalar>& i_rImage,
                 PixelPosition i_centralPixel,
                 int i_radius,
                 int i_border = 0);

      ImageWindowSize getSize() const;

      const scalar* operator*() const;

      iterator& operator++();

      bool hasEnded() const;

      const scalar& operator[](int i_dimensionIndex) const;

      bool operator!=(const iterator& i_rIt) const;

    private:
      int m_width;
      int m_height;
      int m_depth;
      PixelPosition m_minCorner;
      PixelPosition m_maxCorner;
      PixelPosition m_currentPixel;
      const scalar* m_pCurrentDataPointer;
    };

  public:
    ConstImageWindow();
    ConstImageWindow(const DeepImage<scalar>& i_rImage,
                     PixelPosition i_centralPixel,
                     int i_radius,
                     int i_border = 0);

  public:
    void reset(const DeepImage<scalar>& i_rImage,
               PixelPosition i_centralPixel,
               int i_radius,
               int i_border = 0);

    ImageWindowSize getSize() const;

    iterator begin() const;
    iterator end() const;

  private:
    int m_width;
    int m_height;
    int m_depth;
    PixelPosition m_minCorner;
    PixelPosition m_maxCorner;
    const scalar* m_pMinCornerDataPointer;
};

typedef ConstImageWindow<float> ConstWin;
typedef ConstWin ConstPatch;

typedef ConstWin::iterator ConstWinIt;
typedef ConstWinIt ConstPatchIt;


// ------------------------ PixelPosition ------------------------

inline PixelPosition::PixelPosition()
  : m_line(0)
  , m_column(0)
{
}

inline PixelPosition::PixelPosition(int i_line, int i_column)
  : m_line(i_line)
  , m_column(i_column)
{
}

inline PixelPosition::PixelPosition(const PixelPosition& i_rPos)
  : m_line(i_rPos.m_line)
  , m_column(i_rPos.m_column)
{
}

inline PixelPosition&
PixelPosition::operator=(const PixelPosition& i_rPos)
{
    m_line = i_rPos.m_line;
    m_column = i_rPos.m_column;
    return *this;
}

inline void
PixelPosition::get(int& o_rLine, int& o_rColumn) const
{
    o_rLine = m_line;
    o_rColumn = m_column;
}

inline PixelPosition
PixelPosition::operator+(const PixelPosition& i_rPix) const
{
    return PixelPosition(m_line + i_rPix.m_line, m_column + i_rPix.m_column);
}

inline PixelPosition
PixelPosition::operator-(const PixelPosition& i_rPix) const
{
    return PixelPosition(m_line - i_rPix.m_line, m_column - i_rPix.m_column);
}

inline bool
PixelPosition::operator==(const PixelPosition& i_rPix) const
{
    return (m_line == i_rPix.m_line) && (m_column == i_rPix.m_column);
}

inline bool
PixelPosition::operator!=(const PixelPosition& i_rPix) const
{
    return (m_line != i_rPix.m_line) || (m_column != i_rPix.m_column);
}

// ------------------------ PixelWindow::iterator ------------------------

inline PixelWindow::iterator::iterator()
  : m_minCorner()
  , m_maxCorner()
  , m_currentPixel()
{
}

inline PixelWindow::iterator::iterator(PixelPosition i_centralPixel, int i_radius)
  : m_minCorner(i_centralPixel.m_line - i_radius,
                i_centralPixel.m_column - i_radius)
  , m_maxCorner(i_centralPixel.m_line + i_radius,
                i_centralPixel.m_column + i_radius)
  , m_currentPixel(m_minCorner)
{
}

inline PixelWindow::iterator::iterator(int i_bufferWidth,
                                       int i_bufferHeight,
                                       PixelPosition i_centralPixel,
                                       int i_radius,
                                       int i_border)
{
    reset(i_bufferWidth, i_bufferHeight, i_centralPixel, i_radius, i_border);
}

inline PixelWindow::iterator::iterator(PixelPosition i_minCorner,
                                       PixelPosition i_maxCorner,
                                       PixelPosition i_currentPixel)
  : m_minCorner(i_minCorner)
  , m_maxCorner(i_maxCorner)
  , m_currentPixel(i_currentPixel)
{
}

inline void
PixelWindow::iterator::reset(PixelPosition i_centralPixel, int i_radius)
{
    m_minCorner = PixelPosition(i_centralPixel.m_line - i_radius,
                                i_centralPixel.m_column - i_radius);
    m_maxCorner = PixelPosition(i_centralPixel.m_line + i_radius,
                                i_centralPixel.m_column + i_radius);
    m_currentPixel = m_minCorner;
}

inline void
PixelWindow::iterator::reset(int i_bufferWidth,
                             int i_bufferHeight,
                             PixelPosition i_centralPixel,
                             int i_radius,
                             int i_border)
{
    m_minCorner =
      PixelPosition(std::max(i_border, i_centralPixel.m_line - i_radius),
                    std::max(i_border, i_centralPixel.m_column - i_radius));
    m_maxCorner = PixelPosition(
      std::min(i_bufferHeight - 1 - i_border, i_centralPixel.m_line + i_radius),
      std::min(i_bufferWidth - 1 - i_border, i_centralPixel.m_column + i_radius));
    m_currentPixel = m_minCorner;
}

inline PixelWindowSize
PixelWindow::iterator::getSize() const
{
    return PixelWindowSize(1, 1) + m_maxCorner - m_minCorner;
}

inline const PixelPosition& PixelWindow::iterator::operator*() const
{
    return m_currentPixel;
}

inline PixelWindow::iterator& PixelWindow::iterator::operator++()
{
    if (m_currentPixel.m_column == m_maxCorner.m_column)
    {
        m_currentPixel.m_line++;
        m_currentPixel.m_column = m_minCorner.m_column;
    }
    else
        m_currentPixel.m_column++;

    return *this;
}

inline bool
PixelWindow::iterator::hasEnded() const
{
    return m_currentPixel.m_line > m_maxCorner.m_line;
}

inline bool
PixelWindow::iterator::operator!=(const iterator& i_rIt) const
{
    return m_currentPixel != i_rIt.m_currentPixel;
}

// ------------------------ PixelWindow ------------------------

inline PixelWindow::PixelWindow()
  : m_width(0)
  , m_height(0)
  , m_minCorner()
  , m_maxCorner()
{
}

inline PixelWindow::PixelWindow(int i_bufferWidth,
                                int i_bufferHeight,
                                PixelPosition i_centralPixel,
                                int i_radius,
                                int i_border)
{
    reset(i_bufferWidth, i_bufferHeight, i_centralPixel, i_radius, i_border);
}

inline void
PixelWindow::reset(int i_bufferWidth,
                   int i_bufferHeight,
                   PixelPosition i_centralPixel,
                   int i_radius,
                   int i_border)
{
    m_width = i_bufferWidth;
    m_height = i_bufferHeight;
    m_minCorner =
      PixelPosition(std::max(i_border, i_centralPixel.m_line - i_radius),
                    std::max(i_border, i_centralPixel.m_column - i_radius));
    m_maxCorner = PixelPosition(
      std::min(m_height - 1 - i_border, i_centralPixel.m_line + i_radius),
      std::min(m_width - 1 - i_border, i_centralPixel.m_column + i_radius));
}

inline PixelWindowSize
PixelWindow::getSize() const
{
    return PixelWindowSize(1, 1) + m_maxCorner - m_minCorner;
}

inline PixelWindow::iterator
PixelWindow::begin() const
{
    return iterator(m_minCorner, m_maxCorner, m_minCorner);
}

inline PixelWindow::iterator
PixelWindow::end() const
{
    return iterator(m_minCorner,
                    m_maxCorner,
                    PixelPosition(m_maxCorner.m_line + 1, m_minCorner.m_column));
}

// ------------------------ DeepImage<>::iterator ------------------------

template<typename scalar>
inline DeepImage<scalar>::iterator::iterator()
  : m_pCurrentPixelDataPtr(nullptr)
  , m_nbOfScalarsInPixelData(0)
{
}

template<typename scalar>
inline DeepImage<scalar>::iterator::iterator(scalar* i_pPixelDataPtr,
                                             int i_nbOfScalarsInPixelData)
  : m_pCurrentPixelDataPtr(i_pPixelDataPtr)
  , m_nbOfScalarsInPixelData(i_nbOfScalarsInPixelData)
{
}

template<typename scalar>
inline scalar* DeepImage<scalar>::iterator::operator*() const
{
    return m_pCurrentPixelDataPtr;
}

template<typename scalar>
inline typename DeepImage<scalar>::iterator& DeepImage<scalar>::iterator::
operator++()
{
    m_pCurrentPixelDataPtr += m_nbOfScalarsInPixelData;
    return *this;
}

template<typename scalar>
inline scalar& DeepImage<scalar>::iterator::operator[](
  int i_dimensionIndex) const
{
    return m_pCurrentPixelDataPtr[i_dimensionIndex];
}

template<typename scalar>
inline bool
DeepImage<scalar>::iterator::operator!=(const iterator& i_rIt) const
{
    return m_pCurrentPixelDataPtr != i_rIt.m_pCurrentPixelDataPtr;
}

// ------------------------ DeepImage<>::const_iterator ------------------------

template<typename scalar>
inline DeepImage<scalar>::const_iterator::const_iterator()
  : m_pCurrentPixelDataPtr(nullptr)
  , m_nbOfScalarsInPixelData(0)
{
}

template<typename scalar>
inline DeepImage<scalar>::const_iterator::const_iterator(
  const scalar* i_pPixelDataPtr,
  int i_nbOfScalarsInPixelData)
  : m_pCurrentPixelDataPtr(i_pPixelDataPtr)
  , m_nbOfScalarsInPixelData(i_nbOfScalarsInPixelData)
{
}

template<typename scalar>
inline const scalar* DeepImage<scalar>::const_iterator::operator*() const
{
    return m_pCurrentPixelDataPtr;
}

template<typename scalar>
inline typename DeepImage<scalar>::const_iterator&
  DeepImage<scalar>::const_iterator::operator++()
{
    m_pCurrentPixelDataPtr += m_nbOfScalarsInPixelData;
    return *this;
}

template<typename scalar>
inline const scalar& DeepImage<scalar>::const_iterator::operator[](
  int i_dimensionIndex) const
{
    return m_pCurrentPixelDataPtr[i_dimensionIndex];
}

template<typename scalar>
inline bool
DeepImage<scalar>::const_iterator::operator!=(const const_iterator& i_rIt) const
{
    return m_pCurrentPixelDataPtr != i_rIt.m_pCurrentPixelDataPtr;
}

// ------------------------ DeepImage<> ------------------------

template<typename scalar>
inline DeepImage<scalar>::DeepImage()
  : m_width(0u)
  , m_height(0u)
  , m_depth(0u)
  , m_widthTimesDepth(0u)
  , m_data()
{
}

template<typename scalar>
inline DeepImage<scalar>::DeepImage(int i_width, int i_height, int i_depth)
  : m_width(i_width)
  , m_height(i_height)
  , m_depth(i_depth)
  , m_widthTimesDepth(i_width * i_depth)
  , m_data(i_width * i_height * i_depth)
{
}

template<typename scalar>
inline DeepImage<scalar>::DeepImage(const DeepImage<scalar>& i_tImage)
  : m_width(i_tImage.m_width)
  , m_height(i_tImage.m_height)
  , m_depth(i_tImage.m_depth)
  , m_widthTimesDepth(i_tImage.m_width * i_tImage.m_depth)
  , m_data(i_tImage.m_data)
{
}

template<typename scalar>
inline DeepImage<scalar>::DeepImage(DeepImage<scalar>&& i_tImage)
  : m_width(i_tImage.m_width)
  , m_height(i_tImage.m_height)
  , m_depth(i_tImage.m_depth)
  , m_widthTimesDepth(i_tImage.m_width * i_tImage.m_depth)
  , m_data(std::move(i_tImage.m_data))
{
}

template<typename scalar>
inline DeepImage<scalar>&
DeepImage<scalar>::operator=(const DeepImage<scalar>& i_tImage)
{
    m_width = i_tImage.m_width;
    m_height = i_tImage.m_height;
    m_depth = i_tImage.m_depth;
    m_widthTimesDepth = i_tImage.m_width * i_tImage.m_depth;
    m_data = i_tImage.m_data;

    return *this;
}

template<typename scalar>
inline DeepImage<scalar>&
DeepImage<scalar>::operator=(DeepImage<scalar>&& i_tImage)
{
    m_width = i_tImage.m_width;
    m_height = i_tImage.m_height;
    m_depth = i_tImage.m_depth;
    m_widthTimesDepth = i_tImage.m_width * i_tImage.m_depth;
    m_data = std::move(i_tImage.m_data);

    return *this;
}

template<typename scalar>
inline void
DeepImage<scalar>::resize(int i_width, int i_height, int i_depth)
{
    m_width = i_width;
    m_height = i_height;
    m_depth = i_depth;
    m_widthTimesDepth = i_width * i_depth;
    m_data.resize(i_width * i_height * i_depth);
}

template<typename scalar>
inline void
DeepImage<scalar>::copyDataFrom(const scalar* i_pData)
{
    std::copy(i_pData, i_pData + getSize(), m_data.begin());
}

template<typename scalar>
inline void
DeepImage<scalar>::copyDataTo(scalar* i_pData) const
{
    std::copy(m_data.begin(), m_data.end(), i_pData);
}

template<typename scalar>
inline int
DeepImage<scalar>::getWidth() const
{
    return m_width;
}
template<typename scalar>
inline int
DeepImage<scalar>::getHeight() const
{
    return m_height;
}
template<typename scalar>
inline int
DeepImage<scalar>::getDepth() const
{
    return m_depth;
}
template<typename scalar>
inline int
DeepImage<scalar>::getSize() const
{
    return m_data.size();
}
template<typename scalar>
inline scalar*
DeepImage<scalar>::getDataPtr()
{
    return m_data.data();
}
template<typename scalar>
inline const scalar*
DeepImage<scalar>::getDataPtr() const
{
    return m_data.data();
}

template<typename scalar>
inline PixelPosition
DeepImage< scalar >::clamp(const PixelPosition& pos) const
{
    return PixelPosition(
        std::max(0, std::min(pos.m_line, m_height - 1)),
        std::max(0, std::min(pos.m_column, m_width - 1)));
}

template<typename scalar>
inline int
DeepImage<scalar>::glueIndices(int i_line,
                               int i_column,
                               int i_dimensionIndex) const
{
    assert(i_line >= 0);
    assert(i_line < m_height);
    assert(i_column >= 0);
    assert(i_column < m_width);
    assert(i_dimensionIndex >= 0);
    assert(i_dimensionIndex < m_depth);

    return i_line * m_widthTimesDepth + i_column * m_depth + i_dimensionIndex;
}

template<typename scalar>
inline int
DeepImage<scalar>::glueIndices(int i_width,
                               int i_height,
                               int i_depth,
                               int i_line,
                               int i_column,
                               int i_dimensionIndex)
{
    assert(i_line >= 0);
    assert(i_line < i_height);
    assert(i_column >= 0);
    assert(i_column < i_width);
    assert(i_dimensionIndex >= 0);
    assert(i_dimensionIndex < i_depth);

    return (i_line * i_width + i_column) * i_depth + i_dimensionIndex;
}

template<typename scalar>
inline void
DeepImage<scalar>::splitIndex(int& o_rLine,
                              int& o_rColumn,
                              int& o_rDimensionIndex,
                              int i_buffer1DIndex) const
{
    o_rLine = i_buffer1DIndex / m_widthTimesDepth;
    o_rColumn = (i_buffer1DIndex / m_depth) % m_width;
    o_rDimensionIndex = i_buffer1DIndex % m_depth;
}

template<typename scalar>
inline void
DeepImage<scalar>::splitIndex(int& o_rLine,
                              int& o_rColumn,
                              int& o_rDimensionIndex,
                              int i_buffer1DIndex,
                              int i_width,
                              int i_height,
                              int i_depth)
{
    o_rLine = i_buffer1DIndex / (i_width * i_depth);
    o_rColumn = (i_buffer1DIndex / i_depth) % i_width;
    o_rDimensionIndex = i_buffer1DIndex % i_depth;
}

template<typename scalar>
inline const scalar&
DeepImage<scalar>::get(int i_line, int i_column, int i_dimensionIndex) const
{
    return m_data[glueIndices(i_line, i_column, i_dimensionIndex)];
}

template<typename scalar>
inline scalar&
DeepImage<scalar>::get(int i_line, int i_column, int i_dimensionIndex)
{
    return m_data[glueIndices(i_line, i_column, i_dimensionIndex)];
}

template<typename scalar>
inline const scalar&
DeepImage<scalar>::get(PixelPosition i_pixel, int i_dimensionIndex) const
{
    return m_data[glueIndices(
      i_pixel.m_line, i_pixel.m_column, i_dimensionIndex)];
}

template<typename scalar>
inline scalar&
DeepImage<scalar>::get(PixelPosition i_pixel, int i_dimensionIndex)
{
    return m_data[glueIndices(
      i_pixel.m_line, i_pixel.m_column, i_dimensionIndex)];
}

template<typename scalar>
inline const scalar&
DeepImage<scalar>::get(int i_buffer1DIndex) const
{
    return m_data[i_buffer1DIndex];
}

template<typename scalar>
inline scalar&
DeepImage<scalar>::get(int i_buffer1DIndex)
{
    return m_data[i_buffer1DIndex];
}

template<typename scalar>
inline const scalar
DeepImage<scalar>::getValue(PixelPosition i_pixel, int i_dimensionIndex) const
{
    return m_data[glueIndices(
      i_pixel.m_line, i_pixel.m_column, i_dimensionIndex)];
}

template<typename scalar>
inline scalar
DeepImage<scalar>::getValue(PixelPosition i_pixel, int i_dimensionIndex)
{
    return m_data[glueIndices(
      i_pixel.m_line, i_pixel.m_column, i_dimensionIndex)];
}

template<typename scalar>
inline void
DeepImage<scalar>::set(int i_line,
                       int i_column,
                       int i_dimensionIndex,
                       scalar i_value)
{
    m_data[glueIndices(i_line, i_column, i_dimensionIndex)] = i_value;
}

template<typename scalar>
inline void
DeepImage<scalar>::set(PixelPosition i_pixel,
                       int i_dimensionIndex,
                       scalar i_value)
{
    m_data[glueIndices(i_pixel.m_line, i_pixel.m_column, i_dimensionIndex)] = i_value;
}

template<typename scalar>
inline void
DeepImage<scalar>::set(int i_buffer1DIndex, scalar i_value)
{
    m_data[i_buffer1DIndex] = i_value;
}

template<typename scalar>
inline void
DeepImage<scalar>::set(int i_line, int i_column, const scalar* i_pVectorValue)
{
    std::copy(i_pVectorValue, i_pVectorValue + static_cast<size_t>(m_depth), &(get(i_line, i_column, 0)));
}

template<typename scalar>
inline void
DeepImage<scalar>::set(PixelPosition i_pixel, const scalar* i_pVectorValue)
{
    std::copy(i_pVectorValue, i_pVectorValue + static_cast<size_t>(m_depth), &(get(i_pixel)));
}

template<typename scalar>
inline void
DeepImage<scalar>::isotropicalScale(scalar i_scaleFactor)
{
    for (auto it = m_data.begin(); it != m_data.end(); it++)
      *it *= i_scaleFactor;
}

template<typename scalar>
inline void
DeepImage<scalar>::anisotropicalScale(const scalar* i_scaleFactors)
{
    int size = getSize();
    for (int d = 0; d < size; d++)
      m_data[d] *= i_scaleFactors[d % m_depth];
}

template<typename scalar>
inline void
DeepImage<scalar>::fill(scalar f)
{
    std::fill(m_data.begin(), m_data.end(), f);
}

template<typename scalar>
inline bool
DeepImage<scalar>::isEmpty() const
{
    return m_width == 0 || m_height == 0 || m_depth == 0;
}

template<typename scalar>
inline void
DeepImage<scalar>::clearAndFreeMemory()
{
    m_width = m_height = m_depth = m_widthTimesDepth = 0;
    std::vector<scalar>().swap(m_data); // swap trick to free memory
}

template<typename scalar>
inline typename DeepImage<scalar>::iterator
DeepImage<scalar>::begin()
{
    return DeepImage<scalar>::iterator(m_data.data(), m_depth);
}

template<typename scalar>
inline typename DeepImage<scalar>::iterator
DeepImage<scalar>::end()
{
    return DeepImage<scalar>::iterator(
      m_data.data() + m_widthTimesDepth * m_height, m_depth);
}

template<typename scalar>
inline typename DeepImage<scalar>::const_iterator
DeepImage<scalar>::begin() const
{
    return DeepImage<scalar>::const_iterator(m_data.data(), m_depth);
}

template<typename scalar>
inline typename DeepImage<scalar>::const_iterator
DeepImage<scalar>::end() const
{
    return DeepImage<scalar>::const_iterator(
      m_data.data() + m_widthTimesDepth * m_height, m_depth);
}

template<typename scalar>
inline DeepImage<scalar>&
DeepImage<scalar>::operator+=(const DeepImage& i_rImage)
{
    typename std::vector<scalar>::const_iterator it = i_rImage.m_data.cbegin();
    for (scalar& rValue : m_data)
      rValue += *it++;

    return *this;
}

template<typename scalar>
inline DeepImage<scalar>&
DeepImage<scalar>::operator-=(const DeepImage& i_rImage)
{
    typename std::vector<scalar>::const_iterator it = i_rImage.m_data.cbegin();
    for (scalar& rValue : m_data)
      rValue -= *it++;

    return *this;
}

// ------------------------ ImageWindow<>::iterator ------------------------

template<typename scalar>
inline ImageWindow<scalar>::iterator::iterator()
  : m_width(0)
  , m_height(0)
  , m_depth(0)
  , m_minCorner()
  , m_maxCorner()
  , m_currentPixel()
  , m_pCurrentDataPointer(nullptr)
{
}

template<typename scalar>
inline ImageWindow<scalar>::iterator::iterator(DeepImage<scalar>& i_rImage,
                                               PixelPosition i_centralPixel,
                                               int i_radius,
                                               int i_border)
{
    reset(i_rImage, i_centralPixel, i_radius, i_border);
}

template<typename scalar>
inline ImageWindow<scalar>::iterator::iterator(int i_width,
                                               int i_height,
                                               int i_depth,
                                               PixelPosition i_minCorner,
                                               PixelPosition i_maxCorner,
                                               PixelPosition i_currentPixel,
                                               scalar* i_pCurrentDataPointer)
  : m_width(i_width)
  , m_height(i_height)
  , m_depth(i_depth)
  , m_minCorner(i_minCorner)
  , m_maxCorner(i_maxCorner)
  , m_currentPixel(i_currentPixel)
  , m_pCurrentDataPointer(i_pCurrentDataPointer)
{
}

template<typename scalar>
inline void
ImageWindow<scalar>::iterator::reset(DeepImage<scalar>& i_rImage,
                                     PixelPosition i_centralPixel,
                                     int i_radius,
                                     int i_border)
{
    m_width = i_rImage.getWidth();
    m_height = i_rImage.getHeight();
    m_depth = i_rImage.getDepth();
    m_minCorner =
      PixelPosition(std::max(i_border, i_centralPixel.m_line - i_radius),
                    std::max(i_border, i_centralPixel.m_column - i_radius));
    m_maxCorner = PixelPosition(
      std::min(m_height - 1 - i_border, i_centralPixel.m_line + i_radius),
      std::min(m_width - 1 - i_border, i_centralPixel.m_column + i_radius));
    m_currentPixel = m_minCorner;
    m_pCurrentDataPointer = &(i_rImage.get(m_currentPixel, 0));
}

template<typename scalar>
inline ImageWindowSize
ImageWindow<scalar>::iterator::getSize() const
{
    return ImageWindowSize(1, 1) + m_maxCorner - m_minCorner;
}

template<typename scalar>
inline scalar* ImageWindow<scalar>::iterator::operator*() const
{
    return m_pCurrentDataPointer;
}

template<typename scalar>
inline typename ImageWindow<scalar>::iterator& ImageWindow<scalar>::iterator::
operator++()
{
    if (m_currentPixel.m_column == m_maxCorner.m_column)
    {
      m_currentPixel.m_line++;
      m_currentPixel.m_column = m_minCorner.m_column;
      m_pCurrentDataPointer +=
        m_depth * (m_width + m_minCorner.m_column - m_maxCorner.m_column);
    }
    else
    {
      m_currentPixel.m_column++;
      m_pCurrentDataPointer += m_depth;
    }

    return *this;
}

template<typename scalar>
inline bool
ImageWindow<scalar>::iterator::hasEnded() const
{
    return m_currentPixel.m_line > m_maxCorner.m_line;
}

template<typename scalar>
inline scalar& ImageWindow<scalar>::iterator::operator[](
  int i_dimensionIndex) const
{
    return m_pCurrentDataPointer[i_dimensionIndex];
}

template<typename scalar>
inline bool
ImageWindow<scalar>::iterator::operator!=(const iterator& i_rIt) const
{
    return m_pCurrentDataPointer != i_rIt.m_pCurrentDataPointer;
}

// ------------------------ ImageWindow<> ------------------------

template<typename scalar>
inline ImageWindow<scalar>::ImageWindow()
  : m_width(0)
  , m_height(0)
  , m_depth(0)
  , m_minCorner()
  , m_maxCorner()
  , m_pMinCornerDataPointer(nullptr)
{
}

template<typename scalar>
inline ImageWindow<scalar>::ImageWindow(DeepImage<scalar>& i_rImage,
                                        PixelPosition i_centralPixel,
                                        int i_radius,
                                        int i_border)
{
    reset(i_rImage, i_centralPixel, i_radius, i_border);
}

template<typename scalar>
inline void
ImageWindow<scalar>::reset(DeepImage<scalar>& i_rImage,
                           PixelPosition i_centralPixel,
                           int i_radius,
                           int i_border)
{
    m_width = i_rImage.getWidth();
    m_height = i_rImage.getHeight();
    m_depth = i_rImage.getDepth();
    m_minCorner =
      PixelPosition(std::max(i_border, i_centralPixel.m_line - i_radius),
                    std::max(i_border, i_centralPixel.m_column - i_radius));
    m_maxCorner = PixelPosition(
      std::min(m_height - 1 - i_border, i_centralPixel.m_line + i_radius),
      std::min(m_width - 1 - i_border, i_centralPixel.m_column + i_radius));
    m_pMinCornerDataPointer = &(i_rImage.get(m_minCorner, 0));
}

template<typename scalar>
inline ImageWindowSize
ImageWindow<scalar>::getSize() const
{
    return ImageWindowSize(1, 1) + m_maxCorner - m_minCorner;
}

template<typename scalar>
inline typename ImageWindow<scalar>::iterator
ImageWindow<scalar>::begin() const
{
    return iterator(m_width,
                    m_height,
                    m_depth,
                    m_minCorner,
                    m_maxCorner,
                    m_minCorner,
                    m_pMinCornerDataPointer);
}

template<typename scalar>
inline typename ImageWindow<scalar>::iterator
ImageWindow<scalar>::end() const
{
    return iterator(m_width,
                    m_height,
                    m_depth,
                    m_minCorner,
                    m_maxCorner,
                    PixelPosition(m_maxCorner.m_line + 1, m_minCorner.m_column),
                    m_pMinCornerDataPointer +
                      (1 + m_maxCorner.m_line - m_minCorner.m_line) * m_depth *
                        m_width);
}

// ------------------------ ConstImageWindow<>::iterator
// ------------------------

template<typename scalar>
inline ConstImageWindow<scalar>::iterator::iterator()
  : m_width(0)
  , m_height(0)
  , m_depth(0)
  , m_minCorner()
  , m_maxCorner()
  , m_currentPixel()
  , m_pCurrentDataPointer(nullptr)
{
}

template<typename scalar>
inline ConstImageWindow<scalar>::iterator::iterator(
  const DeepImage<scalar>& i_rImage,
  PixelPosition i_centralPixel,
  int i_radius,
  int i_border)
{
    reset(i_rImage, i_centralPixel, i_radius, i_border);
}

template<typename scalar>
inline ConstImageWindow<scalar>::iterator::iterator(
  int i_width,
  int i_height,
  int i_depth,
  PixelPosition i_minCorner,
  PixelPosition i_maxCorner,
  PixelPosition i_currentPixel,
  const scalar* i_pCurrentDataPointer)
  : m_width(i_width)
  , m_height(i_height)
  , m_depth(i_depth)
  , m_minCorner(i_minCorner)
  , m_maxCorner(i_maxCorner)
  , m_currentPixel(i_currentPixel)
  , m_pCurrentDataPointer(i_pCurrentDataPointer)
{
}

template<typename scalar>
inline void
ConstImageWindow<scalar>::iterator::reset(const DeepImage<scalar>& i_rImage,
                                          PixelPosition i_centralPixel,
                                          int i_radius,
                                          int i_border)
{
    m_width = i_rImage.getWidth();
    m_height = i_rImage.getHeight();
    m_depth = i_rImage.getDepth();
    m_minCorner =
      PixelPosition(std::max(i_border, i_centralPixel.m_line - i_radius),
                    std::max(i_border, i_centralPixel.m_column - i_radius));
    m_maxCorner = PixelPosition(
      std::min(m_height - 1 - i_border, i_centralPixel.m_line + i_radius),
      std::min(m_width - 1 - i_border, i_centralPixel.m_column + i_radius));
    m_currentPixel = m_minCorner;
    m_pCurrentDataPointer = &(i_rImage.get(m_currentPixel, 0));
}

template<typename scalar>
inline ImageWindowSize
ConstImageWindow<scalar>::iterator::getSize() const
{
    return ImageWindowSize(1, 1) + m_maxCorner - m_minCorner;
}

template<typename scalar>
inline const scalar* ConstImageWindow<scalar>::iterator::operator*() const
{
    return m_pCurrentDataPointer;
}

template<typename scalar>
inline typename ConstImageWindow<scalar>::iterator&
  ConstImageWindow<scalar>::iterator::operator++()
{
    if (m_currentPixel.m_column == m_maxCorner.m_column)
    {
      m_currentPixel.m_line++;
      m_currentPixel.m_column = m_minCorner.m_column;
      m_pCurrentDataPointer +=
        m_depth * (m_width + m_minCorner.m_column - m_maxCorner.m_column);
    }
    else
    {
      m_currentPixel.m_column++;
      m_pCurrentDataPointer += m_depth;
    }
    return *this;
}

template<typename scalar>
inline bool
ConstImageWindow<scalar>::iterator::hasEnded() const
{
    return m_currentPixel.m_line > m_maxCorner.m_line;
}

template<typename scalar>
inline const scalar& ConstImageWindow<scalar>::iterator::operator[](
  int i_dimensionIndex) const
{
    return m_pCurrentDataPointer[i_dimensionIndex];
}

template<typename scalar>
inline bool
ConstImageWindow<scalar>::iterator::operator!=(const iterator& i_rIt) const
{
    return m_pCurrentDataPointer != i_rIt.m_pCurrentDataPointer;
}

// ------------------------ ImageWindow<> ------------------------

template<typename scalar>
inline ConstImageWindow<scalar>::ConstImageWindow()
  : m_width(0)
  , m_height(0)
  , m_depth(0)
  , m_minCorner()
  , m_maxCorner()
  , m_pMinCornerDataPointer(nullptr)
{
}

template<typename scalar>
inline ConstImageWindow<scalar>::ConstImageWindow(
  const DeepImage<scalar>& i_rImage,
  PixelPosition i_centralPixel,
  int i_radius,
  int i_border)
{
    reset(i_rImage, i_centralPixel, i_radius, i_border);
}

template<typename scalar>
inline void
ConstImageWindow<scalar>::reset(const DeepImage<scalar>& i_rImage,
                                PixelPosition i_centralPixel,
                                int i_radius,
                                int i_border)
{
    m_width = i_rImage.getWidth();
    m_height = i_rImage.getHeight();
    m_depth = i_rImage.getDepth();
    m_minCorner =
      PixelPosition(std::max(i_border, i_centralPixel.m_line - i_radius),
                    std::max(i_border, i_centralPixel.m_column - i_radius));
    m_maxCorner = PixelPosition(
      std::min(m_height - 1 - i_border, i_centralPixel.m_line + i_radius),
      std::min(m_width - 1 - i_border, i_centralPixel.m_column + i_radius));
    m_pMinCornerDataPointer = &(i_rImage.get(m_minCorner, 0));
}

template<typename scalar>
inline ImageWindowSize
ConstImageWindow<scalar>::getSize() const
{
    return ImageWindowSize(1, 1) + m_maxCorner - m_minCorner;
}

template<typename scalar>
inline typename ConstImageWindow<scalar>::iterator
ConstImageWindow<scalar>::begin() const
{
    return ConstImageWindow<scalar>::iterator(m_width,
                                              m_height,
                                              m_depth,
                                              m_minCorner,
                                              m_maxCorner,
                                              m_minCorner,
                                              m_pMinCornerDataPointer);
}

template<typename scalar>
inline typename ConstImageWindow<scalar>::iterator
ConstImageWindow<scalar>::end() const
{
    return ConstImageWindow<scalar>::iterator(
      m_width,
      m_height,
      m_depth,
      m_minCorner,
      m_maxCorner,
      PixelPosition(m_maxCorner.m_line + 1, m_minCorner.m_column),
      m_pMinCornerDataPointer +
        (1 + m_maxCorner.m_line - m_minCorner.m_line) * m_depth * m_width);
}

} // namespace bcd

#endif // DEEP_IMAGE_H
