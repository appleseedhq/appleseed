// (C) Copyright 2008 CodeRage, LLC (turkanis at coderage dot com)
// (C) Copyright 2003-2007 Jonathan Turkanis
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt.)

// See http://www.boost.org/libs/iostreams for documentation.

// Inspired by fdstream.hpp, (C) Copyright Nicolai M. Josuttis 2001,
// available at http://www.josuttis.com/cppcode/fdstream.html.

// Define BOOST_IOSTREAMS_SOURCE so that <boost/iostreams/detail/config.hpp>
// knows that we are building the library (possibly exporting code), rather
// than using it (possibly importing code).
#define BOOST_IOSTREAMS_SOURCE

#include <cassert>
#include <cerrno>
#include <cstdio>                                 // SEEK_SET, etc.
#include <boost/config.hpp>                       // BOOST_JOIN
#include <boost/iostreams/detail/error.hpp>
#include <boost/iostreams/detail/config/dyn_link.hpp>
#include <boost/iostreams/detail/config/rtl.hpp>  // BOOST_IOSTREAMS_FD_XXX
#include <boost/iostreams/detail/config/windows_posix.hpp>
#include <boost/iostreams/detail/ios.hpp>         // openmodes, failure.
#include <boost/iostreams/device/file_descriptor.hpp>
#include <boost/integer_traits.hpp>

    // OS-specific headers for low-level i/o.

#include <fcntl.h>       // file opening flags.
#include <sys/stat.h>    // file access permissions.
#ifdef BOOST_IOSTREAMS_WINDOWS
# include <io.h>         // low-level file i/o.
# define WINDOWS_LEAN_AND_MEAN
# include <windows.h>
# ifndef INVALID_SET_FILE_POINTER
#  define INVALID_SET_FILE_POINTER ((DWORD)-1)
# endif
#else
# include <sys/types.h>  // mode_t.
# include <unistd.h>     // low-level file i/o.
#endif

namespace boost { namespace iostreams {

//------------------Implementation of file_descriptor-------------------------//

file_descriptor::file_descriptor() : pimpl_(new impl) { }

file_descriptor::file_descriptor(handle_type fd, bool close_on_exit)
    : pimpl_(new impl(fd, close_on_exit))
    { }

#ifdef BOOST_IOSTREAMS_WINDOWS
    file_descriptor::file_descriptor(int fd, bool close_on_exit)
        : pimpl_(new impl(int_to_handle(fd), close_on_exit))
        { }
#endif

file_descriptor::file_descriptor( const char* path,
                                  BOOST_IOS::openmode mode,
                                  BOOST_IOS::openmode base_mode )
    : pimpl_(new impl)
{ open(std::string(path), mode, base_mode); }

file_descriptor::file_descriptor( const std::string& path,
                                  BOOST_IOS::openmode mode,
                                  BOOST_IOS::openmode base_mode )
    : pimpl_(new impl)
{ open(path, mode, base_mode); }

void file_descriptor::open
    ( const std::string& path, BOOST_IOS::openmode m,
      BOOST_IOS::openmode base )
{
    using namespace std;
    m |= base;
#ifdef BOOST_IOSTREAMS_WINDOWS //---------------------------------------------//
    DWORD dwDesiredAccess;
    DWORD dwCreationDisposition;
    if ( (m & (BOOST_IOS::in | BOOST_IOS::out))
             ==
         (BOOST_IOS::in | BOOST_IOS::out) )
    {
        if (m & BOOST_IOS::app)
            throw BOOST_IOSTREAMS_FAILURE("bad open mode");
        dwDesiredAccess = GENERIC_READ | GENERIC_WRITE;
        dwCreationDisposition =
            (m & BOOST_IOS::trunc) ?
                OPEN_ALWAYS :
                OPEN_EXISTING;
    } else if (m & BOOST_IOS::in) {
        if (m & (BOOST_IOS::app |BOOST_IOS::trunc))
            throw BOOST_IOSTREAMS_FAILURE("bad open mode");
        dwDesiredAccess = GENERIC_READ;
        dwCreationDisposition = OPEN_EXISTING;
    } else if (m & BOOST_IOS::out) {
        dwDesiredAccess = GENERIC_WRITE;
        dwCreationDisposition = OPEN_ALWAYS;
        if (m & BOOST_IOS::app)
            pimpl_->flags_ |= impl::append;
    } else {
        throw BOOST_IOSTREAMS_FAILURE("bad open mode");
    }

    HANDLE handle =
        ::CreateFileA( path.c_str(),
                       dwDesiredAccess,
                       FILE_SHARE_READ | FILE_SHARE_WRITE,
                       NULL,                   // lpSecurityAttributes
                       dwCreationDisposition,
                       FILE_ATTRIBUTE_NORMAL,
                       NULL );                 // hTemplateFile
    if (handle != INVALID_HANDLE_VALUE) {
        pimpl_->handle_ = handle;
        pimpl_->flags_ |= impl::close_on_exit;
    } else {
        pimpl_->flags_ = 0;
        throw BOOST_IOSTREAMS_FAILURE("bad open");
    }
#else // #ifdef BOOST_IOSTREAMS_WINDOWS //------------------------------------//

        // Calculate oflag argument to open.

    int oflag = 0;
    if ( (m & (BOOST_IOS::in | BOOST_IOS::out))
             ==
         (BOOST_IOS::in | BOOST_IOS::out) )
    {
        assert(!(m & BOOST_IOS::app));
        oflag |= O_RDWR;
    } else if (m & BOOST_IOS::in) {
        assert(!(m & (BOOST_IOS::app |BOOST_IOS::trunc)));
        oflag |= O_RDONLY;
    } else if (m & BOOST_IOS::out) {
        oflag |= O_WRONLY;
        m |= BOOST_IOS::trunc;
        if (m & BOOST_IOS::app)
            oflag |= O_APPEND;
    }
    if (m & BOOST_IOS::trunc)
        oflag |= O_CREAT;
    #ifdef _LARGEFILE64_SOURCE
        oflag |= O_LARGEFILE;
    #endif

        // Calculate pmode argument to open.

    mode_t pmode = S_IRUSR | S_IWUSR |
                   S_IRGRP | S_IWGRP |
                   S_IROTH | S_IWOTH;

        // Open file.

    int fd = BOOST_IOSTREAMS_FD_OPEN(path.c_str(), oflag, pmode);
    if (fd == -1) {
        throw BOOST_IOSTREAMS_FAILURE("bad open");
    } else {
        pimpl_->handle_ = fd;
        pimpl_->flags_ = impl::close_on_exit;
    }
#endif // #ifndef BOOST_IOSTREAMS_WINDOWS //----------------------------------//
}

void file_descriptor::open
    ( const char* path, BOOST_IOS::openmode m,
      BOOST_IOS::openmode base )
{ open(std::string(path), m, base); }

std::streamsize file_descriptor::read(char_type* s, std::streamsize n)
{
#ifdef BOOST_IOSTREAMS_WINDOWS
    DWORD result;
    if (!::ReadFile(pimpl_->handle_, s, n, &result, NULL))
        throw detail::bad_read();
    return result == 0 ? -1 : static_cast<std::streamsize>(result);
#else // #ifdef BOOST_IOSTREAMS_WINDOWS
    errno = 0;
    std::streamsize result = BOOST_IOSTREAMS_FD_READ(pimpl_->handle_, s, n);
    if (errno != 0)
        throw detail::bad_read();
    return result == 0 ? -1 : result;
#endif // #ifdef BOOST_IOSTREAMS_WINDOWS
}

std::streamsize file_descriptor::write(const char_type* s, std::streamsize n)
{
#ifdef BOOST_IOSTREAMS_WINDOWS
    if (pimpl_->flags_ & impl::append) {
        DWORD const dwResult =
            ::SetFilePointer(pimpl_->handle_, 0, NULL, FILE_END);
        if ( dwResult == INVALID_SET_FILE_POINTER &&
             ::GetLastError() != NO_ERROR )
        {
            throw detail::bad_seek();
        }
    }
    DWORD ignore;
    if (!::WriteFile(pimpl_->handle_, s, n, &ignore, NULL))
        throw detail::bad_write();
    return n;
#else // #ifdef BOOST_IOSTREAMS_WINDOWS
    int amt = BOOST_IOSTREAMS_FD_WRITE(pimpl_->handle_, s, n);
    if (amt < n)
        throw detail::bad_write(); // Handles blocking fd's only.
    return n;
#endif // #ifdef BOOST_IOSTREAMS_WINDOWS
}

std::streampos file_descriptor::seek
    (stream_offset off, BOOST_IOS::seekdir way)
{
    using namespace std;
#ifdef BOOST_IOSTREAMS_WINDOWS
    LONG lDistanceToMove = static_cast<LONG>(off & 0xffffffff);
    LONG lDistanceToMoveHigh = static_cast<LONG>(off >> 32);
    DWORD dwResultLow =
        ::SetFilePointer( pimpl_->handle_,
                          lDistanceToMove,
                          &lDistanceToMoveHigh,
                          way == BOOST_IOS::beg ?
                              FILE_BEGIN :
                              way == BOOST_IOS::cur ?
                                FILE_CURRENT :
                                FILE_END );
    if ( dwResultLow == INVALID_SET_FILE_POINTER &&
         ::GetLastError() != NO_ERROR )
    {
        throw detail::bad_seek();
    } else {
       return offset_to_position(
   	              (stream_offset(lDistanceToMoveHigh) << 32) + dwResultLow
              );
    }
#else // #ifdef BOOST_IOSTREAMS_WINDOWS
    if ( off > integer_traits<BOOST_IOSTREAMS_FD_OFFSET>::const_max ||
         off < integer_traits<BOOST_IOSTREAMS_FD_OFFSET>::const_min )
    {
        throw BOOST_IOSTREAMS_FAILURE("bad offset");
    }
    stream_offset result =
        BOOST_IOSTREAMS_FD_SEEK(
            pimpl_->handle_,
            static_cast<BOOST_IOSTREAMS_FD_OFFSET>(off),
            ( way == BOOST_IOS::beg ?
                  SEEK_SET :
                  way == BOOST_IOS::cur ?
                      SEEK_CUR :
                      SEEK_END ) 
        );
    if (result == -1)
        throw detail::bad_seek();
    return offset_to_position(result);
#endif // #ifdef BOOST_IOSTREAMS_WINDOWS
}

void file_descriptor::close() { close_impl(*pimpl_); }

void file_descriptor::close_impl(impl& i)
{
#ifdef BOOST_IOSTREAMS_WINDOWS
    if (i.handle_ != reinterpret_cast<handle_type>(-1)) {
        if (!::CloseHandle(i.handle_))
            throw BOOST_IOSTREAMS_FAILURE("bad close");
        i.handle_ = reinterpret_cast<handle_type>(-1);
        i.flags_ = 0;
        return;
    }
#else // #ifdef BOOST_IOSTREAMS_WINDOWS
    if (i.handle_ != -1) {
        if (BOOST_IOSTREAMS_FD_CLOSE(i.handle_) == -1)
            throw BOOST_IOSTREAMS_FAILURE("bad close");
        i.handle_ = -1;
        i.flags_ = 0;
    }
#endif // #ifdef BOOST_IOSTREAMS_WINDOWS
}

#ifdef BOOST_IOSTREAMS_WINDOWS
file_descriptor::handle_type file_descriptor::int_to_handle(int fd)
{
    return reinterpret_cast<handle_type>(_get_osfhandle(fd));
}
#endif

} } // End namespaces iostreams, boost.
