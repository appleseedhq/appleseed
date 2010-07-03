// (C) Copyright 2008 CodeRage, LLC (turkanis at coderage dot com)
// (C) Copyright 2004-2007 Jonathan Turkanis
// (C) Copyright Craig Henderson 2002 'boost/memmap.hpp' from sandbox
// (C) Copyright Jonathan Graehl 2004.

// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt.)

// See http://www.boost.org/libs/iostreams for documentation.

// Define BOOST_IOSTREAMS_SOURCE so that <boost/iostreams/detail/config.hpp>
// knows that we are building the library (possibly exporting code), rather
// than using it (possibly importing code).
#define BOOST_IOSTREAMS_SOURCE

#include <cassert>
#ifndef NDEBUG
# include <boost/iostreams/detail/absolute_path.hpp>
#endif
#include <boost/iostreams/detail/config/dyn_link.hpp>
#include <boost/iostreams/detail/config/windows_posix.hpp>
#include <boost/iostreams/detail/ios.hpp>  // failure.
#include <boost/iostreams/detail/system_failure.hpp>
#include <boost/iostreams/device/mapped_file.hpp>

#ifdef BOOST_IOSTREAMS_WINDOWS
# define WIN32_LEAN_AND_MEAN  // Exclude rarely-used stuff from Windows headers
# include <windows.h>
# ifndef INVALID_SET_FILE_POINTER
#  define INVALID_SET_FILE_POINTER ((DWORD)-1)
# endif
#else
# include <errno.h>
# include <fcntl.h>
# include <sys/mman.h>      // mmap, munmap.
# include <sys/stat.h>
# include <sys/types.h>     // struct stat.
# include <unistd.h>        // sysconf.
#endif

#include <boost/iostreams/detail/config/disable_warnings.hpp>

namespace boost { namespace iostreams {

namespace detail {

struct mapped_file_impl {
    mapped_file_impl() { clear(false); }
    ~mapped_file_impl() { try { close(); } catch (...) { } }
    void clear(bool error)
    {
        data_ = 0;
        size_ = 0;
        mode_ = BOOST_IOS::openmode();
        error_ = error;
    #ifdef BOOST_IOSTREAMS_WINDOWS
        handle_ = INVALID_HANDLE_VALUE;
        mapped_handle_ = NULL;
    #else
        handle_ = 0;
    #endif
    #ifndef NDEBUG
        path_.erase();
    #endif
    }
    void close()
    {
        bool error = false;
    #ifdef BOOST_IOSTREAMS_WINDOWS
        if (handle_ == INVALID_HANDLE_VALUE)
            return;
        error = !::UnmapViewOfFile(data_) || error;
        error = !::CloseHandle(mapped_handle_) || error;
        error = !::CloseHandle(handle_) || error;
        handle_ = INVALID_HANDLE_VALUE;
        mapped_handle_ = NULL;
    #else
        if (!handle_)
            return;
        error = ::munmap(reinterpret_cast<char*>(data_), size_) != 0 || error;
        error = ::close(handle_) != 0 || error;
        handle_ = 0;
    #endif
        data_ = 0;
        size_ = 0;
        mode_ = BOOST_IOS::openmode();
        if (error) {
            std::string msg("error closing mapped file");
            #ifndef NDEBUG
                msg += std::string(" (\"") + path_ + "\")";
            #endif
            throw_system_failure(msg);
        }
    #ifndef NDEBUG
        path_.erase();
    #endif
    }
    char*                data_;
    std::size_t          size_;
    BOOST_IOS::openmode  mode_;
    bool                 error_;
#ifdef BOOST_IOSTREAMS_WINDOWS
    HANDLE               handle_;
    HANDLE               mapped_handle_;
#else
    int                  handle_;
#endif
#ifndef NDEBUG
    std::string          path_;
#endif
};

} // End namespace detail.

//------------------Definition of mapped_file_source--------------------------//

mapped_file_source::mapped_file_source(mapped_file_params p) { open(p); }

mapped_file_source::mapped_file_source( const std::string& path,
                                        mapped_file_source::size_type length,
                                        boost::intmax_t offset )
{ open(path, length, offset); }

void mapped_file_source::open(mapped_file_params p)
{
    p.mode &= ~BOOST_IOS::out;
    open_impl(p);
}

void mapped_file_source::open( const std::string& path,
                               mapped_file_source::size_type length,
                               boost::intmax_t offset )
{
    mapped_file_params p(path);
    p.mode = BOOST_IOS::in;
    p.length = length;
    p.offset = offset;
    open_impl(p);
}

mapped_file_source::size_type mapped_file_source::size() const
{ return pimpl_->size_; }

void mapped_file_source::close() { pimpl_->close(); }

mapped_file_source::operator mapped_file_source::safe_bool() const
{
    return !!pimpl_ && pimpl_->error_ == false ?
        &safe_bool_helper::x : 0;
}

bool mapped_file_source::operator!() const
{ return !!pimpl_ || pimpl_->error_; }

BOOST_IOS::openmode mapped_file_source::mode() const { return pimpl_->mode_; }

const char* mapped_file_source::data() const { return pimpl_->data_; }

const char* mapped_file_source::begin() const { return data(); }

const char* mapped_file_source::end() const { return data() + size(); }

#ifdef BOOST_IOSTREAMS_WINDOWS //---------------------------------------------//

namespace detail {

void cleanup_and_throw(detail::mapped_file_impl& impl, std::string msg)
{
    #ifndef NDEBUG
        msg += std::string(" (\"") + impl.path_ + "\")";
    #endif
    if (impl.mapped_handle_ != INVALID_HANDLE_VALUE)
        ::CloseHandle(impl.mapped_handle_);
    if (impl.handle_ != NULL)
        ::CloseHandle(impl.handle_);
    impl.clear(true);
    throw_system_failure(msg);
}

} // End namespace detail.

void mapped_file_source::open_impl(mapped_file_params p)
{
    using namespace std;

    if (is_open())
        throw BOOST_IOSTREAMS_FAILURE("file already open");
    if (!pimpl_)
        pimpl_.reset(new impl_type);
    else
        pimpl_->clear(false);
    bool readonly = (p.mode & BOOST_IOS::out) == 0;
    pimpl_->mode_ = readonly ? BOOST_IOS::in : (BOOST_IOS::in | BOOST_IOS::out);
    #ifndef NDEBUG
        pimpl_->path_ = detail::absolute_path(p.path);
    #endif

    //--------------Open underlying file--------------------------------------//

    pimpl_->handle_ =
        ::CreateFileA( p.path.c_str(),
                       readonly ? GENERIC_READ : GENERIC_ALL,
                       FILE_SHARE_READ,
                       NULL,
                       (p.new_file_size != 0 && !readonly) ? 
                           CREATE_ALWAYS : 
                           OPEN_EXISTING,
                       readonly ?
                           FILE_ATTRIBUTE_READONLY :
                           FILE_ATTRIBUTE_TEMPORARY,
                       NULL );

    if (pimpl_->handle_ == INVALID_HANDLE_VALUE) {
        detail::cleanup_and_throw(*pimpl_, "failed opening file");
    }

    //--------------Set file size---------------------------------------------//

    if (p.new_file_size != 0 && !readonly) {
        LONG sizehigh = (p.new_file_size >> (sizeof(LONG) * 8));
        LONG sizelow = (p.new_file_size & 0xffffffff);
        DWORD result =
            ::SetFilePointer(pimpl_->handle_, sizelow, &sizehigh, FILE_BEGIN);
        if ( result == INVALID_SET_FILE_POINTER && 
                 ::GetLastError() != NO_ERROR || 
             !::SetEndOfFile(pimpl_->handle_) )
        {
            detail::cleanup_and_throw(*pimpl_, "failed setting file size");
        }
    }

    //--------------Create mapping--------------------------------------------//

    try_again: // Target of goto in following section.

    pimpl_->mapped_handle_ =
        ::CreateFileMappingA( pimpl_->handle_, NULL,
                              readonly ? PAGE_READONLY : PAGE_READWRITE,
                              0, 0, NULL );
    if (pimpl_->mapped_handle_ == NULL) {
        detail::cleanup_and_throw(*pimpl_, "couldn't create mapping");
    }

    //--------------Access data-----------------------------------------------//

    void* data =
        ::MapViewOfFileEx( pimpl_->mapped_handle_,
                           readonly ? FILE_MAP_READ : FILE_MAP_WRITE,
                           (DWORD) (p.offset >> 32),
                           (DWORD) (p.offset & 0xffffffff),
                           p.length != max_length ? p.length : 0, (LPVOID) p.hint );
    if (!data) {
        if (p.hint != 0) {
            p.hint = 0;
            goto try_again;
        }
        detail::cleanup_and_throw(*pimpl_, "failed mapping view");
    }

    //--------------Determing file size---------------------------------------//

    // Dynamically locate GetFileSizeEx (thanks to Pavel Vozenilik).
    typedef BOOL (WINAPI *func)(HANDLE, PLARGE_INTEGER);
    HMODULE hmod = ::GetModuleHandleA("kernel32.dll");
    func get_size =
        reinterpret_cast<func>(::GetProcAddress(hmod, "GetFileSizeEx"));

    if (get_size) {
        LARGE_INTEGER info;
        if (get_size(pimpl_->handle_, &info)) {
            boost::intmax_t size =
                ( (static_cast<boost::intmax_t>(info.HighPart) << 32) |
                  info.LowPart );
            pimpl_->size_ =
                static_cast<std::size_t>(
                    p.length != max_length ?
                        std::min<boost::intmax_t>(p.length, size) :
                        size
                );
        } else {
            detail::cleanup_and_throw(*pimpl_, "failed getting file size");
            return;
        }
    } else {
        DWORD hi;
        DWORD low;
        if ( (low = ::GetFileSize(pimpl_->handle_, &hi))
                 !=
             INVALID_FILE_SIZE )
        {
            boost::intmax_t size =
                (static_cast<boost::intmax_t>(hi) << 32) | low;
            pimpl_->size_ =
                static_cast<std::size_t>(
                    p.length != max_length ?
                        std::min<boost::intmax_t>(p.length, size) :
                        size
                );
        } else {
            detail::cleanup_and_throw(*pimpl_, "failed getting file size");
            return;
        }
    }

    pimpl_->data_ = reinterpret_cast<char*>(data);
}

bool mapped_file_source::is_open() const
{ return !!pimpl_ && pimpl_->handle_ != INVALID_HANDLE_VALUE; }

int mapped_file_source::alignment()
{
    SYSTEM_INFO info;
    ::GetSystemInfo(&info);
    return static_cast<int>(info.dwAllocationGranularity);
}

#else // #ifdef BOOST_IOSTREAMS_WINDOWS //------------------------------------//

namespace detail {

    void cleanup_and_throw(detail::mapped_file_impl& impl, std::string msg)
{
    #ifndef NDEBUG
        msg += std::string(" (\"") + impl.path_ + "\")";
    #endif
    if (impl.handle_ != 0)
        ::close(impl.handle_);
    impl.clear(true);
    throw_system_failure(msg);
}

} // End namespace detail.


void mapped_file_source::open_impl(mapped_file_params p)
{
    using namespace std;

    if (is_open())
        throw BOOST_IOSTREAMS_FAILURE("file already open");
    if (!pimpl_)
        pimpl_.reset(new impl_type);
    else
        pimpl_->clear(false);
    bool readonly = (p.mode & BOOST_IOS::out) == 0;
    pimpl_->mode_ = readonly ? BOOST_IOS::in : (BOOST_IOS::in | BOOST_IOS::out);
    #ifndef NDEBUG
        pimpl_->path_ = detail::absolute_path(p.path);
    #endif

    //--------------Open underlying file--------------------------------------//

    int flags = (readonly ? O_RDONLY : O_RDWR);
    if (p.new_file_size != 0 && !readonly)
        flags |= (O_CREAT | O_TRUNC);
    errno = 0;
    pimpl_->handle_ = ::open(p.path.c_str(), flags, S_IRWXU);
    if (errno != 0)
        detail::cleanup_and_throw(*pimpl_, "failed opening file");

    //--------------Set file size---------------------------------------------//

    if (p.new_file_size != 0 && !readonly)
        if (ftruncate(pimpl_->handle_, p.new_file_size) == -1)
            detail::cleanup_and_throw(*pimpl_, "failed setting file size");

    //--------------Determine file size---------------------------------------//

    bool success = true;
    struct stat info;
    if (p.length != max_length)
        pimpl_->size_ = p.length;
    else {
        success = ::fstat(pimpl_->handle_, &info) != -1;
        pimpl_->size_ = info.st_size;
    }
    if (!success)
        detail::cleanup_and_throw(*pimpl_, "failed getting file size");

    //--------------Create mapping--------------------------------------------//

    try_again: // Target of goto in following section.

    char* hint = const_cast<char*>(p.hint);
    void* data = ::mmap( hint, pimpl_->size_,
                         readonly ? PROT_READ : (PROT_READ | PROT_WRITE),
                         readonly ? MAP_PRIVATE : MAP_SHARED,
                         pimpl_->handle_, p.offset );
    if (data == MAP_FAILED) {
        if (hint != 0) {
            hint = 0;
            goto try_again;
        }
        detail::cleanup_and_throw(*pimpl_, "failed mapping file");
    }
    pimpl_->data_ = reinterpret_cast<char*>(data);

    return;
}

bool mapped_file_source::is_open() const
{ return !!pimpl_ && pimpl_->handle_ != 0; }

int mapped_file_source::alignment()
{ return static_cast<int>(sysconf(_SC_PAGESIZE)); }

#endif // #ifdef BOOST_IOSTREAMS_WINDOWS //-----------------------------------//

//------------------Implementation of mapped_file-----------------------------//

mapped_file::mapped_file(mapped_file_params p) { delegate_.open_impl(p); }

mapped_file::mapped_file( const std::string& path, BOOST_IOS::openmode mode,
                          size_type length, stream_offset offset )
{ open(path, mode, length, offset); }

void mapped_file::open(mapped_file_params p)
{ delegate_.open_impl(p); }

void mapped_file::open( const std::string& path, BOOST_IOS::openmode mode,
                        size_type length, stream_offset offset )
{
    mapped_file_params p(path);
    p.mode = mode;
    p.length = length;
    p.offset = offset;
    open(p);
}

//------------------Implementation of mapped_file_sink------------------------//

mapped_file_sink::mapped_file_sink(mapped_file_params p) { open(p); }

mapped_file_sink::mapped_file_sink( const std::string& path,
                                    size_type length, stream_offset offset )
{ open(path, length, offset); }

void mapped_file_sink::open(mapped_file_params p)
{
    p.mode |= BOOST_IOS::out;
    p.mode &= ~BOOST_IOS::in;
    mapped_file::open(p);
}

void mapped_file_sink::open( const std::string& path, size_type length,
                             stream_offset offset )
{
    mapped_file_params p(path);
    p.mode = BOOST_IOS::out;
    p.length = length;
    p.offset = offset;
    open(p);
}

//----------------------------------------------------------------------------//

} } // End namespaces iostreams, boost.

#include <boost/iostreams/detail/config/enable_warnings.hpp>
