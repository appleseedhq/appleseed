// (C) Copyright 2005 Matthias Troyer 

// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//  Authors: Matthias Troyer

#define BOOST_ARCHIVE_SOURCE
#include <boost/archive/impl/archive_pointer_iserializer.ipp>
#include <boost/archive/impl/archive_pointer_oserializer.ipp>
#include <boost/mpi/packed_iarchive.hpp>

#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/impl/basic_binary_iprimitive.ipp>
#include <boost/archive/impl/basic_binary_iarchive.ipp>

namespace boost { namespace archive {

// explicitly instantiate all required templates

template class basic_binary_iarchive<mpi::packed_iarchive> ;
template class detail::archive_pointer_iserializer<mpi::packed_iarchive> ;
//template class binary_iarchive_impl<mpi_packed_iarchive> ;

} } // end namespace boost::archive
