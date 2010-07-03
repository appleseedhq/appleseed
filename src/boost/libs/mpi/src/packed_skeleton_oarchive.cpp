// (C) Copyright 2005 Matthias Troyer

// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

//  Authors: Matthias Troyer

#define BOOST_ARCHIVE_SOURCE
#include <boost/mpi/skeleton_and_content.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/impl/basic_binary_oprimitive.ipp>
#include <boost/archive/impl/basic_binary_oarchive.ipp>
#include <boost/archive/impl/archive_pointer_oserializer.ipp>

namespace boost { namespace archive {
// explicitly instantiate all required templates

template class detail::archive_pointer_oserializer<mpi::packed_skeleton_oarchive> ;
template class detail::archive_pointer_oserializer<
  mpi::detail::forward_skeleton_oarchive<
    boost::mpi::packed_skeleton_oarchive, boost::mpi::packed_oarchive> > ;
template class basic_binary_oarchive<mpi::packed_skeleton_oarchive> ;
//template class binary_oarchive_impl<mpi_packed_oarchive> ;

} } // end namespace boost::archive
