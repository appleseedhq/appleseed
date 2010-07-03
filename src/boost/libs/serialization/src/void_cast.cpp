/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
// void_cast.cpp: implementation of run-time casting of void pointers

// (C) Copyright 2002 Robert Ramey - http://www.rrsd.com . 
// Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
// <gennadiy.rozental@tfn.com>

//  See http://www.boost.org for updates, documentation, and revision history.

#if (defined _MSC_VER) && (_MSC_VER == 1200)
# pragma warning (disable : 4786) // too long name, harmless warning
#endif

#include <cassert>
#include <cstddef> // NULL

// STL
#include <set>
#include <functional>
#include <algorithm>
#include <cassert>

// BOOST
#define BOOST_SERIALIZATION_SOURCE
#include <boost/serialization/singleton.hpp>

#define BOOST_SERIALIZATION_SOURCE
#include <boost/serialization/extended_type_info.hpp>
#include <boost/serialization/void_cast.hpp>

namespace boost { 
namespace serialization {
namespace void_cast_detail {

struct void_caster_compare {
    bool operator()(const void_caster * lhs, const void_caster * rhs) const {
        return *lhs < *rhs;
    }
};

typedef std::set<const void_caster *, void_caster_compare> set_type;
typedef boost::serialization::singleton<set_type> void_caster_registry;

// implementation of shortcut void caster
class void_caster_shortcut : public void_caster
{
    bool m_includes_virtual_base;

    void const * 
    vbc_upcast(
        void const * const t
    ) const;
    void const *
    vbc_downcast(
        void const * const t
    ) const;
    virtual void const *
    upcast(void const * const t) const{
        if(m_includes_virtual_base)
            return vbc_upcast(t);
        return static_cast<const char *> ( t ) - m_difference;
    }
    virtual void const *
    downcast(void const * const t) const{
        if(m_includes_virtual_base)
            return vbc_downcast(t);
        return static_cast<const char *> ( t ) + m_difference;
    }
    virtual bool is_shortcut() const {
        return true;
    }
public:
    void_caster_shortcut(
        extended_type_info const * derived,
        extended_type_info const * base,
        std::ptrdiff_t difference,
        bool includes_virtual_base
    ) :
        void_caster(derived, base, difference),
        m_includes_virtual_base(includes_virtual_base)
    {
        recursive_register(includes_virtual_base);
    }
    ~void_caster_shortcut(){
        recursive_unregister();
    }
};

void const * 
void_caster_shortcut::vbc_downcast(
    void const * const t
) const {
    // try to find a chain that gives us what we want
    const void_cast_detail::set_type & s
        = void_cast_detail::void_caster_registry::get_const_instance();
    void_cast_detail::set_type::const_iterator it;
    for(it = s.begin(); it != s.end(); ++it){
        // if the current candidate casts to the desired target type
        if ((*it)->m_derived == m_derived){
            // and if it's not us
            if ((*it)->m_base != m_base){
                // try to cast from the candidate base to our base
                const void * t_new;
                t_new = void_downcast(*(*it)->m_base, *m_base, t);
                // if we were successful
                if(NULL != t_new)
                    // recast to our derived
                    return (*it)->downcast(t_new);
            }
        }
    }
    return NULL;
}

void const * 
void_caster_shortcut::vbc_upcast(
    void const * const t
) const {
    // try to find a chain that gives us what we want
    const void_cast_detail::set_type & s
        = void_cast_detail::void_caster_registry::get_const_instance();
    void_cast_detail::set_type::const_iterator it;
    for(it = s.begin(); it != s.end(); ++it){
        // if the current candidate casts from the desired base type
        if((*it)->m_base == m_base){
            // and if it's not us
            if ((*it)->m_derived != m_derived){
                // try to cast from the candidate derived to our our derived
                const void * t_new;
                t_new = void_upcast(*m_derived, *(*it)->m_derived, t);
                if(NULL != t_new)
                    return (*it)->upcast(t_new);
            }
        }
    }
    return NULL;
}

// just used as a search key
class void_caster_argument : public void_caster
{
    virtual void const *
    upcast(void const * const t) const {
        assert(false);
        return NULL;
    }
    virtual void const *
    downcast( void const * const t) const {
        assert(false);
        return NULL;
    }
public:
    void_caster_argument(
        extended_type_info const * derived,
        extended_type_info const * base
    ) :
        void_caster(derived, base)
    {}
    ~void_caster_argument(){};
};

// implementation of void caster base class
BOOST_SERIALIZATION_DECL(void)
void_caster::recursive_register(bool includes_virtual_base) const {
    void_cast_detail::set_type & s
        = void_cast_detail::void_caster_registry::get_mutable_instance();

    s.insert(this);

    // generate all implied void_casts.

    void_cast_detail::set_type::const_iterator it;
    for(it = s.begin(); it != s.end(); ++it){
        if(m_derived == (*it)->m_base)
            new void_caster_shortcut(
                (*it)->m_derived, 
                m_base,
                m_difference + (*it)->m_difference,
                includes_virtual_base
            );
        if((*it)->m_derived == m_base)
            new void_caster_shortcut(
                m_derived, 
                (*it)->m_base, 
                m_difference + (*it)->m_difference,
                includes_virtual_base
            );
    }
}
                         
BOOST_SERIALIZATION_DECL(void)
void_caster::recursive_unregister() const {
    if(void_caster_registry::is_destroyed())
        return;

    void_cast_detail::set_type & s 
        = void_caster_registry::get_mutable_instance();

    // delete all implied void_casts.
    void_cast_detail::set_type::iterator it;
    for(it = s.begin(); it != s.end(); ++it){
        if((*it)->is_shortcut()){
            if(m_derived == (*it)->m_base
            || (*it)->m_derived == m_base){
                delete *it;
                it = s.begin();
            }
        }
    }   

    const void_cast_detail::void_caster_argument ca(m_derived, m_base);
    it = s.find(& ca);
    if(s.end() == it)
        return;

    s.erase(it);
}



} // namespace void_cast_detail

// Given a void *, assume that it really points to an instance of one type
// and alter it so that it would point to an instance of a related type.
// Return the altered pointer. If there exists no sequence of casts that
// can transform from_type to to_type, return a NULL.  
BOOST_SERIALIZATION_DECL(void const *)  
void_upcast(
    extended_type_info const & derived,
    extended_type_info const & base,
    void const * const t
){
    // same types - trivial case
    if (derived == base)
        return t;

    // check to see if base/derived pair is found in the registry
    const void_cast_detail::set_type & s
        = void_cast_detail::void_caster_registry::get_const_instance();
    void_cast_detail::void_caster_argument ca(& derived, & base);

    void_cast_detail::set_type::const_iterator it;
    it = s.find(& ca);
    if (s.end() != it)
        return (*it)->upcast(t);

    return NULL;
}

BOOST_SERIALIZATION_DECL(void const *)  
void_downcast(
    extended_type_info const & derived,
    extended_type_info const & base,
    void const * const t
){
    // same types - trivial case
    if (derived == base)
        return t;

    // check to see if base/derived pair is found in the registry
    const void_cast_detail::set_type & s
        = void_cast_detail::void_caster_registry::get_const_instance();
    void_cast_detail::void_caster_argument ca(& derived, & base);

    void_cast_detail::set_type::const_iterator it;
    it = s.find(&ca);
    if (s.end() != it)
        return(*it)->downcast(t);

    return NULL;
}

} // namespace serialization
} // namespace boost
