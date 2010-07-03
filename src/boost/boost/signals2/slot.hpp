// Boost.Signals2 library

// Copyright Frank Mori Hess 2007-2008.
// Copyright Timmo Stange 2007.
// Copyright Douglas Gregor 2001-2004. Use, modification and
// distribution is subject to the Boost Software License, Version
// 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

// For more information, see http://www.boost.org

#ifndef BOOST_SIGNALS2_SLOT_HPP
#define BOOST_SIGNALS2_SLOT_HPP

#include <boost/bind.hpp>
#include <boost/function.hpp>
#include <boost/preprocessor/repetition.hpp>
#include <boost/ref.hpp>
#include <boost/signals2/detail/signals_common.hpp>
#include <boost/signals2/detail/signals_common_macros.hpp>
#include <boost/signals2/detail/tracked_objects_visitor.hpp>
#include <boost/signals2/slot_base.hpp>
#include <boost/type_traits.hpp>
#include <boost/visit_each.hpp>
#include <boost/weak_ptr.hpp>

namespace boost
{
  namespace signals2
  {
    namespace detail
    {
      // Get the slot so that it can be copied
      template<typename F>
      typename F::weak_signal_type
      get_invocable_slot(const F &signal, signal_tag)
      { return typename F::weak_signal_type(signal); }

      template<typename F>
      const F&
      get_invocable_slot(const F& f, reference_tag)
      { return f; }

      template<typename F>
      const F&
      get_invocable_slot(const F& f, value_tag)
      { return f; }

      // Determines the type of the slot - is it a signal, a reference to a
      // slot or just a normal slot.
      template<typename F>
      typename get_slot_tag<F>::type
      tag_type(const F&)
      {
        typedef typename get_slot_tag<F>::type
          the_tag_type;
        the_tag_type tag = the_tag_type();
        return tag;
      }
    }
  }
} // end namespace boost

#define BOOST_PP_ITERATION_LIMITS (0, BOOST_PP_INC(BOOST_SIGNALS2_MAX_ARGS))
#define BOOST_PP_FILENAME_1 <boost/signals2/detail/slot_template.hpp>
#include BOOST_PP_ITERATE()

namespace boost
{
  namespace signals2
  {
    template<typename Signature,
      typename SlotFunction = boost::function<Signature> >
    class slot: public detail::slotN<function_traits<Signature>::arity,
      Signature, SlotFunction>::type
    {
    private:
      typedef typename detail::slotN<boost::function_traits<Signature>::arity,
        Signature, SlotFunction>::type base_type;
    public:
      template<typename F>
      slot(const F& f): base_type(f)
      {}
      // bind syntactic sugar
// const AN & aN
#define BOOST_SIGNALS2_SLOT_BINDING_ARG_DECL(z, n, data) \
  const BOOST_PP_CAT(A, n) & BOOST_PP_CAT(a, n)
// template<typename F, typename A0, typename A1, ..., typename An-1> slot(...
#define BOOST_SIGNALS2_SLOT_BINDING_CONSTRUCTOR(z, n, data) \
  template<typename F, BOOST_PP_ENUM_PARAMS(n, typename A)> \
  slot(const F &f, BOOST_PP_ENUM(n, BOOST_SIGNALS2_SLOT_BINDING_ARG_DECL, ~)): \
    base_type(f, BOOST_PP_ENUM_PARAMS(n, a)) \
  {}
#define BOOST_SIGNALS2_SLOT_MAX_BINDING_ARGS 10
      BOOST_PP_REPEAT_FROM_TO(1, BOOST_SIGNALS2_SLOT_MAX_BINDING_ARGS, BOOST_SIGNALS2_SLOT_BINDING_CONSTRUCTOR, ~)
#undef BOOST_SIGNALS2_SLOT_MAX_BINDING_ARGS
#undef BOOST_SIGNALS2_SLOT_BINDING_ARG_DECL
#undef BOOST_SIGNALS2_SLOT_BINDING_CONSTRUCTOR
    };
  } // namespace signals2
}

#endif // BOOST_SIGNALS2_SLOT_HPP
