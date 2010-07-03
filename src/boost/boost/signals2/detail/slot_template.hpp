// Boost.Signals2 library

// Copyright Frank Mori Hess 2007-2008.
// Copyright Timmo Stange 2007.
// Copyright Douglas Gregor 2001-2004. Use, modification and
// distribution is subject to the Boost Software License, Version
// 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

// For more information, see http://www.boost.org

// This file is included iteratively, and should not be protected from multiple inclusion

#define BOOST_SIGNALS2_NUM_ARGS BOOST_PP_ITERATION()

namespace boost
{
  namespace signals2
  {
    template<typename Signature, typename SlotFunction> class slot;

    // slot class template.
    template<BOOST_SIGNALS2_SIGNATURE_TEMPLATE_DECL(BOOST_SIGNALS2_NUM_ARGS),
      typename SlotFunction = BOOST_SIGNALS2_FUNCTION_N_DECL(BOOST_SIGNALS2_NUM_ARGS)>
    class BOOST_SIGNALS2_SLOT_CLASS_NAME(BOOST_SIGNALS2_NUM_ARGS): public slot_base
    {
    public:
      template<BOOST_SIGNALS2_PREFIXED_SIGNATURE_TEMPLATE_DECL(BOOST_SIGNALS2_NUM_ARGS, Other), typename OtherSlotFunction>
      friend class BOOST_SIGNALS2_SLOT_CLASS_NAME(BOOST_SIGNALS2_NUM_ARGS);

      typedef SlotFunction slot_function_type;
      typedef R result_type;
// typedef Tn argn_type;
#define BOOST_SIGNALS2_MISC_STATEMENT(z, n, data) \
    typedef BOOST_PP_CAT(T, BOOST_PP_INC(n)) BOOST_PP_CAT(BOOST_PP_CAT(arg, BOOST_PP_INC(n)), _type);
          BOOST_PP_REPEAT(BOOST_SIGNALS2_NUM_ARGS, BOOST_SIGNALS2_MISC_STATEMENT, ~)
#undef BOOST_SIGNALS2_MISC_STATEMENT
#if BOOST_SIGNALS2_NUM_ARGS == 1
      typedef arg1_type argument_type;
#elif BOOST_SIGNALS2_NUM_ARGS == 2
      typedef arg1_type first_argument_type;
      typedef arg2_type second_argument_type;
#endif
      BOOST_STATIC_CONSTANT(int, arity = BOOST_SIGNALS2_NUM_ARGS);

      template<typename F>
      BOOST_SIGNALS2_SLOT_CLASS_NAME(BOOST_SIGNALS2_NUM_ARGS)(const F& f)
      {
        init_slot_function(f);
      }
      // copy constructors
      template<BOOST_SIGNALS2_PREFIXED_SIGNATURE_TEMPLATE_DECL(BOOST_SIGNALS2_NUM_ARGS, Other), typename OtherSlotFunction>
      BOOST_SIGNALS2_SLOT_CLASS_NAME(BOOST_SIGNALS2_NUM_ARGS)(const BOOST_SIGNALS2_SLOT_CLASS_NAME(BOOST_SIGNALS2_NUM_ARGS)
        <BOOST_SIGNALS2_PREFIXED_SIGNATURE_TEMPLATE_INSTANTIATION(BOOST_SIGNALS2_NUM_ARGS, Other), OtherSlotFunction> &other_slot):
        slot_base(other_slot), _slot_function(other_slot._slot_function)
      {
      }
      template<typename Signature, typename OtherSlotFunction>
      BOOST_SIGNALS2_SLOT_CLASS_NAME(BOOST_SIGNALS2_NUM_ARGS)(const slot<Signature, OtherSlotFunction> &other_slot):
        slot_base(other_slot), _slot_function(other_slot._slot_function)
      {
      }
      // bind syntactic sugar
// const ArgTypeN & argN
#define BOOST_SIGNALS2_SLOT_BINDING_ARG_DECL(z, n, data) \
    const BOOST_PP_CAT(ArgType, n) & BOOST_PP_CAT(arg, n)
// template<typename Func, typename ArgType0, typename ArgType1, ..., typename ArgTypen-1> slotN(...
#define BOOST_SIGNALS2_SLOT_BINDING_CONSTRUCTOR(z, n, data) \
      template<typename Func, BOOST_PP_ENUM_PARAMS(n, typename ArgType)> \
      BOOST_SIGNALS2_SLOT_CLASS_NAME(BOOST_SIGNALS2_NUM_ARGS)(const Func &func,  BOOST_PP_ENUM(n, BOOST_SIGNALS2_SLOT_BINDING_ARG_DECL, ~)) \
      { \
        init_slot_function(bind(func, BOOST_PP_ENUM_PARAMS(n, arg))); \
      }
#define BOOST_SIGNALS2_SLOT_MAX_BINDING_ARGS 10
      BOOST_PP_REPEAT_FROM_TO(1, BOOST_SIGNALS2_SLOT_MAX_BINDING_ARGS, BOOST_SIGNALS2_SLOT_BINDING_CONSTRUCTOR, ~)
#undef BOOST_SIGNALS2_SLOT_MAX_BINDING_ARGS
#undef BOOST_SIGNALS2_SLOT_BINDING_ARG_DECL
#undef BOOST_SIGNALS2_SLOT_BINDING_CONSTRUCTOR
      // invocation
      R operator()(BOOST_SIGNALS2_SIGNATURE_FULL_ARGS(BOOST_SIGNALS2_NUM_ARGS))
      {
        locked_container_type locked_objects = lock();
        return _slot_function(BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS));
      }
      R operator()(BOOST_SIGNALS2_SIGNATURE_FULL_ARGS(BOOST_SIGNALS2_NUM_ARGS)) const
      {
        locked_container_type locked_objects = lock();
        return _slot_function(BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS));
      }
      // tracking
      BOOST_SIGNALS2_SLOT_CLASS_NAME(BOOST_SIGNALS2_NUM_ARGS)& track(const weak_ptr<void> &tracked)
      {
        _tracked_objects.push_back(tracked);
        return *this;
      }
      BOOST_SIGNALS2_SLOT_CLASS_NAME(BOOST_SIGNALS2_NUM_ARGS)& track(const signal_base &signal)
      {
        track_signal(signal);
        return *this;
      }
      BOOST_SIGNALS2_SLOT_CLASS_NAME(BOOST_SIGNALS2_NUM_ARGS)& track(const slot_base &slot)
      {
        tracked_container_type::const_iterator it;
        for(it = slot.tracked_objects().begin(); it != slot.tracked_objects().end(); ++it)
        {
          track(*it);
        }
        return *this;
      }

      const slot_function_type& slot_function() const {return _slot_function;}
      slot_function_type& slot_function() {return _slot_function;}
    private:
      template<typename F>
      void init_slot_function(const F& f)
      {
        _slot_function = detail::get_invocable_slot(f, detail::tag_type(f));
        signals2::detail::tracked_objects_visitor visitor(this);
        boost::visit_each(visitor, f);
      }

      SlotFunction _slot_function;
    };

    namespace detail
    {
      template<unsigned arity, typename Signature, typename SlotFunction>
      class slotN;
      // partial template specialization
      template<typename Signature, typename SlotFunction>
      class slotN<BOOST_SIGNALS2_NUM_ARGS, Signature, SlotFunction>
      {
      public:
        typedef BOOST_SIGNALS2_SLOT_CLASS_NAME(BOOST_SIGNALS2_NUM_ARGS)<
          BOOST_SIGNALS2_PORTABLE_SIGNATURE(BOOST_SIGNALS2_NUM_ARGS, Signature),
          SlotFunction> type;
      };
    }
  } // end namespace signals2
} // end namespace boost

#undef BOOST_SIGNALS2_NUM_ARGS
