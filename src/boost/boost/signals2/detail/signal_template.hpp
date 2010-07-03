/*
  Template for Signa1, Signal2, ... classes that support signals
  with 1, 2, ... parameters

  Begin: 2007-01-23
*/
// Copyright Frank Mori Hess 2007-2008
//
// Use, modification and
// distribution is subject to the Boost Software License, Version
// 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

// This file is included iteratively, and should not be protected from multiple inclusion

#define BOOST_SIGNALS2_NUM_ARGS BOOST_PP_ITERATION()

#define BOOST_SIGNALS2_SIGNAL_CLASS_NAME BOOST_PP_CAT(signal, BOOST_SIGNALS2_NUM_ARGS)
#define BOOST_SIGNALS2_WEAK_SIGNAL_CLASS_NAME BOOST_PP_CAT(weak_, BOOST_SIGNALS2_SIGNAL_CLASS_NAME)
#define BOOST_SIGNALS2_SIGNAL_IMPL_CLASS_NAME BOOST_PP_CAT(BOOST_SIGNALS2_SIGNAL_CLASS_NAME, _impl)

// typename R, typename T1, typename T2, ..., typename TN, typename Combiner = optional_last_value<R>, ...
#define BOOST_SIGNALS2_SIGNAL_TEMPLATE_DEFAULTED_DECL \
  BOOST_SIGNALS2_SIGNATURE_TEMPLATE_DECL(BOOST_SIGNALS2_NUM_ARGS), \
  typename Combiner = optional_last_value<R>, \
  typename Group = int, \
  typename GroupCompare = std::less<Group>, \
  typename SlotFunction = BOOST_SIGNALS2_FUNCTION_N_DECL(BOOST_SIGNALS2_NUM_ARGS), \
  typename ExtendedSlotFunction = BOOST_SIGNALS2_EXT_FUNCTION_N_DECL(BOOST_SIGNALS2_NUM_ARGS), \
  typename Mutex = signals2::mutex
// typename R, typename T1, typename T2, ..., typename TN, typename Combiner, ...
#define BOOST_SIGNALS2_SIGNAL_TEMPLATE_DECL \
  BOOST_SIGNALS2_SIGNATURE_TEMPLATE_DECL(BOOST_SIGNALS2_NUM_ARGS), \
  typename Combiner, \
  typename Group, \
  typename GroupCompare, \
  typename SlotFunction, \
  typename ExtendedSlotFunction, \
  typename Mutex
// R, T1, T2, ..., TN, Combiner, Group, GroupCompare, SlotFunction, ExtendedSlotFunction, Mutex
#define BOOST_SIGNALS2_SIGNAL_TEMPLATE_INSTANTIATION \
  BOOST_SIGNALS2_SIGNATURE_TEMPLATE_INSTANTIATION(BOOST_SIGNALS2_NUM_ARGS), \
  Combiner, Group, GroupCompare, SlotFunction, ExtendedSlotFunction, Mutex

namespace boost
{
  namespace signals2
  {
    namespace detail
    {
// typename T1, typename T2, ..., typename TN
#define BOOST_SIGNALS2_MISC_TEMPLATE_DECL \
  BOOST_PP_ENUM_SHIFTED_PARAMS(BOOST_PP_INC(BOOST_SIGNALS2_NUM_ARGS), typename T)
// Tn & argn
#define BOOST_SIGNALS2_FULL_REF_ARG(z, n, data) \
  BOOST_PP_CAT(T, BOOST_PP_INC(n)) & BOOST_SIGNALS2_SIGNATURE_ARG_NAME(~, n, ~)
// T1 & arg1, T2 & arg2, ..., Tn & argn
#define BOOST_SIGNALS2_FULL_REF_ARGS(arity) \
  BOOST_PP_ENUM(arity, BOOST_SIGNALS2_FULL_REF_ARG, ~)

// wrapper around an signalN::extended_slot_function which binds the
// connection argument so it looks like a normal
// signalN::slot_function

// bound_extended_slot_functionN
#define BOOST_SIGNALS2_BOUND_EXTENDED_SLOT_FUNCTION_N BOOST_PP_CAT(bound_extended_slot_function, BOOST_SIGNALS2_NUM_ARGS)
      template<typename ExtendedSlotFunction>
        class BOOST_SIGNALS2_BOUND_EXTENDED_SLOT_FUNCTION_N
      {
      public:
        typedef typename result_type_wrapper<typename ExtendedSlotFunction::result_type>::type result_type;
        BOOST_SIGNALS2_BOUND_EXTENDED_SLOT_FUNCTION_N(const ExtendedSlotFunction &fun):
          _fun(fun), _connection(new connection)
        {}
        void set_connection(const connection &conn)
        {
          *_connection = conn;
        }

#if BOOST_SIGNALS2_NUM_ARGS > 0
        template<BOOST_SIGNALS2_MISC_TEMPLATE_DECL>
#endif // BOOST_SIGNALS2_NUM_ARGS > 0
          result_type operator()(BOOST_SIGNALS2_FULL_REF_ARGS(BOOST_SIGNALS2_NUM_ARGS))
        {
          typename ExtendedSlotFunction::result_type *resolver = 0;
          return m_invoke(BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS)
            BOOST_PP_COMMA_IF(BOOST_SIGNALS2_NUM_ARGS) resolver);
        }
        // const overload
#if BOOST_SIGNALS2_NUM_ARGS > 0
        template<BOOST_SIGNALS2_MISC_TEMPLATE_DECL>
#endif // BOOST_SIGNALS2_NUM_ARGS > 0
          result_type operator()(BOOST_SIGNALS2_FULL_REF_ARGS(BOOST_SIGNALS2_NUM_ARGS)) const
        {
          typename ExtendedSlotFunction::result_type *resolver = 0;
          return m_invoke(BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS)
            BOOST_PP_COMMA_IF(BOOST_SIGNALS2_NUM_ARGS) resolver);
        }
        template<typename T>
          bool operator==(const T &other) const
        {
          return _fun == other;
        }
      private:
        BOOST_SIGNALS2_BOUND_EXTENDED_SLOT_FUNCTION_N()
        {}
#if BOOST_SIGNALS2_NUM_ARGS > 0
        template<BOOST_SIGNALS2_MISC_TEMPLATE_DECL>
#endif // BOOST_SIGNALS2_NUM_ARGS > 0
          result_type m_invoke(BOOST_SIGNALS2_FULL_REF_ARGS(BOOST_SIGNALS2_NUM_ARGS) BOOST_PP_COMMA_IF(BOOST_SIGNALS2_NUM_ARGS) ...)
        {
          return _fun(*_connection BOOST_PP_COMMA_IF(BOOST_SIGNALS2_NUM_ARGS)
            BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS));
        }
#if BOOST_SIGNALS2_NUM_ARGS > 0
        template<BOOST_SIGNALS2_MISC_TEMPLATE_DECL>
#endif // BOOST_SIGNALS2_NUM_ARGS > 0
          result_type m_invoke(BOOST_SIGNALS2_FULL_REF_ARGS(BOOST_SIGNALS2_NUM_ARGS) BOOST_PP_COMMA_IF(BOOST_SIGNALS2_NUM_ARGS) const void*)
        {
          _fun(*_connection BOOST_PP_COMMA_IF(BOOST_SIGNALS2_NUM_ARGS)
            BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS));
          return result_type();
        }
        // const overloads
#if BOOST_SIGNALS2_NUM_ARGS > 0
        template<BOOST_SIGNALS2_MISC_TEMPLATE_DECL>
#endif // BOOST_SIGNALS2_NUM_ARGS > 0
          result_type m_invoke(BOOST_SIGNALS2_FULL_REF_ARGS(BOOST_SIGNALS2_NUM_ARGS) BOOST_PP_COMMA_IF(BOOST_SIGNALS2_NUM_ARGS) ...) const
        {
          return _fun(*_connection BOOST_PP_COMMA_IF(BOOST_SIGNALS2_NUM_ARGS)
            BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS));
        }
#if BOOST_SIGNALS2_NUM_ARGS > 0
        template<BOOST_SIGNALS2_MISC_TEMPLATE_DECL>
#endif // BOOST_SIGNALS2_NUM_ARGS > 0
          result_type m_invoke(BOOST_SIGNALS2_FULL_REF_ARGS(BOOST_SIGNALS2_NUM_ARGS) BOOST_PP_COMMA_IF(BOOST_SIGNALS2_NUM_ARGS) const void*) const
        {
          _fun(*_connection BOOST_PP_COMMA_IF(BOOST_SIGNALS2_NUM_ARGS)
            BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS));
          return result_type();
        }

        ExtendedSlotFunction _fun;
        boost::shared_ptr<connection> _connection;
      };

#undef BOOST_SIGNALS2_MISC_TEMPLATE_DECL
#undef BOOST_SIGNALS2_FULL_REF_ARG
#undef BOOST_SIGNALS2_FULL_REF_ARGS

      template<BOOST_SIGNALS2_SIGNAL_TEMPLATE_DECL>
      class BOOST_SIGNALS2_SIGNAL_IMPL_CLASS_NAME
      {
      public:
        typedef SlotFunction slot_function_type;
        // typedef slotN<Signature, SlotFunction> slot_type;
        typedef BOOST_SIGNALS2_SLOT_CLASS_NAME(BOOST_SIGNALS2_NUM_ARGS)<BOOST_SIGNALS2_SIGNATURE_TEMPLATE_INSTANTIATION(BOOST_SIGNALS2_NUM_ARGS),
          slot_function_type> slot_type;
        typedef ExtendedSlotFunction extended_slot_function_type;
        // typedef slotN<R, const connection &, T1, T2, ..., TN, extended_slot_function_type> extended_slot_type;
        typedef BOOST_SIGNALS2_SLOT_CLASS_NAME(BOOST_PP_INC(BOOST_SIGNALS2_NUM_ARGS))<
          BOOST_SIGNALS2_EXT_SLOT_TEMPLATE_INSTANTIATION(BOOST_SIGNALS2_NUM_ARGS),
          extended_slot_function_type> extended_slot_type;
      private:
        class slot_invoker;
        typedef typename group_key<Group>::type group_key_type;
        typedef shared_ptr<connection_body<group_key_type, slot_type, Mutex> > connection_body_type;
        typedef grouped_list<Group, GroupCompare, connection_body_type> connection_list_type;
        typedef BOOST_SIGNALS2_BOUND_EXTENDED_SLOT_FUNCTION_N<extended_slot_function_type>
          bound_extended_slot_function_type;
      public:
        typedef typename nonvoid<typename slot_function_type::result_type>::type slot_result_type;
        typedef Combiner combiner_type;
        typedef typename combiner_type::result_type result_type;
        typedef Group group_type;
        typedef GroupCompare group_compare_type;
        typedef typename detail::slot_call_iterator_t<slot_invoker,
          typename connection_list_type::iterator, connection_body<group_key_type, slot_type, Mutex> > slot_call_iterator;

        BOOST_SIGNALS2_SIGNAL_IMPL_CLASS_NAME(const combiner_type &combiner,
          const group_compare_type &group_compare):
          _shared_state(new invocation_state(connection_list_type(group_compare), combiner)),
          _garbage_collector_it(_shared_state->connection_bodies().end())
        {}
        // connect slot
        connection connect(const slot_type &slot, connect_position position = at_back)
        {
          unique_lock<mutex_type> lock(_mutex);
          return nolock_connect(slot, position);
        }
        connection connect(const group_type &group,
          const slot_type &slot, connect_position position = at_back)
        {
          unique_lock<Mutex> lock(_mutex);
          return nolock_connect(group, slot, position);
        }
        // connect extended slot
        connection connect_extended(const extended_slot_type &ext_slot, connect_position position = at_back)
        {
          unique_lock<mutex_type> lock(_mutex);
          bound_extended_slot_function_type bound_slot(ext_slot.slot_function());
          slot_type slot = replace_slot_function<slot_type>(ext_slot, bound_slot);
          connection conn = nolock_connect(slot, position);
          bound_slot.set_connection(conn);
          return conn;
        }
        connection connect_extended(const group_type &group,
          const extended_slot_type &ext_slot, connect_position position = at_back)
        {
          unique_lock<Mutex> lock(_mutex);
          bound_extended_slot_function_type bound_slot(ext_slot.slot_function());
          slot_type slot = replace_slot_function<slot_type>(ext_slot, bound_slot);
          connection conn = nolock_connect(group, slot, position);
          bound_slot.set_connection(conn);
          return conn;
        }
        // disconnect slot(s)
        void disconnect_all_slots()
        {
          shared_ptr<invocation_state> local_state =
            get_readable_state();
          typename connection_list_type::iterator it;
          for(it = local_state->connection_bodies().begin();
            it != local_state->connection_bodies().end(); ++it)
          {
            (*it)->disconnect();
          }
        }
        void disconnect(const group_type &group)
        {
          shared_ptr<invocation_state> local_state =
            get_readable_state();
          group_key_type group_key(grouped_slots, group);
          typename connection_list_type::iterator it;
          typename connection_list_type::iterator end_it =
            local_state->connection_bodies().upper_bound(group_key);
          for(it = local_state->connection_bodies().lower_bound(group_key);
            it != end_it; ++it)
          {
            (*it)->disconnect();
          }
        }
        template <typename T>
        void disconnect(const T &slot)
        {
          typedef mpl::bool_<(is_convertible<T, group_type>::value)> is_group;
          do_disconnect(slot, is_group());
        }
        // emit signal
        result_type operator ()(BOOST_SIGNALS2_SIGNATURE_FULL_ARGS(BOOST_SIGNALS2_NUM_ARGS))
        {
          shared_ptr<invocation_state> local_state;
          typename connection_list_type::iterator it;
          {
            unique_lock<mutex_type> list_lock(_mutex);
            // only clean up if it is safe to do so
            if(_shared_state.unique())
              nolock_cleanup_connections(false);
            /* Make a local copy of _shared_state while holding mutex, so we are
            thread safe against the combiner or connection list getting modified
            during invocation. */
            local_state = _shared_state;
          }
          slot_invoker invoker BOOST_PP_IF(BOOST_SIGNALS2_NUM_ARGS, \
            (BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS)), );
          slot_call_iterator_cache<slot_result_type, slot_invoker> cache(invoker);
          return local_state->combiner()(
            slot_call_iterator(local_state->connection_bodies().begin(), local_state->connection_bodies().end(), cache),
            slot_call_iterator(local_state->connection_bodies().end(), local_state->connection_bodies().end(), cache));
        }
        result_type operator ()(BOOST_SIGNALS2_SIGNATURE_FULL_ARGS(BOOST_SIGNALS2_NUM_ARGS)) const
        {
          shared_ptr<invocation_state> local_state;
          typename connection_list_type::iterator it;
          {
            unique_lock<mutex_type> list_lock(_mutex);
            // only clean up if it is safe to do so
            if(_shared_state.unique())
              nolock_cleanup_connections(false);
            /* Make a local copy of _shared_state while holding mutex, so we are
            thread safe against the combiner or connection list getting modified
            during invocation. */
            local_state = _shared_state;
          }
          slot_invoker invoker BOOST_PP_IF(BOOST_SIGNALS2_NUM_ARGS, \
            (BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS)), );
          slot_call_iterator_cache<slot_result_type, slot_invoker> cache(invoker);
          return local_state->combiner()(
            slot_call_iterator(local_state->connection_bodies().begin(), local_state->connection_bodies().end(), cache),
            slot_call_iterator(local_state->connection_bodies().end(), local_state->connection_bodies().end(), cache));
        }
        std::size_t num_slots() const
        {
          shared_ptr<invocation_state> local_state =
            get_readable_state();
          typename connection_list_type::iterator it;
          std::size_t count = 0;
          for(it = local_state->connection_bodies().begin();
            it != local_state->connection_bodies().end(); ++it)
          {
            if((*it)->connected()) ++count;
          }
          return count;
        }
        bool empty() const
        {
          shared_ptr<invocation_state> local_state =
            get_readable_state();
          typename connection_list_type::iterator it;
          for(it = local_state->connection_bodies().begin();
            it != local_state->connection_bodies().end(); ++it)
          {
            if((*it)->connected()) return false;
          }
          return true;
        }
        combiner_type combiner() const
        {
          unique_lock<mutex_type> lock(_mutex);
          return _shared_state->combiner;
        }
        void set_combiner(const combiner_type &combiner)
        {
          unique_lock<mutex_type> lock(_mutex);
          if(_shared_state.unique())
            _shared_state->combiner = combiner;
          else
            _shared_state.reset(new invocation_state(*_shared_state, combiner));
        }
      private:
        typedef Mutex mutex_type;

        // slot_invoker is passed to slot_call_iterator_t to run slots
        class slot_invoker
        {
        public:
          typedef slot_result_type result_type;

          slot_invoker(BOOST_SIGNALS2_SIGNATURE_FULL_ARGS(BOOST_SIGNALS2_NUM_ARGS)) BOOST_PP_IF(BOOST_SIGNALS2_NUM_ARGS, :, )
// argn ( argn ) ,
#define BOOST_SIGNALS2_MISC_STATEMENT(z, n, data) \
  BOOST_PP_CAT(arg, n) ( BOOST_PP_CAT(arg, n) )
// arg1(arg1), arg2(arg2), ..., argn(argn)
            BOOST_PP_ENUM_SHIFTED(BOOST_PP_INC(BOOST_SIGNALS2_NUM_ARGS), BOOST_SIGNALS2_MISC_STATEMENT, ~)
#undef BOOST_SIGNALS2_MISC_STATEMENT
          {}
          result_type operator ()(const connection_body_type &connectionBody) const
          {
            result_type *resolver = 0;
            return m_invoke(connectionBody,
              resolver);
          }
// Tn argn;
#define BOOST_SIGNALS2_MISC_STATEMENT(z, n, Signature) \
  BOOST_PP_CAT(T, BOOST_PP_INC(n)) BOOST_SIGNALS2_SIGNATURE_ARG_NAME(~, n, ~);
          BOOST_PP_REPEAT(BOOST_SIGNALS2_NUM_ARGS, BOOST_SIGNALS2_MISC_STATEMENT, ~)
#undef BOOST_SIGNALS2_MISC_STATEMENT
        private:
          result_type m_invoke(const connection_body_type &connectionBody,
            const void_type *) const
          {
            connectionBody->slot.slot_function()(BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS));
            return void_type();
          }
          result_type m_invoke(const connection_body_type &connectionBody, ...) const
          {
            return connectionBody->slot.slot_function()(BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS));
          }
        };
        // a struct used to optimize (minimize) the number of shared_ptrs that need to be created
        // inside operator()
        class invocation_state
        {
        public:
          invocation_state(const connection_list_type &connections_in,
            const combiner_type &combiner_in): _connection_bodies(new connection_list_type(connections_in)),
            _combiner(new combiner_type(combiner_in))
          {}
          invocation_state(const invocation_state &other, const connection_list_type &connections_in):
            _connection_bodies(new connection_list_type(connections_in)),
            _combiner(other._combiner)
          {}
          invocation_state(const invocation_state &other, const combiner_type &combiner_in):
            _connection_bodies(other._connection_bodies),
            _combiner(new combiner_type(combiner_in))
          {}
          connection_list_type & connection_bodies() { return *_connection_bodies; }
          const connection_list_type & connection_bodies() const { return *_connection_bodies; }
          combiner_type & combiner() { return *_combiner; }
          const combiner_type & combiner() const { return *_combiner; }
        private:
          invocation_state(const invocation_state &);

          shared_ptr<connection_list_type> _connection_bodies;
          shared_ptr<combiner_type> _combiner;
        };

        // clean up disconnected connections
        void nolock_cleanup_connections(bool grab_tracked,
          const typename connection_list_type::iterator &begin, bool break_on_connected = false) const
        {
          BOOST_ASSERT(_shared_state.unique());
          typename connection_list_type::iterator it;
          for(it = begin; it != _shared_state->connection_bodies().end();)
          {
            bool connected;
            {
              unique_lock<connection_body_base> lock(**it);
              if(grab_tracked)
                (*it)->nolock_slot_expired();
              connected = (*it)->nolock_nograb_connected();
            }// scoped lock destructs here, safe to erase now
            if(connected == false)
            {
              it = _shared_state->connection_bodies().erase((*it)->group_key(), it);
            }else
            {
              ++it;
              if(break_on_connected) break;
            }
          }
          _garbage_collector_it = it;
        }
        // clean up a few connections in constant time
        void nolock_cleanup_connections(bool grab_tracked) const
        {
          BOOST_ASSERT(_shared_state.unique());
          typename connection_list_type::iterator begin;
          if(_garbage_collector_it == _shared_state->connection_bodies().end())
          {
            begin = _shared_state->connection_bodies().begin();
          }else
          {
            begin = _garbage_collector_it;
          }
          nolock_cleanup_connections(grab_tracked, begin, true);
        }
        /* Make a new copy of the slot list if it is currently being read somewhere else
        */
        void nolock_force_unique_connection_list()
        {
          if(_shared_state.unique() == false)
          {
            _shared_state.reset(new invocation_state(*_shared_state, _shared_state->connection_bodies()));
            nolock_cleanup_connections(true, _shared_state->connection_bodies().begin());
          }else
          {
            nolock_cleanup_connections(true);
          }
        }
        shared_ptr<invocation_state> get_readable_state() const
        {
          unique_lock<mutex_type> list_lock(_mutex);
          return _shared_state;
        }
        connection_body_type create_new_connection(const slot_type &slot)
        {
          nolock_force_unique_connection_list();
          return connection_body_type(new connection_body<group_key_type, slot_type, Mutex>(slot));
        }
        void do_disconnect(const group_type &group, mpl::bool_<true> is_group)
        {
          disconnect(group);
        }
        template<typename T>
        void do_disconnect(const T &slot, mpl::bool_<false> is_group)
        {
          shared_ptr<invocation_state> local_state =
            get_readable_state();
          typename connection_list_type::iterator it;
          for(it = local_state->connection_bodies().begin();
            it != local_state->connection_bodies().end(); ++it)
          {
            unique_lock<connection_body_base> lock(**it);
            if((*it)->slot.slot_function() == slot)
            {
              (*it)->nolock_disconnect();
            }else
            {
              // check for wrapped extended slot
              bound_extended_slot_function_type *fp;
              fp = (*it)->slot.slot_function().template target<bound_extended_slot_function_type>();
              if(fp && *fp == slot)
              {
                (*it)->nolock_disconnect();
              }
            }
          }
        }
        // connect slot
        connection nolock_connect(const slot_type &slot, connect_position position)
        {
          connection_body_type newConnectionBody =
            create_new_connection(slot);
          group_key_type group_key;
          if(position == at_back)
          {
            group_key.first = back_ungrouped_slots;
            _shared_state->connection_bodies().push_back(group_key, newConnectionBody);
          }else
          {
            group_key.first = front_ungrouped_slots;
            _shared_state->connection_bodies().push_front(group_key, newConnectionBody);
          }
          newConnectionBody->set_group_key(group_key);
          return connection(newConnectionBody);
        }
        connection nolock_connect(const group_type &group,
          const slot_type &slot, connect_position position)
        {
          connection_body_type newConnectionBody =
            create_new_connection(slot);
          // update map to first connection body in group if needed
          group_key_type group_key(grouped_slots, group);
          newConnectionBody->set_group_key(group_key);
          if(position == at_back)
          {
            _shared_state->connection_bodies().push_back(group_key, newConnectionBody);
          }else  // at_front
          {
            _shared_state->connection_bodies().push_front(group_key, newConnectionBody);
          }
          return connection(newConnectionBody);
        }

        shared_ptr<invocation_state> _shared_state;
        mutable typename connection_list_type::iterator _garbage_collector_it;
        // connection list mutex must never be locked when attempting a blocking lock on a slot,
        // or you could deadlock.
        mutable mutex_type _mutex;
      };

      template<BOOST_SIGNALS2_SIGNAL_TEMPLATE_DECL>
      class BOOST_SIGNALS2_WEAK_SIGNAL_CLASS_NAME;
    }

    template<BOOST_SIGNALS2_SIGNAL_TEMPLATE_DEFAULTED_DECL>
    class BOOST_SIGNALS2_SIGNAL_CLASS_NAME: public signal_base
    {
      typedef detail::BOOST_SIGNALS2_SIGNAL_IMPL_CLASS_NAME<BOOST_SIGNALS2_SIGNAL_TEMPLATE_INSTANTIATION> impl_class;
    public:
      typedef detail::BOOST_SIGNALS2_WEAK_SIGNAL_CLASS_NAME<BOOST_SIGNALS2_SIGNAL_TEMPLATE_INSTANTIATION> weak_signal_type;
      friend class detail::BOOST_SIGNALS2_WEAK_SIGNAL_CLASS_NAME<BOOST_SIGNALS2_SIGNAL_TEMPLATE_INSTANTIATION>;

      typedef SlotFunction slot_function_type;
      // typedef slotN<Signature, SlotFunction> slot_type;
      typedef typename impl_class::slot_type slot_type;
      typedef typename impl_class::extended_slot_function_type extended_slot_function_type;
      typedef typename impl_class::extended_slot_type extended_slot_type;
      typedef typename impl_class::slot_result_type slot_result_type;
      typedef Combiner combiner_type;
      typedef typename combiner_type::result_type result_type;
      typedef Group group_type;
      typedef GroupCompare group_compare_type;
      typedef typename impl_class::slot_call_iterator
        slot_call_iterator;
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

      BOOST_SIGNALS2_SIGNAL_CLASS_NAME(const combiner_type &combiner = combiner_type(),
        const group_compare_type &group_compare = group_compare_type()):
        _pimpl(new impl_class(combiner, group_compare))
      {};
      virtual ~BOOST_SIGNALS2_SIGNAL_CLASS_NAME()
      {
        disconnect_all_slots();
      }
      connection connect(const slot_type &slot, connect_position position = at_back)
      {
        return (*_pimpl).connect(slot, position);
      }
      connection connect(const group_type &group,
        const slot_type &slot, connect_position position = at_back)
      {
        return (*_pimpl).connect(group, slot, position);
      }
      connection connect_extended(const extended_slot_type &slot, connect_position position = at_back)
      {
        return (*_pimpl).connect_extended(slot, position);
      }
      connection connect_extended(const group_type &group,
        const extended_slot_type &slot, connect_position position = at_back)
      {
        return (*_pimpl).connect_extended(group, slot, position);
      }
      void disconnect_all_slots()
      {
        (*_pimpl).disconnect_all_slots();
      }
      void disconnect(const group_type &group)
      {
        (*_pimpl).disconnect(group);
      }
      template <typename T>
      void disconnect(const T &slot)
      {
        (*_pimpl).disconnect(slot);
      }
      result_type operator ()(BOOST_SIGNALS2_SIGNATURE_FULL_ARGS(BOOST_SIGNALS2_NUM_ARGS))
      {
        return (*_pimpl)(BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS));
      }
      result_type operator ()(BOOST_SIGNALS2_SIGNATURE_FULL_ARGS(BOOST_SIGNALS2_NUM_ARGS)) const
      {
        return (*_pimpl)(BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS));
      }
      std::size_t num_slots() const
      {
        return (*_pimpl).num_slots();
      }
      bool empty() const
      {
        return (*_pimpl).empty();
      }
      combiner_type combiner() const
      {
        return (*_pimpl).combiner();
      }
      void set_combiner(const combiner_type &combiner)
      {
        return (*_pimpl).set_combiner(combiner);
      }
    protected:
      virtual shared_ptr<void> lock_pimpl() const
      {
        return _pimpl;
      }
    private:
      shared_ptr<impl_class>
        _pimpl;
    };

    namespace detail
    {
      // wrapper class for storing other signals as slots with automatic lifetime tracking
      template<BOOST_SIGNALS2_SIGNAL_TEMPLATE_DECL>
      class BOOST_SIGNALS2_WEAK_SIGNAL_CLASS_NAME
      {
      public:
        typedef SlotFunction slot_function_type;
        typedef typename nonvoid<typename slot_function_type::result_type>::type slot_result_type;
        typedef typename BOOST_SIGNALS2_SIGNAL_CLASS_NAME<BOOST_SIGNALS2_SIGNAL_TEMPLATE_INSTANTIATION>::result_type
          result_type;

        BOOST_SIGNALS2_WEAK_SIGNAL_CLASS_NAME(const BOOST_SIGNALS2_SIGNAL_CLASS_NAME<BOOST_SIGNALS2_SIGNAL_TEMPLATE_INSTANTIATION>
          &signal):
          _weak_pimpl(signal._pimpl)
        {}
        result_type operator ()(BOOST_SIGNALS2_SIGNATURE_FULL_ARGS(BOOST_SIGNALS2_NUM_ARGS))
        {
          shared_ptr<detail::BOOST_SIGNALS2_SIGNAL_IMPL_CLASS_NAME<BOOST_SIGNALS2_SIGNAL_TEMPLATE_INSTANTIATION> >
            shared_pimpl(_weak_pimpl.lock());
          if(shared_pimpl == 0) boost::throw_exception(expired_slot());
          return (*shared_pimpl)(BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS));
        }
        result_type operator ()(BOOST_SIGNALS2_SIGNATURE_FULL_ARGS(BOOST_SIGNALS2_NUM_ARGS)) const
        {
          shared_ptr<detail::BOOST_SIGNALS2_SIGNAL_IMPL_CLASS_NAME<BOOST_SIGNALS2_SIGNAL_TEMPLATE_INSTANTIATION> >
            shared_pimpl(_weak_pimpl.lock());
          if(shared_pimpl == 0) boost::throw_exception(expired_slot());
          return (*shared_pimpl)(BOOST_SIGNALS2_SIGNATURE_ARG_NAMES(BOOST_SIGNALS2_NUM_ARGS));
        }
      private:
        boost::weak_ptr<detail::BOOST_SIGNALS2_SIGNAL_IMPL_CLASS_NAME<BOOST_SIGNALS2_SIGNAL_TEMPLATE_INSTANTIATION> >
          _weak_pimpl;
      };

      template<unsigned arity, typename Signature, typename Combiner,
        typename Group, typename GroupCompare, typename SlotFunction,
        typename ExtendedSlotFunction, typename Mutex>
      class signalN;
      // partial template specialization
      template<typename Signature, typename Combiner, typename Group,
        typename GroupCompare, typename SlotFunction,
        typename ExtendedSlotFunction, typename Mutex>
      class signalN<BOOST_SIGNALS2_NUM_ARGS, Signature, Combiner, Group,
        GroupCompare, SlotFunction, ExtendedSlotFunction, Mutex>
      {
      public:
        typedef BOOST_SIGNALS2_SIGNAL_CLASS_NAME<
          BOOST_SIGNALS2_PORTABLE_SIGNATURE(BOOST_SIGNALS2_NUM_ARGS, Signature),
          Combiner, Group,
          GroupCompare, SlotFunction, ExtendedSlotFunction, Mutex> type;
      };

      template<int arity, typename Signature>
        class extended_signature;
      // partial template specialization
      template<typename Signature>
        class extended_signature<BOOST_SIGNALS2_NUM_ARGS, Signature>
      {
      public:
// typename function_traits<Signature>::result_type (
// const boost::signals2::connection &,
// typename function_traits<Signature>::arg1_type,
// typename function_traits<Signature>::arg2_type,
// ...,
// typename function_traits<Signature>::argn_type)
#define BOOST_SIGNALS2_EXT_SIGNATURE(arity, Signature) \
  typename function_traits<Signature>::result_type ( \
  const boost::signals2::connection & BOOST_PP_COMMA_IF(BOOST_SIGNALS2_NUM_ARGS) \
  BOOST_PP_ENUM(arity, BOOST_SIGNALS2_SIGNATURE_TO_ARGN_TYPE, Signature) )
        typedef function<BOOST_SIGNALS2_EXT_SIGNATURE(BOOST_SIGNALS2_NUM_ARGS, Signature)> function_type;
#undef BOOST_SIGNALS2_EXT_SIGNATURE
      };

    } // namespace detail
  } // namespace signals2
} // namespace boost

#undef BOOST_SIGNALS2_NUM_ARGS
#undef BOOST_SIGNALS2_SIGNAL_CLASS_NAME
#undef BOOST_SIGNALS2_WEAK_SIGNAL_CLASS_NAME
#undef BOOST_SIGNALS2_SIGNAL_IMPL_CLASS_NAME
#undef BOOST_SIGNALS2_SIGNAL_TEMPLATE_DEFAULTED_DECL
#undef BOOST_SIGNALS2_SIGNAL_TEMPLATE_DECL
#undef BOOST_SIGNALS2_SIGNAL_TEMPLATE_INSTANTIATION
#undef BOOST_SIGNALS2_BOUND_EXTENDED_SLOT_FUNCTION_N
