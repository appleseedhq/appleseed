// Copyright Alexander Nasonov 2009
//
// Distributed under the Boost Software License, Version 1.0. 
// (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt)

#include <vector>
#include <ostream>

#include <boost/foreach.hpp>
#include <boost/scope_exit.hpp>

// The following is required for typeof emulation mode:
#include <boost/typeof/typeof.hpp>
#include <boost/typeof/std/vector.hpp>
#include BOOST_TYPEOF_INCREMENT_REGISTRATION_GROUP()

class World;
class Person
{
    friend class World;
public:
    typedef unsigned int id_t;
    typedef unsigned int evolution_t;

    Person()
        : m_id(0)
        , m_evolution(0)
    {}

    friend std::ostream& operator<<(std::ostream& o, Person const& p)
    {
        return o << "Person(" << p.m_id << ", " << p.m_evolution << ')';
    }
private:
    id_t m_id;
    evolution_t m_evolution;
};

BOOST_TYPEOF_REGISTER_TYPE(Person)

class World
{
public:
    typedef unsigned int id_t;

    World()
        : m_next_id(1)
    {}
    void addPerson(Person const& aPerson);

    friend std::ostream& operator<<(std::ostream& o, World const& w)
    {
        o << "World(" << w.m_next_id << ", {";
        BOOST_FOREACH(Person const& p, w.m_persons)
        {
             o << ' ' << p << ',';
        }
        return o << "})";
    }
private:
    id_t m_next_id;
    std::vector<Person> m_persons;
};

BOOST_TYPEOF_REGISTER_TYPE(World)

void World::addPerson(Person const& aPerson) {
    m_persons.push_back(aPerson);

    // This block must be no-throw
    Person& person = m_persons.back();
    Person::evolution_t checkpoint = person.m_evolution;

    BOOST_SCOPE_EXIT( (checkpoint)(&person)(&m_persons) )
    {
        if(checkpoint == person.m_evolution)
            m_persons.pop_back();
    } BOOST_SCOPE_EXIT_END

    // ...

    checkpoint = ++person.m_evolution;

    // Assign new id to the person
    World::id_t const prev_id = person.m_id;
    person.m_id = m_next_id++;
    BOOST_SCOPE_EXIT( (checkpoint)(&person)(&m_next_id)(prev_id) )
    {
        if(checkpoint == person.m_evolution) {
            m_next_id = person.m_id;
            person.m_id = prev_id;
        }
    } BOOST_SCOPE_EXIT_END

    // ...

    checkpoint = ++person.m_evolution;
}

#include <iostream>

int main()
{
    Person adam, eva;
    std::cout << adam << '\n';
    std::cout << eva  << '\n';

    World w;
    w.addPerson(adam);
    w.addPerson(eva);
    std::cout << w << '\n';
}

