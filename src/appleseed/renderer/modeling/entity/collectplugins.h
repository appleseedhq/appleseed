//
// Created by syrix on 3/11/18.
//

#ifndef APPLESEED_COLLECTPLUGINS_H
#define APPLESEED_COLLECTPLUGINS_H

// Interface header.
#include "entityfactoryregistrar.h"
// Boost headers.
#include "boost/filesystem.hpp"
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <utility>

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/utility/plugin.h"

// appleseed.foundation headers.
#include "foundation/platform/sharedlibrary.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <string>
#include<iostream>



template <typename Entity>
void EntityFactoryRegistrar::collect_plugins(
        const std::function<void (void*)>&  register_factory)
{

    const std::string entry_point_name =
            foundation::format("appleseed_create_{0}_factory", EntityTraits<Entity>::get_entity_type_name());


    std::vector<std::pair<std::string ,boost::function<void(void*)>>> register_factories;

    register_factories.push_back(std::make_pair(entry_point_name,register_factory));
    //functions.push_back(register_factory);
    std::cout<<"vector size is " <<register_factories.size()<<"\n";
}

#endif //APPLESEED_COLLECTPLUGINS_H
