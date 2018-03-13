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


// this function is called by every plugin wants to register it's register_function and entry_point
// to collect all plugins then traverse paths searching for plugins ( plugins in vector)
namespace renderer {
    void EntityFactoryRegistrar::test()
    {
        return;
    }

}


#endif //APPLESEED_COLLECTPLUGINS_H
