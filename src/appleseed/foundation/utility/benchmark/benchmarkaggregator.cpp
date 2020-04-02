
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

// Interface header.
#include "benchmarkaggregator.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/string/string.h"
#include "foundation/utility/benchmark/benchmarkdatapoint.h"
#include "foundation/utility/benchmark/benchmarkseries.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/xercesc.h"

// Boost headers.
#include "boost/date_time/posix_time/posix_time.hpp"
#include "boost/filesystem.hpp"
#include "boost/range/iterator_range.hpp"
#include "boost/regex.hpp"

// Xerces-C++ headers.
#include "xercesc/dom/DOM.hpp"
#include "xercesc/parsers/XercesDOMParser.hpp"
#include "xercesc/util/XMLException.hpp"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <map>
#include <string>
#include <vector>

using namespace boost;
using namespace xercesc;
namespace bf = boost::filesystem;

namespace foundation
{

namespace
{
    Dictionary& push(Dictionary& dictionary, const std::string& name)
    {
        if (!dictionary.dictionaries().exist(name.c_str()))
            dictionary.dictionaries().insert(name.c_str(), Dictionary());

        return dictionary.dictionaries().get(name.c_str());
    }

    const DOMNode* find_next_element_node(const DOMNode* node)
    {
        while (node && node->getNodeType() != DOMNode::ELEMENT_NODE)
            node = node->getNextSibling();

        return node;
    }
}

struct BenchmarkAggregator::Impl
{
    XercesCContext      m_xerces_context;
    XercesDOMParser     m_xerces_parser;

    const regex         m_filename_regex;

    typedef std::map<UniqueID, BenchmarkSeries> SeriesMap;

    Dictionary          m_benchmarks;
    SeriesMap           m_series;

    Impl()
      : m_filename_regex("benchmark\\.(\\d{8})\\.(\\d{6})\\.(\\d{3})\\.xml")
    {
        m_xerces_parser.setCreateCommentNodes(false);
    }

    bool scan_document(
        const DOMDocument*          document,
        const posix_time::ptime&    date)
    {
        assert(document);

        const DOMNode* node = find_next_element_node(document->getFirstChild());

        if (!node)
            return false;

        if (transcode(node->getNodeName()) != "benchmarkexecution")
            return false;

        const DOMNamedNodeMap* attributes = node->getAttributes();
        const DOMNode* config_attribute = attributes->getNamedItem(transcode("configuration").c_str());
        const std::string config = transcode(config_attribute->getNodeValue());

        Dictionary& suites_dic = push(m_benchmarks, config);

        scan_suites(node, date, suites_dic);

        return true;
    }

    void scan_suites(
        const DOMNode*              node,
        const posix_time::ptime&    date,
        Dictionary&                 suites_dic)
    {
        assert(node);

        node = node->getFirstChild();

        while (node)
        {
            if (node->getNodeType() == DOMNode::ELEMENT_NODE)
            {
                if (transcode(node->getNodeName()) == "benchmarksuite")
                {
                    const DOMNamedNodeMap* attributes = node->getAttributes();
                    const DOMNode* name_attribute = attributes->getNamedItem(transcode("name").c_str());
                    const std::string name = transcode(name_attribute->getNodeValue());

                    Dictionary& cases_dic = push(suites_dic, name);

                    scan_cases(node, date, cases_dic);
                }
            }

            node = node->getNextSibling();
        }
    }

    void scan_cases(
        const DOMNode*              node,
        const posix_time::ptime&    date,
        Dictionary&                 cases_dic)
    {
        assert(node);

        node = node->getFirstChild();

        while (node)
        {
            if (node->getNodeType() == DOMNode::ELEMENT_NODE)
            {
                if (transcode(node->getNodeName()) == "benchmarkcase")
                {
                    const DOMNamedNodeMap* attributes = node->getAttributes();
                    const DOMNode* name_attribute = attributes->getNamedItem(transcode("name").c_str());
                    const std::string name = transcode(name_attribute->getNodeValue());

                    UniqueID series_uid;

                    if (cases_dic.strings().exist(name.c_str()))
                        series_uid = cases_dic.get<UniqueID>(name.c_str());
                    else
                    {
                        series_uid = new_guid();
                        cases_dic.insert(name.c_str(), series_uid);
                    }

                    scan_results(node, date, m_series[series_uid]);
                }
            }

            node = node->getNextSibling();
        }
    }

    void scan_results(
        const DOMNode*              node,
        const posix_time::ptime&    date,
        BenchmarkSeries&            series)
    {
        assert(node);

        node = find_next_element_node(node->getFirstChild());

        if (!node)
            return;

        if (transcode(node->getNodeName()) != "results")
            return;

        node = node->getFirstChild();

        while (node)
        {
            if (node->getNodeType() == DOMNode::ELEMENT_NODE)
            {
                if (transcode(node->getNodeName()) == "ticks")
                {
                    node = node->getFirstChild();

                    if (node->getNodeType() == DOMNode::TEXT_NODE)
                    {
                        const std::string text = transcode(node->getTextContent());
                        const double ticks = from_string<double>(text);
                        series.push_back(BenchmarkDataPoint(date, ticks));
                    }

                    break;
                }
            }

            node = node->getNextSibling();
        }
    }
};

BenchmarkAggregator::BenchmarkAggregator()
  : impl(new Impl())
{
}

BenchmarkAggregator::~BenchmarkAggregator()
{
    delete impl;
}

void BenchmarkAggregator::clear()
{
    impl->m_benchmarks.clear();
    impl->m_series.clear();
}

bool BenchmarkAggregator::scan_file(const char* path)
{
    assert(path);

    if (!impl->m_xerces_context.is_initialized())
        return false;

    if (!bf::is_regular_file(path))
        return false;

    const std::string filename = bf::path(path).filename().string();

    smatch match;

    if (!regex_match(filename, match, impl->m_filename_regex))
        return false;

    const std::string date_string = match[1].str();
    const std::string time_string = match[2].str();
    const std::string iso_string = date_string + "T" + time_string;
    const posix_time::ptime date = posix_time::from_iso_string(iso_string);

    try
    {
        impl->m_xerces_parser.parse(path);
    }
    catch (const XMLException&)
    {
        return false;
    }
    catch (const DOMException&)
    {
        return false;
    }

    const DOMDocument* document = impl->m_xerces_parser.getDocument();

    if (!document)
        return false;

    return impl->scan_document(document, date);
}

void BenchmarkAggregator::scan_directory(const char* path)
{
    assert(path);

    if (!impl->m_xerces_context.is_initialized())
        return;

    if (!bf::is_directory(path))
        return;

    for (const bf::path& entry_path : boost::make_iterator_range(bf::directory_iterator(path)))
    {
        if (bf::is_regular_file(entry_path))
            scan_file(entry_path.string().c_str());
    }
}

void BenchmarkAggregator::sort_series()
{
    for (each<Impl::SeriesMap> i = impl->m_series; i; ++i)
    {
        if (i->second.empty())
            continue;

        std::sort(&i->second[0], &i->second[0] + i->second.size());
    }
}

const Dictionary& BenchmarkAggregator::get_benchmarks() const
{
    return impl->m_benchmarks;
}

const BenchmarkSeries& BenchmarkAggregator::get_series(const UniqueID case_uid) const
{
    const Impl::SeriesMap::const_iterator i = impl->m_series.find(case_uid);

    assert(i != impl->m_series.end());

    return i->second;
}

}   // namespace foundation
