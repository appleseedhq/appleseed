
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

using System;
using System.IO;
using System.Text.RegularExpressions;

namespace buildtracker
{
    class BuildTracker
    {
        static void Main(string[] args)
        {
            // Check command line and print usage if incomplete.
            if (args.Length < 2)
            {
                Console.WriteLine("Usage: BuildTracker filename token");
                return;
            }

            // Collect command line arguments.
            string path = args[0];
            string token = args[1];

            // Read the contents of the file.
            string contents;
            try
            {
                contents = File.ReadAllText(path);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                return;
            }

            // Find all build counters.
            Regex regexp = new Regex(@"(?<decl>\b" + token + @"\b\s*=\s*)(?<value>\d+)");
            MatchCollection matches = regexp.Matches(contents);

            // Increase all build counters.
            foreach (Match match in matches)
            {
                int value = Int32.Parse(match.Groups["value"].Value);
                value += 1;
                contents = regexp.Replace(contents, "${decl}" + value.ToString(), 1, match.Index);
            }

            // Write back the new file contents.
            try
            {
                File.WriteAllText(path, contents);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                return;
            }

            Console.WriteLine("Successfully updated {0} build counter{1}.",
                matches.Count, matches.Count > 1 ? "s" : "");
        }
    }
}
