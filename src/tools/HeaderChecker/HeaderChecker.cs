
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
using System.Collections;
using System.Diagnostics;
using System.IO;

namespace headerchecker
{
    class HeaderChecker
    {
        // Return true if a given source file compiles.
        static bool CheckFile(
            String sourcePath,
            String searchPaths)
        {
            Console.WriteLine("Checking {0}", sourcePath);

            // Add quotes if source filename has spaces.
            if (sourcePath.IndexOf(" ") != -1)
                sourcePath = "\"" + sourcePath + "\"";

            // Create compiler command line.
            String command = "cl.exe";
            String arguments = "/nologo /TP /EHsc /MTd /c ";
            arguments += sourcePath;
            arguments += searchPaths;

            // Create compiler process.
            Process compiler = new Process();
            compiler.StartInfo.UseShellExecute = false;
            compiler.StartInfo.RedirectStandardOutput = true;
            compiler.StartInfo.FileName = command;
            compiler.StartInfo.Arguments = arguments;

            // Run compiler, recording standard output.
            String output;
            try
            {
                compiler.Start();
                output = compiler.StandardOutput.ReadToEnd();
                compiler.WaitForExit();
            }
            catch (Exception)
            {
                Console.WriteLine();
                Console.WriteLine("Error, failed to execute compiler command line:");
                Console.WriteLine("{0} {1}", command, arguments);
                return false;
            }

            // Display compiler output if compilation failed.
            if (compiler.ExitCode == 0)
                return true;
            else
            {
                Console.WriteLine();
                Console.WriteLine("Error:");
                Console.WriteLine("{0} {1}", command, arguments);
                Console.WriteLine(output);
                return false;
            }
        }

		// The main entry point for the application.
		static void Main(String[] args)
		{
            // Check command line and print usage if incomplete.
            if (args.Length < 1)
            {
                Console.WriteLine("Usage: HeaderChecker filename [search paths...]");
                Console.WriteLine("");
                Console.WriteLine("Wildcards are accepted.");
                return;
            }

            // Collect command line arguments.
            String mask = args[0];

            // Build search paths command line.
            String searchPaths = String.Empty;
            for (int i = args.GetLowerBound(0) + 1; i <= args.GetUpperBound(0); ++i)
            {
                searchPaths += " /I" + args[i];
                if (!searchPaths.EndsWith("/"))
                    searchPaths += "\\";
                searchPaths += "";
            }

            // Establish the list of files matching the mask.
            String[]  filenames = Directory.GetFiles(".", mask);

            // Print a message and exit if the filename list is empty.
            if (filenames.Length == 0)
            {
                Console.WriteLine("No files found.");
                return;
            }

            // Call CheckFile() on each file matching the mask.
            ArrayList failures = new ArrayList();
            foreach (String sourcePath in filenames)
            {
                // Check this source file.
                if (CheckFile(sourcePath, searchPaths) == false)
                    failures.Add(sourcePath);

                // Remove object file if one was created.
                String objectFile = Path.ChangeExtension(sourcePath, ".obj");
                if (File.Exists(objectFile))
                    File.Delete(objectFile);
            }

            // Print results.
            Console.WriteLine();
            Console.Write("Finished, ");
            if (failures.Count == 0)
            {
                Console.WriteLine("all files compile.");
            }
            else
            {
                Console.WriteLine("{0} file(s) failed to compile:", failures.Count);
                foreach (String failedFile in failures)
                    Console.WriteLine("  {0}", failedFile);
            }
        }
	}
}
