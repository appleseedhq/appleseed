
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
using System.Collections.Generic;
using System.IO;

namespace GenerateCMakeLists
{
    class Program
    {
        static void Main(string[] args)
        {
            var currentDirectory = new DirectoryInfo(Directory.GetCurrentDirectory());

            new VisitorGenerator(currentDirectory).VisitGenerate();
        }
    };

    class VisitorGenerator
    {
        private readonly DirectoryInfo startDirectory;

        private readonly string parentVariableName;

        public VisitorGenerator(DirectoryInfo startDirectory)
        {
            this.startDirectory = startDirectory;

            parentVariableName = startDirectory.Name + "_sources";
        }

        public void VisitGenerate()
        {
            EmitHeader();

            VisitDirectory(startDirectory);

            EmitFooter();
        }

        private static List<FileInfo> GetFiles(DirectoryInfo directoryInfo, params string[] patterns)
        {
            var files = new List<FileInfo>();

            foreach (var pattern in patterns)
            {
                foreach (var file in directoryInfo.GetFiles(pattern))
                {
                    files.Add(file);
                }
            }

            return files;
        }

        private void VisitDirectory(DirectoryInfo directoryInfo)
        {
            foreach (var subDirectory in directoryInfo.GetDirectories())
            {
                VisitDirectory(subDirectory);
            }

            var files = GetFiles(directoryInfo, "*.h", "*.cpp");

            files.Sort((f1, f2) => f1.Name.CompareTo(f2.Name));

            if (files.Count > 0)
            {
                GenerateSection(directoryInfo, files);
            }
        }

        private string GetRelativePath(string path)
        {
            return path.Substring(startDirectory.FullName.Length).Trim('\\');
        }

        private void GenerateSection(DirectoryInfo directoryInfo, List<FileInfo> files)
        {
            string relativePath = GetRelativePath(directoryInfo.FullName);
            string sourcesVariableName = relativePath.Replace('\\', '_') + "_sources";

            Console.WriteLine(string.Format("set({0}", sourcesVariableName));

            foreach (var file in files)
            {
                string relativeFilePath = GetRelativePath(file.FullName);

                relativeFilePath = relativeFilePath.Replace('\\', '/');
                relativeFilePath = relativeFilePath.Replace(" ", "\\ ");

                Console.WriteLine("    " + relativeFilePath);
            }

            Console.WriteLine(")");

            Console.WriteLine(
                string.Format(
                    "list(APPEND {0}\n" +
                    "    ${{{1}}}\n" +
                    ")", parentVariableName, sourcesVariableName));

            string sourceGroupPath = relativePath.Replace("\\", "\\\\");
            Console.WriteLine(
                string.Format(
                    "source_group(\"{0}\" FILES\n" +
                    "    ${{{1}}}\n" +
                    ")", sourceGroupPath, sourcesVariableName));

            Console.WriteLine();
        }

        private void EmitHeader()
        {
        }

        private void EmitFooter()
        {
            Console.WriteLine(
                string.Format("add_library({0} SHARED ${{{1}}})", startDirectory.Name, parentVariableName));
        }
    }
}
