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
