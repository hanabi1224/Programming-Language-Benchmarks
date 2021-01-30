using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace System
{
    public static class StringExtensions
    {
        public static bool IsEmptyOrWhiteSpace(this string s)
        {
            return string.IsNullOrWhiteSpace(s);
        }

        public static string FallBackTo(this string s, string fallback)
        {
            if (s.IsEmptyOrWhiteSpace())
            {
                return fallback;
            }

            return s;
        }

        public static void EnsureFileExists(this string path)
        {
            if (!File.Exists(path))
            {
                throw new FileNotFoundException(path);
            }
        }

        public static void EnsureDirectoryExists(this string path)
        {
            if (!Directory.Exists(path))
            {
                throw new DirectoryNotFoundException(path);
            }
        }

        public static bool IsDirectoryNotEmpty(this string path)
        {
            return !path.IsEmptyOrWhiteSpace()
                && Directory.Exists(path)
                && Directory.EnumerateFiles(path, string.Empty, SearchOption.AllDirectories).Any();
        }

        public static void CreateDirectoryIfNotExist(this string path)
        {
            if (!Directory.Exists(path))
            {
                Directory.CreateDirectory(path);
            }
        }

        public static ProcessStartInfo ConvertToCommand(this string command)
        {
            var array = command.Split(' ', 2, StringSplitOptions.RemoveEmptyEntries);
            return new ProcessStartInfo
            {
                FileName = array[0],
                Arguments = array.Length > 1 ? array[1] : null,
            };
        }

        public static string WrapCommandWithSh(this string command)
        {
            return $"sh -c \"{command}\"";
        }
    }
}
