using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace BenchTool
{
    public class TempFolder : IDisposable
    {
        public string FullPath { get; private set; }

        public string RootDirName { get; set; }

        public TempFolder()
        {
            RootDirName = Path.GetFileNameWithoutExtension(Path.GetRandomFileName());
            FullPath = Path.Combine(Environment.CurrentDirectory, ".tmp", RootDirName);
        }

        public void CreateIfNotExist()
        {
            if (!FullPath.IsEmptyOrWhiteSpace())
            {
                FullPath.CreateDirectoryIfNotExist();
            }
        }

        public void Dispose()
        {
            if (Directory.Exists(FullPath))
            {
                Directory.Delete(FullPath, recursive: true);
            }

            FullPath = null;
        }
    }

    public class TempFile : IDisposable
    {
        public string FullPath { get; private set; }
        public TempFile()
        {
            var fileName = Path.GetFileNameWithoutExtension(Path.GetRandomFileName());
            FullPath = Path.Combine(Path.GetTempPath(), fileName);
        }

        public void Dispose()
        {
            if (File.Exists(FullPath))
            {
                File.Delete(FullPath);
            }

            FullPath = null;
        }
    }
}
