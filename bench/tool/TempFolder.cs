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
            //FullPath = Path.Combine(Environment.CurrentDirectory, ".tmp", RootDirName);
            FullPath = Path.Combine(Path.GetTempPath(), RootDirName);
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
                try
                {
                    Directory.Delete(FullPath, recursive: true);
                }
                catch (IOException e)
                {
                    Console.Error.WriteLine(e);
                }
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
            //FullPath = Path.Combine(Environment.CurrentDirectory, ".tmp", fileName);
            FullPath = Path.Combine(Path.GetTempPath(), fileName);
        }

        public void Dispose()
        {
            if (File.Exists(FullPath))
            {
                try
                {
                    File.Delete(FullPath);
                }
                catch (IOException e)
                {
                    Console.Error.WriteLine(e);
                }
            }

            FullPath = null;
        }
    }
}
