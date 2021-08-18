using System;
using System.IO;
using NLog;
using NLog.Config;

namespace BenchTool
{
    public static class NLogUtils
    {
        public static void Configure()
        {
            string configPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "nlog.config");
            if (File.Exists(configPath))
            {
                LogManager.Configuration = XmlLoggingConfiguration.CreateFromXmlString(File.ReadAllText(configPath));
            }
        }
    }
}
