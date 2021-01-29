using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using NLog;
using NLog.Config;

namespace BenchTool
{
    public static class NLogUtils
    {
        public static void Configure()
        {
            var configPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "nlog.config");
            if (File.Exists(configPath))
            {
                LogManager.Configuration = XmlLoggingConfiguration.CreateFromXmlString(File.ReadAllText(configPath));
            }
        }
    }
}
