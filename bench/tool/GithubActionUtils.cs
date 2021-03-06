using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BenchTool
{
    // https://docs.github.com/en/actions/reference/environment-variables
    public static class GithubActionUtils
    {
        public static string RunId => Environment.GetEnvironmentVariable("GITHUB_RUN_ID");

        public static bool IsGithubBuild => !RunId.IsEmptyOrWhiteSpace();

        public static string GithubHeadRef => Environment.GetEnvironmentVariable("GITHUB_HEAD_REF");

        public static bool IsPullRequest => !GithubHeadRef.IsEmptyOrWhiteSpace();
    }
}
