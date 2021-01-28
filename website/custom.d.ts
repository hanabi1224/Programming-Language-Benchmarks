declare module "*.vue" {
  import V from 'vue'
  export default interface Vue extends V {
  }
}

type osType = 'linux' | 'osx' | 'windows';

type LangBenchResults = {
  lang: string,
  langDisplay: string,
  benchmarks: BenchResult[],
};

type BenchResult = {
  lang: string,
  os: osType,
  compiler: string,
  compilerVersion: string,
  test: string,
  input: string,
  code: string,
  timeMS: number,
  memBytes: number,
  cpuTimeMS: number,

  compilerOptions?: string,
};

type LangPageMeta = {
  lang: LangBenchResults,
  all: LangBenchResults[],
  other?: LangBenchResults,
};
