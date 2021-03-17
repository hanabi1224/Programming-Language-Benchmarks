import { contentFunc, IContentDocument } from '@nuxt/content/types/content';
import _ from 'lodash';

const lang2Display: { [key: string]: string } = {
  'csharp': 'C#',
};

export async function getLangBenchResults($content: contentFunc) {
  const pages = await $content('/', { deep: true }).fetch() as IContentDocument[];
  var benchResults = pages as unknown as BenchResult[];
  return mergeLangBenchResults(benchResults);
}

export function mergeLangBenchResults(benchResults: BenchResult[]) {
  benchResults = _.chain(benchResults).filter(i => !!i.lang).value();
  benchResults.forEach(i => {
    i.compilerVersion = getRealShortCompilerVersion(i)
  })

  var groupsByLang = _.chain(benchResults).groupBy(i => i.lang).value();
  var r: LangBenchResults[] = [];
  for (var k in groupsByLang) {
    const benches = groupsByLang[k];
    console.log(`${k}: ${benches.length} benchmark results`)
    r.push({ lang: k, langDisplay: lang2Display[k] ?? _.capitalize(k), benchmarks: benches })
  }

  return _.chain(r).orderBy(['langDisplay'], ['asc']).value()
}

export function getFullCompilerVersion(i: BenchResult) {
  return (i.testLog?.runtimeVersion ?? '') + (i.buildLog?.compilerVersion ?? '')
}

export function getRealShortCompilerVersion(i: BenchResult) {
  const full = getFullCompilerVersion(i)
  const match = /\d+\.\d+(\.\d+)?/.exec(full)
  return match ? match[0] : 'unknown'
}
