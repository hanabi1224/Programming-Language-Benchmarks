import { contentFunc, IContentDocument } from '@nuxt/content/types/content'
import _ from 'lodash'
import findVersions from 'find-versions'

const lang2Display: { [key: string]: string } = {
  'csharp': 'C#',
}

export async function getLangBenchResults($content: contentFunc) {
  const pages = await $content('/', { deep: true }).fetch() as IContentDocument[]
  var benchResults = pages as unknown as BenchResult[]
  return mergeLangBenchResults(benchResults)
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
  const versions = findVersions(full, { loose: true })
  if (versions && versions.length > 0) {
    return versions[0]
  }

  // "Backfill for openjdk version 16"
  const match = /\d+(\.\d+){0,3}([_+]\d+)?/.exec(full)
  if (match) {
    return match[0]
  }

  return 'unknown'
}
