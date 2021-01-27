import { contentFunc, IContentDocument } from '@nuxt/content/types/content';
import _ from 'lodash';

const lang2Display: { [key: string]: string } = {
  'csharp': 'C#',
  'java': 'Java',
};

export async function getLangBenchResults($content: contentFunc) {
  const pages = await $content('/', { deep: true }).fetch() as IContentDocument[];
  var groupsByLang = _.chain(pages as unknown as BenchResult[]).filter(i => !!i.lang).groupBy(i => i.lang).value();
  var r: LangBenchResults[] = [];
  for (var k in groupsByLang) {
    r.push({ lang: k, langDisplay: lang2Display[k] ?? k, benchmarks: groupsByLang[k] });
  }
  return r;
}
