<template>
  <div class="flex flex-wrap relative">
    <menu-button @toggle="toggleMenu"></menu-button>
    <aside :class="sideBarClass">
      <h2 title="Benchmarks for other computer languages" class="text-xl">
        Languages
      </h2>
      <ul class="text-base">
        <li
          v-for="(i, idx) in langs"
          :key="idx"
          class="text-light-onSurfacePrimary"
        >
          <a
            v-if="!isLinkActive(i.lang, '')"
            :href="`/${i.lang}`"
            :class="getLinkClass(i.lang, '')"
            >{{ i.langDisplay }}</a
          >
          <span
            v-if="isLinkActive(i.lang, '')"
            :class="getLinkClass(i.lang, '')"
            >{{ i.langDisplay }}</span
          >
        </li>
      </ul>
    </aside>
    <div :class="contentClass">
      <h1 v-if="lang && !other" class="text-3xl">
        All {{ lang.langDisplay }} benchmarks
      </h1>
      <h1 v-if="other" class="text-3xl">
        {{ lang.langDisplay }} VS {{ other.langDisplay }} benchmarks
      </h1>
      <h1 v-if="problem" class="text-3xl">
        All {{ problem }} problem benchmarks
      </h1>
      <div class="text-base italic leading-loose">
        <p class="py-3">
          Current benchmark data was generated on
          <span class="text-pink-800">{{ benchmarkDate }}</span
          >, full log can be found
          <a
            :href="buildLogUrl"
            target="_blank"
            class="underline bold text-blue-500"
            >HERE</a
          >
        </p>
        <p>
          <a
            class="underline bold text-blue-500"
            href="https://github.com/hanabi1224/Programming-Language-Benchmarks"
            target="_blank"
            >CONTRIBUTIONS</a
          >
          are WELCOME!
        </p>
      </div>

      <!-- <div class="mt-5">
        <span>OS</span>
        <select v-model="osSelected" class="px-2 py-2 rounded bg-purple-200">
          <option v-for="i in osOptions" :key="i" :value="i">{{ i }}</option>
        </select>
      </div> -->

      <div v-if="cpuInfo" class="mt-5 text-xs">
        <label class="font-bold">CPU INFO:</label
        ><span class="italic">{{ cpuInfo }}</span>
      </div>

      <div class="mt-5 text-xs">
        <p class="italic">
          * -m in a file name stands for multi-threading or multi-processing
        </p>
        <p class="italic">
          * -i in a file name stands for direct intrinsics usage. (Usage of simd
          intrinsics via libraries is not counted)
        </p>
        <p class="italic">
          * -ffi in a file name stands for non-stdlib
          <a
            href="http://en.wikipedia.org/wiki/Foreign_function_interface"
            class="underline text-blue-500"
            >FFI</a
          >
          usage
        </p>
        <p class="italic">
          * (You may find time &lt; time(user) + time(sys) for some
          non-parallelized programs, the overhead is from GC or JIT compiler,
          which are allowed to take advantage of multi-cores as that's more
          close to real-world scenarios.)
        </p>
      </div>

      <div class="mt-5 text-xs">
        <div class="form-check form-check-inline">
          <input
            v-model="show_st"
            class="form-check-input inline-block"
            type="checkbox"
            style="vertical-align: bottom"
          />
          <label class="form-check-label inline-block text-gray-500"
            >show numbers without parallelization</label
          >
        </div>
        <div class="mt-2 form-check form-check-inline">
          <input
            v-model="show_mt"
            class="form-check-input inline-block"
            type="checkbox"
            style="vertical-align: bottom"
          />
          <label class="form-check-label inline-block text-gray-500"
            >show numbers with parallelization</label
          >
        </div>
      </div>

      <div v-for="test in testOptions" :key="test">
        <h2 class="text-2xl my-5 mb-2 underline text-blue-500">
          <a :id="test" :href="`/problem/${test}`"> {{ test }} </a>
        </h2>
        <div v-for="input in getInputs(test)" :key="input" class="mt-5">
          <h3 class="text-base font-bold text-red-800">Input: {{ input }}</h3>
          <table class="table-auto w-full text-xs leading-loose">
            <tr class="border-b-2 border-dotted py-1">
              <th
                v-show="other || problem"
                :class="['text-left', 'pl-4', mdHide]"
              >
                lang
              </th>
              <th class="text-right">code</th>
              <!-- <th class="text-right">N</th> -->
              <th class="text-right" title="total time">time</th>
              <th
                :class="['text-right', mdHide]"
                title="total time standard deviation"
              >
                stddev
              </th>
              <th class="text-right">
                <span class="md-hide">peak-mem</span>
                <span class="md-show">mem</span>
              </th>
              <th :class="['text-right', mdHide]" title="cpu-time-user">
                time(user)
              </th>
              <th :class="['text-right', mdHide]" title="cpu-time-kernel">
                time(sys)
              </th>
              <th class="text-left pl-5">
                <span class="md-show">compiler</span>
                <span class="md-hide">compiler/runtime</span>
              </th>
            </tr>
            <tbody class="font-light">
              <tr
                v-for="(i, idx) in filterBenches(test, input)"
                :key="idx"
                :class="
                  'border-b-2 border-dotted py-1 ' +
                  (idx % 2 == 0 ? 'bg-gray-200' : '')
                "
              >
                <td
                  v-show="other || problem"
                  :class="['text-left', 'pl-4', mdHide]"
                >
                  <a
                    :href="`/${i.lang}`"
                    class="underline text-blue-500"
                    >{{ i.lang }}</a
                  >
                </td>
                <td class="text-right">
                  <a
                    :href="`https://github.com/hanabi1224/Programming-Language-Benchmarks/blob/main/bench/algorithm/${test}/${i.code}`"
                    target="_blank"
                    class="underline text-blue-500"
                    >{{ getNormalizedCode(i) }}</a
                  >
                </td>
                <!-- <td class="text-right">{{ i.input }}</td> -->
                <td class="text-right">
                  {{ msToText(i.timeMS) }}
                </td>
                <td :class="['text-right', mdHide]">
                  {{ msToFixed(i.timeStdDevMS) }}ms
                </td>
                <td class="text-right">
                  {{ (i.memBytes / (1024 * 1024)).toFixed(1) }}MB
                </td>
                <td :class="['text-right', mdHide]">
                  {{ i.cpuTimeUserMS.toFixed(0) }}ms
                </td>
                <td :class="['text-right', mdHide]">
                  {{ i.cpuTimeKernelMS.toFixed(0) }}ms
                </td>
                <td class="text-left pl-5" :title="getFullCompilerVersion(i)">
                  {{ i.compiler }} {{ i.compilerVersion }}
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>
    <aside :class="sideBarClass">
      <div class="pb-5">
        <h2 class="text-xl">Problems</h2>
        <ul class="text-base">
          <li
            v-for="(i, idx) in allProblems"
            :key="idx"
            class="text-light-onSurfacePrimary"
          >
            <a
              v-if="i != problem"
              :href="`/problem/${i}`"
              :class="getLinkClass(i, i)"
            >
              {{ i }}</a
            >
            <span v-if="i == problem" :class="getLinkClass(i, i)">{{ i }}</span>
          </li>
        </ul>
      </div>
      <div v-if="!problem">
        <h2 class="text-xl">Compare</h2>
        <ul class="text-base">
          <li
            v-for="(i, idx) in otherLangs"
            :key="idx"
            class="text-light-onSurfacePrimary"
          >
            <a
              v-if="!isLinkActive(lang.lang, i.lang)"
              :href="`/${lang.lang}-vs-${i.lang}`"
              :class="getLinkClass(lang.lang, i.lang)"
            >
              {{ lang.langDisplay }} VS {{ i.langDisplay }}</a
            >
            <span
              v-if="isLinkActive(lang.lang, i.lang)"
              :class="getLinkClass(lang.lang, i.lang)"
              >{{ lang.langDisplay }} VS {{ i.langDisplay }}</span
            >
          </li>
        </ul>
      </div>
    </aside>
  </div>
</template>
<script lang="ts">
import { Component, Watch, Vue } from 'nuxt-property-decorator'
import _ from 'lodash'
import MenuButton from './MenuButton.vue'
import { getFullCompilerVersion, mergeLangBenchResults } from '~/contentUtils'

function requireAll(requireContext: any) {
  const r = requireContext.keys().map(requireContext)
  return mergeLangBenchResults(r)
}
const langs = requireAll((require as any).context('../content', true, /.json$/))
const problems = _.chain(langs)
  .flatMap((i) => i.benchmarks)
  .map((i) => i.test)
  .uniq()
  .sort()
  .value()

Component.registerHooks(['head'])

@Component({
  components: {
    MenuButton,
  },
})
export default class LangMetaPage extends Vue {
  isMenuOn = false
  show_st = true
  show_mt = true
  meta?: LangPageMeta
  problem?: string
  allProblems?: string[] = problems
  lang?: LangBenchResults
  other?: LangBenchResults
  langs: LangBenchResults[] = langs

  activeBenchmarks: BenchResult[] = []

  testOptions: string[] = []

  osOptions: string[] = []
  compilerOptions: string[] = []
  compilerVersionOptions: string[] = []
  compilerOptionOptions: string[] = []

  osSelected = ''
  compilerSelected = ''
  compilerVersionSelected = ''
  compilerOptionSelected = ''

  toggleMenu() {
    this.isMenuOn = !this.isMenuOn
  }

  msToText(ms: number): string {
    return ms <= 0 ? 'timeout' : `${this.msToFixed(ms)}ms`
  }

  msToFixed(ms: number): string {
    return ms < 10 ? ms.toFixed(1) : ms.toFixed(0)
  }

  get cpuInfo() {
    return this.activeBenchmarks[0].cpuInfo
  }

  get sideBarClass() {
    return this.isMenuOn ? 'block w-1/6 half-width' : 'block w-1/6 md-hide'
  }

  get contentClass() {
    return this.isMenuOn
      ? 'block w-4/6 pr-4 md-hide'
      : 'block w-4/6 full-width mx-auto'
  }

  get mdHide() {
    return 'md-hide'
  }

  get otherLangs() {
    return _.chain(this.langs)
      .filter((i) => i.lang !== this.lang?.lang)
      .value()
  }

  get buildLogUrl() {
    // const buildId = this.activeBenchmarks[0].appveyorBuildId
    // return `https://ci.appveyor.com/project/hanabi1224/another-benchmarks-game/builds/${buildId}`
    const runId = this.activeBenchmarks[0].githubRunId
    return `https://github.com/hanabi1224/Programming-Language-Benchmarks/actions/runs/${runId}`
  }

  get benchmarkDate() {
    const ts = this.activeBenchmarks[0].testLog.finished as string
    return new Date(ts).toDateString()
  }

  getFullCompilerVersion(i: BenchResult) {
    return getFullCompilerVersion(i)
  }

  isLinkActive(lang: string, otherLang: string) {
    if (this.problem) {
      // HACK: use lang or otherLang to represent current problem
      return this.problem === lang
    } else if (otherLang && this.other) {
      return this.other?.lang === otherLang
    } else if (!otherLang && !this.other) {
      return this.lang?.lang === lang
    }

    return false
  }

  getLinkClass(lang: string, otherLang: string) {
    const baseClass =
      'p-1 pl-3 flex rounded transition-colors duration-300 ease-linear justify-between'
    const activeClass = ' text-gray-700 cursor-default'
    const inactiveClass = ' underline text-blue-500 hover:text-green-400'
    return (
      baseClass +
      (this.isLinkActive(lang, otherLang) ? activeClass : inactiveClass)
    )
  }

  getInputs(test: string) {
    return _.chain(this.activeBenchmarks)
      .filter((i) => i.test === test && i.os === this.osSelected)
      .map((i) => i.input)
      .uniq()
      .orderBy((i) => parseInt(i) ?? i, 'desc')
      .value()
  }

  filterBenches(test: string, input: string) {
    let exp = _.chain(this.activeBenchmarks).filter(
      (i) => i.test === test && i.os === this.osSelected && i.input === input
    )

    if (this.other?.benchmarks && this.other?.benchmarks?.length > 0) {
      exp = exp.unionWith(
        _.chain(this.other?.benchmarks)
          .filter(
            (i) =>
              i.test === test && i.os === this.osSelected && i.input === input
          )
          .value()
      )
    }

    if (!this.problem) {
      // Lang page sort
      exp = exp.orderBy(
        [
          'input',
          'timeout',
          'timeMS',
          'os',
          'lang',
          'compiler',
          'compilerVersion',
        ],
        ['asc', 'asc', 'asc', 'asc', 'asc', 'asc', 'asc']
      )
    }

    exp = exp.filter(
      (i) => (!i.par && this.show_st) || (!!i.par && this.show_mt)
    )

    return exp.value()
  }

  getNormalizedCode(i: BenchResult) {
    return !i.code.includes('-') && i.par ? i.code.replace('.', '-m.') : i.code
  }

  @Watch('compilerSelected')
  onPropertyChanged(value: string, oldValue: string) {
    if (value !== oldValue) {
      this.compilerVersionOptions = _.chain(this.activeBenchmarks)
        .filter((i) => i.compiler === this.compilerSelected)
        .map((i) => i.compilerVersion)
        .uniq()
        .value()
      this.compilerVersionSelected = this.compilerVersionOptions[0]
    }
  }

  head() {
    const suffix =
      'benchmarks, Which programming language or compiler is faster'
    let title = ''
    if (this.problem) {
      title = `${this.problem} - ${suffix}`
    } else {
      title = `${this.lang?.langDisplay}${
        this.other ? ' VS ' + this.other?.langDisplay : ''
      } ${suffix}`
    }

    let langsStrs: string[] = []
    if (this.problem) {
      langsStrs = _.chain(this.langs)
        .map((i) => i.langDisplay)
        .uniq()
        .unionWith(
          _.chain(this.langs)
            .map((i) => `${i.langDisplay} lang`)
            .uniq()
            .value()
        )
        .value()
      langsStrs.push(this.problem)
    } else if (this.other) {
      langsStrs = _.chain(this.lang?.benchmarks)
        .map((i) => i.test)
        .uniq()
        .value()
      langsStrs.push(this.lang!.langDisplay)
      langsStrs.push(`${this.lang!.langDisplay} lang`)
      langsStrs.push(this.other.langDisplay)
      langsStrs.push(`${this.other.langDisplay} lang`)
    } else {
      langsStrs = _.chain(this.lang?.benchmarks)
        .map((i) => i.test)
        .uniq()
        .value()
      langsStrs.push(this.lang!.langDisplay)
      langsStrs.push(`${this.lang!.langDisplay} lang`)
    }

    return {
      title,
      meta: [
        {
          hid: 'description',
          name: 'description',
          content: `benchmarks,benchmark,performance,${langsStrs.join(',')}`,
        },
      ],
    }
  }

  created() {
    this.meta = this.$route.meta
    this.problem = this.meta?.problem
    if (this.meta?.lang) {
      this.lang = _.chain(langs).find({ lang: this.meta.lang }).value()
    }
    if (this.meta?.other) {
      this.other = _.chain(langs).find({ lang: this.meta.other }).value()
    }

    this.activeBenchmarks =
      this.lang?.benchmarks ??
      _.chain(this.langs)
        .flatMap((i) => i.benchmarks)
        .filter((i) => i.test === this.problem)
        .orderBy(['input', 'timeout', 'timeMS'], ['asc', 'asc', 'asc'])
        .value()

    this.testOptions = _.chain(this.activeBenchmarks)
      .map((i) => i.test)
      .uniq()
      .value()

    this.osOptions = _.chain(this.activeBenchmarks)
      .map((i) => i.os)
      .uniq()
      .value()
    this.osSelected = this.osOptions[0]

    this.compilerOptions = _.chain(this.activeBenchmarks)
      .map((i) => i.compiler)
      .uniq()
      .value()
    this.compilerSelected = this.compilerOptions[0]
  }
}
</script>
