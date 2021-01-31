<template>
  <div class="flex flex-wrap relative">
    <aside class="block w-1/6">
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
            :href="
              isLinkActive(i.lang, '') ? `javascript:void(0)` : `/${i.lang}`
            "
            :class="getLinkClass(i.lang, '')"
            >{{ i.langDisplay }}</a
          >
        </li>
      </ul>
    </aside>
    <div class="block w-4/6 pr-4">
      <h1 v-if="lang && !other" class="text-3xl">
        All {{ lang.langDisplay }} benchmarks
      </h1>
      <h1 v-if="other" class="text-3xl">
        {{ lang.langDisplay }} Versus {{ other.langDisplay }} benchmarks
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
          Your
          <a
            class="underline bold text-blue-500"
            href="https://github.com/hanabi1224/Another-Benchmarks-Game"
            target="_blank"
            >CONTRIBUTION</a
          >
          is WELCOME!
        </p>
      </div>
      <div class="mt-5">
        <span>OS</span>
        <select v-model="osSelected" class="px-2 py-2 rounded bg-purple-200">
          <option v-for="i in osOptions" :key="i" :value="i">{{ i }}</option>
        </select>
      </div>

      <div v-for="test in testOptions" :key="test">
        <h2 class="text-2xl my-5 mb-2 underline text-blue-500">
          <a :href="`/problem/${test}`"> {{ test }} </a>
        </h2>
        <div v-for="input in getInputs(test, input)" :key="input" class="mt-5">
          <h3 class="text-base">Input: {{ input }}</h3>
          <table class="table-auto w-full text-base leading-loose">
            <tr class="border-b-2 border-dotted py-1">
              <th v-show="other || problem" class="text-left">lang</th>
              <th class="text-right">code</th>
              <!-- <th class="text-right">N</th> -->
              <th class="text-right">time(ms)</th>
              <th class="text-right">mem</th>
              <th class="text-right">cpu-time(ms)</th>
              <th class="text-left pl-5">compiler</th>
            </tr>
            <tbody>
              <tr
                v-for="(i, idx) in filterBenches(test, input)"
                :key="idx"
                :class="
                  'border-b-2 border-dotted py-1 ' +
                  (idx % 2 == 0 ? 'bg-gray-200' : '')
                "
              >
                <td v-show="other || problem" class="text-left">
                  <a :href="`/${i.lang}`">{{ i.lang }}</a>
                </td>
                <td class="text-right">
                  <a
                    :href="`https://github.com/hanabi1224/Another-Benchmarks-Game/blob/main/bench/algorithm/${test}/${i.code}`"
                    target="_blank"
                    class="underline text-blue-500"
                    >{{ i.code }}</a
                  >
                </td>
                <!-- <td class="text-right">{{ i.input }}</td> -->
                <td class="text-right">{{ i.timeMS.toFixed(2) }}</td>
                <td class="text-right">
                  {{ (i.memBytes / (1024 * 1024)).toFixed(2) }}MB
                </td>
                <td class="text-right">{{ i.cpuTimeMS.toFixed(2) }}</td>
                <td class="text-left pl-5" :title="getFullCompilerVersion(i)">
                  {{ i.compiler }} {{ i.compilerVersion }}
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>
    <aside class="block w-1/6">
      <div v-if="problem">
        <h2 class="text-xl">Problems</h2>
        <ul class="text-base">
          <li
            v-for="(i, idx) in allProblems"
            :key="idx"
            class="text-light-onSurfacePrimary"
          >
            <a
              :href="i === problem ? `javascript:void(0)` : `/problem/${i}`"
              :class="getLinkClass(i, i)"
            >
              {{ i }}</a
            >
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
              :href="
                isLinkActive(lang.lang, i.lang)
                  ? `javascript:void(0)`
                  : `/${lang.lang}-vs-${i.lang}`
              "
              :class="getLinkClass(lang.lang, i.lang)"
            >
              {{ lang.langDisplay }} VS {{ i.langDisplay }}</a
            >
          </li>
        </ul>
      </div>
    </aside>
  </div>
</template>
<script lang="ts">
import Vue from 'vue'
import { Component, Watch } from 'vue-property-decorator'
import $ from 'jquery'
import _ from 'lodash'
import { getFullCompilerVersion } from '~/contentUtils'

@Component({
  components: {},
})
export default class LangMetaPage extends Vue {
  meta?: LangPageMeta
  problem?: string
  allProblems?: string[]
  lang?: LangBenchResults
  other?: LangBenchResults
  langs?: LangBenchResults[]

  activeBenchmarks: BenchResult[] = []

  langOptions: string[] = []
  testOptions: string[] = []

  osOptions: string[] = []
  compilerOptions: string[] = []
  compilerVersionOptions: string[] = []
  compilerOptionOptions: string[] = []

  osSelected = ''
  compilerSelected = ''
  compilerVersionSelected = ''
  compilerOptionSelected = ''

  get otherLangs() {
    return _.chain(this.langs)
      .filter((i) => i.lang !== this.lang?.lang)
      .value()
  }

  get buildLogUrl() {
    const buildId = this.activeBenchmarks[0].appveyorBuildId
    return `https://ci.appveyor.com/project/hanabi1224/another-benchmarks-game/builds/${buildId}`
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
      .orderBy((i) => parseInt(i) ?? i)
      .value()
  }

  filterBenches(test: string, input: string) {
    let exp = _.chain(this.activeBenchmarks).filter(
      (i) => i.test === test && i.os === this.osSelected && i.input === input
    )

    if (this.other?.benchmarks && this.other?.benchmarks?.length > 0) {
      exp = exp.unionWith(
        _.chain(this.other?.benchmarks)
          .filter((i) => i.test === test && i.os === this.osSelected)
          .groupBy((i) => i.input)
          .value()
      )
    }

    if (!this.problem) {
      // Lang page sort
      exp = exp.orderBy(
        ['input', 'timeMS', 'os', 'lang', 'compiler', 'compilerVersion'],
        ['asc', 'asc', 'asc', 'asc', 'asc', 'asc']
      )
    }

    return exp.value()
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

  mounted() {
    // Update head
    let title = ''
    if (this.problem) {
      title = `${this.problem} - benchmarks game`
    } else {
      title = `${this.lang?.langDisplay} ${
        this.other ? 'VS ' + this.other?.langDisplay : ''
      } benchmarks game`
    }

    $('head title').text(title)

    const metaDesc = $('head meta[name="description"]')
    const metaContent = metaDesc.attr('content') as string
    const langsStr = _.chain(this.langs)
      .map((i) => i.langDisplay)
      .uniq()
      .value()
    metaDesc.attr('content', `${metaContent}, ${title}, ${langsStr}`)
  }

  created() {
    this.meta = this.$route.meta
    this.problem = this.meta?.problem
    this.allProblems = this.meta?.allProblems
    this.lang = this.meta?.lang
    this.other = this.meta?.other
    this.langs = _.chain(this.meta?.all).value()

    this.activeBenchmarks =
      this.lang?.benchmarks ??
      _.chain(this.langs)
        .flatMap((i) => i.benchmarks)
        .filter((i) => i.test === this.problem)
        .orderBy(['input', 'timeMS'], ['asc', 'asc'])
        .value()

    this.langOptions = _.chain(this.meta?.all)
      .map((i) => i.lang)
      .uniq()
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
