<template>
  <div class="flex flex-wrap relative">
    <aside class="block w-1/6">
      <h2 title="Benchmarks for other computer languages" class="text-xl">
        Benchmarks
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
      <h1 v-if="!other" class="text-3xl">
        All {{ lang.langDisplay }} benchmarks
      </h1>
      <h1 v-if="other" class="text-3xl">
        {{ lang.langDisplay }} Versus {{ other.langDisplay }} benchmarks
      </h1>
      <div class="text-sm italic leading-loose">
        <p class="py-3">
          Benchmark data was generated on
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
      <!-- <span>Compiler</span>
    <select v-model="compilerSelected">
      <option v-for="i in compilerOptions" :value="i">{{ i }}</option>
    </select>
    <span>Version</span>
    <select v-model="compilerVersionSelected">
      <option v-for="i in compilerVersionOptions" :value="i">{{ i }}</option>
    </select> -->

      <div v-for="test in testOptions" :key="test">
        <h2 class="text-2xl my-5 mb-2">{{ test }}</h2>
        <table class="table-auto w-full italic text-sm leading-loose">
          <tr class="border-b-2 border-dotted py-1">
            <th v-show="other" class="text-left">lang</th>
            <th class="text-right">code</th>
            <th class="text-right">N</th>
            <th class="text-right">t(ms)</th>
            <th class="text-right">mem</th>
            <th class="text-right">cpu-t(ms)</th>
            <th class="text-left pl-5">compiler</th>
            <!-- <th class="text-right">version</th> -->
            <!-- <th class="text-right">options</th> -->
          </tr>
          <tbody>
            <tr
              v-for="(i, idx) in filterBenches(test)"
              :key="idx"
              :class="
                'border-b-2 border-dotted py-1 ' +
                (idx % 2 == 0 ? 'bg-gray-200' : '')
              "
            >
              <td v-show="other" class="text-left">{{ i.lang }}</td>
              <td class="text-right">
                <a
                  :href="`https://github.com/hanabi1224/Another-Benchmarks-Game/blob/main/bench/algorithm/${test}/${i.code}`"
                  target="_blank"
                  class="underline text-blue-500"
                  >{{ i.code }}</a
                >
              </td>
              <td class="text-right">{{ i.input }}</td>
              <td class="text-right">{{ i.timeMS.toFixed(2) }}</td>
              <td class="text-right">
                {{ (i.memBytes / (1024 * 1024)).toFixed(2) }}MB
              </td>
              <td class="text-right">{{ i.cpuTimeMS.toFixed(2) }}</td>
              <td class="text-left pl-5" :title="getFullCompilerVersion(i)">
                {{ i.compiler }} {{ i.compilerVersion }}
              </td>
              <!-- <td class="text-right">{{ i.compilerVersion }}</td> -->
              <!-- <td class="text-right">{{ i.compilerVersionOption }}</td> -->
            </tr>
          </tbody>
        </table>
      </div>
    </div>
    <aside class="block w-1/6">
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
  lang?: LangBenchResults
  other?: LangBenchResults
  langs?: LangBenchResults[]

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
    const buildId = this.lang?.benchmarks[0].appveyorBuildId
    return `https://ci.appveyor.com/project/hanabi1224/another-benchmarks-game/builds/${buildId}`
  }

  get benchmarkDate() {
    const ts = this.lang?.benchmarks[0].testLog.finished as string
    return new Date(ts).toDateString()
  }

  getFullCompilerVersion(i: BenchResult) {
    return getFullCompilerVersion(i)
  }

  isLinkActive(lang: string, otherLang: string) {
    if (otherLang && this.other) {
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

  filterBenches(test: string) {
    let exp = _.chain(this.lang?.benchmarks).filter(
      (i) => i.test === test && i.os === this.osSelected // &&
      // i.compiler === this.compilerSelected &&
      // i.compilerVersion === this.compilerVersionSelected &&
      // (!this.compilerOptionSelected ||
      //   i.compilerOptions === this.compilerOptionSelected)
    )

    if (this.other?.benchmarks && this.other?.benchmarks?.length > 0) {
      exp = exp.unionWith(
        _.chain(this.other?.benchmarks)
          .filter((i) => i.test === test && i.os === this.osSelected)
          .value()
      )
    }

    // Sort
    exp = exp.orderBy(
      ['input', 'timeMS', 'os', 'lang', 'compiler', 'compilerVersion'],
      ['asc', 'asc', 'asc', 'asc', 'asc', 'asc']
    )

    return exp.value()
  }

  @Watch('compilerSelected')
  onPropertyChanged(value: string, oldValue: string) {
    if (value !== oldValue) {
      this.compilerVersionOptions = _.chain(this.lang?.benchmarks)
        .filter((i) => i.compiler === this.compilerSelected)
        .map((i) => i.compilerVersion)
        .uniq()
        .value()
      this.compilerVersionSelected = this.compilerVersionOptions[0]
    }
  }

  mounted() {
    // Update head
    const title = `${this.lang?.langDisplay} ${
      this.other ? 'VS ' + this.other?.langDisplay : ''
    } benchmarks game`

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
    this.lang = this.meta?.lang
    this.other = this.meta?.other
    this.langs = _.chain(this.meta?.all)
      // .filter((i) => i.lang != this.lang?.lang)
      .value()
    // this.langs.forEach((i) => {
    //   for (var j = 0; j < 10; j++) this.langs?.push(i)
    // })
    const lang = this.lang!

    this.langOptions = _.chain(this.meta?.all)
      .map((i) => i.lang)
      .uniq()
      .value()
    this.testOptions = _.chain(lang.benchmarks)
      .map((i) => i.test)
      .uniq()
      .value()

    this.osOptions = _.chain(lang.benchmarks)
      .map((i) => i.os)
      .uniq()
      .value()
    this.osSelected = this.osOptions[0]

    this.compilerOptions = _.chain(lang.benchmarks)
      .map((i) => i.compiler)
      .uniq()
      .value()
    this.compilerSelected = this.compilerOptions[0]
  }
}
</script>
