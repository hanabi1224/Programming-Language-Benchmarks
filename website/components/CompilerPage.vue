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
          <a :href="`/${i.lang}`" :class="getLinkClass()">{{
            i.langDisplay
          }}</a>
        </li>
      </ul>
    </aside>
    <article :class="contentClass">
    <h1 class="text-3xl">
        Compiler/Runtime Version List
    </h1>
      <div class="px-5 max-w-prose">
        <div class="italic text-base leading-8">
          <p class="pt-5">
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
          <p class="pt-5">
            <a
              class="underline bold text-blue-500"
              href="https://github.com/hanabi1224/Programming-Language-Benchmarks"
              target="_blank"
              >CONTRIBUTIONS</a
            >
            are WELCOME!
          </p>
        </div>
      </div>
      <div class="mt-5 text-xs">
        <p class="italic">
          * Star (*) symbol indicates latest unstable/nightly compiler
        </p>
      </div>
      <table class="table-auto w-full text-xs leading-loose">
        <tr class="border-b-2 border-dotted py-1">
          <th :class="['text-left', 'pl-4']">lang</th>
          <th class="text-left pl-5">
            <span class="md-show">compiler</span>
            <span class="md-hide">compiler/runtime</span>
          </th>
          <th class="text-left pl-5">version</th>
        </tr>
        <tbody class="font-light">
          <tr
            v-for="(i, idx) in uniqueCompilerBenchmarks"
            :key="idx"
            :class="
              'border-b-2 border-dotted py-1 ' +
              (idx % 2 == 0 ? 'bg-gray-200' : '')
            "
          >
            <td>{{ i.lang }}</td>
            <td>{{ i.compiler }}{{ i.unstable ? "*" : "" }}</td>
            <td>{{ i.compilerVersion }}</td>
          </tr>
        </tbody>
      </table>
    </article>
    <aside :class="sideBarClass">
      <div>
        <h2 class="text-xl">Problems</h2>
        <ul class="text-base">
          <li
            v-for="(i, idx) in problems"
            :key="idx"
            class="text-light-onSurfacePrimary"
          >
            <a
              :href="`/problem/${i}`"
              class="p-1 pl-3 flex rounded transition-colors duration-300 ease-linear justify-between underline text-blue-500 hover:text-green-400"
            >
              {{ i }}</a
            >
          </li>
        </ul>
      </div>
    </aside>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'nuxt-property-decorator'
import _ from 'lodash'
import MenuButton from './../components/MenuButton.vue'
import { mergeLangBenchResults } from '~/contentUtils'

function requireAll(requireContext: any) {
  const r = requireContext.keys().map(requireContext)
  return mergeLangBenchResults(r)
}
const langs = requireAll((require as any).context('../content', true, /.json$/))

Component.registerHooks(['head'])

@Component({
  components: {
    MenuButton,
  },
})
export default class CompilerPage extends Vue {
  isMenuOn = false
  langs: LangBenchResults[] = langs
  problems: string[] = []
  uniqueCompilerBenchmarks: BenchResult[] = []

  toggleMenu() {
    this.isMenuOn = !this.isMenuOn
  }

  get sideBarClass() {
    return this.isMenuOn ? 'block w-1/6 half-width' : 'block w-1/6 md-hide'
  }

  get contentClass() {
    return this.isMenuOn
      ? 'block w-4/6 md-hide'
      : 'block w-4/6 full-width mx-auto'
  }

  created() {
    this.problems = _.chain(this.langs)
      .map((i) => i.benchmarks)
      .flatten()
      .map((i) => i.test)
      .uniq()
      .sort()
      .value()
    this.uniqueCompilerBenchmarks = _.chain(this.langs)
      .map((i) => i.benchmarks)
      .flatten()
      .uniqWith((i, j) => i.compiler === j.compiler && i.compilerVersion === j.compilerVersion)
      .orderBy(['lang', 'compiler', 'compilerVersion'])
      .value()
  }

  head() {
    const langsStrs = _.chain(this.langs)
      .map((i) => i.langDisplay)
      .uniq()
      .value()
    return {
      title:
        'Benchmarks for programming languages and compilers, Which programming language or compiler is faster',
      meta: [
        {
          hid: 'description',
          name: 'description',
          content: `programming languages,benchmark,benchmarks,performance,${langsStrs.join(
            ','
          )},${this.problems.join(',')}`,
        },
      ],
    }
  }

  getLinkClass() {
    const baseClass =
      'p-1 pl-3 flex rounded transition-colors duration-300 ease-linear justify-between'
    // const activeClass = ' text-gray-700 cursor-default'
    const inactiveClass = ' underline text-blue-500 hover:text-green-400'
    return baseClass + inactiveClass
  }

  get buildLogUrl() {
    // const buildId = this.activeBenchmarks[0].appveyorBuildId
    // return `https://ci.appveyor.com/project/hanabi1224/another-benchmarks-game/builds/${buildId}`
    const runId = this.langs[0].benchmarks[0].githubRunId
    return `https://github.com/hanabi1224/Programming-Language-Benchmarks/actions/runs/${runId}`
  }

  get benchmarkDate() {
    const ts = this.langs[0].benchmarks[0].testLog.finished as string
    return new Date(ts).toDateString()
  }
}
</script>
