<template>
  <div class="flex flex-wrap relative">
    <menu-button @toggle="toggleMenu"></menu-button>
    <aside :class="sideBarClass">
      <h2 title="Benchmarks for other computer languages" class="text-xl">
        Benchmarks
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
      <div class="px-5 max-w-prose">
        <div class="text-lg tracking-wider leading-8">
          <p class="pt-5">
            This site provide side by side comparison of several programming
            languages and their different compilers or runtime
          </p>
          <p class="pt-5">
            It currently uses CI to generate benchmark results to garantee all
            the numbers are generated from the same environment at nearly the
            same time. All benchmark tests are executed in a single CI job
          </p>
        </div>
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
            It's inspired by
            <a
              class="underline text-blue-500"
              href="https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html"
              target="_blank"
              >Benchmarks Game</a
            >, some of the benchmark problems and implementation are borrowed
            from it.
          </p>
          <p class="pt-5">
            Your
            <a
              class="underline bold text-blue-500"
              href="https://github.com/hanabi1224/Programming-Language-Benchmarks"
              target="_blank"
              >CONTRIBUTION</a
            >
            is WELCOME!
          </p>
        </div>
      </div>
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

Component.registerHooks(['head'])

@Component({
  components: {
    MenuButton,
  },
})
export default class IndexPage extends Vue {
  isMenuOn = false
  langs: LangBenchResults[] = []
  problems: string[] = []

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
    this.langs = this.$route.meta
    this.problems = _.chain(this.langs)
      .map((i) => i.benchmarks)
      .flatten()
      .map((i) => i.test)
      .uniq()
      .sort()
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
<style lang="scss" scoped>
@media screen and (max-width: 768px) {
  .md-hide {
    display: none;
  }
  .half-width {
    width: 50%;
  }
  .full-width {
    width: 100%;
  }
}
</style>
