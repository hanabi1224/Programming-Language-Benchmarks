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
          <a :href="`/${i.lang}`" :class="getLinkClass()">{{
            i.langDisplay
          }}</a>
        </li>
      </ul>
    </aside>
    <div class="block w-4/6">
      <div class="text-base italic leading-loose">
        <p class="text-xl not-italic">
          This is another implementation of
          <a
            class="underline text-blue-500"
            href="https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html"
            target="_blank"
            >Benchmarks Game site</a
          >.
        </p>
        <p class="pt-5">
          It currently uses CI to generate benchmark results to garantee all the
          numbers are generated from the same environment at nearly the same
          time. All benchmark tests are executed in a single CI job
        </p>
        <p class="pt-5">
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
    </div>
    <aside class="block w-1/6">
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

Component.registerHooks(['head'])

@Component({
  components: {},
})
export default class IndexPage extends Vue {
  langs: LangBenchResults[] = []
  problems: string[] = []
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
        'Another Benchmarks Game for computer languages | Which programming language is faster',
      meta: [
        {
          hid: 'description',
          name: 'description',
          content: `programming languages,benchmarks game,performance,${langsStrs.join(
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
}
</script>

<style>
/* Sample `apply` at-rules with Tailwind CSS
.container {
@apply min-h-screen flex justify-center items-center text-center mx-auto;
}
*/
</style>
