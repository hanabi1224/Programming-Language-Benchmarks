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
      <p>
        This is another implementation of
        <a
          href="https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html"
          target="_blank"
          >Benchmarks Game site</a
        >
      </p>
      <p>Details to be added</p>
    </div>
    <!-- <aside class="block w-1/6"></aside> -->
  </div>
</template>

<script lang="ts">
import Vue from 'vue'
import { Component } from 'vue-property-decorator'
import $ from 'jquery'
import _ from 'lodash'

@Component({
  components: {},
})
export default class IndexPage extends Vue {
  langs: LangBenchResults[] = []
  created() {
    this.langs = this.$route.meta
  }

  mounted() {
    // Update head
    const metaDesc = $('head meta[name="description"]')
    const metaContent = metaDesc.attr('content') as string
    const langsStr = _.chain(this.langs)
      .map((i) => i.langDisplay)
      .uniq()
      .value()
    metaDesc.attr('content', `${metaContent}, ${langsStr}`)
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
