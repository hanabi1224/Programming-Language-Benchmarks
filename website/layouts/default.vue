<template>
  <div
    class="
      text-gray-700 text-base
      subpixel-antialiased
      text-lg
      tracking-wide
      .leading-relaxed
      py-5
      md:container
      md:mx-auto
      min-h-screen
      shadow-lg
      bg-gray-100
      p-5
    "
  >
    <div class="bg-pink-800 text-white p-5 mb-5 text-3xl rounded">
      <a href="/"
        ><small class="text-lg">Programming Language and compiler</small> <br />
        Benchmarks</a
      >
    </div>
    <Nuxt />
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'nuxt-property-decorator'
@Component({
  components: {},
})
export default class DefaultLayout extends Vue {}
</script>
