<template>
  <div v-touch="toggleMenu" :class="['menu-btn', openStatus]">
    <div class="bar1"></div>
    <div class="bar2"></div>
    <div class="bar3"></div>
  </div>
</template>
<script lang="ts">
import { Component, Vue } from 'nuxt-property-decorator'
import Vue2TouchEvents from 'vue2-touch-events'

Vue.use(Vue2TouchEvents)

@Component({
  components: {},
})
export default class MenuButtonPage extends Vue {
  isOpen = false

  // mounted() {
  //   this.$el.addEventListener('click', this.toggleMenu)
  //   this.$el.addEventListener('touchstart', this.toggleMenu)
  // }

  toggleMenu(e: Event) {
    e.preventDefault()
    this.$emit('toggle')
    this.isOpen = !this.isOpen
  }

  get openStatus() {
    return this.isOpen ? 'open' : ''
  }
}
</script>
<style lang="scss" scoped>
@media screen and (max-width: 768px) {
  .menu-btn {
    display: inline-block;
    background-color: transparent;
    cursor: pointer;
    position: fixed;
    top: 40px;
    right: 40px;
    a {
      display: inline-block;
    }
    .bar1,
    .bar2,
    .bar3 {
      width: 35px;
      height: 5px;
      background-color: #ddd;
      margin: 6px 0;
      transition: 0.4s;
    }
    &.open {
      .bar1 {
        -webkit-transform: rotate(-45deg) translate(-9px, 6px);
        transform: rotate(-45deg) translate(-9px, 6px);
      }
      .bar2 {
        opacity: 0;
      }
      .bar3 {
        -webkit-transform: rotate(45deg) translate(-8px, -8px);
        transform: rotate(45deg) translate(-8px, -8px);
      }
    }
  }
}
@media screen and (min-width: 769px) {
  .menu-btn {
    display: none;
  }
}
</style>
