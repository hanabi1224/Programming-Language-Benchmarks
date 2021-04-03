<template>
  <div :class="'menu-btn' + openStatus" @click="toggle">
    <div class="bar1"></div>
    <div class="bar2"></div>
    <div class="bar3"></div>
  </div>
</template>
<script lang="ts">
import { Component, Vue } from 'nuxt-property-decorator'
@Component({
  components: {},
})
export default class MenuButtonPage extends Vue {
  isOpen = false
  toggle() {
    this.$emit('toggle')
    this.isOpen = !this.isOpen
  }

  get openStatus() {
    return this.isOpen ? ' open' : ''
  }
}
</script>
<style lang="scss" scoped>
@media screen and (max-width: 768px) {
  .menu-btn {
    display: inline-block;
    cursor: pointer;
    position: fixed;
    top: 40px;
    right: 40px;
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
@media screen and (min-width: 768px) {
  .menu-btn {
    display: none;
  }
}
</style>
