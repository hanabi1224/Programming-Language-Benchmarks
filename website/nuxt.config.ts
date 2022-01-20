import { NuxtConfig } from '@nuxt/types'
import { $content } from '@nuxt/content'
import _ from 'lodash'
import { getLangBenchResults } from './contentUtils'

const config: NuxtConfig = {
  // Target: https://go.nuxtjs.dev/config-target
  target: 'static',
  ssr: true,
  loading: {
    color: 'cyan',
  },
  cache: true,
  // Global page headers: https://go.nuxtjs.dev/config-head
  head: {
    title: 'Benchmarks for programming languages and compilers',
    meta: [
      { charset: 'utf-8' },
      { name: 'viewport', content: 'width=device-width, initial-scale=1' },
      {
        hid: 'description',
        name: 'description',
        content: 'benchmarks for programming languages and compilers',
      },
    ],
    link: [{ rel: 'icon', type: 'image/x-icon', href: '/favicon.ico' }],
  },

  // Global CSS: https://go.nuxtjs.dev/config-css
  css: ['@/assets/css/site.scss'],

  // Plugins to run before rendering page: https://go.nuxtjs.dev/config-plugins
  plugins: [],

  // Auto import components: https://go.nuxtjs.dev/config-components
  components: true,

  // Modules for dev and build (recommended): https://go.nuxtjs.dev/config-modules
  buildModules: [
    // https://go.nuxtjs.dev/typescript
    '@nuxt/typescript-build',
    // https://go.nuxtjs.dev/tailwindcss
    '@nuxtjs/tailwindcss',
  ],

  // Modules: https://go.nuxtjs.dev/config-modules
  modules: [
    // https://go.nuxtjs.dev/axios
    // '@nuxtjs/axios',
    // https://go.nuxtjs.dev/pwa
    // '@nuxtjs/pwa',
    // https://go.nuxtjs.dev/content
    '@nuxt/content',
  ],

  // Axios module configuration: https://go.nuxtjs.dev/config-axios
  axios: {},

  // PWA module configuration: https://go.nuxtjs.dev/pwa
  // pwa: {
  //   manifest: {
  //     lang: 'en',
  //   },
  // },

  // Content module configuration: https://go.nuxtjs.dev/config-content
  content: {},

  // Build Configuration: https://go.nuxtjs.dev/config-build
  build: {
    parallel: true,
    extractCSS: true,
    optimizeCSS: true,
  },
  generate: {
    crawler: false,
  },
  tailwindcss: {
    plugins: [require('@tailwindcss/forms')],
  },
  router: {
    async extendRoutes(routes, resolve) {
      const langBenchResults = await getLangBenchResults($content)

      langBenchResults.forEach((l) => {
        routes.push({
          name: l.lang,
          path: `/${l.lang}`,
          component: resolve(__dirname, 'components/LangMetaPage.vue'),
          meta: {
            lang: l.lang,
          } as LangPageMeta,
        })

        langBenchResults.forEach((l2) => {
          if (l.lang !== l2.lang) {
            routes.push({
              name: `${l.lang}-vs-${l2.lang}`,
              path: `/${l.lang}-vs-${l2.lang}`,
              component: resolve(__dirname, 'components/LangMetaPage.vue'),
              meta: {
                lang: l.lang,
                other: l2.lang,
              } as LangPageMeta,
            })
          }
        })
      })

      const problems = _.chain(langBenchResults)
        .flatMap((i) => i.benchmarks)
        .map((i) => i.test)
        .uniq()
        .value()

      problems.forEach((p) => {
        routes.push({
          name: `problem/${p}`,
          path: `/problem/${p}`,
          component: resolve(__dirname, 'components/LangMetaPage.vue'),
          meta: {
            problem: p,
          } as LangPageMeta,
        })
      })
    },
  },
}

if (process.env.SKIP_LINT) {
  // eslint-disable-next-line no-console
  console.log(`Skipping nuxt lint`)
} else {
  // https://go.nuxtjs.dev/stylelint
  config.buildModules?.push('@nuxtjs/stylelint-module')
}

// sitemap
// https://sitemap.nuxtjs.org/
if (process.env.APP_HOST_NAME) {
  // eslint-disable-next-line no-console
  console.log(`Turning on sitemap generation for ${process.env.APP_HOST_NAME}`)
  config.buildModules?.push('@nuxtjs/sitemap')
  config.sitemap = {
    hostname: process.env.APP_HOST_NAME,
    // defaults: {
    //   changefreq: 'weekly',
    //   priority: 1,
    //   lastmod: new Date(),
    // },
  }
}

// ga
// https://google-analytics.nuxtjs.org/
if (process.env.GOOGLE_ANALYTICS_ID) {
  // eslint-disable-next-line no-console
  console.log(`Turning on google analytics`)
  config.buildModules?.push('@nuxtjs/google-analytics')
  config.googleAnalytics = {
    id: process.env.GOOGLE_ANALYTICS_ID,
    checkDuplicatedScript: true,
  }
}

export default config
