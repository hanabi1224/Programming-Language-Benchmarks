import { NuxtConfig } from '@nuxt/types';
import { $content } from '@nuxt/content';
import { getLangBenchResults } from './contentUtils';
import _ from 'lodash';

const config: NuxtConfig = {
  // Target: https://go.nuxtjs.dev/config-target
  target: 'static',

  loading: {
    color: 'cyan',
  },

  // Global page headers: https://go.nuxtjs.dev/config-head
  head: {
    title: 'Another Benchmarks Game for computer languages',
    meta: [
      { charset: 'utf-8' },
      { name: 'viewport', content: 'width=device-width, initial-scale=1' },
      { hid: 'description', name: 'description', content: 'benchmarks game for computer languages' },
    ],
    link: [{ rel: 'icon', type: 'image/x-icon', href: '/favicon.ico' }],
  },

  // Global CSS: https://go.nuxtjs.dev/config-css
  css: [],

  // Plugins to run before rendering page: https://go.nuxtjs.dev/config-plugins
  plugins: [],

  // Auto import components: https://go.nuxtjs.dev/config-components
  components: true,

  // Modules for dev and build (recommended): https://go.nuxtjs.dev/config-modules
  buildModules: [
    // https://go.nuxtjs.dev/typescript
    '@nuxt/typescript-build',
    // https://go.nuxtjs.dev/stylelint
    '@nuxtjs/stylelint-module',
    // https://go.nuxtjs.dev/tailwindcss
    '@nuxtjs/tailwindcss',
  ],

  // Modules: https://go.nuxtjs.dev/config-modules
  modules: [
    // https://go.nuxtjs.dev/axios
    '@nuxtjs/axios',
    // https://go.nuxtjs.dev/pwa
    '@nuxtjs/pwa',
    // https://go.nuxtjs.dev/content
    '@nuxt/content',
  ],

  // Axios module configuration: https://go.nuxtjs.dev/config-axios
  axios: {},

  // PWA module configuration: https://go.nuxtjs.dev/pwa
  pwa: {
    manifest: {
      lang: 'en',
    },
  },

  // Content module configuration: https://go.nuxtjs.dev/config-content
  content: {},

  // Build Configuration: https://go.nuxtjs.dev/config-build
  build: {},
  tailwindcss: {
    plugins: [
      require('@tailwindcss/forms'),
    ],
  },
  router: {
    async extendRoutes(routes, resolve) {
      const langBenchResults = await getLangBenchResults($content);
      routes.forEach(r => {
        r.meta = langBenchResults
      });

      langBenchResults.forEach(l => {
        routes.push({
          name: l.lang,
          path: `/${l.lang}`,
          component: resolve(__dirname, 'components/LangMetaPage.vue'),
          meta: {
            lang: l,
            all: langBenchResults,
          } as LangPageMeta,
        });

        langBenchResults.forEach(l2 => {
          if (l.lang != l2.lang) {
            routes.push({
              name: `${l.lang}-versus-${l2.lang}`,
              path: `/${l.lang}-vs-${l2.lang}`,
              component: resolve(__dirname, 'components/LangMetaPage.vue'),
              meta: {
                lang: l,
                other: l2,
                all: langBenchResults,
              } as LangPageMeta,
            });
          }
        });
      });

      const problems = _.chain(langBenchResults)
        .flatMap(i => i.benchmarks)
        .map(i => i.test)
        .uniq()
        .value()

      problems.forEach(p => {
        routes.push({
          name: `problem/${p}`,
          path: `/problem/${p}`,
          component: resolve(__dirname, 'components/LangMetaPage.vue'),
          meta: {
            problem: p,
            allProblems: problems,
            all: langBenchResults,
          } as LangPageMeta,
        });
      })
    },
  }
};

export default config;
