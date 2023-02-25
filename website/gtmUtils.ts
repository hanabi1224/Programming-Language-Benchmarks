import { MetaInfo } from 'vue-meta'

export function importGoogleTagIfNeeded(metaInfo: MetaInfo) {
  const tag = googleTagId()
  if (tag) {
    metaInfo.script = [
      {
        src: `//www.googletagmanager.com/gtag/js?id=${tag}`,
        async: true,
      },
      {
        innerHTML: `
                window.dataLayer = window.dataLayer || [];
                function gtag() {
                    dataLayer.push(arguments);
                }
                gtag("js", new Date());
                gtag("config", "${tag}");
                `,
      },
    ]
    metaInfo.__dangerouslyDisableSanitizers = ['script']
  }
}

function googleTagId() {
  return process.env.GOOGLE_TAG_ID
}
