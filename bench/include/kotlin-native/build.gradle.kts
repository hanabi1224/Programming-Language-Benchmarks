plugins {
    kotlin("multiplatform") version "1.4.21"
}

repositories {
    mavenCentral()
}

kotlin {
  linuxX64("native") {
    binaries {
      executable("app")
    }
  }
}

tasks.withType<Wrapper> {
  gradleVersion = "6.8"
  distributionType = Wrapper.DistributionType.BIN
}
