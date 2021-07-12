import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask

object Constants {
    const val kotlinVersion = "1.5.20"
}

plugins {
    kotlin("jvm").version("1.5.20")
    kotlin("plugin.serialization").version("1.5.20")
    id("com.github.johnrengelman.shadow").version("7.0.0")
    id("com.github.ben-manes.versions").version("0.39.0")

    java
    application
}

repositories {
    google()
    mavenCentral()
    gradlePluginPortal()
}

application {
    // Define the main class for the application.
    mainClassName = "MainKt"
}

dependencies {
    // implementation(kotlin("stdlib"))
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.5.1")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.2.2")
}

tasks.register("kotlinVersion") {
    doLast {
        // $HOME/.konan/kotlin-native-prebuilt-linux-${Constants.kotlinVersion}/bin/kotlinc-native
        // -version
        println("Kotlinc ${Constants.kotlinVersion}")
    }
}

tasks.named<DependencyUpdatesTask>("dependencyUpdates") {
    resolutionStrategy {
        componentSelection {
            all {
                val rejected =
                        listOf("alpha", "beta", "rc", "cr", "m", "preview", "b", "ea").any {
                                qualifier ->
                            candidate.version.matches(Regex("(?i).*[.-]$qualifier[.\\d-+]*"))
                        }
                if (rejected) {
                    reject("Release candidate")
                }
            }
        }
    }
    // optional parameters
    checkForGradleUpdate = true
    outputFormatter = "json"
    outputDir = "build/dependencyUpdates"
    reportfileName = "report"
}
