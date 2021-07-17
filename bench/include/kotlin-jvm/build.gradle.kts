import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask

object Constants {
    const val kotlinVersion = "1.5.21"
}

plugins {
    val kotlinVersion = "1.5.21"
    kotlin("jvm").version(kotlinVersion)
    kotlin("plugin.serialization").version(kotlinVersion)
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
    val ktor_version = "1.6.1"
    implementation("io.ktor:ktor-server-core:$ktor_version")
    implementation("io.ktor:ktor-server-netty:$ktor_version")
    implementation("io.ktor:ktor-client-cio:$ktor_version")
    //    implementation("io.ktor:ktor-serialization:$ktor_version")
    //    implementation("ch.qos.logback:logback-classic:1.2.3")
}

tasks.register("kotlinVersion") {
    doLast {
        // $HOME/.konan/kotlin-native-prebuilt-linux-${Constants.kotlinVersion}/bin/kotlinc-native
        // -version
        println("Kotlinc ${Constants.kotlinVersion}")
    }
}

tasks.named<DependencyUpdatesTask>("dependencyUpdates") {
    rejectVersionIf { isNonStable(candidate.version) && !isNonStable(currentVersion) }
    // optional parameters
    checkForGradleUpdate = true
    outputFormatter = "json"
    outputDir = "build/dependencyUpdates"
    reportfileName = "report"
}

fun isNonStable(version: String): Boolean {
    val stableKeyword = listOf("RELEASE", "FINAL", "GA").any { version.toUpperCase().contains(it) }
    val regex = "^[0-9,.v-]+(-r)?$".toRegex()
    val isStable = stableKeyword || regex.matches(version)
    return isStable.not()
}
