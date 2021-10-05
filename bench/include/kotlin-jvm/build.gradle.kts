import com.github.benmanes.gradle.versions.updates.DependencyUpdatesTask
import org.jetbrains.kotlin.cli.jvm.K2JVMCompiler
import org.jetbrains.kotlin.config.KotlinCompilerVersion

plugins {
    val kotlinVersion = "1.5.31"
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
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.5.2")
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.3.0")
    val ktor_version = "1.6.4"
    implementation("io.ktor:ktor-server-core:$ktor_version")
    implementation("io.ktor:ktor-server-netty:$ktor_version")
    // implementation("io.ktor:ktor-client-apache:$ktor_version")
    implementation("io.ktor:ktor-client-cio:$ktor_version")
    // implementation("io.ktor:ktor-client-java:$ktor_version")
    // implementation("io.ktor:ktor-client-jetty:$ktor_version")
    // implementation("io.ktor:ktor-client-okhttp:$ktor_version")
}

tasks.register("version") {
    doLast {
        // https://github.com/JetBrains/kotlin/blob/master/compiler/cli/src/org/jetbrains/kotlin/cli/common/CLITool.kt#L178
        val jreVersion = System.getProperty("java.runtime.version")
        println("${K2JVMCompiler().executableScriptFileName()} ${KotlinCompilerVersion.VERSION} (JRE $jreVersion)")
    }
}

tasks.register("du") {
    dependsOn("dependencyUpdates")
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
