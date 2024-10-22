//
// Copyright (c) 2024, Byteplug LLC.
//
// This source file is part of a project made by the Erlangsters community and
// is released under the MIT license. Please refer to the LICENSE.md file that
// can be found at the root of the project repository.
//
// Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
//
const os = require('os');
const path = require('path');
const fs = require('fs');
const { execSync, execFileSync } = require('child_process');
const core = require('@actions/core');
const tc = require('@actions/tool-cache');
const exec = require('@actions/exec');

// XXX: Perhaps allow customizing wher the pre-built Erlang binaries are
//      downloaded from.
// XXX: Detection of libc must be reworked to be more robust.

// By default, this is where we download the pre-built Erlang binaries.
const S3_STORAGE_URL = 'https://storage.byteplug.io';
const S3_BUCKET_NAME = 'erlangsters';

// The supported Erlang/OTP versions, in descending order (important!).
const ERLANG_VERSIONS = [
  "27.1.2",
  "27.1.1",
  "27.1",
  "27.0.1",
  "27.0",
  "26.2.5.4",
  "26.2.5.3",
  "26.2.5.2",
  "26.2.5.1",
  "26.2.5",
  "26.2.4",
  "26.2.3",
  "26.2.2",
  "26.2.1",
  "26.2",
  "26.1.2",
  "26.1.1",
  "26.1",
  "26.0.2",
  "26.0.1",
  "26.0",
  "25.3.2.15",
  "25.3.2.14",
  "25.3.2.13",
  "25.3.2.12",
  "25.3.2.11",
  "25.3.2.10",
  "25.3.2.9",
  "25.3.2.8",
  "25.3.2.7",
  "25.3.2.6",
  "25.3.2.5",
  "25.3.2.4",
  "25.3.2.3",
  "25.3.2.2",
  "25.3.2.1",
  "25.3.2",
  "25.3.1",
  "25.3",
  "25.2.3",
  "25.2.2",
  "25.2.1",
  "25.2",
  "25.1.2.1",
  "25.1.2",
  "25.1.1",
  "25.1",
  "25.0.4",
  "25.0.3",
  "25.0.2",
  "25.0.1",
  "25.0"
];

const REBAR3_DOWNLOAD_URL = "https://s3.amazonaws.com/rebar3/rebar3";

// Default Erlang version is the latest stable version, used when the Erlang
// version is not specified.
function defaultErlangVersion() {
  return ERLANG_VERSIONS[0];
}

// Normalize the Erlang version by finding the latest version that matches the
// given version prefix. For example, if the specified version is "25", it will
// return "25.3.2.15".
function normalizeVersion(version) {
  const versionPrefix = version.toString();
  const matchingVersions = ERLANG_VERSIONS.filter(v => v.startsWith(versionPrefix));
  if (matchingVersions.length === 0) {
    throw new Error(`No matching versions found for ${version}`);
  }
  return matchingVersions[0];
}

// Detect the runner's OS and architecture so we can download the correct
// pre-built Erlang binaries.
function getRunnerOS() {
  const platform = os.platform();
  if (platform === 'linux') {
    return 'linux';
  } else if (platform === 'win32') {
    return 'windows';
  } else if (platform === 'darwin') {
    return 'macos';
  } else {
    throw new Error(`Unsupported platform: ${platform}`);
  }
}
function getRunnerArchitecture() {
  const arch = os.arch();
  if (arch === 'x64') {
    return 'amd64';
  } else if (arch === 'arm64') {
    return 'arm64';
  } else {
    throw new Error(`Unsupported architecture: ${arch}`);
  }
}
function getRunnerLibC() {
  // Only relevant on Linux. Note that the implementation is rather fragile
  // (the 'ldd' command is not consistent across all Linux distributions).
  if (os.platform() !== 'linux') {
    return null;
  }

  try {
    const lddOutput = execSync('ldd --version || true').toString();
    if (lddOutput.includes('glibc') || lddOutput.includes('GLIBC')) {
      return 'glibc';
    } else {
      return 'musl';
    }
  } catch (error) {
    throw new Error('Failed to determine the standard C library (glibc or musl)');
  }
}
function detectPlatform() {
  const platform = {
    os: getRunnerOS(),
    arch: getRunnerArchitecture(),
    libc: getRunnerLibC()
  };
  return platform;
}

// Compute the platform name, which is used in the tarball name of the
// pre-built binaries.
function computePlatformName(platform) {
  let osName = platform.os;
  if (platform.os === 'linux' && platform.libc === 'musl') {
    osName = 'linux-musl';
  }
  const platformName = `${osName}-${platform.arch}`;
  return platformName;
}

// The name of the tarball (which contains the pre-built binaries) follows a
// specific format: erlang-<version>-<os>-<arch>.tar.gz
function computeTarballName(version, platform) {
  const platformName = computePlatformName(platform);
  const tarballName = `erlang-${version}-build-${platformName}.tar.gz`;
  return tarballName;
}

// The tarballs folder is where the pre-built binaries are stored in the S3
// bucket (for a given Erlang version).
function computeTarballsFolder(version) {
  const tarballFolder = `${S3_STORAGE_URL}/${S3_BUCKET_NAME}/erlang/${version}`;
  return tarballFolder;
}

async function run() {
  try {
    // Read the Erlang/OTP version from the input. Use the latest version if
    // not specified.
    let erlangVersion = core.getInput('erlang-version');
    if (!erlangVersion) {
      console.log('No Erlang/OTP version specified, using the latest version.');
      erlangVersion = defaultErlangVersion();
    }
    else {
      console.log(`Erlang/OTP version ${erlangVersion} is requested.`);
      erlangVersion = normalizeVersion(erlangVersion);
      console.log(`Selected Erlang/OTP version is ${erlangVersion}.`);
    }

    console.log(`Setting up Erlang/OTP version ${erlangVersion}.`);

    // Detect the platform where the action is running (so we understand what
    // pre-built binaries to install).
    const platform = detectPlatform();
    console.log(`Detected platform is ${JSON.stringify(platform)}.`);

    // Based on the platform, compute the location of the tarball to download
    // from the S3 bucket.
    const platformName = computePlatformName(platform);

    const tarballName = computeTarballName(erlangVersion, platform);
    const tarballsFolder = computeTarballsFolder(erlangVersion, platform);
    const tarballLocation = `${tarballsFolder}/${tarballName}`;

    // We download (if not already cached) the pre-built binaries and extract
    // into the installation directory.
    let toolPath = tc.find('erlang', erlangVersion, platformName);
    if (!toolPath) {
      const tarballDownloadPath = await tc.downloadTool(tarballLocation);
      console.log(`Downloaded Erlang to ${tarballDownloadPath}.`);

      const tempExtractedPath = await tc.extractTar(tarballDownloadPath);
      console.log(`Extracted Erlang to ${tempExtractedPath}.`);

      // Cache the final installation directory
      toolPath = await tc.cacheDir(tempExtractedPath, 'erlang', erlangVersion, platform.arch);
      console.log(`Cached Erlang to ${toolPath}`);

    } else {
      console.log(`Erlang found in cache at ${toolPath}`);
    }

    // Now we need to run the 'Install' script (so it generates the bin
    // directory with the Erlang/OTP executables). See Erlang OTP documentation
    // for more details.
    const erlangInstallDir = path.join(toolPath, `otp_build_${erlangVersion}`);
    const installScript = path.join(erlangInstallDir, 'Install');

    if (platform.os !== 'windows') {
      const args = ['-minimal', erlangInstallDir];
      await exec.exec(installScript, args, {cwd: erlangInstallDir});
    }
    else {
      // On Windows, there's no -minimal flag and it will prompt the user. This
      // is why we simulate pressing the "n" and Enter keys.
      const args = [erlangInstallDir];
      const options = {input: 'n\n', cwd: erlangInstallDir};
      await execFileSync(installScript, args, options);
    }

    // Add the Erlang installation path to the PATH environment variable.
    core.addPath(`${erlangInstallDir}/bin`);

    // Indicate the Erlang/OTP version that has actually been installed.
    core.setOutput('erlang-version', erlangVersion);

    // Indicate the location of the Erlang/OTP installation.
    core.setOutput('erlang-location', erlangInstallDir);

    // Install rebar3 script if requested.
    const installRebar3 = core.getBooleanInput('install-rebar3', {required: false});
    if (installRebar3 === true) {
      console.log('Installing rebar3 script...');

      // Download the rebar3 script (temporary location).
      let tmpRebar3Script = await tc.downloadTool(REBAR3_DOWNLOAD_URL);

      // Create an installation directory for the rebar3 script(s).
      const rebar3InstallDir = path.join(toolPath, `rebar`);
      fs.mkdirSync(rebar3InstallDir);

      // Move the rebar3 script to the installation directory.
      const rebar3Script = path.join(rebar3InstallDir, 'rebar3');
      fs.copyFileSync(tmpRebar3Script, rebar3Script);
      fs.unlinkSync(tmpRebar3Script);

      // Make the rebar3 script executable.
      if (platform.os !== 'windows') {
        fs.chmodSync(rebar3Script, '755');
      }

      // On Windows, an additional rebar3.cmd script must be placed alongside.
      if (platform.os === 'windows') {
        const rebar3CmdScriptText = `
@echo off
setlocal
set rebarscript=%~f0
escript.exe "%rebarscript:.cmd=%" %*
        `;
        const rebar3CmdScript = path.join(rebar3InstallDir, 'rebar3.cmd');
        fs.writeFileSync(rebar3CmdScript, rebar3CmdScriptText.trim());
      }

      core.addPath(rebar3InstallDir);
      console.log('The rebar3 script is installed.');
    }
    else {
      console.log('Installation of rebar3 script not requested.');
    }
  } catch (error) {
    core.setFailed(error.message);
  }
}

run();
