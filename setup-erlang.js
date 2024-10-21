//
// Copyright (c) 2024, Byteplug LLC.
//
// This source file is part of a project made by the Erlangsters community and
// is released under the MIT license. Please refer to the LICENSE.md file that
// can be found at the root of the project repository.
//
// Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
//
const core = require('@actions/core');

async function run() {
  try {
    let erlangVersion = core.getInput('erlang-version');
    if (!erlangVersion) {
      erlangVersion = "25.3";
    }
    console.log(`Setting up Erlang/OTP version ${erlangVersion}`);
  } catch (error) {
    core.setFailed(error.message);
  }
}

run();
