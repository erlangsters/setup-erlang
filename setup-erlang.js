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
