#!/bin/bash
#
# Copyright 2024 Figure AI, Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -eu

pushd "$(dirname $0)" >/dev/null
test_dir="$(pwd)"

# Possible build output paths.
BUILD_OUTPUT_PATHS=("../build" "..")
for build_output_path in "${BUILD_OUTPUT_PATHS[@]}"; do
  if [ -f "${build_output_path}/flatc" ]; then
    break
  fi
done

PYTHONPATH="${build_output_path}" python3 pybind_test.py
