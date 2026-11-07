/*
 * Copyright 2023 Google Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef FLATBUFFERS_IDL_GEN_JSON_SCHEMA_H_
#define FLATBUFFERS_IDL_GEN_JSON_SCHEMA_H_

#include <memory>
#include <string>

#include "flatbuffers/code_generator.h"
#include "flatbuffers/idl.h"

namespace flatbuffers {

// Constructs a new JsonSchema Code generator.
std::unique_ptr<CodeGenerator> NewJsonSchemaCodeGenerator();

// Generates the JSON schema for the root type of an already parsed (or
// deserialized) schema and returns it as text instead of writing a file.
bool GenerateJsonSchemaString(const Parser &parser, std::string *out);

}  // namespace flatbuffers

#endif  // FLATBUFFERS_IDL_GEN_JSON_SCHEMA_H_
