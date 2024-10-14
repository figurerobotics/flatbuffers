/*
 * Copyright 2024 Figure AI, Inc. All rights reserved.
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

/*
TODO(michaelahn): Feature completion:
- Include docstrings in py::doc.
- Vector __contains__, more sequence methods.
- C++-specific flatbuffer features (native types).
*/

#include "idl_gen_pybind.h"

#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "flatbuffers/code_generators.h"
#include "flatbuffers/flatbuffers.h"
#include "flatbuffers/flatc.h"
#include "flatbuffers/idl.h"
#include "flatbuffers/util.h"
#include "idl_namer.h"

namespace flatbuffers {
namespace pybind {

namespace {

// TODO(michaelahn): Factor out keywords and share with cpp/python code
// generators. Taken from idl_gen_cpp.cpp.
const std::unordered_set<std::string> &CppKeywords() {
  const auto *const kKeywords = new std::unordered_set<std::string>{
    "alignas",
    "alignof",
    "and",
    "and_eq",
    "asm",
    "atomic_cancel",
    "atomic_commit",
    "atomic_noexcept",
    "auto",
    "bitand",
    "bitor",
    "bool",
    "break",
    "case",
    "catch",
    "char",
    "char16_t",
    "char32_t",
    "class",
    "compl",
    "concept",
    "const",
    "constexpr",
    "const_cast",
    "continue",
    "co_await",
    "co_return",
    "co_yield",
    "decltype",
    "default",
    "delete",
    "do",
    "double",
    "dynamic_cast",
    "else",
    "enum",
    "explicit",
    "export",
    "extern",
    "false",
    "float",
    "for",
    "friend",
    "goto",
    "if",
    "import",
    "inline",
    "int",
    "long",
    "module",
    "mutable",
    "namespace",
    "new",
    "noexcept",
    "not",
    "not_eq",
    "nullptr",
    "operator",
    "or",
    "or_eq",
    "private",
    "protected",
    "public",
    "register",
    "reinterpret_cast",
    "requires",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "static_assert",
    "static_cast",
    "struct",
    "switch",
    "synchronized",
    "template",
    "this",
    "thread_local",
    "throw",
    "true",
    "try",
    "typedef",
    "typeid",
    "typename",
    "union",
    "unsigned",
    "using",
    "virtual",
    "void",
    "volatile",
    "wchar_t",
    "while",
    "xor",
    "xor_eq",
  };
  return *kKeywords;
}

// Taken from idl_gen_python.cpp.
const std::unordered_set<std::string> &PythonKeywords() {
  const auto *const kKeywords = new std::unordered_set<std::string>{
    "False",   "None",     "True",     "and",    "as",   "assert", "break",
    "class",   "continue", "def",      "del",    "elif", "else",   "except",
    "finally", "for",      "from",     "global", "if",   "import", "in",
    "is",      "lambda",   "nonlocal", "not",    "or",   "pass",   "raise",
    "return",  "try",      "while",    "with",   "yield"
  };
  return *kKeywords;
}

// Extension of IDLOptions for pybind-generator.
struct IDLOptionsPybind : public IDLOptions {
  explicit IDLOptionsPybind(const IDLOptions &opts) : IDLOptions(opts) {}
};

struct OpaqueTypeInfo {
  std::set<std::string> declaration_code;
  std::set<std::string> definition_code;
};

Namer::Config MakeCppConfig(const IDLOptionsPybind &opts,
                            const std::string &path) {
  Namer::Config config{
    /*types=*/Case::kKeep,
    /*constants=*/Case::kScreamingSnake,
    /*methods=*/Case::kSnake,
    /*functions=*/Case::kSnake,
    /*fields=*/Case::kKeep,
    /*variable=*/Case::kSnake,
    /*variants=*/Case::kKeep,
    /*enum_variant_seperator=*/"::",
    /*escape_keywords=*/Namer::Config::Escape::BeforeConvertingCase,
    /*namespaces=*/Case::kKeep,
    /*namespace_seperator=*/"::",
    /*object_prefix=*/"",
    /*object_suffix=*/"T",
    /*keyword_prefix=*/"",
    /*keyword_suffix=*/"_",
    /*filenames=*/Case::kKeep,
    /*directories=*/Case::kKeep,
    /*output_path=*/"",
    /*filename_suffix=*/"",
    /*filename_extension=*/".cpp"
  };
  config = WithFlagOptions(config, opts, path);
  return config;
}

Namer::Config MakePythonConfig(const IDLOptionsPybind &opts,
                               const std::string &path) {
  Namer::Config config{
    /*types=*/Case::kKeep,
    /*constants=*/Case::kScreamingSnake,
    /*methods=*/Case::kSnake,
    /*functions=*/Case::kSnake,
    /*fields=*/Case::kKeep,
    /*variable=*/Case::kSnake,
    /*variants=*/Case::kKeep,
    /*enum_variant_seperator=*/".",
    /*escape_keywords=*/Namer::Config::Escape::BeforeConvertingCase,
    /*namespaces=*/Case::kKeep,
    /*namespace_seperator=*/".",
    /*object_prefix=*/"",
    /*object_suffix=*/"T",
    /*keyword_prefix=*/"",
    /*keyword_suffix=*/"_",
    /*filenames=*/Case::kKeep,
    /*directories=*/Case::kKeep,
    /*output_path=*/"",
    /*filename_suffix=*/"",
    /*filename_extension=*/".py"
  };
  config = WithFlagOptions(config, opts, path);
  return config;
}

const std::string &Indent(int level = 1) {
  static auto *kCache = new std::unordered_map<int, std::string>();
  auto it = kCache->find(level);
  if (it != kCache->end()) { return it->second; }
  return kCache->emplace(level, std::string(level * 2, ' ')).first->second;
}

std::string StrJoin(const std::vector<std::string> &items, const char *sep) {
  std::string result;
  if (items.empty()) return result;
  size_t size = 0;
  for (const auto &item : items) { size += item.size(); }
  size += strlen(sep) * (items.size() - 1);
  result.reserve(size);
  result += items[0];
  for (size_t i = 1; i < items.size(); ++i) {
    result += sep;
    result += items[i];
  }
  return result;
}

std::string PathToPyModule(std::string path) {
  std::replace(path.begin(), path.end(), '/', '.');
  return path;
}

}  // namespace

class PybindGenerator : public BaseGenerator {
 public:
  PybindGenerator(const Parser &parser, const std::string &path,
                  const std::string &file_name, const IDLOptionsPybind &opts)
      : BaseGenerator(parser, path, file_name, "" /* not used */,
                      "::" /* not used */, "cpp"),
        opts_(opts),
        cpp_namer_(
            MakeCppConfig(opts, path),
            std::set<std::string>(CppKeywords().begin(), CppKeywords().end())),
        // NOTE: FlagOptions will trample `filename_extension`, but this is
        // unused (we only emit C++ code).
        py_namer_(MakePythonConfig(opts, path),
                  std::set<std::string>(PythonKeywords().begin(),
                                        PythonKeywords().end())),
        // Copied from idl_gen_cpp.cpp.
        float_const_gen_("std::numeric_limits<double>::",
                         "std::numeric_limits<float>::", "quiet_NaN()",
                         "infinity()") {}

  bool generate() {
    code_.Clear();
    code_ += "// " + std::string(FlatBuffersGeneratedWarning()) + "\n\n";

    // Pybind includes.
    code_ += "#include <pybind11/pybind11.h>";
    code_ += "";

    // Standard library dependencies.
    code_ += "#include <optional>";
    code_ += "#include <variant>";
    code_ += "";

    GenerateCppIncludeDeps();

    code_ += "namespace py = pybind11;";
    code_ += "";

    auto opaque_types = GetOpaqueTypes();
    // Generate opaque type declarations.
    for (const auto &declaration : opaque_types.declaration_code) {
      code_ += declaration;
    }
    if (!opaque_types.declaration_code.empty()) { code_ += ""; }

    const std::string module_name = file_name_ + opts_.filename_suffix;
    code_ += "PYBIND11_MODULE(" + module_name + ", m) {";

    GeneratePythonImports();

    // Generate empty class binding variables for each struct/tables, since
    // their attribute bindings may reference each other.
    for (const auto &struct_def : parser_.structs_.vec) {
      if (!struct_def->generated) {
        GenerateStructOrTableDeclaration(*struct_def);
      }
    }
    code_ += "";

    // Generate enum bindings.
    for (const auto &enum_def : parser_.enums_.vec) {
      if (enum_def->generated) continue;
      GenerateEnum(*enum_def);
    }

    // Generate opaque type binding definitions.
    for (const auto &definition : opaque_types.definition_code) {
      code_ += Indent() + definition;
    }
    if (!opaque_types.definition_code.empty()) { code_ += ""; }

    // Generate struct bindings.
    for (const auto &struct_def : parser_.structs_.vec) {
      if (struct_def->fixed && !struct_def->generated) {
        GenerateStruct(*struct_def);
      }
    }
    // Generate table bindings.
    for (const auto &struct_def : parser_.structs_.vec) {
      if (!struct_def->fixed && !struct_def->generated) {
        GenerateTable(*struct_def);
        if (opts_.generate_object_based_api) {
          GenerateTableObjectApi(*struct_def);
        }
      }
    }

    code_ += "}";

    const std::string file_path = GeneratedFileName(path_, file_name_, opts_);
    const std::string final_code = code_.ToString();

    return SaveFile(file_path.c_str(), final_code, false);
  }

 private:
  void GenerateComment(const std::vector<std::string> &dc,
                       const char *prefix = "") {
    std::string text;
    ::flatbuffers::GenComment(dc, &text, nullptr, prefix);
    code_ += text + "\\";
  }

  std::vector<std::string> GetDependencyModuleNames() {
    // Get the list of includes, sorted alphabetically as there should be no
    // dependence on ordering.
    std::vector<IncludedFile> included_files(parser_.GetIncludedFiles());
    std::stable_sort(included_files.begin(), included_files.end());
    std::vector<std::string> module_names;
    module_names.reserve(included_files.size());

    for (const IncludedFile &included_file : included_files) {
      // Strip the .fbs extension, and optionally strip the path prefix if
      // specified.
      std::string name_without_ext = StripExtension(included_file.schema_name);
      module_names.push_back(opts_.keep_prefix ? name_without_ext
                                               : StripPath(name_without_ext));
    }
    return module_names;
  }

  void GenerateCppIncludeDeps() {
    IDLOptions cpp_opts = opts_;
    cpp_opts.filename_extension = "h";
    if (!opts_.pybind_include_filename_suffix.empty()) {
      cpp_opts.filename_suffix = opts_.pybind_include_filename_suffix;
    }
    const std::string file_path =
        GeneratedFileName(path_, file_name_, cpp_opts);
    code_ += "#include \"" + file_path + "\"";

    for (const std::string &cpp_include : opts_.cpp_includes) {
      code_ += "#include \"" + cpp_include + "\"";
    }

    if (opts_.include_dependence_headers) {
      auto dependency_names = GetDependencyModuleNames();
      for (const std::string &dependency_name : dependency_names) {
        code_ +=
            "#include \"" +
            GeneratedFileName(opts_.include_prefix, dependency_name, cpp_opts) +
            "\"";
      }
    }

    // Add flatbuffer pybind libraries.
    code_ += "#include \"flatbuffers/pybind/bind_array.h\"";
    code_ += "#include \"flatbuffers/pybind/casters.h\"";
    code_ += "#include \"flatbuffers/pybind/memory.h\"";

    code_ += "";
  }

  void GeneratePythonImports() {
    if (!opts_.include_dependence_headers) { return; }
    auto dependency_names = GetDependencyModuleNames();
    for (std::string dependency_name : dependency_names) {
      if (opts_.pybind_include_filename_suffix.empty()) {
        dependency_name += opts_.pybind_include_filename_suffix;
      } else {
        dependency_name += opts_.filename_suffix;
      }
      code_ += Indent() + "py::module::import(\"" +
               PathToPyModule(dependency_name) + "\");";
    }
    if (!dependency_names.empty()) { code_ += ""; }
  }

  OpaqueTypeInfo GetOpaqueTypes() {
    OpaqueTypeInfo info;
    for (const auto *struct_def : parser_.structs_.vec) {
      if (struct_def->generated) continue;
      for (const auto *field : GetFieldDefs(*struct_def)) {
        const auto &field_type = field->value.type;
        if (!IsArray(field_type) && !IsVector(field_type)) { continue; }
        const auto cpp_type = CppType(field_type);
        const auto element_type = field_type.VectorType();
        const auto pybind_name = PyBindingName(field_type);

        if (IsScalar(element_type.base_type) && !element_type.enum_def) {
          info.definition_code.insert(
              "::flatbuffers::pybind::BindArrayArithmetic<" + cpp_type +
              ">(m, \"" + pybind_name + "\");");
        } else if (!struct_def->fixed &&
                   (element_type.base_type == BASE_TYPE_STRUCT ||
                    element_type.base_type == BASE_TYPE_STRING)) {
          // Vectors of structs/strings in packed flatbuffer tables are not
          // writeable.
          info.definition_code.insert(
              "::flatbuffers::pybind::BindArrayReadonly<" + cpp_type +
              ">(m, \"" + pybind_name + "\");");
        } else {
          info.definition_code.insert(
              "::flatbuffers::pybind::BindArrayReadwrite<" + cpp_type +
              ">(m, \"" + pybind_name + "\");");
        }

        if (!struct_def->fixed && opts_.generate_object_based_api) {
          const auto obj_cpp_type = CppType(field_type, /*object_api=*/true);
          const auto obj_pybind_name =
              PyBindingName(field_type, /*object_api=*/true);

          info.declaration_code.insert("PYBIND11_MAKE_OPAQUE(" + obj_cpp_type +
                                       ");");
          if (IsScalar(element_type.base_type)) {
            info.definition_code.insert(
                "::flatbuffers::pybind::BindStdVectorArithmetic<" +
                obj_cpp_type + ">(m, \"" + obj_pybind_name + "\");");
          } else {
            info.definition_code.insert(
                "::flatbuffers::pybind::BindStdVector<" + obj_cpp_type +
                ">(m, \"" + obj_pybind_name + "\");");
          }
        }
      }
    }
    return info;
  }

  // Generates a pybind definition for the given enum.
  void GenerateEnum(const EnumDef &def) {
    GenerateComment(def.doc_comment);
    code_ += Indent() + "py::enum_<" + cpp_namer_.NamespacedType(def) +
             ">(m, \"" + py_namer_.Type(def) + "\")\\";

    const std::string line_indent = "\n" + Indent(3);
    for (const auto *ev : def.Vals()) {
      GenerateComment(ev->doc_comment, line_indent.c_str());
      code_ += line_indent + ".value(\"" + py_namer_.Variant(*ev) + "\", " +
               CppEnumValueName(def, *ev) + ")\\";
    }
    code_ += ";\n";  // Extra new-line.
  }

  // Sets formatting variables for the given struct.
  void SetCodeValuesStruct(const StructDef &def) {
    code_.SetValue("BIND_VAR", cpp_namer_.Variable(def));
    code_.SetValue("CPP_TYPE", cpp_namer_.NamespacedType(def));
    code_.SetValue("PY_TYPE", py_namer_.Type(def));
  }

  // Sets formatting variables for the given table.
  void SetCodeValuesTable(const StructDef &def) {
    code_.SetValue("BIND_VAR", cpp_namer_.Variable(def));
    code_.SetValue("CPP_TYPE", cpp_namer_.NamespacedType(def));
    code_.SetValue("PY_TYPE", py_namer_.Type(def));
  }

  // Sets formatting variables for the given table's object API.
  void SetCodeValuesTableObjectApi(const StructDef &def) {
    const std::string obj_type_name =
        CppObjectApiType(def, /*namespaced=*/false);
    code_.SetValue("BIND_VAR", cpp_namer_.Variable(obj_type_name));
    code_.SetValue("CPP_TYPE", CppObjectApiType(def));
    code_.SetValue("PY_TYPE", py_namer_.Type(obj_type_name));
  }

  // Generates an initially empty class binding for a struct/table.
  void GenerateStructOrTableDeclaration(const StructDef &def) {
    if (def.fixed) {
      SetCodeValuesStruct(def);
    } else {
      if (opts_.generate_object_based_api) {
        SetCodeValuesTableObjectApi(def);
        code_ += Indent() +
                 "py::class_<{{CPP_TYPE}}> {{BIND_VAR}}(m, \"{{PY_TYPE}}\");";
      }
      SetCodeValuesTable(def);
    }
    code_ +=
        Indent() + "py::class_<{{CPP_TYPE}}> {{BIND_VAR}}(m, \"{{PY_TYPE}}\");";
  }

  // Generates an internal property that marks that this is a pybind flatbuffer
  // class.
  // TODO(michael-ahn): This is better as a class property once supported by
  // pybind.
  void GenerateInternalPybindTypeMarker(int base_type) {
    code_ += Indent() +
             "{{BIND_VAR}}.def_property_readonly(\"_fbs_pybind_type\", "
             "[](py::handle) -> int { return " +
             NumToString(base_type) + "; });";
  }

  // Generates a pybind definition for a struct.
  void GenerateStruct(const StructDef &def) {
    SetCodeValuesStruct(def);
    GenerateComment(def.doc_comment);
    GenerateInternalPybindTypeMarker(BASE_TYPE_STRUCT);

    // Default constructor.
    code_ += Indent() + "{{BIND_VAR}}.def(py::init<>());";

    auto field_defs = GetFieldDefs(def);

    // Keyword args constructor.
    if (field_defs.size() > 0) {
      std::vector<std::string> arg_types, py_args;
      arg_types.reserve(field_defs.size());
      py_args.reserve(field_defs.size());
      for (const auto *field : field_defs) {
        arg_types.push_back(CppArgumentType(field->value.type));
        py_args.push_back("py::arg(\"" + field->name +
                          "\") = " + PyArgDefaultValue(*field));
      }
      code_ += Indent() + "{{BIND_VAR}}.def(";
      code_ += Indent(3) + "py::init<" + StrJoin(arg_types, ", ") + ">(),";
      code_ += Indent(3) + StrJoin(py_args, ", ") + ");";
    }

    // Generate accessor bindings.
    for (const auto *field : field_defs) {
      const auto &field_type = field->value.type;
      code_.SetValue("CPP_FIELD", cpp_namer_.Field(*field));
      code_.SetValue("PY_FIELD", py_namer_.Field(*field));

      if (IsStruct(field_type)) {
        code_ += Indent() + "{{BIND_VAR}}.def_property(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ += Indent(3) + "&{{CPP_TYPE}}::{{CPP_FIELD}},";
        code_ += Indent(3) + "[]({{CPP_TYPE}} &self, " +
                 CppArgumentType(field_type) + " value) {";
        code_ += Indent(4) + "self.mutable_{{CPP_FIELD}}() = value;";
        code_ += Indent(3) + "});";
        continue;
      }

      if (IsArray(field_type)) {
        code_ += Indent() +
                 "{{BIND_VAR}}.def_property_readonly(\"{{PY_FIELD}}\", "
                 "&{{CPP_TYPE}}::mutable_{{CPP_FIELD}});";
        continue;
      }

      // POD types.
      if (opts_.mutable_buffer) {
        code_ += Indent() +
                 "{{BIND_VAR}}.def_property(\"{{PY_FIELD}}\", "
                 "&{{CPP_TYPE}}::{{CPP_FIELD}}, "
                 "&{{CPP_TYPE}}::mutate_{{CPP_FIELD}});";
      } else {
        code_ += Indent() +
                 "{{BIND_VAR}}.def_property_readonly(\"{{PY_FIELD}}\", "
                 "&{{CPP_TYPE}}::{{CPP_FIELD}});";
      }
    }

    // operators.
    if (opts_.gen_compare) {
      code_ += Indent() + "{{BIND_VAR}}.def(py::self == py::self);";
      code_ += Indent() + "{{BIND_VAR}}.def(py::self != py::self);";
    }

    // __repr__
    GenerateReprBinding(field_defs);

    code_ += "";
  }

  void GenerateTable(const StructDef &def) {
    SetCodeValuesTable(def);
    GenerateInternalPybindTypeMarker(BASE_TYPE_STRUCT);

    auto field_defs = GetFieldDefs(def);

    // Class methods to interpret a buffer.
    code_ += Indent() +
             "{{BIND_VAR}}.def_static(\"get_root\", [](py::buffer buffer, bool "
             "verify_buffer) {";
    code_ += Indent(3) + "::flatbuffers::span<const uint8_t> buffer_span;";
    code_ += Indent(3) +
             "::flatbuffers::pybind::MakeSpanFromObject(buffer_span, buffer, "
             "/*throw_error=*/true);";
    code_ += Indent(3) + "if (verify_buffer) {";
    code_ += Indent(4) +
             "flatbuffers::Verifier verifier(buffer_span.data(), "
             "buffer_span.size());";
    code_ += Indent(4) + "if (!verifier.VerifyBuffer<{{CPP_TYPE}}>(nullptr)) {";
    code_ += Indent(5) +
             "throw std::runtime_error(\"Invalid buffer for {{PY_TYPE}}\");";
    code_ += Indent(4) + "}";
    code_ += Indent(3) + "}";
    code_ += Indent(3) +
             "return flatbuffers::GetRoot<{{CPP_TYPE}}>(buffer_span.data());";
    // Keep the buffer alive.
    code_ += Indent() +
             "}, py::arg(\"buffer\"), py::arg(\"verify_buffer\") = false, "
             "py::keep_alive<0, 1>(), py::return_value_policy::reference);";

    // Generate accessor bindings.
    for (const auto *field : field_defs) {
      const auto &field_type = field->value.type;
      code_.SetValue("CPP_FIELD", cpp_namer_.Field(*field));
      code_.SetValue("PY_FIELD", py_namer_.Field(*field));
      code_.SetValue("CPP_FIELD_TYPE",
                     CppType(field_type, /*object_api=*/true));

      if (IsUnion(field_type)) {
        const auto &enum_def = *field_type.enum_def;
        code_.SetValue("UNION_VARIANT_TYPE", CppUnionVariantType(enum_def));
        code_.SetValue("UNION_ENUM_TYPE", cpp_namer_.NamespacedType(enum_def));

        code_ += Indent() + "{{BIND_VAR}}.def_property_readonly(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ +=
            Indent(3) + "[]({{CPP_TYPE}} &self) -> {{UNION_VARIANT_TYPE}} {";
        code_ += Indent(4) + "switch (self.{{CPP_FIELD}}_type()) {";
        for (const auto *val : enum_def.Vals()) {
          code_ += Indent(5) + "case " + CppEnumValueName(enum_def, *val) + ":";
          if (val->union_type.base_type == BASE_TYPE_NONE) {
            code_ += Indent(6) + "return std::nullopt;";
          } else if (val->union_type.base_type == BASE_TYPE_STRUCT) {
            code_ += Indent(6) + "return static_cast<" +
                     CppType(val->union_type) +
                     "*>(self.mutable_{{CPP_FIELD}}());";
          }
        }
        code_ += Indent(5) + "default:";
        code_ +=
            Indent(6) + "throw std::runtime_error(\"Invalid variant type\");";
        code_ += Indent(4) + "}";
        code_ += Indent(3) + "}, py::return_value_policy::reference_internal);";
        continue;
      }

      if (IsStruct(field_type) || IsTable(field_type) || IsString(field_type) ||
          IsVector(field_type)) {
        code_.SetValue("MUTABLE_PREFIX",
                       opts_.mutable_buffer ? "mutable_" : "");
        code_ += Indent() + "{{BIND_VAR}}.def_property_readonly(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ +=
            Indent(3) +
            "[]({{CPP_TYPE}} &self) { return "
            "std::make_optional(self.{{MUTABLE_PREFIX}}{{CPP_FIELD}}()); });";
        continue;
      }

      // POD types.
      if (opts_.mutable_buffer) {
        code_ += Indent() + "{{BIND_VAR}}.def_property(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ += Indent(3) + "&{{CPP_TYPE}}::{{CPP_FIELD}},";
        code_ += Indent(3) + "[]({{CPP_TYPE}} &self, " +
                 CppArgumentType(field_type) + " value) {";
        code_ += Indent(4) + "if (!self.mutate_{{CPP_FIELD}}(value)) {";
        code_ += Indent(5) +
                 "throw py::buffer_error(\"{{PY_FIELD}} is not writeable\");";
        code_ += Indent(4) + "}";
        code_ += Indent(3) + "});";
      } else {
        code_ += Indent() +
                 "{{BIND_VAR}}.def_property_readonly(\"{{PY_FIELD}}\", "
                 "&{{CPP_TYPE}}::{{CPP_FIELD}});";
      }
    }

    // Unpacking.
    if (opts_.generate_object_based_api) {
      code_ += Indent() +
               "{{BIND_VAR}}.def(\"unpack_to\", [](const {{CPP_TYPE}} &self, " +
               CppObjectApiType(def) + " &o) {";
      code_ += Indent(3) + "self.UnPackTo(&o);";
      code_ += Indent() + "}, py::call_guard<py::gil_scoped_release>());";
    }

    // __repr__
    GenerateReprBinding(field_defs);

    code_ += "";
  }

  // Generates a pybind definition for a table's object API (i.e. T-type).
  void GenerateTableObjectApi(const StructDef &def) {
    SetCodeValuesTableObjectApi(def);
    GenerateComment(def.doc_comment);
    GenerateInternalPybindTypeMarker(BASE_TYPE_STRUCT);

    // Default constructor.
    code_ += Indent() + "{{BIND_VAR}}.def(py::init<>());";

    auto field_defs = GetFieldDefs(def);

    // Keyword args constructor.
    if (!field_defs.empty()) {
      std::vector<std::string> ctor_params, py_args;
      ctor_params.reserve(field_defs.size());
      py_args.reserve(field_defs.size());
      for (const auto *field : field_defs) {
        std::string argument_type = CppArgumentType(
            field->value.type, /*object_api=*/true, /*optional=*/true);
        ctor_params.push_back(argument_type + " " +
                              cpp_namer_.Field(field->name));
        py_args.push_back("py::arg(\"" + py_namer_.Field(field->name) +
                          "\") = " + PyArgDefaultValue(*field));
      }
      code_ += Indent() + "{{BIND_VAR}}.def(";
      code_ += Indent(3) + "py::init([](" + StrJoin(ctor_params, ", ") + ") {";
      code_ += Indent(4) + CppObjectApiType(def) + " self;";
      for (const auto *field : field_defs) {
        const auto &field_type = field->value.type;
        code_.SetValue("CPP_FIELD", cpp_namer_.Field(*field));
        code_.SetValue("CPP_FIELD_TYPE",
                       CppType(field_type, /*object_api=*/true));
        code_.SetValue("CPP_FIELD_VALUE", cpp_namer_.Field(field->name));

        if (IsString(field_type)) {
          code_ += Indent(4) +
                   "self.{{CPP_FIELD}} = std::move({{CPP_FIELD_VALUE}});";
          continue;
        }

        if (IsUnion(field_type)) {
          code_ += Indent(4) + "if ({{CPP_FIELD_VALUE}}.has_value()) {";
          code_ += Indent(5) +
                   "std::visit([&self](auto *value) { "
                   "self.{{CPP_FIELD}}.Set(*value); }, *{{CPP_FIELD_VALUE}});";
          code_ += Indent(4) + "}";
          continue;
        }

        if (IsStruct(field_type) || IsTable(field_type)) {
          code_ += Indent(4) + "if ({{CPP_FIELD_VALUE}}.has_value()) {";
          code_ +=
              Indent(5) +
              "self.{{CPP_FIELD}} = "
              "std::make_unique<{{CPP_FIELD_TYPE}}>(**{{CPP_FIELD_VALUE}});";
          code_ += Indent(4) + "}";
          continue;
        }

        if (IsVector(field_type)) {
          const auto element_type = field_type.VectorType();
          if (IsTable(element_type)) {
            code_ += Indent(4) +
                     "self.{{CPP_FIELD}}.reserve({{CPP_FIELD_VALUE}}.size());";
            code_ +=
                Indent(4) + "for (const auto *value : {{CPP_FIELD_VALUE}}) {";
            code_ += Indent(5) +
                     "self.{{CPP_FIELD}}.emplace_back(std::make_unique<" +
                     CppType(element_type, /*object_api=*/true) + ">(*value));";
            code_ += Indent(4) + "}";
          } else {
            code_ += Indent(4) +
                     "self.{{CPP_FIELD}}.assign({{CPP_FIELD_VALUE}}.begin(), "
                     "{{CPP_FIELD_VALUE}}.end());";
          }
          continue;
        }

        // POD types.
        code_ += Indent(4) + "self.{{CPP_FIELD}} = {{CPP_FIELD_VALUE}};";
      }
      code_ += Indent(4) + "return self;";
      if (!py_args.empty()) {
        code_ +=
            Indent(3) + "}), py::kw_only(), " + StrJoin(py_args, ", ") + ");";
      } else {
        code_ += Indent(3) + "}));";
      }
    }

    // Generate accessor bindings.
    for (const auto *field : field_defs) {
      const auto &field_type = field->value.type;
      code_.SetValue("CPP_FIELD", cpp_namer_.Field(*field));
      code_.SetValue("PY_FIELD", py_namer_.Field(*field));
      code_.SetValue("CPP_FIELD_TYPE",
                     CppType(field_type, /*object_api=*/true));

      if (IsUnion(field_type)) {
        const auto &enum_def = *field_type.enum_def;
        code_.SetValue("UNION_VARIANT_TYPE",
                       CppUnionVariantType(enum_def, /*object_api=*/true));
        code_.SetValue("UNION_ENUM_TYPE", cpp_namer_.NamespacedType(enum_def));
        // Unions should at least have a NONE item as its first entry.
        FLATBUFFERS_ASSERT(enum_def.Vals().size() > 0);
        code_.SetValue("UNION_ENUM_NONE_NAME",
                       CppEnumValueName(enum_def, *enum_def.Vals()[0]));

        code_ += Indent() + "{{BIND_VAR}}.def_property(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        // Getter.
        code_ +=
            Indent(3) + "[]({{CPP_TYPE}} &self) -> {{UNION_VARIANT_TYPE}} {";
        code_ += Indent(4) + "switch (self.{{CPP_FIELD}}.type) {";
        for (const auto *val : enum_def.Vals()) {
          code_ += Indent(5) + "case " + CppEnumValueName(enum_def, *val) + ":";
          if (val->union_type.base_type == BASE_TYPE_NONE) {
            code_ += Indent(6) + "return std::nullopt;";
          } else {
            FLATBUFFERS_ASSERT(val->union_type.base_type == BASE_TYPE_STRUCT);
            code_ += Indent(6) + "return self.{{CPP_FIELD}}.As" +
                     cpp_namer_.Type(*val->union_type.struct_def) + "();";
          }
        }
        code_ += Indent(5) + "default:";
        code_ +=
            Indent(6) + "throw std::runtime_error(\"Invalid variant type\");";
        code_ += Indent(4) + "}";
        // Setter.
        code_ += Indent(3) +
                 "}, []({{CPP_TYPE}} &self, {{UNION_VARIANT_TYPE}} value) {;";
        code_ += Indent(4) + "if (value.has_value()) {";
        code_ += Indent(5) +
                 "std::visit([&self](auto *v) { "
                 "self.{{CPP_FIELD}}.Set(*v); }, *value);";
        code_ += Indent(4) + "} else {";
        code_ += Indent(5) + "self.{{CPP_FIELD}}.Reset();";
        code_ += Indent(4) + "}";
        code_ += Indent(3) + "}, py::return_value_policy::reference_internal);";

        // `ensure_{name}` method, which instantiates the union with the given
        // type if not set already.
        code_ += Indent() + "{{BIND_VAR}}.def(";
        code_ += Indent(3) + "\"ensure_{{PY_FIELD}}\",";
        code_ += Indent(3) +
                 "[]({{CPP_TYPE}} &self, py::type union_type) "
                 "-> {{UNION_VARIANT_TYPE}} {";
        code_ += Indent(4) + "{{UNION_ENUM_TYPE}} union_enum;";
        bool has_first = false;
        for (const auto *val : enum_def.Vals()) {
          if (val->union_type.base_type != BASE_TYPE_STRUCT) continue;
          code_ += Indent(4) + (has_first ? "else " : "") +
                   "if (union_type.is(py::type::of<" +
                   CppType(val->union_type, /*object_api=*/true) + ">())) {";
          code_ += Indent(5) +
                   "union_enum = " + CppEnumValueName(enum_def, *val) + ";";
          code_ += Indent(4) + "}";
          has_first = true;
        }
        code_ += Indent(4) + "else {";
        code_ += Indent(5) +
                 "throw py::value_error(py::str(\"{} is not part of union " +
                 cpp_namer_.Type(enum_def) + "\").format(union_type));";
        code_ += Indent(4) + "}";
        code_ += Indent(4) +
                 "bool holds_none = self.{{CPP_FIELD}}.type == "
                 "{{UNION_ENUM_NONE_NAME}};";
        code_ += Indent(4) +
                 "if (!holds_none && self.{{CPP_FIELD}}.type != union_enum) {";
        code_ += Indent(5) +
                 "throw py::value_error(py::str(\"Union field is already set "
                 "to {}\").format(self.{{CPP_FIELD}}.type));";
        code_ += Indent(4) + "}";
        code_ += Indent(4) + "switch (union_enum) {";
        for (const auto *val : enum_def.Vals()) {
          code_ += Indent(5) + "case " + CppEnumValueName(enum_def, *val) + ":";
          if (val->union_type.base_type == BASE_TYPE_NONE) {
            code_ += Indent(6) + "return std::nullopt;";
          } else {
            FLATBUFFERS_ASSERT(val->union_type.base_type == BASE_TYPE_STRUCT);
            code_ += Indent(6) + "if (holds_none) self.{{CPP_FIELD}}.Set(" +
                     CppObjectApiType(*val->union_type.struct_def) + "());";
            code_ += Indent(6) + "return self.{{CPP_FIELD}}.As" +
                     cpp_namer_.Type(*val->union_type.struct_def) + "();";
          }
        }
        code_ += Indent(5) + "default:";
        code_ +=
            Indent(6) + "throw std::runtime_error(\"Invalid variant type\");";
        code_ += Indent(4) + "}";
        code_ += Indent(3) + "}, py::return_value_policy::reference_internal);";
        continue;
      }

      if (IsStruct(field_type) || IsTable(field_type)) {
        code_ += Indent() + "{{BIND_VAR}}.def_property(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ += Indent(3) +
                 "[](const {{CPP_TYPE}} &self) -> "
                 "std::optional<{{CPP_FIELD_TYPE}}*> {";
        code_ += Indent(4) +
                 "if (self.{{CPP_FIELD}} == nullptr) { return std::nullopt; }";
        code_ += Indent(4) + "return self.{{CPP_FIELD}}.get();";
        code_ += Indent(3) + "},";
        code_ += Indent(3) +
                 "[]({{CPP_TYPE}} &self, std::optional<const "
                 "{{CPP_FIELD_TYPE}}*> value) {";
        code_ += Indent(4) + "if (!value.has_value()) {";
        code_ += Indent(5) + "self.{{CPP_FIELD}} = nullptr;";
        code_ += Indent(4) + "} else if (self.{{CPP_FIELD}} == nullptr) {";
        code_ += Indent(5) +
                 "self.{{CPP_FIELD}} = "
                 "std::make_unique<{{CPP_FIELD_TYPE}}>(**value);";
        code_ += Indent(4) + "} else {";
        code_ += Indent(5) + "*self.{{CPP_FIELD}} = **value;";
        code_ += Indent(4) + "}";
        code_ += Indent(3) + "});";

        // `ensure_{name}` method, which instantiates the struct/table if not
        // set already.
        code_ += Indent() + "{{BIND_VAR}}.def(";
        code_ += Indent(3) + "\"ensure_{{PY_FIELD}}\",";
        code_ += Indent(3) + "[]({{CPP_TYPE}} &self) -> {{CPP_FIELD_TYPE}}* {";
        code_ += Indent(4) + "if (self.{{CPP_FIELD}} == nullptr) {";
        code_ += Indent(5) +
                 "self.{{CPP_FIELD}} = std::make_unique<{{CPP_FIELD_TYPE}}>();";
        code_ += Indent(4) + "}";
        code_ += Indent(4) + "return self.{{CPP_FIELD}}.get();";
        code_ += Indent(3) + "}, py::return_value_policy::reference_internal);";
        continue;
      }

      if (IsVector(field_type)) {
        code_ += Indent() +
                 "{{BIND_VAR}}.def_readonly(\"{{PY_FIELD}}\", "
                 "&{{CPP_TYPE}}::{{CPP_FIELD}});";
        continue;
      }

      // POD types.
      code_ += Indent() +
               "{{BIND_VAR}}.def_readwrite(\"{{PY_FIELD}}\", "
               "&{{CPP_TYPE}}::{{CPP_FIELD}});";
    }

    // Packing.
    code_ += Indent() +
             "{{BIND_VAR}}.def(\"pack\", [](const {{CPP_TYPE}} &self, "
             "py::bytearray buffer) {";
    code_ +=
        Indent(3) +
        "auto fbb = ::flatbuffers::pybind::CreateFlatBufferBuilder(buffer);";
    code_ += Indent(3) + "fbb.Finish(" + cpp_namer_.NamespacedType(def) +
             "::Pack(fbb, &self));";
    code_ += Indent(3) + "return ::flatbuffers::pybind::AsMemoryView(fbb);";
    // The memoryview keeps the bytearray buffer alive.
    code_ += Indent() + "}, py::keep_alive<0, 2>());";

    // operators.
    if (opts_.gen_compare) {
      code_ += Indent() + "{{BIND_VAR}}.def(py::self == py::self);";
      code_ += Indent() + "{{BIND_VAR}}.def(py::self != py::self);";
    }

    // __repr__
    GenerateReprBinding(field_defs);

    code_ += "";
  }

  // Generates "__repr__" for a struct/table with the given fields.
  // Expects that {{PY_TYPE}} is already set.
  void GenerateReprBinding(const std::vector<const FieldDef *> &field_defs) {
    if (field_defs.empty()) {
      code_ += Indent() +
               "{{BIND_VAR}}.def(\"__repr__\", [](py::handle) { return "
               "\"{{PY_TYPE}}()\"; });";
      return;
    }
    code_ += Indent() + "{{BIND_VAR}}.def(\"__repr__\", [](py::handle self) {";
    code_ += Indent(3) + "py::list items;";
    for (const auto *field : field_defs) {
      code_.SetValue("PY_FIELD", py_namer_.Field(*field));
      code_ += Indent(3) +
               "items.append(py::str(\"{{PY_FIELD}}=\") + "
               "py::repr(self.attr(\"{{PY_FIELD}}\")));";
    }
    code_ += Indent(3) +
             "return py::str(\"{{PY_TYPE}}({})\").format(py::str(\", "
             "\").attr(\"join\")(items));";
    code_ += Indent() + "});";
  }

  std::vector<const FieldDef *> GetFieldDefs(const StructDef &def) {
    std::vector<const FieldDef *> field_defs;
    field_defs.reserve(def.fields.vec.size());
    for (const auto *field : def.fields.vec) {
      if (field->deprecated) continue;
      if (field->value.type.base_type == BASE_TYPE_UTYPE) continue;
      // TODO(michael-ahn): Support vectors of unions.
      if (IsVector(field->value.type) &&
          (field->value.type.element == BASE_TYPE_UNION ||
           field->value.type.element == BASE_TYPE_UTYPE)) {
        continue;
      }
      field_defs.push_back(field);
    }
    return field_defs;
  }

  std::string CppType(const Type &type, bool object_api = false) const {
    if (IsScalar(type.base_type)) {
      if (type.enum_def) { return cpp_namer_.NamespacedType(*type.enum_def); }
      return StringOf(type.base_type);
    }
    switch (type.base_type) {
      case BASE_TYPE_STRING: {
        if (object_api) { return "std::string"; }
        return "::flatbuffers::String";
      }
      case BASE_TYPE_ARRAY: {
        return "::flatbuffers::Array<" +
               CppType(type.VectorType(), object_api) + ", " +
               NumToString(type.fixed_length) + ">";
      }
      case BASE_TYPE_VECTOR64:
      case BASE_TYPE_VECTOR: {
        const auto element_type = type.VectorType();
        if (object_api) {
          if (IsScalar(element_type.base_type) || IsStruct(element_type) ||
              IsString(element_type)) {
            return "std::vector<" + CppType(type.VectorType(), object_api) +
                   ">";
          }
          return "std::vector<" +
                 CppPointerType(type.VectorType(), object_api) + ">";
        }
        if (IsScalar(element_type.base_type)) {
          return "::flatbuffers::Vector<" + CppType(type.VectorType()) + ">";
        }
        if (IsStruct(element_type)) {
          return "::flatbuffers::Vector<const " + CppType(type.VectorType()) +
                 " *>";
        }
        return "::flatbuffers::Vector<" + CppPointerType(type.VectorType()) +
               ">";
      }
      case BASE_TYPE_STRUCT: {
        if (!type.struct_def->fixed && object_api) {
          return CppObjectApiType(*type.struct_def);
        }
        return cpp_namer_.NamespacedType(*type.struct_def);
      }
      case BASE_TYPE_UNION:
        // Fall-through.  // TODO(michael-ahn): Fixme.
      default: {
        return "void";
      }
    }
  }

  std::string CppArgumentType(const Type &type, bool object_api = false,
                              bool optional = false) const {
    if (IsArray(type)) {
      return "::flatbuffers::span<const " +
             CppType(type.VectorType(), object_api) + ", " +
             NumToString(type.fixed_length) + ">";
    }
    if (IsVector(type)) {
      const auto element_type = type.VectorType();
      std::string element_type_str;
      if (IsString(element_type)) {
        element_type_str = "std::string";
      } else {
        element_type_str = CppType(element_type, object_api);
      }
      if (IsTable(element_type)) {
        return "::flatbuffers::span<const " + element_type_str + "*>";
      }
      return "::flatbuffers::span<const " + element_type_str + ">";
    }
    if (IsString(type)) { return "std::string"; }
    if (IsUnion(type)) {
      return CppUnionVariantType(*type.enum_def, object_api);
    }
    // For boolean arguments, use "bool" instead of "uint8_t".
    if (IsBool(type.base_type)) { return "bool"; }
    std::string type_str = CppType(type, object_api);
    if (IsScalar(type.base_type)) { return type_str; }
    // NOTE: While a raw pointer argument type can take None (as nullptr),
    // std::optional will generate the correct signature (`T | None`).
    if (optional) { return "std::optional<const " + type_str + "*>"; }
    return "const " + type_str + " &";
  }

  std::string CppObjectApiType(const StructDef &def,
                               bool namespaced = true) const {
    FLATBUFFERS_ASSERT(!def.fixed);
    std::string type_name =
        opts_.object_prefix + cpp_namer_.Type(def.name) + opts_.object_suffix;
    if (namespaced) {
      return cpp_namer_.Namespace(*def.defined_namespace) + "::" + type_name;
    }
    return type_name;
  }

  std::string CppUnionVariantType(const EnumDef &def,
                                  bool object_api = false) const {
    std::vector<std::string> variant_types;
    variant_types.reserve(def.Vals().size());
    for (const auto *ev : def.Vals()) {
      if (ev->union_type.base_type == BASE_TYPE_NONE) { continue; }
      variant_types.push_back(CppType(ev->union_type, object_api) + "*");
    }
    return "std::optional<std::variant<" + StrJoin(variant_types, ", ") + ">>";
  }

  const std::string &CppPointerType(const FieldDef *field) const {
    auto attr = field ? field->attributes.Lookup("cpp_ptr_type") : nullptr;
    if (attr == nullptr || attr->constant == "default_ptr_type") {
      return opts_.cpp_object_api_pointer_type;
    }
    return attr->constant;
  }

  std::string CppPointerType(const Type &type, bool object_api = false,
                             const FieldDef *field = nullptr) const {
    const auto base_type = CppType(type, object_api);
    if (!object_api && (IsTable(type) || IsString(type))) {
      return "::flatbuffers::Offset<" + base_type + ">";
    }
    const auto &ptr_type = CppPointerType(field);
    if (ptr_type == "naked") { return base_type + " *"; }
    return ptr_type + "<" + base_type + ">";
  }

  std::string CppEnumValueName(const EnumDef &def, const EnumVal &val,
                               bool namespaced = true) const {
    std::string value_name;
    if (opts_.scoped_enums) {
      value_name = cpp_namer_.Type(def) + "::" + cpp_namer_.Variant(val);
    } else if (opts_.prefixed_enums) {
      value_name = cpp_namer_.Type(def) + "_" + cpp_namer_.Variant(val);
    } else {
      value_name = cpp_namer_.Variant(val);
    }
    if (namespaced) {
      return cpp_namer_.Namespace(*def.defined_namespace) + "::" + value_name;
    }
    return value_name;
  }

  std::string PyArgDefaultValue(const FieldDef &field) const {
    auto *native_default = field.attributes.Lookup("native_default");
    if (native_default != nullptr) { return native_default->constant; }

    const auto &field_type = field.value.type;
    if (IsScalar(field_type.base_type)) {
      if (field.IsScalarOptional()) { return "py::none()"; }
      if (IsEnum(field_type)) {
        auto *ev = field_type.enum_def->FindByValue(field.value.constant);
        if (ev != nullptr) {
          return CppEnumValueName(*field_type.enum_def, *ev);
        } else {
          return "static_cast<" + CppType(field_type) + ">(" +
                 field.value.constant + ")";
        }
      }
      if (IsFloat(field_type.base_type)) {
        return float_const_gen_.GenFloatConstant(field);
      }
      if (IsBool(field_type.base_type)) {
        return field.value.constant == "0" ? "false" : "true";
      }
      // TODO(michael-ahn): Consider reusing NumToStringCpp.
      return field.value.constant;
    }
    if (IsStruct(field_type)) { return CppType(field_type) + "()"; }
    if (IsString(field_type)) { return "\"\""; }
    return "py::none()";
  }

  std::string PyBindingName(const Type &type, bool object_api = false) const {
    if (IsScalar(type.base_type)) {
      if (type.enum_def) { return py_namer_.Type(*type.enum_def); }
      std::string scalar_type_str = StringOf(type.base_type);
      // Strip a "_t" suffix.
      if (scalar_type_str.rfind("_t") == scalar_type_str.size() - 2) {
        scalar_type_str = scalar_type_str.substr(0, scalar_type_str.size() - 2);
      }
      return ConvertCase(py_namer_.EscapeKeyword(scalar_type_str),
                         Case::kUpperCamel, Case::kSnake);
    }
    switch (type.base_type) {
      case BASE_TYPE_STRING: {
        return "Str";
      }
      case BASE_TYPE_ARRAY: {
        return PyBindingName(type.VectorType(), object_api) + "Array" +
               NumToString(type.fixed_length);
      }
      case BASE_TYPE_VECTOR64:
      case BASE_TYPE_VECTOR: {
        std::string vector_type = object_api ? "StdVector" : "FbsVector";
        return PyBindingName(type.VectorType(), object_api) +
               (object_api ? "StdVector" : "FbsVector");
      }
      case BASE_TYPE_STRUCT: {
        if (!type.struct_def->fixed && object_api) {
          return opts_.object_prefix + py_namer_.Type(*type.struct_def) +
                 opts_.object_suffix;
        }
        return py_namer_.Type(*type.struct_def);
      }
      case BASE_TYPE_UNION: {
        return py_namer_.Type(*type.enum_def);
      }
      default: {
        return "Unknown";
      }
    }
  }

  const IDLOptionsPybind opts_;
  const IdlNamer cpp_namer_;
  const IdlNamer py_namer_;
  const TypedFloatConstantGenerator float_const_gen_;
  CodeWriter code_;

  std::unordered_set<std::string> keywords_;
};

}  // namespace pybind

static bool GeneratePybind(const Parser &parser, const std::string &path,
                           const std::string &file_name) {
  pybind::IDLOptionsPybind opts(parser.opts);
  pybind::PybindGenerator generator(parser, path, file_name, opts);

  return generator.generate();
}

namespace {

class PybindCodeGenerator : public CodeGenerator {
 public:
  Status GenerateCode(const Parser &parser, const std::string &path,
                      const std::string &filename) override {
    if (!GeneratePybind(parser, path, filename)) { return Status::ERROR; }
    return Status::OK;
  }

  Status GenerateCode(const uint8_t *, int64_t,
                      const CodeGenOptions &) override {
    return Status::NOT_IMPLEMENTED;
  }

  Status GenerateMakeRule(const Parser &parser, const std::string &path,
                          const std::string &filename,
                          std::string &output) override {
    (void)parser;
    (void)path;
    (void)filename;
    (void)output;
    return Status::NOT_IMPLEMENTED;
  }

  Status GenerateGrpcCode(const Parser &parser, const std::string &path,
                          const std::string &filename) override {
    (void)parser;
    (void)path;
    (void)filename;
    return Status::NOT_IMPLEMENTED;
  }

  Status GenerateRootFile(const Parser &parser,
                          const std::string &path) override {
    (void)parser;
    (void)path;
    return Status::NOT_IMPLEMENTED;
  }

  bool IsSchemaOnly() const override { return true; }

  bool SupportsBfbsGeneration() const override { return false; }
  bool SupportsRootFileGeneration() const override { return false; }

  IDLOptions::Language Language() const override { return IDLOptions::kPybind; }

  std::string LanguageName() const override { return "Pybind"; }
};

}  // namespace

std::unique_ptr<CodeGenerator> NewPybindCodeGenerator() {
  return std::unique_ptr<PybindCodeGenerator>(new PybindCodeGenerator());
}

}  // namespace flatbuffers
