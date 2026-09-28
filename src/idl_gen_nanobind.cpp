/*
 * Copyright 2026 Figure AI, Inc. All rights reserved.
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
- Include docstrings in nb::doc.
- Unions with struct/string variants and aliased variants.
- C++-specific flatbuffer features (native types).
*/

#include "idl_gen_nanobind.h"

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
namespace nanobind {

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

// Extension of IDLOptions for nanobind-generator.
struct IDLOptionsNanobind : public IDLOptions {
  explicit IDLOptionsNanobind(const IDLOptions &opts) : IDLOptions(opts) {}
};

struct OpaqueTypeInfo {
  std::set<std::string> declaration_code;
  std::set<std::string> definition_code;
};

Namer::Config MakeCppConfig(const IDLOptionsNanobind &opts,
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

Namer::Config MakePythonConfig(const IDLOptionsNanobind &opts,
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

// Converts a namespaced C++ name to an identifier (e.g. "a::B" -> "a_B").
std::string CppIdentifier(std::string name) {
  std::string result;
  result.reserve(name.size());
  for (const char c : name) {
    if (c == ':') {
      if (result.empty() || result.back() != '_') { result += '_'; }
    } else {
      result += c;
    }
  }
  if (!result.empty() && result.front() == '_') { result.erase(0, 1); }
  return result;
}

bool IsBoolArrayOrVector(const Type &type) {
  return (IsArray(type) || IsVector(type)) &&
         type.VectorType().base_type == BASE_TYPE_BOOL;
}

bool IsUnionVector(const Type &type) {
  return IsVector(type) && type.element == BASE_TYPE_UNION;
}

}  // namespace

class NanobindGenerator : public BaseGenerator {
 public:
  NanobindGenerator(const Parser &parser, const std::string &path,
                    const std::string &file_name,
                    const IDLOptionsNanobind &opts)
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
    if (!CheckUnions()) { return false; }

    code_.Clear();
    code_ += "// " + std::string(FlatBuffersGeneratedWarning()) + "\n\n";

    // Nanobind includes.
    code_ += "#include <nanobind/nanobind.h>";
    code_ += "#include <nanobind/operators.h>";
    code_ += "#include <nanobind/typing.h>";
    code_ += "#include <nanobind/stl/optional.h>";
    code_ += "#include <nanobind/stl/string.h>";
    code_ += "#include <nanobind/stl/variant.h>";
    code_ += "";

    // Standard library dependencies.
    code_ += "#include <array>";
    code_ += "#include <optional>";
    code_ += "#include <string>";
    code_ += "#include <variant>";
    code_ += "#include <vector>";
    code_ += "";

    GenerateCppIncludeDeps();

    code_ += "namespace nb = nanobind;";
    code_ += "using namespace nb::literals;";
    code_ += "";

    auto opaque_types = GetOpaqueTypes();
    // Generate opaque type declarations.
    for (const auto &declaration : opaque_types.declaration_code) {
      code_ += declaration;
    }
    if (!opaque_types.declaration_code.empty()) { code_ += ""; }

    GenerateUnionTraits();

    const std::string module_name = file_name_ + opts_.filename_suffix;
    code_ += "NB_MODULE(" + module_name + ", m) {";

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

    // Generate type variables for unions.
    if (opts_.generate_object_based_api) {
      for (const auto &enum_def : parser_.enums_.vec) {
        if (enum_def->generated || !enum_def->is_union) continue;
        GenerateUnionTypeVar(*enum_def);
      }
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

  // Only unions of tables are supported; returns false (and logs an error)
  // otherwise.
  bool CheckUnions() const {
    for (const auto *enum_def : GetUsedUnions()) {
      std::set<const StructDef *> variants;
      for (const auto *val : enum_def->Vals()) {
        const auto &type = val->union_type;
        if (type.base_type == BASE_TYPE_NONE) { continue; }
        if (type.base_type != BASE_TYPE_STRUCT || type.struct_def->fixed) {
          LogCompilerError(
              "Nanobind generator only supports unions of tables: " +
              enum_def->name + "." + val->name);
          return false;
        }
        if (!variants.insert(type.struct_def).second) {
          LogCompilerError(
              "Nanobind generator does not support aliased union variants: " +
              enum_def->name + "." + val->name);
          return false;
        }
      }
    }
    return true;
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
    if (!opts_.nanobind_include_filename_suffix.empty()) {
      cpp_opts.filename_suffix = opts_.nanobind_include_filename_suffix;
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

    // Add flatbuffer nanobind libraries.
    code_ += "#include \"flatbuffers/nanobind/bind_array.h\"";
    code_ += "#include \"flatbuffers/nanobind/casters.h\"";
    code_ += "#include \"flatbuffers/nanobind/memory.h\"";

    code_ += "";
  }

  void GeneratePythonImports() {
    if (!opts_.include_dependence_headers) { return; }
    auto dependency_names = GetDependencyModuleNames();
    for (const std::string &dependency_name : dependency_names) {
      code_ += Indent() + "nb::module_::import_(\"" +
               PathToPyModule(dependency_name + opts_.filename_suffix) + "\");";
    }
    if (!dependency_names.empty()) { code_ += ""; }
  }

  // Returns the unions which are used by fields of structs/tables generated
  // for this file.
  std::vector<const EnumDef *> GetUsedUnions() const {
    std::vector<const EnumDef *> unions;
    std::set<const EnumDef *> seen;
    for (const auto *struct_def : parser_.structs_.vec) {
      if (struct_def->generated) continue;
      for (const auto *field : GetFieldDefs(*struct_def)) {
        const auto &type = field->value.type;
        if (!IsUnion(type) && !IsUnionVector(type)) { continue; }
        if (seen.insert(type.enum_def).second) {
          unions.push_back(type.enum_def);
        }
      }
    }
    return unions;
  }

  // Returns the name of the generated object API traits for a union.
  std::string UnionTraitsName(const EnumDef &def) const {
    return "UnionTraits_" + CppIdentifier(cpp_namer_.NamespacedType(def));
  }

  // Returns the name of the generated packed traits for a union.
  std::string FbsUnionTraitsName(const EnumDef &def) const {
    return "FbsUnionTraits_" + CppIdentifier(cpp_namer_.NamespacedType(def));
  }

  // Returns the name of the object API type variable for a union.
  std::string UnionTypeVarName(const EnumDef &def) const {
    return py_namer_.Type(def) + "VariantT";
  }

  // Returns a C++ expression for the name of a union's type variable, as
  // referenced from this module.
  std::string UnionTypeVarNameExpr(const EnumDef &def) const {
    return "::flatbuffers::nanobind::TypeVarName<" +
           cpp_namer_.NamespacedType(def) + ">(m, \"" + UnionTypeVarName(def) +
           "\")";
  }

  // Generates the helper traits which convert between unions and variants.
  void GenerateUnionTraits() {
    auto unions = GetUsedUnions();
    if (unions.empty()) { return; }

    code_ += "namespace {";
    code_ += "";
    for (const auto *enum_def : unions) {
      if (opts_.generate_object_based_api) {
        GenerateObjectApiUnionTraits(*enum_def);
      }
      GenerateFbsUnionTraits(*enum_def);
    }
    code_ += "}  // namespace";
    code_ += "";
  }

  void GenerateObjectApiUnionTraits(const EnumDef &def) {
    // Unions should at least have a NONE item as its first entry.
    FLATBUFFERS_ASSERT(def.Vals().size() > 0);
    code_.SetValue("TRAITS", UnionTraitsName(def));
    code_.SetValue("UNION_TYPE", CppObjectApiUnionType(def));
    code_.SetValue("UNION_ENUM_TYPE", cpp_namer_.NamespacedType(def));
    code_.SetValue("UNION_ENUM_NONE_NAME",
                   CppEnumValueName(def, *def.Vals()[0]));
    code_.SetValue("VARIANT_TYPE",
                   CppUnionVariantType(def, /*object_api=*/true));

    code_ += "// Converts between {{UNION_TYPE}} and Python objects.";
    code_ += "struct {{TRAITS}} {";
    code_ += Indent() + "using union_type = {{UNION_TYPE}};";
    code_ += Indent() + "using variant_type = {{VARIANT_TYPE}};";
    code_ += "";

    // Get.
    code_ += Indent() + "static variant_type Get(union_type &u) {";
    code_ += Indent(2) + "switch (u.type) {";
    for (const auto *val : def.Vals()) {
      code_ += Indent(3) + "case " + CppEnumValueName(def, *val) + ":";
      if (val->union_type.base_type == BASE_TYPE_NONE) {
        code_ += Indent(4) + "return std::nullopt;";
      } else {
        code_ += Indent(4) + "return u.As" + cpp_namer_.Variant(*val) + "();";
      }
    }
    code_ += Indent(3) + "default:";
    code_ += Indent(4) + "throw std::runtime_error(\"Invalid variant type\");";
    code_ += Indent(2) + "}";
    code_ += Indent() + "}";
    code_ += "";

    // Set. The value is copied first, as it may be owned by `u`.
    code_ += Indent() +
             "static void Set(union_type &u, const variant_type &value) {";
    code_ += Indent(2) + "if (!value.has_value()) {";
    code_ += Indent(3) + "u.Reset();";
    code_ += Indent(3) + "return;";
    code_ += Indent(2) + "}";
    code_ += Indent(2) + "std::visit([&u](auto *v) {";
    code_ += Indent(3) + "auto copy = *v;";
    code_ += Indent(3) + "u.Set(std::move(copy));";
    code_ += Indent(2) + "}, *value);";
    code_ += Indent() + "}";
    code_ += "";

    // Ensure, which instantiates the union with the given type if not set
    // already.
    code_ += Indent() +
             "static variant_type Ensure(union_type &u, nb::handle "
             "union_type_obj) {";
    code_ += Indent(2) + "{{UNION_ENUM_TYPE}} union_enum;";
    bool has_first = false;
    for (const auto *val : def.Vals()) {
      if (val->union_type.base_type != BASE_TYPE_STRUCT) continue;
      code_ += Indent(2) + (has_first ? "} else " : "") +
               "if (union_type_obj.is(nb::type<" +
               CppType(val->union_type, /*object_api=*/true) + ">())) {";
      code_ += Indent(3) + "union_enum = " + CppEnumValueName(def, *val) + ";";
      has_first = true;
    }
    code_ += Indent(2) + (has_first ? "} else {" : "{");
    code_ += Indent(3) +
             "throw nb::value_error(nb::str(\"{} is not part of union " +
             cpp_namer_.Type(def) + "\").format(union_type_obj).c_str());";
    code_ += Indent(2) + "}";
    code_ += Indent(2) + "if (u.type == {{UNION_ENUM_NONE_NAME}}) {";
    code_ += Indent(3) + "switch (union_enum) {";
    for (const auto *val : def.Vals()) {
      if (val->union_type.base_type != BASE_TYPE_STRUCT) continue;
      code_ += Indent(4) + "case " + CppEnumValueName(def, *val) + ":";
      code_ += Indent(5) + "u.Set(" +
               CppObjectApiType(*val->union_type.struct_def) + "());";
      code_ += Indent(5) + "break;";
    }
    code_ += Indent(4) + "default:";
    code_ += Indent(5) + "break;";
    code_ += Indent(3) + "}";
    code_ += Indent(2) + "} else if (u.type != union_enum) {";
    code_ += Indent(3) +
             "throw nb::value_error(nb::str(\"Union field is already set to "
             "{}\").format(nb::cast(u.type)).c_str());";
    code_ += Indent(2) + "}";
    code_ += Indent(2) + "return Get(u);";
    code_ += Indent() + "}";
    code_ += "};";
    code_ += "";
  }

  void GenerateFbsUnionTraits(const EnumDef &def) {
    code_.SetValue("TRAITS", FbsUnionTraitsName(def));
    code_.SetValue("UNION_ENUM_TYPE", cpp_namer_.NamespacedType(def));
    code_.SetValue("VARIANT_TYPE", CppUnionVariantType(def));

    code_ += "// Converts packed {{UNION_ENUM_TYPE}} values to Python objects.";
    code_ += "struct {{TRAITS}} {";
    code_ += Indent() + "using enum_type = {{UNION_ENUM_TYPE}};";
    code_ += Indent() + "using variant_type = {{VARIANT_TYPE}};";
    code_ += "";
    code_ += Indent() +
             "static variant_type Get(enum_type type, const void *value) {";
    code_ += Indent(2) + "if (value == nullptr) { return std::nullopt; }";
    code_ += Indent(2) + "switch (type) {";
    for (const auto *val : def.Vals()) {
      code_ += Indent(3) + "case " + CppEnumValueName(def, *val) + ":";
      if (val->union_type.base_type == BASE_TYPE_NONE) {
        code_ += Indent(4) + "return std::nullopt;";
      } else {
        code_ += Indent(4) + "return static_cast<" + CppType(val->union_type) +
                 " *>(const_cast<void *>(value));";
      }
    }
    code_ += Indent(3) + "default:";
    code_ += Indent(4) + "throw std::runtime_error(\"Invalid variant type\");";
    code_ += Indent(2) + "}";
    code_ += Indent() + "}";
    code_ += "};";
    code_ += "";
  }

  // Generates the type variable used to type `ensure_{field}` methods of a
  // union.
  void GenerateUnionTypeVar(const EnumDef &def) {
    std::vector<std::string> variant_types;
    for (const auto *val : def.Vals()) {
      if (val->union_type.base_type != BASE_TYPE_STRUCT ||
          val->union_type.struct_def->fixed) {
        continue;
      }
      variant_types.push_back(
          "nb::type<" + CppType(val->union_type, /*object_api=*/true) + ">()");
    }
    if (variant_types.empty()) { return; }
    const std::string name = UnionTypeVarName(def);
    code_ += Indent() + "m.attr(\"" + name + "\") = nb::type_var(";
    code_ += Indent(3) + "\"" + name + "\",";
    code_ += Indent(3) +
             "\"bound\"_a = nb::module_::import_(\"typing\").attr(\"Union\")["
             "nb::make_tuple(" +
             StrJoin(variant_types, ", ") + ")]);";
    code_ += "";
  }

  OpaqueTypeInfo GetOpaqueTypes() {
    OpaqueTypeInfo info;
    for (const auto *struct_def : parser_.structs_.vec) {
      if (struct_def->generated) continue;
      for (const auto *field : GetFieldDefs(*struct_def)) {
        const auto &field_type = field->value.type;
        if (!IsArray(field_type) && !IsVector(field_type)) { continue; }

        if (IsUnionVector(field_type)) {
          const auto &enum_def = *field_type.enum_def;
          const std::string union_name = py_namer_.Type(enum_def);
          // The vector of union types.
          Type types_type = field_type;
          types_type.element = BASE_TYPE_UTYPE;
          info.definition_code.insert(
              "::flatbuffers::nanobind::BindArrayReadonly<" +
              CppType(types_type) + ">(m, \"" + PyBindingName(types_type) +
              "\");");
          info.definition_code.insert(
              "::flatbuffers::nanobind::BindUnionFbsVector<" +
              FbsUnionTraitsName(enum_def) + ">(m, \"" + union_name +
              "UnionFbsVector\");");
          if (opts_.generate_object_based_api) {
            const auto obj_cpp_type = CppType(field_type, /*object_api=*/true);
            info.declaration_code.insert("NB_MAKE_OPAQUE(" + obj_cpp_type +
                                         ");");
            info.definition_code.insert(
                "::flatbuffers::nanobind::BindUnionStdVector<" + obj_cpp_type +
                ", " + UnionTraitsName(enum_def) + ">(m, \"" + union_name +
                "UnionStdVector\", " + UnionTypeVarNameExpr(enum_def) + ");");
          }
          continue;
        }

        const auto cpp_type = CppType(field_type);
        const auto element_type = field_type.VectorType();
        const auto nanobind_name = PyBindingName(field_type);

        if (IsBoolArrayOrVector(field_type)) {
          info.definition_code.insert(
              "::flatbuffers::nanobind::BindArrayArithmetic<" +
              CppBoolViewType(field_type) + ">(m, \"" + nanobind_name + "\");");
        } else if (IsScalar(element_type.base_type)) {
          info.definition_code.insert(
              "::flatbuffers::nanobind::BindArrayArithmetic<" + cpp_type +
              ">(m, \"" + nanobind_name + "\");");
        } else if (!struct_def->fixed &&
                   (element_type.base_type == BASE_TYPE_STRUCT ||
                    element_type.base_type == BASE_TYPE_STRING)) {
          // Vectors of structs/strings in packed flatbuffer tables are not
          // writeable.
          info.definition_code.insert(
              "::flatbuffers::nanobind::BindArrayReadonly<" + cpp_type +
              ">(m, \"" + nanobind_name + "\");");
        } else {
          info.definition_code.insert(
              "::flatbuffers::nanobind::BindArrayReadwrite<" + cpp_type +
              ">(m, \"" + nanobind_name + "\");");
        }

        if (!struct_def->fixed && opts_.generate_object_based_api) {
          const auto obj_cpp_type = CppType(field_type, /*object_api=*/true);
          const auto obj_nanobind_name =
              PyBindingName(field_type, /*object_api=*/true);

          info.declaration_code.insert("NB_MAKE_OPAQUE(" + obj_cpp_type + ");");
          if (IsScalar(element_type.base_type)) {
            info.definition_code.insert(
                "::flatbuffers::nanobind::BindStdVectorArithmetic<" +
                obj_cpp_type + ">(m, \"" + obj_nanobind_name + "\");");
          } else {
            info.definition_code.insert(
                "::flatbuffers::nanobind::BindStdVector<" + obj_cpp_type +
                ">(m, \"" + obj_nanobind_name + "\");");
          }
        }
      }
    }
    return info;
  }

  // Generates a nanobind definition for the given enum.
  void GenerateEnum(const EnumDef &def) {
    GenerateComment(def.doc_comment);
    const bool is_flag = def.attributes.Lookup("bit_flags") != nullptr;
    code_ += Indent() + "nb::enum_<" + cpp_namer_.NamespacedType(def) +
             ">(m, \"" + py_namer_.Type(def) + "\", nb::is_arithmetic()" +
             (is_flag ? ", nb::is_flag()" : "") + ")\\";

    const std::string line_indent = "\n" + Indent(3);
    for (const auto *ev : def.Vals()) {
      GenerateComment(ev->doc_comment, line_indent.c_str());
      code_ += line_indent + ".value(\"" + py_namer_.Variant(*ev) + "\", " +
               CppEnumValueName(def, *ev) + ")\\";
    }
    code_ += ";\n";  // Extra new-line.
  }

  // Sets formatting variables for the given struct or table.
  void SetCodeValuesStructOrTable(const StructDef &def) {
    code_.SetValue("BIND_VAR",
                   "cls_" + CppIdentifier(cpp_namer_.NamespacedType(def)));
    code_.SetValue("CPP_TYPE", cpp_namer_.NamespacedType(def));
    code_.SetValue("PY_TYPE", py_namer_.Type(def));
  }

  // Sets formatting variables for the given table's object API.
  void SetCodeValuesTableObjectApi(const StructDef &def) {
    const std::string obj_type_name =
        CppObjectApiType(def, /*namespaced=*/false);
    code_.SetValue("BIND_VAR", "cls_" + CppIdentifier(CppObjectApiType(def)));
    code_.SetValue("CPP_TYPE", CppObjectApiType(def));
    code_.SetValue("PY_TYPE", py_namer_.Type(obj_type_name));
  }

  // Generates an initially empty class binding for a struct/table.
  void GenerateStructOrTableDeclaration(const StructDef &def) {
    if (!def.fixed && opts_.generate_object_based_api) {
      SetCodeValuesTableObjectApi(def);
      code_ += Indent() +
               "nb::class_<{{CPP_TYPE}}> {{BIND_VAR}}(m, \"{{PY_TYPE}}\");";
    }
    SetCodeValuesStructOrTable(def);
    code_ +=
        Indent() + "nb::class_<{{CPP_TYPE}}> {{BIND_VAR}}(m, \"{{PY_TYPE}}\");";
  }

  // Generates an internal class attribute that marks that this is a flatbuffer
  // class. The name is shared with pybind-generated flatbuffers so that both
  // are handled alike.
  void GenerateInternalTypeMarker(int base_type) {
    code_ += Indent() + "{{BIND_VAR}}.attr(\"_fbs_pybind_type\") = " +
             NumToString(base_type) + ";";
  }

  // Generates a nanobind definition for a struct.
  void GenerateStruct(const StructDef &def) {
    SetCodeValuesStructOrTable(def);
    GenerateComment(def.doc_comment);
    GenerateInternalTypeMarker(BASE_TYPE_STRUCT);

    // Default constructor.
    code_ += Indent() + "{{BIND_VAR}}.def(nb::init<>());";

    auto field_defs = GetFieldDefs(def);

    // Keyword args constructor.
    if (field_defs.size() > 0) {
      std::vector<std::string> ctor_params, ctor_args, py_args;
      ctor_params.reserve(field_defs.size());
      ctor_args.reserve(field_defs.size());
      py_args.reserve(field_defs.size());
      for (const auto *field : field_defs) {
        const std::string name = cpp_namer_.Field(field->name);
        ctor_params.push_back(CppArgumentType(field->value.type) + " " + name);
        // Bool arrays are stored (and constructed) as uint8_t.
        ctor_args.push_back(IsBoolArrayOrVector(field->value.type)
                                ? "::flatbuffers::nanobind::BoolsToBytes(" +
                                      name + ")"
                                : name);
        py_args.push_back(PyArg(*field));
      }
      code_ += Indent() + "{{BIND_VAR}}.def(";
      code_ += Indent(3) + "\"__init__\",";
      code_ += Indent(3) + "[]({{CPP_TYPE}} *self, " +
               StrJoin(ctor_params, ", ") + ") {";
      code_ += Indent(4) + "new (self) {{CPP_TYPE}}(" +
               StrJoin(ctor_args, ", ") + ");";
      code_ += Indent(3) + "}, " + StrJoin(py_args, ", ") + ");";
    }

    // Generate accessor bindings.
    for (const auto *field : field_defs) {
      const auto &field_type = field->value.type;
      code_.SetValue("CPP_FIELD", cpp_namer_.Field(*field));
      code_.SetValue("PY_FIELD", py_namer_.Field(*field));
      code_.SetValue("CPP_FIELD_TYPE", CppType(field_type));

      if (IsStruct(field_type)) {
        if (opts_.mutable_buffer) {
          code_ += Indent() + "{{BIND_VAR}}.def_prop_rw(";
          code_ += Indent(3) + "\"{{PY_FIELD}}\",";
          code_ += Indent(3) +
                   "[]({{CPP_TYPE}} &self) -> {{CPP_FIELD_TYPE}} & { return "
                   "self.mutable_{{CPP_FIELD}}(); },";
          code_ += Indent(3) + "[]({{CPP_TYPE}} &self, " +
                   CppArgumentType(field_type) + " value) {";
          code_ += Indent(4) + "self.mutable_{{CPP_FIELD}}() = value;";
          code_ += Indent(3) + "});";
        } else {
          code_ += Indent() +
                   "{{BIND_VAR}}.def_prop_ro(\"{{PY_FIELD}}\", "
                   "&{{CPP_TYPE}}::{{CPP_FIELD}}, "
                   "nb::rv_policy::reference_internal);";
        }
        continue;
      }

      if (IsArray(field_type)) {
        const bool is_bool = IsBoolArrayOrVector(field_type);
        code_.SetValue("BOOL_VIEW", CppBoolViewType(field_type));
        if (!opts_.mutable_buffer) {
          if (is_bool) {
            code_ += Indent() + "{{BIND_VAR}}.def_prop_ro(";
            code_ += Indent(3) + "\"{{PY_FIELD}}\",";
            code_ += Indent(3) +
                     "[]({{CPP_TYPE}} &self) { return {{BOOL_VIEW}}{"
                     "const_cast<{{CPP_FIELD_TYPE}} *>(self.{{CPP_FIELD}}())}; "
                     "},";
            code_ += Indent(3) + "nb::keep_alive<0, 1>());";
          } else {
            code_ += Indent() +
                     "{{BIND_VAR}}.def_prop_ro(\"{{PY_FIELD}}\", "
                     "&{{CPP_TYPE}}::{{CPP_FIELD}}, "
                     "nb::rv_policy::reference_internal);";
          }
          continue;
        }
        // Assignment copies the elements. They are copied to a temporary
        // first, as the source may alias the array.
        const auto element_type = field_type.VectorType();
        code_.SetValue("ELEMENT_TYPE", CppType(element_type));
        code_.SetValue("LENGTH", NumToString(field_type.fixed_length));
        code_ += Indent() + "{{BIND_VAR}}.def_prop_rw(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        if (is_bool) {
          code_ += Indent(3) +
                   "[]({{CPP_TYPE}} &self) { return "
                   "{{BOOL_VIEW}}{self.mutable_{{CPP_FIELD}}()}; },";
        } else {
          code_ += Indent(3) +
                   "[]({{CPP_TYPE}} &self) { return "
                   "self.mutable_{{CPP_FIELD}}(); },";
        }
        code_ += Indent(3) + "[]({{CPP_TYPE}} &self, " +
                 CppArgumentType(field_type) + " value) {";
        code_ += Indent(4) + "std::array<{{ELEMENT_TYPE}}, {{LENGTH}}> copy;";
        code_ +=
            Indent(4) + "std::copy(value.begin(), value.end(), copy.begin());";
        code_ += Indent(4) +
                 "for (::flatbuffers::uoffset_t i = 0; i < {{LENGTH}}; ++i) {";
        code_ +=
            Indent(5) + "self.mutable_{{CPP_FIELD}}()->Mutate(i, copy[i]);";
        code_ += Indent(4) + "}";
        code_ += Indent(3) + "}, " +
                 (is_bool ? "nb::for_getter(nb::keep_alive<0, 1>())"
                          : "nb::rv_policy::reference_internal") +
                 ");";
        continue;
      }

      // POD types.
      if (opts_.mutable_buffer) {
        code_ += Indent() +
                 "{{BIND_VAR}}.def_prop_rw(\"{{PY_FIELD}}\", "
                 "&{{CPP_TYPE}}::{{CPP_FIELD}}, "
                 "&{{CPP_TYPE}}::mutate_{{CPP_FIELD}});";
      } else {
        code_ += Indent() +
                 "{{BIND_VAR}}.def_prop_ro(\"{{PY_FIELD}}\", "
                 "&{{CPP_TYPE}}::{{CPP_FIELD}});";
      }
    }

    // operators.
    if (opts_.gen_compare) {
      code_ += Indent() + "{{BIND_VAR}}.def(nb::self == nb::self);";
      code_ += Indent() + "{{BIND_VAR}}.def(nb::self != nb::self);";
    }

    // __repr__
    GenerateReprBinding(field_defs);

    code_ += "";
  }

  void GenerateTable(const StructDef &def) {
    SetCodeValuesStructOrTable(def);
    GenerateInternalTypeMarker(BASE_TYPE_STRUCT);

    auto field_defs = GetFieldDefs(def);

    // Class methods to interpret a buffer.
    code_ += Indent() +
             "{{BIND_VAR}}.def_static(\"get_root\", [](nb::handle buffer, bool "
             "verify_buffer) {";
    code_ += Indent(3) + "::flatbuffers::nanobind::BufferRequest request;";
    code_ += Indent(3) +
             "const auto buffer_span = "
             "::flatbuffers::nanobind::GetBufferBytes(buffer, request);";
    code_ += Indent(3) + "if (verify_buffer) {";
    code_ += Indent(4) +
             "::flatbuffers::Verifier verifier(buffer_span.data(), "
             "buffer_span.size());";
    code_ += Indent(4) + "if (!verifier.VerifyBuffer<{{CPP_TYPE}}>(nullptr)) {";
    code_ += Indent(5) +
             "throw std::runtime_error(\"Invalid buffer for {{PY_TYPE}}\");";
    code_ += Indent(4) + "}";
    code_ += Indent(3) + "}";
    code_ += Indent(3) +
             "return ::flatbuffers::GetRoot<{{CPP_TYPE}}>(buffer_span.data());";
    // Keep the buffer alive.
    code_ += Indent() +
             "}, nb::arg(\"buffer\"), nb::arg(\"verify_buffer\") = false, "
             "nb::keep_alive<0, 1>(), nb::rv_policy::reference,";
    code_ += Indent(3) +
             "nb::sig(\"def get_root(buffer: typing_extensions.Buffer | "
             "numpy.ndarray, verify_buffer: bool = False) -> {{PY_TYPE}}\"));";

    // Generate accessor bindings.
    for (const auto *field : field_defs) {
      const auto &field_type = field->value.type;
      code_.SetValue("CPP_FIELD", cpp_namer_.Field(*field));
      code_.SetValue("PY_FIELD", py_namer_.Field(*field));
      code_.SetValue("MUTABLE_PREFIX", opts_.mutable_buffer ? "mutable_" : "");

      if (field_type.base_type == BASE_TYPE_UNION) {
        code_.SetValue("TRAITS", FbsUnionTraitsName(*field_type.enum_def));
        code_ += Indent() + "{{BIND_VAR}}.def_prop_ro(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ += Indent(3) + "[]({{CPP_TYPE}} &self) {";
        code_ += Indent(4) +
                 "return {{TRAITS}}::Get(self.{{CPP_FIELD}}_type(), "
                 "self.{{CPP_FIELD}}());";
        code_ += Indent(3) + "}, nb::rv_policy::reference_internal);";
        code_ += Indent() +
                 "{{BIND_VAR}}.def_prop_ro(\"{{PY_FIELD}}_type\", "
                 "&{{CPP_TYPE}}::{{CPP_FIELD}}_type);";
        continue;
      }

      if (IsUnionVector(field_type)) {
        code_.SetValue("TRAITS", FbsUnionTraitsName(*field_type.enum_def));
        code_ += Indent() + "{{BIND_VAR}}.def_prop_ro(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ += Indent(3) +
                 "[]({{CPP_TYPE}} &self) -> "
                 "std::optional<::flatbuffers::nanobind::detail::"
                 "UnionFbsVectorView<{{TRAITS}}>> {";
        code_ +=
            Indent(4) +
            "if (self.{{CPP_FIELD}}() == nullptr || "
            "self.{{CPP_FIELD}}_type() == nullptr) { return std::nullopt; }";
        code_ +=
            Indent(4) +
            "return ::flatbuffers::nanobind::detail::UnionFbsVectorView<"
            "{{TRAITS}}>{self.{{CPP_FIELD}}_type(), self.{{CPP_FIELD}}()};";
        // The view references the table's buffer.
        code_ += Indent(3) + "}, nb::keep_alive<0, 1>());";
        code_ += Indent() + "{{BIND_VAR}}.def_prop_ro(";
        code_ += Indent(3) + "\"{{PY_FIELD}}_type\",";
        code_ +=
            Indent(3) +
            "[]({{CPP_TYPE}} &self) { return "
            "std::make_optional(self.{{MUTABLE_PREFIX}}{{CPP_FIELD}}_type()"
            "); },";
        code_ += Indent(3) + "nb::rv_policy::reference_internal);";
        continue;
      }

      if (IsBoolArrayOrVector(field_type)) {
        code_.SetValue("BOOL_VIEW", CppBoolViewType(field_type));
        code_.SetValue("CPP_FIELD_TYPE", CppType(field_type));
        code_ += Indent() + "{{BIND_VAR}}.def_prop_ro(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ += Indent(3) +
                 "[]({{CPP_TYPE}} &self) -> std::optional<{{BOOL_VIEW}}> {";
        code_ += Indent(4) +
                 "auto *vector = const_cast<{{CPP_FIELD_TYPE}} *>("
                 "self.{{CPP_FIELD}}());";
        code_ += Indent(4) + "if (vector == nullptr) { return std::nullopt; }";
        code_ += Indent(4) + "return {{BOOL_VIEW}}{vector};";
        code_ += Indent(3) + "}, nb::keep_alive<0, 1>());";
        continue;
      }

      if (IsStruct(field_type) || IsTable(field_type) || IsString(field_type) ||
          IsVector(field_type)) {
        code_ += Indent() + "{{BIND_VAR}}.def_prop_ro(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ += Indent(3) +
                 "[]({{CPP_TYPE}} &self) { return "
                 "std::make_optional(self.{{MUTABLE_PREFIX}}{{CPP_FIELD}}()); "
                 "},";
        code_ += Indent(3) + "nb::rv_policy::reference_internal);";
        continue;
      }

      // Optional scalars.
      if (field->IsScalarOptional()) {
        code_.SetValue("CPP_FIELD_TYPE", CppArgumentType(field_type));
        const std::string getter =
            "[]({{CPP_TYPE}} &self) -> std::optional<{{CPP_FIELD_TYPE}}> {\n" +
            Indent(4) + "const auto value = self.{{CPP_FIELD}}();\n" +
            Indent(4) + "if (!value) { return std::nullopt; }\n" + Indent(4) +
            "return *value;\n" + Indent(3) + "}";
        if (opts_.mutable_buffer) {
          code_ += Indent() + "{{BIND_VAR}}.def_prop_rw(";
          code_ += Indent(3) + "\"{{PY_FIELD}}\",";
          code_ += Indent(3) + getter + ",";
          GenerateMutateScalarSetter(field_type);
        } else {
          code_ += Indent() + "{{BIND_VAR}}.def_prop_ro(";
          code_ += Indent(3) + "\"{{PY_FIELD}}\",";
          code_ += Indent(3) + getter + ");";
        }
        continue;
      }

      // POD types.
      if (opts_.mutable_buffer) {
        code_ += Indent() + "{{BIND_VAR}}.def_prop_rw(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ += Indent(3) + "&{{CPP_TYPE}}::{{CPP_FIELD}},";
        GenerateMutateScalarSetter(field_type);
      } else {
        code_ += Indent() +
                 "{{BIND_VAR}}.def_prop_ro(\"{{PY_FIELD}}\", "
                 "&{{CPP_TYPE}}::{{CPP_FIELD}});";
      }
    }

    // Unpacking.
    if (opts_.generate_object_based_api) {
      code_ += Indent() +
               "{{BIND_VAR}}.def(\"unpack_to\", [](const {{CPP_TYPE}} &self, " +
               CppObjectApiType(def) + " &obj) {";
      code_ += Indent(3) + "self.UnPackTo(&obj);";
      code_ +=
          Indent() +
          "}, nb::arg(\"obj\"), nb::call_guard<nb::gil_scoped_release>());";
    }

    // __repr__
    GenerateReprBinding(field_defs);

    code_ += "";
  }

  // Generates the setter of a scalar field of a packed table, which throws if
  // the field is not present in the buffer.
  void GenerateMutateScalarSetter(const Type &field_type) {
    code_ += Indent(3) + "[]({{CPP_TYPE}} &self, " +
             CppArgumentType(field_type) + " value) {";
    code_ += Indent(4) + "if (!self.mutate_{{CPP_FIELD}}(value)) {";
    code_ += Indent(5) +
             "throw nb::buffer_error(\"{{PY_FIELD}} is not writeable\");";
    code_ += Indent(4) + "}";
    code_ += Indent(3) + "});";
  }

  // Generates code which assigns the span `{{CPP_FIELD_VALUE}}` to the vector
  // field `self->{{CPP_FIELD}}`. The elements are copied to a temporary first,
  // as the source may alias the vector.
  void GenerateVectorAssignment(const Type &field_type, int level) {
    const auto element_type = field_type.VectorType();
    code_ += Indent(level) + "{{CPP_FIELD_TYPE}} copy;";
    if (IsTable(element_type)) {
      code_ += Indent(level) + "copy.reserve({{CPP_FIELD_VALUE}}.size());";
      code_ +=
          Indent(level) + "for (const auto *element : {{CPP_FIELD_VALUE}}) {";
      code_ += Indent(level + 1) + "copy.emplace_back(std::make_unique<" +
               CppType(element_type, /*object_api=*/true) + ">(*element));";
      code_ += Indent(level) + "}";
    } else if (IsUnion(element_type)) {
      code_ += Indent(level) + "copy.resize({{CPP_FIELD_VALUE}}.size());";
      code_ += Indent(level) + "for (size_t i = 0; i < copy.size(); ++i) {";
      code_ += Indent(level + 1) + UnionTraitsName(*field_type.enum_def) +
               "::Set(copy[i], {{CPP_FIELD_VALUE}}[i]);";
      code_ += Indent(level) + "}";
    } else {
      code_ += Indent(level) +
               "copy.assign({{CPP_FIELD_VALUE}}.begin(), "
               "{{CPP_FIELD_VALUE}}.end());";
    }
    code_ += Indent(level) + "self->{{CPP_FIELD}} = std::move(copy);";
  }

  // Generates a nanobind definition for a table's object API (i.e. T-type).
  void GenerateTableObjectApi(const StructDef &def) {
    SetCodeValuesTableObjectApi(def);
    GenerateComment(def.doc_comment);
    GenerateInternalTypeMarker(BASE_TYPE_STRUCT);

    auto field_defs = GetFieldDefs(def);

    if (field_defs.empty()) {
      // Default constructor.
      code_ += Indent() + "{{BIND_VAR}}.def(nb::init<>());";
    } else {
      // Keyword args constructor.
      std::vector<std::string> ctor_params, py_args;
      ctor_params.reserve(field_defs.size());
      py_args.reserve(field_defs.size());
      for (const auto *field : field_defs) {
        std::string argument_type = CppArgumentType(
            field->value.type, /*object_api=*/true, /*optional=*/true);
        if (field->IsScalarOptional()) {
          argument_type = "std::optional<" + argument_type + ">";
        }
        ctor_params.push_back(argument_type + " " +
                              cpp_namer_.Field(field->name));
        py_args.push_back(PyArg(*field, /*object_api=*/true));
      }
      code_ += Indent() + "{{BIND_VAR}}.def(";
      code_ += Indent(3) + "\"__init__\",";
      code_ += Indent(3) + "[]({{CPP_TYPE}} *self, " +
               StrJoin(ctor_params, ", ") + ") {";
      code_ += Indent(4) + "new (self) {{CPP_TYPE}}();";
      for (const auto *field : field_defs) {
        const auto &field_type = field->value.type;
        code_.SetValue("CPP_FIELD", cpp_namer_.Field(*field));
        code_.SetValue("CPP_FIELD_TYPE",
                       CppType(field_type, /*object_api=*/true));
        code_.SetValue("CPP_FIELD_VALUE", cpp_namer_.Field(field->name));

        if (IsString(field_type)) {
          code_ += Indent(4) +
                   "self->{{CPP_FIELD}} = std::move({{CPP_FIELD_VALUE}});";
          continue;
        }

        if (field_type.base_type == BASE_TYPE_UNION) {
          code_ += Indent(4) + UnionTraitsName(*field_type.enum_def) +
                   "::Set(self->{{CPP_FIELD}}, {{CPP_FIELD_VALUE}});";
          continue;
        }

        if (IsStruct(field_type) || IsTable(field_type)) {
          code_ += Indent(4) + "if ({{CPP_FIELD_VALUE}}.has_value()) {";
          code_ +=
              Indent(5) +
              "self->{{CPP_FIELD}} = "
              "std::make_unique<{{CPP_FIELD_TYPE}}>(**{{CPP_FIELD_VALUE}});";
          code_ += Indent(4) + "}";
          continue;
        }

        if (IsVector(field_type)) {
          code_ += Indent(4) + "{";
          GenerateVectorAssignment(field_type, 5);
          code_ += Indent(4) + "}";
          continue;
        }

        if (field->IsScalarOptional()) {
          code_ += Indent(4) + "if ({{CPP_FIELD_VALUE}}.has_value()) {";
          code_ += Indent(5) + "self->{{CPP_FIELD}} = *{{CPP_FIELD_VALUE}};";
          code_ += Indent(4) + "}";
          continue;
        }

        // POD types.
        code_ += Indent(4) + "self->{{CPP_FIELD}} = {{CPP_FIELD_VALUE}};";
      }
      code_ += Indent(3) + "}, nb::kw_only(), " + StrJoin(py_args, ", ") + ");";
    }

    // Generate accessor bindings.
    for (const auto *field : field_defs) {
      const auto &field_type = field->value.type;
      code_.SetValue("CPP_FIELD", cpp_namer_.Field(*field));
      code_.SetValue("PY_FIELD", py_namer_.Field(*field));
      code_.SetValue("CPP_FIELD_TYPE",
                     CppType(field_type, /*object_api=*/true));

      if (field_type.base_type == BASE_TYPE_UNION) {
        const auto &enum_def = *field_type.enum_def;
        code_.SetValue("TRAITS", UnionTraitsName(enum_def));

        code_ += Indent() + "{{BIND_VAR}}.def_prop_rw(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ += Indent(3) +
                 "[]({{CPP_TYPE}} &self) { return "
                 "{{TRAITS}}::Get(self.{{CPP_FIELD}}); },";
        code_ += Indent(3) +
                 "[]({{CPP_TYPE}} &self, const {{TRAITS}}::variant_type "
                 "&value) {";
        code_ += Indent(4) + "{{TRAITS}}::Set(self.{{CPP_FIELD}}, value);";
        code_ += Indent(3) + "}, nb::rv_policy::reference_internal);";

        // _type accessor (read-only, type is auto-set by the setter).
        code_ += Indent() + "{{BIND_VAR}}.def_prop_ro(";
        code_ += Indent(3) + "\"{{PY_FIELD}}_type\",";
        code_ += Indent(3) + "[]({{CPP_TYPE}} &self) {";
        code_ += Indent(4) + "return self.{{CPP_FIELD}}.type;";
        code_ += Indent(3) + "});";

        // `ensure_{name}` method, which instantiates the union with the given
        // type if not set already. The return type is the given type.
        code_ += Indent() + "{";
        code_ += Indent(2) + "const std::string type_var = " +
                 UnionTypeVarNameExpr(enum_def) + ";";
        code_ += Indent(2) +
                 "const std::string signature = \"def ensure_{{PY_FIELD}}("
                 "self, union_type: type[\" + type_var + \"], /) -> \" + "
                 "type_var;";
        code_ += Indent(2) + "{{BIND_VAR}}.def(";
        code_ += Indent(4) + "\"ensure_{{PY_FIELD}}\",";
        code_ += Indent(4) + "[]({{CPP_TYPE}} &self, nb::handle union_type) {";
        code_ += Indent(5) +
                 "return {{TRAITS}}::Ensure(self.{{CPP_FIELD}}, union_type);";
        code_ += Indent(4) +
                 "}, nb::arg(\"union_type\"), "
                 "nb::rv_policy::reference_internal, "
                 "nb::sig(signature.c_str()));";
        code_ += Indent() + "}";
        continue;
      }

      if (IsStruct(field_type) || IsTable(field_type)) {
        code_ += Indent() + "{{BIND_VAR}}.def_prop_rw(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ += Indent(3) +
                 "[](const {{CPP_TYPE}} &self) -> "
                 "std::optional<{{CPP_FIELD_TYPE}} *> {";
        code_ += Indent(4) +
                 "if (self.{{CPP_FIELD}} == nullptr) { return std::nullopt; }";
        code_ += Indent(4) + "return self.{{CPP_FIELD}}.get();";
        code_ += Indent(3) + "},";
        code_ += Indent(3) +
                 "[]({{CPP_TYPE}} &self, std::optional<const "
                 "{{CPP_FIELD_TYPE}} *> value) {";
        code_ += Indent(4) + "if (!value.has_value()) {";
        code_ += Indent(5) + "self.{{CPP_FIELD}} = nullptr;";
        code_ += Indent(4) + "} else if (self.{{CPP_FIELD}} == nullptr) {";
        code_ += Indent(5) +
                 "self.{{CPP_FIELD}} = "
                 "std::make_unique<{{CPP_FIELD_TYPE}}>(**value);";
        code_ += Indent(4) + "} else {";
        code_ += Indent(5) + "*self.{{CPP_FIELD}} = **value;";
        code_ += Indent(4) + "}";
        code_ += Indent(3) + "}, nb::rv_policy::reference_internal);";

        // `ensure_{name}` method, which instantiates the struct/table if not
        // set already.
        code_ += Indent() + "{{BIND_VAR}}.def(";
        code_ += Indent(3) + "\"ensure_{{PY_FIELD}}\",";
        code_ += Indent(3) + "[]({{CPP_TYPE}} &self) -> {{CPP_FIELD_TYPE}} * {";
        code_ += Indent(4) + "if (self.{{CPP_FIELD}} == nullptr) {";
        code_ += Indent(5) +
                 "self.{{CPP_FIELD}} = std::make_unique<{{CPP_FIELD_TYPE}}>();";
        code_ += Indent(4) + "}";
        code_ += Indent(4) + "return self.{{CPP_FIELD}}.get();";
        code_ += Indent(3) + "}, nb::rv_policy::reference_internal);";
        continue;
      }

      if (IsVector(field_type)) {
        // Assignment copies the elements.
        code_.SetValue("CPP_FIELD_VALUE", "value");
        code_ += Indent() + "{{BIND_VAR}}.def_prop_rw(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ += Indent(3) +
                 "[]({{CPP_TYPE}} &self) -> {{CPP_FIELD_TYPE}} & { return "
                 "self.{{CPP_FIELD}}; },";
        code_ += Indent(3) + "[]({{CPP_TYPE}} *self, " +
                 CppArgumentType(field_type, /*object_api=*/true) + " value) {";
        GenerateVectorAssignment(field_type, 4);
        code_ += Indent(3) +
                 "}, nb::rv_policy::reference_internal, "
                 "nb::for_setter(nb::arg(\"value\").none()));";
        continue;
      }

      if (field->IsScalarOptional()) {
        code_.SetValue("CPP_SCALAR_TYPE", CppArgumentType(field_type));
        code_ += Indent() + "{{BIND_VAR}}.def_prop_rw(";
        code_ += Indent(3) + "\"{{PY_FIELD}}\",";
        code_ += Indent(3) +
                 "[](const {{CPP_TYPE}} &self) -> "
                 "std::optional<{{CPP_SCALAR_TYPE}}> {";
        code_ +=
            Indent(4) + "if (!self.{{CPP_FIELD}}) { return std::nullopt; }";
        code_ += Indent(4) + "return *self.{{CPP_FIELD}};";
        code_ += Indent(3) + "},";
        code_ += Indent(3) +
                 "[]({{CPP_TYPE}} &self, std::optional<{{CPP_SCALAR_TYPE}}> "
                 "value) {";
        code_ += Indent(4) + "if (value.has_value()) {";
        code_ += Indent(5) + "self.{{CPP_FIELD}} = *value;";
        code_ += Indent(4) + "} else {";
        code_ += Indent(5) + "self.{{CPP_FIELD}} = ::flatbuffers::nullopt;";
        code_ += Indent(4) + "}";
        code_ += Indent(3) + "});";
        continue;
      }

      // POD types.
      code_ += Indent() +
               "{{BIND_VAR}}.def_rw(\"{{PY_FIELD}}\", "
               "&{{CPP_TYPE}}::{{CPP_FIELD}});";
    }

    // Packing.
    code_ += Indent() +
             "{{BIND_VAR}}.def(\"pack\", [](const {{CPP_TYPE}} &self, "
             "std::optional<nb::bytearray> buffer) {";
    code_ += Indent(3) +
             "nb::bytearray bytes = buffer.has_value() ? *buffer : "
             "nb::bytearray();";
    code_ +=
        Indent(3) +
        "auto fbb = ::flatbuffers::nanobind::CreateFlatBufferBuilder(bytes);";
    code_ += Indent(3) + "fbb.Finish(" + cpp_namer_.NamespacedType(def) +
             "::Pack(fbb, &self));";
    // The memoryview keeps the bytearray buffer alive.
    code_ +=
        Indent(3) + "return ::flatbuffers::nanobind::AsMemoryView(bytes, fbb);";
    code_ += Indent() + "}, nb::arg(\"buffer\") = nb::none());";

    // operators.
    if (opts_.gen_compare) {
      code_ += Indent() + "{{BIND_VAR}}.def(nb::self == nb::self);";
      code_ += Indent() + "{{BIND_VAR}}.def(nb::self != nb::self);";
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
               "{{BIND_VAR}}.def(\"__repr__\", [](nb::handle) { return "
               "\"{{PY_TYPE}}()\"; });";
      return;
    }
    code_ += Indent() + "{{BIND_VAR}}.def(\"__repr__\", [](nb::handle self) {";
    code_ += Indent(3) + "nb::list items;";
    for (const auto *field : field_defs) {
      code_.SetValue("PY_FIELD", py_namer_.Field(*field));
      code_ += Indent(3) +
               "items.append(nb::str(\"{{PY_FIELD}}={}\").format("
               "nb::repr(self.attr(\"{{PY_FIELD}}\"))));";
    }
    code_ += Indent(3) +
             "return nb::str(\"{{PY_TYPE}}({})\").format(nb::str(\", "
             "\").attr(\"join\")(items));";
    code_ += Indent() + "});";
  }

  std::vector<const FieldDef *> GetFieldDefs(const StructDef &def) const {
    std::vector<const FieldDef *> field_defs;
    field_defs.reserve(def.fields.vec.size());
    for (const auto *field : def.fields.vec) {
      if (field->deprecated) continue;
      // The type of a union (or union vector) is exposed by the union field.
      if (field->value.type.base_type == BASE_TYPE_UTYPE) continue;
      if (IsVector(field->value.type) &&
          field->value.type.element == BASE_TYPE_UTYPE) {
        continue;
      }
      field_defs.push_back(field);
    }
    return field_defs;
  }

  std::string CppType(const Type &type, bool object_api = false) const {
    if (IsScalar(type.base_type)) {
      if (type.enum_def) { return cpp_namer_.NamespacedType(*type.enum_def); }
      // The object API uses `bool` (e.g. std::vector<bool>), but packed
      // flatbuffers store `uint8_t`.
      if (object_api && IsBool(type.base_type)) { return "bool"; }
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
          if (IsUnion(element_type)) {
            return "std::vector<" +
                   CppObjectApiUnionType(*element_type.enum_def) + ">";
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
        if (IsUnion(element_type)) {
          return "::flatbuffers::Vector<::flatbuffers::Offset<void>>";
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
        // Fall-through.
      default: {
        return "void";
      }
    }
  }

  std::string CppArgumentType(const Type &type, bool object_api = false,
                              bool optional = false) const {
    if (IsArray(type)) {
      // Bool arrays are stored as uint8_t, but are passed as bools.
      const std::string element_type_str =
          IsBoolArrayOrVector(type) ? "bool"
                                    : CppType(type.VectorType(), object_api);
      return "::flatbuffers::span<const " + element_type_str + ", " +
             NumToString(type.fixed_length) + ">";
    }
    if (IsVector(type)) {
      const auto element_type = type.VectorType();
      std::string element_type_str;
      if (IsString(element_type)) {
        element_type_str = "std::string";
      } else if (IsUnion(element_type)) {
        element_type_str = UnionTraitsName(*type.enum_def) + "::variant_type";
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
      return UnionTraitsName(*type.enum_def) + "::variant_type";
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

  std::string CppBoolViewType(const Type &type) const {
    return "::flatbuffers::nanobind::BoolView<" + CppType(type) + ">";
  }

  // Returns the object API type which holds a union (e.g. FooUnion).
  std::string CppObjectApiUnionType(const EnumDef &def) const {
    return cpp_namer_.NamespacedType(def) + "Union";
  }

  std::string CppUnionVariantType(const EnumDef &def,
                                  bool object_api = false) const {
    std::vector<std::string> variant_types;
    variant_types.reserve(def.Vals().size());
    for (const auto *ev : def.Vals()) {
      if (ev->union_type.base_type == BASE_TYPE_NONE) { continue; }
      variant_types.push_back(CppType(ev->union_type, object_api) + " *");
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

  // Returns the nanobind argument annotation (with its default value) for a
  // constructor argument.
  std::string PyArg(const FieldDef &field, bool object_api = false) const {
    const auto &field_type = field.value.type;
    std::string arg = "nb::arg(\"" + py_namer_.Field(field) + "\")";
    // Spans accept None. NOTE: Spans are not wrapped in std::optional, since
    // the span must not outlive its type caster.
    if (IsArray(field_type) || IsVector(field_type)) { arg += ".none()"; }
    // Enum defaults which are not an enum value (e.g. 0 for bit flags) have no
    // name, so their signature must be given explicitly.
    if (IsEnum(field_type) && !field.IsScalarOptional() &&
        field.attributes.Lookup("native_default") == nullptr &&
        field_type.enum_def->FindByValue(field.value.constant) == nullptr) {
      arg += ".sig(\"" + py_namer_.Type(*field_type.enum_def) + "(" +
             field.value.constant + ")\")";
    }
    return arg + " = " + PyArgDefaultValue(field, object_api);
  }

  std::string PyArgDefaultValue(const FieldDef &field,
                                bool object_api = false) const {
    auto *native_default = field.attributes.Lookup("native_default");
    if (native_default != nullptr) { return native_default->constant; }

    const auto &field_type = field.value.type;
    if (IsScalar(field_type.base_type)) {
      if (field.IsScalarOptional()) { return "nb::none()"; }
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
      if (field_type.base_type == BASE_TYPE_ULONG) {
        return field.value.constant + "ULL";
      }
      if (field_type.base_type == BASE_TYPE_LONG) {
        return field.value.constant + "LL";
      }
      return field.value.constant;
    }
    // Object API struct fields are nullable.
    if (IsStruct(field_type) && !object_api) {
      return CppType(field_type) + "()";
    }
    if (IsString(field_type)) { return "\"\""; }
    return "nb::none()";
  }

  std::string PyBindingName(const Type &type, bool object_api = false) const {
    if (IsScalar(type.base_type)) {
      if (type.enum_def) { return py_namer_.Type(*type.enum_def); }
      if (IsBool(type.base_type)) { return "Bool"; }
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

  const IDLOptionsNanobind opts_;
  const IdlNamer cpp_namer_;
  const IdlNamer py_namer_;
  const TypedFloatConstantGenerator float_const_gen_;
  CodeWriter code_;
};

}  // namespace nanobind

static bool GenerateNanobind(const Parser &parser, const std::string &path,
                             const std::string &file_name) {
  nanobind::IDLOptionsNanobind opts(parser.opts);
  nanobind::NanobindGenerator generator(parser, path, file_name, opts);

  return generator.generate();
}

namespace {

class NanobindCodeGenerator : public CodeGenerator {
 public:
  Status GenerateCode(const Parser &parser, const std::string &path,
                      const std::string &filename) override {
    if (!GenerateNanobind(parser, path, filename)) { return Status::ERROR; }
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

  IDLOptions::Language Language() const override {
    return IDLOptions::kNanobind;
  }

  std::string LanguageName() const override { return "Nanobind"; }
};

}  // namespace

std::unique_ptr<CodeGenerator> NewNanobindCodeGenerator() {
  return std::unique_ptr<NanobindCodeGenerator>(new NanobindCodeGenerator());
}

}  // namespace flatbuffers
