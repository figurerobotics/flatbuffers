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
"""
Improved bazel build rules for flatbuffers.
"""

load("@rules_cc//cc:defs.bzl", "cc_library")
load("@rules_python//python:defs.bzl", "py_library")

DEFAULT_FLATC_ARGS = [
    "--cpp-ptr-type flatbuffers::unique_ptr",
    "--gen-compare",
    "--gen-mutable",
    "--gen-object-api",
    "--keep-prefix",
    "--reflect-names",
]

LANGUAGE_EXTS = {
    "binary": ".bin",
    "cpp": ".h",
    "csharp": ".cs",
    "dart": ".dart",
    "go": ".go",
    "java": ".java",
    "json": ".json",
    "lua": ".lua",
    "python": ".py",
    "rust": ".rs",
    "ts": ".ts",
    "kotlin": ".kt",
    "nim": ".nim",
    "php": ".php",
    "pybind": ".cpp",
    "swift": ".swift",
}

LANGUAGE_GRPC_EXTS = {
    "cpp": [".grpc.fb.h", ".grpc.fb.cc"],
    "python": ["_grpc_fb.py"],
}

FlatcInfo = provider(
    "Information produced from flatc code generation.",
    fields = {
        "fbs_files": "depset of .fbs files used to generate the code.",
    },
)

def _generated_file_name(fbs_name, filename_suffix, language):
    """Returns the generated file name for the given .fbs file."""
    if language not in LANGUAGE_EXTS:
        fail("Unsupported language: %s" % language)
    name_root = fbs_name.rsplit(".")[0]
    return name_root + filename_suffix + LANGUAGE_EXTS[language]

def _generated_grpc_file_names(fbs_name, filename_suffix, language):
    """Returns the generated file name for the given .fbs file."""
    if language not in LANGUAGE_GRPC_EXTS:
        fail("GRPC generation is unsupported for language: %s" % language)

    name_root = fbs_name.rsplit(".")[0]
    res = []

    for ext in LANGUAGE_GRPC_EXTS[language]:
        res.append(name_root + ext)

    return res

def _flatc_aspect_impl(target, ctx):
    all_fbs_files = []
    for attr in ["srcs", "hdrs"]:
        if not hasattr(ctx.rule.attr, attr):
            continue
        for label in getattr(ctx.rule.attr, attr):
            if FlatcInfo in label:
                all_fbs_files.append(label[FlatcInfo].fbs_files)

    flatc_info = FlatcInfo(fbs_files = depset(transitive = all_fbs_files))
    return [flatc_info]

flatc_aspect = aspect(
    doc = "Propagates FlatcInfo from a target's dependencies.",
    implementation = _flatc_aspect_impl,
    attr_aspects = ["deps"],
    provides = [FlatcInfo],
)

def _flatc_generated_files_impl(ctx):
    if not ctx.files.srcs:
        fail("No .fbs files provided.")

    all_fbs_files = []
    for dep in ctx.attr.deps:
        if FlatcInfo in dep:
            all_fbs_files.append(dep[FlatcInfo].fbs_files)
    dep_fbs_files = []
    for file in ctx.files.deps:
        if file.extension == "fbs":
            dep_fbs_files.append(file)
    fbs_files = depset(direct = ctx.files.srcs + dep_fbs_files, transitive = all_fbs_files)

    all_outs = []
    for src in ctx.files.srcs:
        args = ctx.actions.args()

        out = ctx.actions.declare_file(
            _generated_file_name(src.basename, ctx.attr.filename_suffix, ctx.attr.language),
        )
        outs = [out]
        args.add("-o", out.dirname)
        args.add("-I", "./")
        args.add("--%s" % ctx.attr.language)
        args.add("--filename-suffix", ctx.attr.filename_suffix)
        for arg in ctx.attr.flatc_args:
            arg = ctx.expand_location(arg)
            if "--grpc" in arg:
                for name in _generated_grpc_file_names(src.basename, ctx.attr.filename_suffix, ctx.attr.language):
                    outs.append(ctx.actions.declare_file(name))
            if " " in arg:
                args.add_all(arg.split(" "))
            else:
                args.add(arg)

        args.add(src.path)

        ctx.actions.run(
            mnemonic = "Flatc",
            progress_message = "Generating flatbuffers code: %s" % src,
            inputs = fbs_files,
            executable = ctx.executable.flatc,
            arguments = [args],
            outputs = outs,
        )
        for out in outs:
            all_outs.append(out)

    default_info = DefaultInfo(files = depset(all_outs))
    flatc_info = FlatcInfo(fbs_files = fbs_files)
    return [default_info, flatc_info]

flatc_generated_files = rule(
    doc = "Generates files using flatc.",
    implementation = _flatc_generated_files_impl,
    attrs = {
        "srcs": attr.label_list(
            doc = "The .fbs files to generate code for.",
            mandatory = True,
            allow_files = [".fbs"],
        ),
        "deps": attr.label_list(
            doc = "Other targets or files to be visible to flatc.",
            aspects = [flatc_aspect],
            allow_files = True,
        ),
        "language": attr.string(
            doc = "The language to generate code for.",
            default = "cpp",
        ),
        "filename_suffix": attr.string(
            doc = "The suffix to append to the generated file names.",
            default = "_generated",
        ),
        "flatc_args": attr.string_list(
            doc = "Arguments to pass to flatc.",
            default = DEFAULT_FLATC_ARGS,
        ),
        "flatc": attr.label(
            default = "@com_github_google_flatbuffers//:flatc",
            doc = "The flatc compiler.",
            executable = True,
            allow_single_file = True,
            cfg = "exec",
        ),
    },
)

def flatbuffer_cc_library(
        name,
        srcs,
        deps = [],
        flatc_data = [],
        filename_suffix = None,
        flatc_args = None,
        flatc = None,
        **kwargs):
    """A cc_library which generates and compiles flatbuffers C++ code.

    Args:
        name: Rule name.
        srcs: Source .fbs files.
        deps: C++ dependencies for generated C++ code. This can include other
            `flatbuffer_cc_library` targets whose flatbuffer files are imported by `srcs`, and
            other `cc_library` targets (e.g. libraries referenced by `flatc_args`).
        flatc_data: Additional files to make visible to flatc when generating code. (e.g. files
            specified by `--cpp-include`).
        filename_suffix: Overrides the default filename suffix ("_generated") for generated files.
        flatc_args: Overrides the arguments to pass to flatc.
        flatc: Overrides the flatc executable.
        **kwargs: Additional arguments to pass to `cc_library`.
    """
    gen_target_name = "%s_srcs" % name
    flatc_generated_files(
        name = gen_target_name,
        srcs = srcs,
        language = "cpp",
        deps = deps + flatc_data,
        filename_suffix = filename_suffix,
        flatc_args = flatc_args,
        flatc = flatc,
    )
    cc_library(
        name = name,
        srcs = [":%s" % gen_target_name],
        hdrs = [":%s" % gen_target_name],
        deps = deps + [
            "@com_github_google_flatbuffers//:runtime_cc",
        ],
        **kwargs
    )

def flatbuffer_py_library(
        name,
        srcs,
        deps = [],
        flatc_data = [],
        filename_suffix = None,
        flatc_args = None,
        flatc = None,
        **kwargs):
    """A py_library which generates and compiles flatbuffers Python code.

    Args:
        name: Rule name.
        srcs: Source .fbs files.
        deps: Python dependencies for generated Python code. This can include other
            `flatbuffer_py_library` targets whose flatbuffer files are imported by `srcs`, and
            other `py_library` targets (e.g. libraries referenced by `flatc_args`).
        flatc_data: Additional files to make visible to flatc when generating code.
        filename_suffix: Overrides the default filename suffix ("_generated") for generated files.
        flatc_args: Overrides the arguments to pass to flatc.
        flatc: Overrides the flatc executable.
        **kwargs: Additional arguments to pass to `cc_library`.
    """
    gen_target_name = "%s_srcs" % name
    flatc_generated_files(
        name = gen_target_name,
        srcs = srcs,
        language = "python",
        deps = deps + flatc_data,
        filename_suffix = filename_suffix,
        flatc_args = flatc_args,
        flatc = flatc,
    )
    py_library(
        name = name,
        srcs = [":%s" % gen_target_name],
        deps = deps,
        **kwargs
    )
