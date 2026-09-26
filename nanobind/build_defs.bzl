"""Build rules for flatbuffers-generated nanobind code."""

load("@nanobind_bazel//:build_defs.bzl", "nanobind_extension")
load("//:new_build_defs.bzl", "flatc_generated_files")

DEFAULT_COPTS = [
    "-std=c++17",
    "-Wno-unused-local-typedef",
]

def _nanobind_extension(name, srcs, deps, py_deps = [], **kwargs):
    """Creates a nanobind extension (and an alias named `name` to it)."""
    nanobind_extension(
        name = name,
        srcs = srcs,
        deps = deps,
        data = py_deps,
        **kwargs
    )

def flatbuffer_nanobind_library(
        name,
        srcs,
        cc_deps = [],
        py_deps = [],
        flatc_data = [],
        copts = DEFAULT_COPTS,
        filename_suffix = None,
        flatc_args = None,
        flatc = None,
        extension_macro = None,
        **kwargs):
    """A python extension which generates and compiles flatbuffers nanobind code.

    Args:
        name: Rule name. This must match the generated module name (i.e. the .fbs file name
            with `filename_suffix`).
        srcs: Source .fbs files.
        cc_deps: `flatbuffer_cc_library` targets which correspond to the flatbuffer files in srcs.
        py_deps: Python dependencies for generated nanobind code. This can include other
            `flatbuffer_nanobind_library` targets whose flatbuffer files are imported by `srcs`.
        flatc_data: Additional files to make visible to flatc when generating code. (e.g. files
            specified by `--cpp-include`).
        copts: C++ compiler options for compiling the nanobind extension.
        filename_suffix: Overrides the default filename suffix ("_generated") for generated files.
        flatc_args: Overrides the arguments to pass to flatc.
        flatc: Overrides the flatc executable.
        extension_macro: Macro which creates the extension target `name` from
            `(name, srcs, deps, py_deps, copts, **kwargs)`. Defaults to `nanobind_extension` from
            nanobind_bazel.
        **kwargs: Additional arguments to pass to `extension_macro`.
    """
    gen_target_name = "%s_srcs" % name
    flatc_generated_files(
        name = gen_target_name,
        srcs = srcs,
        language = "nanobind",
        deps = cc_deps + py_deps + flatc_data,
        filename_suffix = filename_suffix,
        flatc_args = flatc_args,
        flatc = flatc,
    )
    (extension_macro or _nanobind_extension)(
        name = name,
        srcs = [":%s" % gen_target_name],
        copts = copts,
        deps = [
            "@flatbuffers//nanobind",
        ] + cc_deps,
        py_deps = py_deps,
        **kwargs
    )
