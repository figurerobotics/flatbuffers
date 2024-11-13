"""Build rules for flatbuffers-generated pybind code."""

load("@pybind11_bazel//:build_defs.bzl", "pybind_extension")
load("@rules_python//python:defs.bzl", "py_library")
load("//:new_build_defs.bzl", "flatc_generated_files")

DEFAULT_COPTS = [
    "-std=c++17",
    "-Wno-unused-local-typedef",
]

def flatbuffer_pybind_library(
        name,
        srcs,
        cc_deps = [],
        py_deps = [],
        flatc_data = [],
        copts = DEFAULT_COPTS,
        filename_suffix = None,
        flatc_args = None,
        flatc = None,
        **kwargs):
    """A py_library which generates and compiles flatbuffers pybind code.

    Args:
        name: Rule name.
        srcs: Source .fbs files.
        cc_deps: `flatbuffer_cc_library` targets which correspond to the flatbuffer files in srcs.
        py_deps: Python dependencies for generated pybind code. This can include other
            `flatbuffer_pybind_library` targets whose flatbuffer files are imported by `srcs`, and
            other `py_library` targets (e.g. libraries referenced by `flatc_args`).
        flatc_data: Additional files to make visible to flatc when generating code. (e.g. files
            specified by `--cpp-include`).
        copts: C++ compiler options for compiling the pybind extension.
        filename_suffix: Overrides the default filename suffix ("_generated") for generated files.
        flatc_args: Overrides the arguments to pass to flatc.
        flatc: Overrides the flatc executable.
        **kwargs: Additional arguments to pass to `py_library`.
    """
    gen_target_name = "%s_srcs" % name
    flatc_generated_files(
        name = gen_target_name,
        srcs = srcs,
        language = "pybind",
        deps = cc_deps + py_deps + flatc_data,
        filename_suffix = filename_suffix,
        flatc_args = flatc_args,
        flatc = flatc,
    )
    pybind_extension(
        name = name,  # This creates a {name}.so target.
        srcs = [":%s" % gen_target_name],
        copts = copts,
        deps = [
            "@com_github_google_flatbuffers//pybind",
        ] + cc_deps,
    )
    py_library(
        name = name + "_lib",
        deps = [
            ":%s.so" % (name),
        ] + py_deps,
        **kwargs
    )
