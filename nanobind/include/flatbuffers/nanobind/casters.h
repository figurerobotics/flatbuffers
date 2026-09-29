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

#ifndef FLATBUFFERS_NANOBIND_CASTERS_H_
#define FLATBUFFERS_NANOBIND_CASTERS_H_

#include <nanobind/nanobind.h>
#include <nanobind/stl/string.h>

#include <array>
#include <memory>
#include <type_traits>

#include "flatbuffers/nanobind/memory.h"
#include "flatbuffers/stl_emulation.h"
#include "flatbuffers/string.h"

NAMESPACE_BEGIN(NB_NAMESPACE)
NAMESPACE_BEGIN(detail)

/// @brief Type caster to pass a Python sequence (or buffer) as a
/// flatbuffers::span argument.
/// For arithmetic element types, objects implementing the buffer protocol with
/// a matching format are adapted without copying. Otherwise, the sequence is
/// copied into storage owned by the caster.
/// None is accepted (for arguments annotated with `.none()`) and yields an
/// empty span for dynamic extents, or default values otherwise.
template<typename Type, std::size_t Extent>
struct type_caster<::flatbuffers::span<Type, Extent>> {
  using SpanType = ::flatbuffers::span<Type, Extent>;
  using ValueT = std::remove_cv_t<Type>;
  using Caster = make_caster<ValueT>;
  static constexpr bool kDynamic = Extent == ::flatbuffers::dynamic_extent;

 private:
  // std::vector<bool> has no data(), so dynamic storage is a plain array.
  using Storage = std::conditional_t<kDynamic, std::unique_ptr<ValueT[]>,
                                     std::array<ValueT, kDynamic ? 1 : Extent>>;
  Storage storage_{};
  ::flatbuffers::nanobind::BufferRequest request_;

  ValueT *StorageData(size_t size) {
    if constexpr (kDynamic) {
      storage_.reset(new ValueT[size]());
      return storage_.get();
    } else {
      (void)size;
      return storage_.data();
    }
  }

 public:
  // Arithmetic spans also accept numpy arrays (and other buffers).
  NB_TYPE_CASTER(
      SpanType,
      const_name("collections.abc.Sequence[") + Caster::Name + const_name("]") +
          const_name<std::is_arithmetic<ValueT>::value>(" | numpy.ndarray", ""))

  // flatbuffers::span is not default-constructible for a fixed extent.
  type_caster()
      : value(StorageData(kDynamic ? 0 : Extent), kDynamic ? 0 : Extent) {}

  bool from_python(handle src, uint8_t flags, cleanup_list *cleanup) noexcept {
    if (src.is_none()) {
      value =
          SpanType(StorageData(kDynamic ? 0 : Extent), kDynamic ? 0 : Extent);
      return true;
    }
    if constexpr (std::is_arithmetic<ValueT>::value) {
      if (::flatbuffers::nanobind::MakeSpanFromObject<Type, Extent>(value, src,
                                                                    request_)) {
        return true;
      }
    }

    size_t size;
    PyObject *temp;
    PyObject **o = seq_get(src.ptr(), &size, &temp);
    bool success = o != nullptr;
    if (success && !kDynamic && size != Extent) { success = false; }

    ValueT *data = success ? StorageData(size) : nullptr;
    Caster caster;
    flags = flags_for_local_caster<ValueT>(flags);
    for (size_t i = 0; success && i < size; ++i) {
      if (!caster.from_python(o[i], flags, cleanup) ||
          !caster.template can_cast<ValueT>()) {
        success = false;
        break;
      }
      data[i] = caster.operator cast_t<ValueT>();
    }
    Py_XDECREF(temp);

    if (success) { value = SpanType(data, size); }
    return success;
  }

  // Flatbuffer generated accessors never return spans.
  template<typename T>
  static handle from_cpp(T &&, rv_policy, cleanup_list *) noexcept {
    PyErr_SetString(PyExc_TypeError,
                    "Returning spans from flatbuffers is not supported.");
    return handle();
  }
};

/// @brief Type caster to allow returning flatbuffers::String to Python.
template<> struct type_caster<::flatbuffers::String> {
  static constexpr auto Name = const_name("str");
  template<typename T> using Cast = const ::flatbuffers::String *;
  template<typename T> static constexpr bool can_cast() { return false; }

  bool from_python(handle, uint8_t, cleanup_list *) noexcept {
    // Flatbuffer strings are not writeable.
    return false;
  }

  explicit operator const ::flatbuffers::String *() { return nullptr; }

  // NOTE: This always copies the string.
  static handle from_cpp(const ::flatbuffers::String *src, rv_policy,
                         cleanup_list *) noexcept {
    if (src == nullptr) { return none().release(); }
    return PyUnicode_FromStringAndSize(src->c_str(),
                                       static_cast<Py_ssize_t>(src->size()));
  }

  static handle from_cpp(const ::flatbuffers::String &src, rv_policy policy,
                         cleanup_list *cleanup) noexcept {
    return from_cpp(&src, policy, cleanup);
  }
};

NAMESPACE_END(detail)
NAMESPACE_END(NB_NAMESPACE)

#endif  // FLATBUFFERS_NANOBIND_CASTERS_H_
