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

#ifndef FLATBUFFERS_NANOBIND_MEMORY_H_
#define FLATBUFFERS_NANOBIND_MEMORY_H_

#include <nanobind/nanobind.h>

#include <cstring>
#include <type_traits>

#include "flatbuffers/allocator.h"
#include "flatbuffers/flatbuffer_builder.h"
#include "flatbuffers/stl_emulation.h"

namespace flatbuffers {
namespace nanobind {

namespace nb = ::nanobind;

/// @brief Custom allocator for python which wraps a resizable bytearray.
class ByteArrayAllocator : public Allocator {
 public:
  explicit ByteArrayAllocator(nb::bytearray bytearray)
      : bytearray_(std::move(bytearray)) {}

  uint8_t *allocate(size_t size) FLATBUFFERS_OVERRIDE {
    if (size > bytearray_.size() &&
        PyByteArray_Resize(bytearray_.ptr(), static_cast<Py_ssize_t>(size)) <
            0) {
      // e.g. BufferError if a memoryview of a previous pack() is still alive.
      nb::raise_python_error();
    }
    FLATBUFFERS_ASSERT(bytearray_.size() >= size);
    return reinterpret_cast<uint8_t *>(PyByteArray_AS_STRING(bytearray_.ptr()));
  }

  void deallocate(uint8_t *p, size_t) FLATBUFFERS_OVERRIDE {
    // Nothing to do; bytearray will be garbage collected by python.
  }

  uint8_t *reallocate_downward(uint8_t *old_p, size_t old_size, size_t new_size,
                               size_t in_use_back,
                               size_t in_use_front) FLATBUFFERS_OVERRIDE {
    FLATBUFFERS_ASSERT(new_size > old_size);  // vector_downward only grows
    uint8_t *resized_p = allocate(new_size);
    // The front part of the buffer should be preserved from the resize
    // operation. If the buffer grew by more than the in-use back part, we can
    // memcpy; otherwise we must memmove to handle the potential overlap.
    if ((new_size - old_size) >= in_use_back) {
      memcpy(resized_p + new_size - in_use_back,
             resized_p + old_size - in_use_back, in_use_back);
    } else {
      memmove(resized_p + new_size - in_use_back,
              resized_p + old_size - in_use_back, in_use_back);
    }
    return resized_p;
  }

 private:
  nb::bytearray bytearray_;
};

/// @brief Create a FlatBufferBuilder which is backed by the given bytearray.
inline FlatBufferBuilder CreateFlatBufferBuilder(
    const nb::bytearray &bytearray) {
  size_t initial_size = bytearray.size();
  return FlatBufferBuilder(initial_size, new ByteArrayAllocator(bytearray),
                           /*own_allocator=*/true);
}

/// @brief Returns a memoryview of the finished buffer in `builder`, which must
/// be backed by `bytearray`.
/// The memoryview holds a buffer export of the bytearray, so it keeps the
/// bytearray alive and prevents it from being resized while the view exists.
inline nb::memoryview AsMemoryView(const nb::bytearray &bytearray,
                                   FlatBufferBuilder &builder) {
  const auto *base =
      reinterpret_cast<const uint8_t *>(PyByteArray_AS_STRING(bytearray.ptr()));
  const Py_ssize_t start =
      static_cast<Py_ssize_t>(builder.GetBufferPointer() - base);
  const Py_ssize_t stop = start + static_cast<Py_ssize_t>(builder.GetSize());
  nb::object view = nb::steal(PyMemoryView_FromObject(bytearray.ptr()));
  if (!view.is_valid()) { nb::raise_python_error(); }
  return nb::borrow<nb::memoryview>(view[nb::slice(start, stop)]);
}

/// @brief RAII wrapper of a Py_buffer request.
class BufferRequest {
 public:
  BufferRequest() { view_.obj = nullptr; }
  ~BufferRequest() { Release(); }
  BufferRequest(const BufferRequest &) = delete;
  BufferRequest &operator=(const BufferRequest &) = delete;

  /// Requests a buffer from `obj`; returns false (with no Python error set) if
  /// `obj` does not support the buffer protocol with the given flags.
  bool Request(PyObject *obj, int flags) {
    Release();
    if (!PyObject_CheckBuffer(obj)) { return false; }
    if (PyObject_GetBuffer(obj, &view_, flags) != 0) {
      view_.obj = nullptr;
      PyErr_Clear();
      return false;
    }
    return true;
  }

  const Py_buffer &view() const { return view_; }

 private:
  void Release() {
    if (view_.obj != nullptr) { PyBuffer_Release(&view_); }
    view_.obj = nullptr;
  }

  Py_buffer view_;
};

namespace detail {

// Returns true if the given PEP 3118 format (and item size) describes `T`.
template<typename T> bool FormatMatches(const char *format, Py_ssize_t size) {
  if (size != static_cast<Py_ssize_t>(sizeof(T))) { return false; }
  if (format == nullptr) { return std::is_same<T, uint8_t>::value; }
  // Native, standard, and little-endian byte orders are all accepted.
  if (*format == '@' || *format == '=' || *format == '<') { ++format; }
  if (format[0] == '\0' || format[1] != '\0') { return false; }
  const char c = format[0];
  if (std::is_same<T, bool>::value) { return c == '?'; }
  if (std::is_floating_point<T>::value) {
    return c == 'e' || c == 'f' || c == 'd';
  }
  if (std::is_signed<T>::value) {
    return c == 'b' || c == 'h' || c == 'i' || c == 'l' || c == 'q' || c == 'n';
  }
  return c == 'B' || c == 'H' || c == 'I' || c == 'L' || c == 'Q' || c == 'N';
}

}  // namespace detail

/// @brief Adapts an object implementing the buffer protocol into a span. The
/// span is valid for as long as `request` is alive.
template<typename T, std::size_t Extent = dynamic_extent>
bool MakeSpanFromObject(span<T, Extent> &result, nb::handle obj,
                        BufferRequest &request) {
  using FormatT = std::remove_cv_t<T>;
  static_assert(std::is_arithmetic<FormatT>::value,
                "Only arithmetic types can be adapted from buffers.");
  int flags = PyBUF_FORMAT | PyBUF_ND | PyBUF_C_CONTIGUOUS;
  if (!std::is_const<T>::value) { flags |= PyBUF_WRITABLE; }
  if (!request.Request(obj.ptr(), flags)) { return false; }
  const Py_buffer &view = request.view();
  if (view.ndim != 1 ||
      !detail::FormatMatches<FormatT>(view.format, view.itemsize)) {
    return false;
  }
  const size_t size = static_cast<size_t>(view.shape[0]);
  if (Extent != dynamic_extent && size != Extent) { return false; }
  result = span<T, Extent>(static_cast<T *>(view.buf), size);
  return true;
}

/// @brief Adapts an object implementing the buffer protocol into a span of
/// bytes. A writable buffer is preferred (so that packed flatbuffers can be
/// mutated), but read-only buffers (e.g. `bytes`) are accepted. Throws a
/// TypeError if the object is not a contiguous buffer.
inline span<const uint8_t> GetBufferBytes(nb::handle obj,
                                          BufferRequest &request) {
  if (!request.Request(obj.ptr(), PyBUF_WRITABLE | PyBUF_C_CONTIGUOUS) &&
      !request.Request(obj.ptr(), PyBUF_C_CONTIGUOUS)) {
    throw nb::type_error(
        "Object does not support the buffer protocol or is not contiguous.");
  }
  const Py_buffer &view = request.view();
  return span<const uint8_t>(static_cast<const uint8_t *>(view.buf),
                             static_cast<size_t>(view.len));
}

}  // namespace nanobind
}  // namespace flatbuffers

#endif  // FLATBUFFERS_NANOBIND_MEMORY_H_
