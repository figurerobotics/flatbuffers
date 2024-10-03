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

#ifndef FLATBUFFERS_PYBIND_MEMORY_H_
#define FLATBUFFERS_PYBIND_MEMORY_H_

#include <pybind11/pybind11.h>

#include "flatbuffers/allocator.h"
#include "flatbuffers/flatbuffer_builder.h"
#include "flatbuffers/stl_emulation.h"

namespace flatbuffers {
namespace pybind {

/// @brief Custom allocator for python which wraps a resizable bytearray.
class ByteArrayAllocator : public Allocator {
 public:
  explicit ByteArrayAllocator(pybind11::bytearray &bytearray)
      : bytearray_(bytearray) {}

  uint8_t *allocate(size_t size) FLATBUFFERS_OVERRIDE {
    if (size > bytearray_.size() &&
        PyByteArray_Resize(bytearray_.ptr(), size) < 0) {
      throw std::runtime_error("Failed to resize bytearray.");
    }
    FLATBUFFERS_ASSERT(PyByteArray_Size(bytearray_.ptr()) >= size);
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
  pybind11::bytearray bytearray_;
};

/// @brief Create a FlatBufferBuilder which is backed by the given bytearray.
FlatBufferBuilder CreateFlatBufferBuilder(pybind11::bytearray &bytearray) {
  size_t initial_size = bytearray.size();
  return FlatBufferBuilder(initial_size, new ByteArrayAllocator(bytearray),
                           /*own_allocator=*/true);
}

/// @brief Adapts the given FlatBufferBuilder as a memoryview.
/// The backing memory allocation must outlive the memoryview.
pybind11::memoryview AsMemoryView(FlatBufferBuilder &builder) {
  return pybind11::memoryview::from_memory(builder.GetBufferPointer(),
                                           builder.GetSize());
}

/// @brief Adapts an object implementing the buffer protocol into a span.
template<typename T, std::size_t Extent = dynamic_extent,
         typename std::enable_if<std::is_arithmetic<T>::value, int>::type = 0>
bool MakeSpanFromBuffer(span<T, Extent> &result, const pybind11::buffer &buffer,
                        bool throw_error = true) {
  using FormatT = std::remove_cv_t<T>;
  auto info = buffer.request(/*writable=*/!std::is_const<T>::value);
  if (info.ndim != 1 || info.strides[0] != static_cast<ssize_t>(sizeof(T))) {
    if (throw_error) {
      throw pybind11::type_error(
          "Buffer must be 1-dimensional and contiguous.");
    }
    return false;
  }
  if (!pybind11::detail::compare_buffer_info<FormatT>::compare(info) ||
      static_cast<ssize_t>(sizeof(T)) != info.itemsize) {
    if (throw_error) {
      throw pybind11::type_error(
          "Expected buffer format " +
          pybind11::format_descriptor<FormatT>::format() + " but got " +
          info.format);
    }
    return false;
  }
  if (Extent != dynamic_extent && info.size != Extent) {
    if (throw_error) {
      throw pybind11::value_error(
          pybind11::str("Expected buffer size {} but got {}")
              .format(Extent, info.size));
    }
    return false;
  }
  T *data = static_cast<T *>(info.ptr);
  result = span<T, Extent>(data, info.size);
  return true;
}

template<typename T, std::size_t Extent = dynamic_extent,
         typename std::enable_if<!std::is_arithmetic<T>::value, int>::type = 0>
bool MakeSpanFromBuffer(span<T, Extent> &result, const pybind11::buffer &buffer,
                        bool throw_error = true) {
  return false;
}

template<typename T, std::size_t Extent = dynamic_extent>
bool MakeSpanFromObject(span<T, Extent> &result, pybind11::handle obj,
                        bool throw_error = true) {
  if (!PyObject_CheckBuffer(obj.ptr())) {
    if (throw_error) {
      throw pybind11::type_error(
          "Object does not support the buffer protocol.");
    }
    return false;
  }
  return MakeSpanFromBuffer<T, Extent>(
      result, pybind11::reinterpret_borrow<pybind11::buffer>(obj), throw_error);
}

}  // namespace pybind
}  // namespace flatbuffers

#endif  // FLATBUFFERS_PYBIND_MEMORY_H_
