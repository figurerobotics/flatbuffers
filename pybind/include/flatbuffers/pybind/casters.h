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

#ifndef FLATBUFFERS_PYBIND_CASTERS_H_
#define FLATBUFFERS_PYBIND_CASTERS_H_

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <array>
#include <type_traits>
#include <vector>

#include "flatbuffers/pybind/memory.h"
#include "flatbuffers/stl_emulation.h"
#include "flatbuffers/string.h"

PYBIND11_NAMESPACE_BEGIN(PYBIND11_NAMESPACE)
PYBIND11_NAMESPACE_BEGIN(detail)

namespace internal {

template<typename C>
using has_resize_method =
    std::is_same<decltype(std::declval<C>().resize(0)), void>;

template<typename C,
         typename std::enable_if<has_resize_method<C>::value, int>::type = 0>
void resize_maybe(C *container, size_t size) {
  container->resize(size);
}
void resize_maybe(void *container, size_t) {}

}  // namespace internal

template<typename SpanType, typename Value,
         std::size_t Extent = flatbuffers::dynamic_extent>
struct span_caster {
  using value_conv = make_caster<Value>;
  using value_holder_type = typename std::conditional<
      Extent == flatbuffers::dynamic_extent,
      std::vector<std::remove_cv_t<Value>>,
      std::array<std::remove_cv_t<Value>, Extent>>::type;

 private:
  value_holder_type value_holder_;

 public:
  bool load(handle src, bool convert) {
    if (src.is_none()) {
      // Use the default-constructed value (empty for dynamic extents, zeros
      // otherwise).
      return true;
    }
    if (::flatbuffers::pybind::MakeSpanFromObject<Value, Extent>(
            value, src, /*throw_error=*/false)) {
      return true;
    }

    if (!isinstance<sequence>(src)) {
      throw value_error("Input must be a sequence.");
    }
    auto seq = reinterpret_borrow<sequence>(src);
    if (Extent != flatbuffers::dynamic_extent && seq.size() != Extent) {
      throw value_error(pybind11::str("Expected sequence size {} but got {}")
                            .format(Extent, seq.size()));
    }
    internal::resize_maybe(&value_holder_, seq.size());
    size_t index = 0;
    for (const auto &it : seq) {
      value_conv conv;
      if (!conv.load(it, convert)) { return false; }
      value_holder_[index++] = cast_op<Value &&>(std::move(conv));
    }
    value = SpanType(value_holder_.data(), value_holder_.size());
    return true;
  }

  // flatbuffer generated accessors currently never return spans, so leave this
  // unimplemented for now.
  template<typename T>
  static handle cast(T &&src, return_value_policy policy, handle parent) {
    throw cast_error("Returning spans from flatbuffers is not yet supported.");
  }

  PYBIND11_TYPE_CASTER(SpanType, const_name("typing.Sequence[") +
                                     value_conv::name + const_name("] | None"));

  // flatbuffers::span is not default-constuctible for a fixed extent.
  template<
      std::size_t S = Extent,
      typename std::enable_if<S != flatbuffers::dynamic_extent, int>::type = 0>
  span_caster()
      : value_holder_(), value(value_holder_.data(), value_holder_.size()) {}
  template<
      std::size_t S = Extent,
      typename std::enable_if<S == flatbuffers::dynamic_extent, int>::type = 0>
  span_caster() : value_holder_(), value() {}
};

template<typename Type, std::size_t Extent>
struct type_caster<::flatbuffers::span<Type, Extent>>
    : span_caster<::flatbuffers::span<Type, Extent>, Type, Extent> {};

/// @brief Type caster to allow returning flatbuffers::String to Python.
template<> struct type_caster<::flatbuffers::String> {
  using value_conv = make_caster<std::string_view>;

  bool load(handle src, bool convert) {
    throw cast_error("Flatbuffer strings are not writeable.");
    return false;
  }

  static handle cast(const ::flatbuffers::String &src,
                     return_value_policy policy, handle parent) {
    // NOTE: This always copies the string.
    return pybind11::str(src.c_str(), src.size()).release();
  }

  PYBIND11_TYPE_CASTER(::flatbuffers::String, const_name("str"));
};

PYBIND11_NAMESPACE_END(detail)
PYBIND11_NAMESPACE_END(PYBIND11_NAMESPACE)

#endif  // FLATBUFFERS_PYBIND_CASTERS_H_
