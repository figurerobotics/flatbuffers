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

#ifndef FLATBUFFERS_PYBIND_BIND_ARRAY_H_
#define FLATBUFFERS_PYBIND_BIND_ARRAY_H_

#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl_bind.h>

#include <type_traits>

namespace flatbuffers {
namespace pybind {

namespace detail {

// Checks if the given type has an `element_type` member (e.g. smart pointers).
template<typename, typename = void>
struct has_element_type : std::false_type {};

template<typename T>
struct has_element_type<T, std::void_t<typename T::element_type>>
    : std::true_type {};

// Type information for scalar types.
template<typename T, typename = void> struct ElementTypeHelper {
  using return_type = T;
  using const_arg_type = T;

  static constexpr pybind11::return_value_policy policy =
      pybind11::return_value_policy::automatic;

  static inline return_type GetReturnValue(T value) { return value; }

  static inline T NewItem() { return T(); }
  static inline const T NewItem(const_arg_type value) { return value; }
};

// For holder types (e.g. smart pointers).
template<typename T>
struct ElementTypeHelper<
    T, typename std::enable_if<has_element_type<T>::value>::type> {
  using return_type = typename T::element_type *;
  using const_arg_type = typename T::element_type &;

  static constexpr pybind11::return_value_policy policy =
      pybind11::return_value_policy::reference_internal;

  static inline return_type GetReturnValue(const T &value) {
    return value.get();
  }

  static inline T NewItem() { return T(new typename T::element_type()); }
  static inline T NewItem(const_arg_type value) {
    return T(new typename T::element_type(value));
  }
};

// For raw pointers of scalars.
template<typename T>
struct ElementTypeHelper<
    T,
    typename std::enable_if<
        std::is_pointer<T>::value &&
        std::is_scalar<typename std::remove_pointer<T>::type>::value>::type> {
  using value_type = typename std::remove_pointer<T>::type;
  using return_type = value_type;
  using const_arg_type = value_type;

  static constexpr pybind11::return_value_policy policy =
      pybind11::return_value_policy::automatic;

  static inline value_type GetReturnValue(T value) { return *value; }
  // No `NewItem`; arrays are never resizable.
};

// For raw pointers of non-scalars (e.g. arrays of structs).
template<typename T>
struct ElementTypeHelper<
    T,
    typename std::enable_if<
        std::is_pointer<T>::value &&
        !std::is_scalar<typename std::remove_pointer<T>::type>::value>::type> {
  using value_type = typename std::remove_pointer<T>::type;
  using return_type = value_type *;
  using const_arg_type = const value_type &;

  static constexpr pybind11::return_value_policy policy =
      pybind11::return_value_policy::reference_internal;

  static inline T GetReturnValue(T value) { return value; }
  // No `NewItem`; arrays are never resizable.
};

// For value-types (e.g. vectors of structs).
template<typename T>
struct ElementTypeHelper<
    T, typename std::enable_if<!has_element_type<T>::value &&
                               !std::is_scalar<T>::value>::type> {
  using return_type = T *;
  using const_arg_type = const T &;

  static constexpr pybind11::return_value_policy policy =
      pybind11::return_value_policy::reference_internal;

  static inline const T *GetReturnValue(const T &value) { return &value; }
  static inline T *GetReturnValue(T &value) { return &value; }

  static inline T NewItem() { return T(); }
  static inline const T &NewItem(const_arg_type value) { return value; }
};

// Returns the return type of operator[] for the templated type.
template<typename T>
using item_type_t = std::remove_reference_t<
    decltype(std::declval<T>()[std::declval<size_t>()])>;

inline size_t WrapIndexOrThrow(ssize_t idx, size_t size) {
  ssize_t new_idx = idx;
  if (new_idx < 0) { new_idx += size; }
  if (new_idx < 0 || new_idx >= size) {
    throw pybind11::index_error(
        pybind11::str("Index {} out of range for size: {}").format(idx, size));
  }
  return new_idx;
}

template<typename ArrayT, typename PyClass>
inline void BindReadOperations(PyClass &c) {
  using item_type = item_type_t<ArrayT>;
  using TypeHelper = detail::ElementTypeHelper<item_type>;

  c.def("__len__", &ArrayT::size);
  c.def("__bool__", [](const ArrayT &self) { return self.size() > 0; });

  c.def(
      "__getitem__",
      [](ArrayT &self, ssize_t i) {
        return TypeHelper::GetReturnValue(
            self[detail::WrapIndexOrThrow(i, self.size())]);
      },
      TypeHelper::policy);
}

template<typename ArrayT, typename PyClass>
inline void BindFbsWriteOperations(PyClass &c) {
  using item_type = item_type_t<ArrayT>;
  using TypeHelper = detail::ElementTypeHelper<item_type>;

  c.def("__setitem__",
        [](ArrayT &self, ssize_t i, typename TypeHelper::const_arg_type value) {
          self.Mutate(detail::WrapIndexOrThrow(i, self.size()), value);
        });
}

template<typename ArrayT, typename PyClass>
inline void BindStdVectorWriteOperations(PyClass &c) {
  using item_type = item_type_t<ArrayT>;
  using TypeHelper = detail::ElementTypeHelper<item_type>;

  c.def("__setitem__", [](ArrayT &self, ssize_t i,
                          typename TypeHelper::const_arg_type v) {
    self[detail::WrapIndexOrThrow(i, self.size())] = TypeHelper::NewItem(v);
  });

  // Copy appending.
  c.def("append", [](ArrayT &self, typename TypeHelper::const_arg_type value) {
    self.push_back(TypeHelper::NewItem(value));
  });

  // In-place appending an element.
  c.def(
      "add",
      [](ArrayT &self) {
        return TypeHelper::GetReturnValue(
            self.emplace_back(TypeHelper::NewItem()));
      },
      pybind11::return_value_policy::reference_internal);

  c.def("reserve", [](ArrayT &self, size_t size) { self.reserve(size); });
  c.def("resize", [](ArrayT &self, size_t size) { self.resize(size); });
}

template<typename ArrayT, typename PyClass>
inline void BindArithmeticOperations(PyClass &c) {
  using item_type = item_type_t<ArrayT>;

  // Check that the storage type is numpy-compatible.
  pybind11::format_descriptor<item_type>::format();

  c.def_buffer([](ArrayT &self) -> pybind11::buffer_info {
    return pybind11::buffer_info(
        self.data(), static_cast<ssize_t>(sizeof(item_type)),
        pybind11::format_descriptor<item_type>::format(), 1, { self.size() },
        { sizeof(item_type) });
  });

  c.def("numpy", [](pybind11::handle self) {
    const ArrayT &self_cpp = pybind11::cast<const ArrayT &>(self);
    return pybind11::array(pybind11::dtype::of<item_type>(), self_cpp.size(),
                           self_cpp.data(), self);
  });
}

}  // namespace detail

// Binds a ::flatbuffers::Array or ::flatbuffers::Vector.
template<typename ArrayT>
inline void BindArrayReadonly(pybind11::handle scope, const char *name) {
  using PyClass = pybind11::class_<ArrayT>;
  PyClass c(scope, name, pybind11::module_local());
  detail::BindReadOperations<ArrayT, PyClass>(c);
}

// Binds a ::flatbuffers::Array or ::flatbuffers::Vector.
template<typename ArrayT>
inline void BindArrayReadwrite(pybind11::handle scope, const char *name) {
  using PyClass = pybind11::class_<ArrayT>;
  PyClass c(scope, name, pybind11::module_local());
  detail::BindReadOperations<ArrayT, PyClass>(c);
  detail::BindFbsWriteOperations<ArrayT, PyClass>(c);
}

// Binds a ::flatbuffers::Array or ::flatbuffers::Vector whose data type is
// arithmetic.
template<typename ArrayT>
inline void BindArrayArithmetic(pybind11::handle scope, const char *name) {
  using PyClass = pybind11::class_<ArrayT>;
  PyClass c(scope, name, pybind11::module_local(), pybind11::buffer_protocol());
  detail::BindReadOperations<ArrayT, PyClass>(c);
  detail::BindFbsWriteOperations<ArrayT, PyClass>(c);
  detail::BindArithmeticOperations<ArrayT, PyClass>(c);
}

// Binds a std::vector (e.g. for object API vector fields).
template<typename ArrayT>
inline void BindStdVector(pybind11::handle scope, const char *name) {
  using PyClass = pybind11::class_<ArrayT>;
  PyClass c(scope, name, pybind11::module_local());
  detail::BindReadOperations<ArrayT, PyClass>(c);
  detail::BindStdVectorWriteOperations<ArrayT, PyClass>(c);
}

// Binds a std::vector (e.g. for object API vector fields) whose data type is
// arithmetic.
template<typename ArrayT>
inline void BindStdVectorArithmetic(pybind11::handle scope, const char *name) {
  using PyClass = pybind11::class_<ArrayT>;
  PyClass c(scope, name, pybind11::module_local(), pybind11::buffer_protocol());
  detail::BindReadOperations<ArrayT, PyClass>(c);
  detail::BindStdVectorWriteOperations<ArrayT, PyClass>(c);
  detail::BindArithmeticOperations<ArrayT, PyClass>(c);
}

}  // namespace pybind
}  // namespace flatbuffers

#endif  // FLATBUFFERS_PYBIND_BIND_ARRAY_H_
