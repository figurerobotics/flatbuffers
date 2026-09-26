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

#ifndef FLATBUFFERS_NANOBIND_BIND_ARRAY_H_
#define FLATBUFFERS_NANOBIND_BIND_ARRAY_H_

#include <nanobind/make_iterator.h>
#include <nanobind/nanobind.h>
#include <nanobind/ndarray.h>
#include <nanobind/stl/detail/traits.h>
#include <nanobind/stl/optional.h>
#include <nanobind/stl/string.h>
#include <nanobind/stl/variant.h>

#include <algorithm>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include "flatbuffers/vector.h"

namespace flatbuffers {
namespace nanobind {

namespace nb = ::nanobind;

namespace detail {

// Checks if the given type has an `element_type` member (e.g. smart pointers).
template<typename, typename = void>
struct has_element_type : std::false_type {};

template<typename T>
struct has_element_type<T, std::void_t<typename T::element_type>>
    : std::true_type {};

template<typename T> struct is_std_vector : std::false_type {};
template<typename T, typename A>
struct is_std_vector<std::vector<T, A>> : std::true_type {};

// Type information for scalar types.
template<typename T, typename = void> struct ElementTypeHelper {
  using return_type = T;
  using const_arg_type = T;

  static constexpr nb::rv_policy policy = nb::rv_policy::automatic;

  static inline return_type GetReturnValue(T value) { return value; }

  static inline T NewItem() { return T(); }
  static inline T NewItem(const_arg_type value) { return value; }
  static inline nb::object Take(T value) { return nb::cast(value); }
};

// For holder types (e.g. smart pointers).
template<typename T>
struct ElementTypeHelper<
    T, typename std::enable_if<has_element_type<T>::value>::type> {
  using element_type = typename T::element_type;
  using return_type = element_type *;
  using const_arg_type = const element_type &;

  static constexpr nb::rv_policy policy = nb::rv_policy::reference_internal;

  static inline return_type GetReturnValue(const T &value) {
    return value.get();
  }

  static inline T NewItem() { return T(new element_type()); }
  static inline T NewItem(const_arg_type value) {
    return T(new element_type(value));
  }
  // Ownership of the element is transferred to Python.
  static inline nb::object Take(T &&value) {
    return nb::cast(value.release(), nb::rv_policy::take_ownership);
  }
};

// For raw pointers of scalars.
template<typename T>
struct ElementTypeHelper<
    T,
    typename std::enable_if<
        std::is_pointer<T>::value &&
        std::is_scalar<typename std::remove_pointer<T>::type>::value>::type> {
  using value_type = std::remove_cv_t<typename std::remove_pointer<T>::type>;
  using return_type = value_type;
  using const_arg_type = value_type;

  static constexpr nb::rv_policy policy = nb::rv_policy::automatic;

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
  using const_arg_type = const std::remove_cv_t<value_type> &;

  static constexpr nb::rv_policy policy = nb::rv_policy::reference_internal;

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

  static constexpr nb::rv_policy policy = nb::rv_policy::reference_internal;

  static inline const T *GetReturnValue(const T &value) { return &value; }
  static inline T *GetReturnValue(T &value) { return &value; }

  static inline T NewItem() { return T(); }
  static inline const T &NewItem(const_arg_type value) { return value; }
  static inline nb::object Take(T &&value) {
    return nb::cast(std::move(value), nb::rv_policy::move);
  }
};

// Returns the element type of an array (the return type of operator[]).
template<typename ArrayT> struct ItemType {
  using type = std::remove_reference_t<decltype(std::declval<ArrayT &>()[0])>;
};
// std::vector<bool> returns proxy references.
template<typename A> struct ItemType<std::vector<bool, A>> {
  using type = bool;
};
template<typename ArrayT> using item_type_t = typename ItemType<ArrayT>::type;

// The element operations of an array, derived from its item type.
template<typename ArrayT> struct DefaultOps {
  using item_type = item_type_t<ArrayT>;
  using Helper = ElementTypeHelper<item_type>;
  using return_type = typename Helper::return_type;
  using const_arg_type = typename Helper::const_arg_type;
  static constexpr nb::rv_policy policy = Helper::policy;

  static inline return_type Get(ArrayT &self, size_t i) {
    return Helper::GetReturnValue(self[i]);
  }
};

// The element operations of a std::vector.
template<typename ArrayT> struct StdVectorOps : DefaultOps<ArrayT> {
  using Base = DefaultOps<ArrayT>;
  using stored_type = typename ArrayT::value_type;
  using Helper = typename Base::Helper;

  static inline stored_type New() { return Helper::NewItem(); }
  static inline stored_type New(typename Base::const_arg_type value) {
    return Helper::NewItem(value);
  }
  static inline nb::object Take(stored_type &&value) {
    return Helper::Take(std::move(value));
  }
};

// The element operations of a std::vector of object API unions. `Traits` is
// generated for each union type (see idl_gen_nanobind.cpp).
template<typename Traits> struct UnionStdVectorOps {
  using stored_type = typename Traits::union_type;
  using return_type = typename Traits::variant_type;
  using const_arg_type = typename Traits::variant_type;
  static constexpr nb::rv_policy policy = nb::rv_policy::reference_internal;

  template<typename ArrayT>
  static inline return_type Get(ArrayT &self, size_t i) {
    return Traits::Get(self[i]);
  }
  static inline stored_type New() { return stored_type(); }
  static inline stored_type New(const const_arg_type &value) {
    stored_type result;
    Traits::Set(result, value);
    return result;
  }
  static inline nb::object Take(stored_type &&value) {
    return nb::cast(Traits::Get(value), nb::rv_policy::copy);
  }
};

// A view of the (types, values) vector pair of a packed union vector field.
template<typename Traits> struct UnionFbsVectorView {
  const ::flatbuffers::Vector<typename Traits::enum_type> *types;
  const ::flatbuffers::Vector<::flatbuffers::Offset<void>> *values;

  size_t size() const { return values->size(); }
  typename Traits::variant_type operator[](size_t i) const {
    return Traits::Get(types->Get(static_cast<uoffset_t>(i)),
                       values->Get(static_cast<uoffset_t>(i)));
  }
};

// The element operations of a packed union vector.
template<typename Traits> struct UnionFbsVectorOps {
  using return_type = typename Traits::variant_type;
  static constexpr nb::rv_policy policy = nb::rv_policy::reference_internal;

  template<typename ArrayT>
  static inline return_type Get(ArrayT &self, size_t i) {
    return self[i];
  }
};

inline size_t WrapIndexOrThrow(Py_ssize_t idx, size_t size) {
  Py_ssize_t new_idx = idx;
  if (new_idx < 0) { new_idx += static_cast<Py_ssize_t>(size); }
  if (new_idx < 0 || new_idx >= static_cast<Py_ssize_t>(size)) {
    throw nb::index_error(nb::str("Index {} out of range for size: {}")
                              .format(idx, size)
                              .c_str());
  }
  return static_cast<size_t>(new_idx);
}

// Clamps an insertion index like `list.insert`.
inline size_t ClampIndex(Py_ssize_t idx, size_t size) {
  const auto ssize = static_cast<Py_ssize_t>(size);
  if (idx < 0) { idx = std::max<Py_ssize_t>(idx + ssize, 0); }
  return static_cast<size_t>(std::min(idx, ssize));
}

// Casts a Python object to an argument of type `T`. Bound types are cast by
// reference to avoid a copy.
template<typename T> decltype(auto) CastArg(nb::handle h) {
  using V = std::remove_cv_t<std::remove_reference_t<T>>;
  if constexpr (nb::detail::is_base_caster_v<nb::detail::make_caster<V>>) {
    return nb::cast<const V &>(h);
  } else {
    return nb::cast<V>(h);
  }
}

// Returns the Python object for element `i`, referencing `self_h`.
template<typename ArrayT, typename Ops>
inline nb::object GetObject(ArrayT &self, nb::handle self_h, size_t i) {
  return nb::cast(Ops::Get(self, i), Ops::policy, self_h);
}

// Returns the index of the first element equal to `value` within [start, stop)
// or -1.
template<typename ArrayT, typename Ops>
inline Py_ssize_t FindIndex(ArrayT &self, nb::handle self_h, nb::handle value,
                            size_t start, size_t stop) {
  stop = std::min<size_t>(stop, self.size());
  for (size_t i = start; i < stop; ++i) {
    if (GetObject<ArrayT, Ops>(self, self_h, i).equal(value)) {
      return static_cast<Py_ssize_t>(i);
    }
  }
  return -1;
}

// Iterates over the indices of an array, re-checking its size at each step so
// that iteration stays in bounds if the array is resized.
template<typename ArrayT, typename Ops> struct IndexIterator {
  ArrayT *self;
  size_t index;
  bool reverse;

  typename Ops::return_type operator*() const {
    return Ops::Get(*self, reverse ? index - 1 : index);
  }
  IndexIterator &operator++() {
    if (reverse) {
      --index;
    } else {
      ++index;
    }
    return *this;
  }
};
struct IndexSentinel {};
template<typename ArrayT, typename Ops>
bool operator==(const IndexIterator<ArrayT, Ops> &it, IndexSentinel) {
  return it.reverse ? it.index == 0 || it.index > it.self->size()
                    : it.index >= it.self->size();
}
template<typename ArrayT, typename Ops>
bool operator==(const IndexIterator<ArrayT, Ops> &a,
                const IndexIterator<ArrayT, Ops> &b) {
  return a.index == b.index;
}

// Returns the Python type name of a bound type, relative to `scope`.
inline std::string TypeName(nb::handle type, nb::handle scope) {
  if (!type.is_valid()) { return "typing.Any"; }
  const std::string module = nb::cast<std::string>(type.attr("__module__"));
  const std::string qualname = nb::cast<std::string>(type.attr("__qualname__"));
  if (module == "builtins") { return qualname; }
  if (nb::hasattr(scope, "__name__") &&
      module == nb::cast<std::string>(scope.attr("__name__"))) {
    return qualname;
  }
  return module + "." + qualname;
}

// Returns the Python type name of a C++ element type.
template<typename T> std::string ElementTypeName(nb::handle scope) {
  using U = std::remove_cv_t<std::remove_pointer_t<std::remove_cv_t<T>>>;
  if constexpr (std::is_same<U, bool>::value) {
    return "bool";
  } else if constexpr (std::is_integral<U>::value) {
    return "int";
  } else if constexpr (std::is_floating_point<U>::value) {
    return "float";
  } else if constexpr (std::is_same<U, std::string>::value ||
                       std::is_same<U, ::flatbuffers::String>::value) {
    return "str";
  } else {
    return TypeName(nb::type<U>(), scope);
  }
}

template<typename Variant> struct VariantTypeNames;
template<typename... Ts>
struct VariantTypeNames<std::optional<std::variant<Ts...>>> {
  static std::string Get(nb::handle scope) {
    std::string result;
    ((result += ElementTypeName<Ts>(scope) + " | "), ...);
    return result + "None";
  }
};

// Returns the class signature of a sequence type for stubs.
inline std::string SequenceClassSignature(const char *name, const char *base,
                                          const std::string &element_name) {
  return std::string("class ") + name + "(collections.abc." + base + "[" +
         element_name + "])";
}

// Returns `signature` with each "{T}" replaced by the element type name.
inline std::string ElementSignature(const char *signature,
                                    const std::string &element_name) {
  std::string result = signature;
  const std::string placeholder = "{T}";
  for (size_t pos = result.find(placeholder); pos != std::string::npos;
       pos = result.find(placeholder, pos + element_name.size())) {
    result.replace(pos, placeholder.size(), element_name);
  }
  return result;
}

// Registers `cls` as a virtual subclass of collections.abc.{base}.
inline void RegisterAbc(nb::handle cls, const char *base) {
  nb::module_::import_("collections.abc").attr(base).attr("register")(cls);
}

// If `T` is already bound (e.g. by another flatbuffers module), exposes the
// existing binding in `scope` and returns true.
template<typename T>
inline bool ReuseBinding(nb::handle scope, const char *name) {
  nb::handle existing = nb::type<T>();
  if (!existing.is_valid()) { return false; }
  scope.attr(name) = existing;
  return true;
}

template<typename ArrayT, typename Ops, typename PyClass>
inline void BindReadOperations(PyClass &c, const std::string &element_name) {
  auto sig = [&element_name](const char *signature) {
    return ElementSignature(signature, element_name);
  };
  using return_type = typename Ops::return_type;
  using Iterator = IndexIterator<ArrayT, Ops>;

  c.def("__len__", [](const ArrayT &self) { return (size_t)self.size(); });
  c.def("__bool__", [](const ArrayT &self) { return self.size() > 0; });

  c.def(
      "__getitem__",
      [](ArrayT &self, Py_ssize_t i) -> return_type {
        return Ops::Get(self, WrapIndexOrThrow(i, self.size()));
      },
      Ops::policy,
      nb::sig(sig("def __getitem__(self, index: int, /) -> {T}").c_str()));
  c.def(
      "__getitem__",
      [](nb::handle self_h,
         const nb::slice &slice) -> nb::typed<nb::list, return_type> {
        ArrayT &self = nb::cast<ArrayT &>(self_h);
        auto [start, stop, step, length] = slice.compute(self.size());
        (void)stop;
        nb::list result;
        for (size_t i = 0; i < length; ++i) {
          result.append(GetObject<ArrayT, Ops>(
              self, self_h, static_cast<size_t>(start + step * i)));
        }
        return nb::borrow<nb::typed<nb::list, return_type>>(result);
      },
      nb::sig(
          sig("def __getitem__(self, index: slice, /) -> list[{T}]").c_str()));

  c.def(
      "__iter__",
      [](ArrayT &self) {
        return nb::make_iterator<Ops::policy>(nb::type<ArrayT>(), "Iterator",
                                              Iterator{ &self, 0, false },
                                              IndexSentinel{});
      },
      nb::keep_alive<0, 1>());
  c.def(
      "__reversed__",
      [](ArrayT &self) {
        return nb::make_iterator<Ops::policy>(
            nb::type<ArrayT>(), "Iterator",
            Iterator{ &self, static_cast<size_t>(self.size()), true },
            IndexSentinel{});
      },
      nb::keep_alive<0, 1>());

  c.def(
      "__contains__",
      [](nb::handle self_h, nb::handle value) {
        ArrayT &self = nb::cast<ArrayT &>(self_h);
        return FindIndex<ArrayT, Ops>(self, self_h, value, 0, self.size()) >= 0;
      },
      nb::sig("def __contains__(self, value: object, /) -> bool"));
  c.def(
      "index",
      [](nb::handle self_h, nb::handle value, Py_ssize_t start,
         Py_ssize_t stop) {
        ArrayT &self = nb::cast<ArrayT &>(self_h);
        const size_t size = self.size();
        const Py_ssize_t index =
            FindIndex<ArrayT, Ops>(self, self_h, value, ClampIndex(start, size),
                                   ClampIndex(stop, size));
        if (index < 0) {
          throw nb::value_error(
              nb::str("{} is not in sequence").format(nb::repr(value)).c_str());
        }
        return index;
      },
      // Matches `list.index`, whose `stop` defaults to sys.maxsize.
      nb::arg(), nb::arg() = 0, nb::arg() = PY_SSIZE_T_MAX,
      nb::sig("def index(self, value: object, start: int = 0, stop: int = "
              "sys.maxsize, /) -> int"));
  c.def(
      "count",
      [](nb::handle self_h, nb::handle value) {
        ArrayT &self = nb::cast<ArrayT &>(self_h);
        size_t count = 0;
        for (size_t i = 0; i < self.size(); ++i) {
          if (GetObject<ArrayT, Ops>(self, self_h, i).equal(value)) { ++count; }
        }
        return count;
      },
      nb::sig("def count(self, value: object, /) -> int"));

  // Elementwise equality with any (non-string) sequence.
  c.def(
      "__eq__",
      [](nb::handle self_h, nb::handle other) -> nb::object {
        if (!PySequence_Check(other.ptr()) || PyUnicode_Check(other.ptr()) ||
            PyBytes_Check(other.ptr())) {
          return nb::borrow(Py_NotImplemented);
        }
        ArrayT &self = nb::cast<ArrayT &>(self_h);
        if (nb::len(other) != self.size()) { return nb::bool_(false); }
        for (size_t i = 0; i < self.size(); ++i) {
          if (!GetObject<ArrayT, Ops>(self, self_h, i).equal(other[i])) {
            return nb::bool_(false);
          }
        }
        return nb::bool_(true);
      },
      nb::sig("def __eq__(self, other: object, /) -> bool"));
  c.def(
      "__ne__",
      [](nb::handle self_h, nb::handle other) -> nb::object {
        nb::object eq = self_h.attr("__eq__")(other);
        if (eq.is(Py_NotImplemented)) { return eq; }
        return nb::bool_(!nb::cast<bool>(eq));
      },
      nb::sig("def __ne__(self, other: object, /) -> bool"));

  c.def("__repr__", [](nb::handle self_h) {
    ArrayT &self = nb::cast<ArrayT &>(self_h);
    nb::list items;
    for (size_t i = 0; i < self.size(); ++i) {
      items.append(nb::repr(GetObject<ArrayT, Ops>(self, self_h, i)));
    }
    return nb::str("[{}]").format(nb::str(", ").attr("join")(items));
  });
}

// Write operations for ::flatbuffers::Array or ::flatbuffers::Vector.
template<typename ArrayT, typename PyClass>
inline void BindFbsWriteOperations(PyClass &c,
                                   const std::string &element_name) {
  using const_arg_type = typename DefaultOps<ArrayT>::const_arg_type;

  c.def(
      "__setitem__",
      [](ArrayT &self, Py_ssize_t i, const_arg_type value) {
        self.Mutate(static_cast<uoffset_t>(WrapIndexOrThrow(i, self.size())),
                    value);
      },
      nb::sig(ElementSignature(
                  "def __setitem__(self, index: int, value: {T}, /) -> None",
                  element_name)
                  .c_str()));
}

// Write operations for a std::vector.
template<typename ArrayT, typename Ops, typename PyClass>
inline void BindStdVectorWriteOperations(PyClass &c,
                                         const std::string &element_name) {
  auto sig = [&element_name](const char *signature) {
    return ElementSignature(signature, element_name);
  };
  using const_arg_type = typename Ops::const_arg_type;
  using stored_type = typename Ops::stored_type;
  using Iterable = nb::typed<nb::iterable, const_arg_type>;

  // Converts an iterable of elements to (copied) stored elements.
  auto from_iterable = [](nb::handle values) {
    std::vector<stored_type> result;
    for (nb::handle value : values) {
      result.push_back(Ops::New(CastArg<const_arg_type>(value)));
    }
    return result;
  };

  c.def(
      "__setitem__",
      [](ArrayT &self, Py_ssize_t i, const_arg_type value) {
        self[WrapIndexOrThrow(i, self.size())] = Ops::New(value);
      },
      nb::sig(sig("def __setitem__(self, index: int, value: {T}, /) -> None")
                  .c_str()));
  c.def(
      "__setitem__",
      [from_iterable](ArrayT &self, const nb::slice &slice, Iterable values) {
        auto [start, stop, step, length] = slice.compute(self.size());
        std::vector<stored_type> items = from_iterable(values);
        if (step == 1) {
          const auto first = self.begin() + start;
          const auto last = first + static_cast<Py_ssize_t>(length);
          self.erase(first, last);
          self.insert(self.begin() + start,
                      std::make_move_iterator(items.begin()),
                      std::make_move_iterator(items.end()));
          return;
        }
        (void)stop;
        if (items.size() != length) {
          throw nb::value_error(
              nb::str("attempt to assign sequence of size {} to extended slice "
                      "of size {}")
                  .format(items.size(), length)
                  .c_str());
        }
        for (size_t i = 0; i < length; ++i) {
          self[static_cast<size_t>(start + step * i)] = std::move(items[i]);
        }
      },
      nb::sig(sig("def __setitem__(self, index: slice, value: "
                  "collections.abc.Iterable[{T}], /) -> None")
                  .c_str()));
  c.def(
      "__delitem__",
      [](ArrayT &self, Py_ssize_t i) {
        self.erase(self.begin() + WrapIndexOrThrow(i, self.size()));
      },
      nb::sig("def __delitem__(self, index: int, /) -> None"));
  c.def(
      "__delitem__",
      [](ArrayT &self, const nb::slice &slice) {
        auto [start, stop, step, length] = slice.compute(self.size());
        (void)stop;
        if (step < 0) {
          start += step * static_cast<Py_ssize_t>(length - 1);
          step = -step;
        }
        // Erase from the back so the remaining indices stay valid.
        for (size_t i = length; i > 0; --i) {
          self.erase(self.begin() + start +
                     step * static_cast<Py_ssize_t>(i - 1));
        }
      },
      nb::sig("def __delitem__(self, index: slice, /) -> None"));

  c.def(
      "insert",
      [](ArrayT &self, Py_ssize_t i, const_arg_type value) {
        self.insert(self.begin() + ClampIndex(i, self.size()), Ops::New(value));
      },
      nb::sig(
          sig("def insert(self, index: int, value: {T}, /) -> None").c_str()));

  // Copy appending.
  c.def(
      "append",
      [](ArrayT &self, const_arg_type value) {
        self.push_back(Ops::New(value));
      },
      nb::sig(sig("def append(self, value: {T}, /) -> None").c_str()));
  c.def(
      "extend",
      [from_iterable](ArrayT &self, Iterable values) {
        std::vector<stored_type> items = from_iterable(values);
        self.insert(self.end(), std::make_move_iterator(items.begin()),
                    std::make_move_iterator(items.end()));
      },
      nb::sig(sig("def extend(self, values: collections.abc.Iterable[{T}], /) "
                  "-> None")
                  .c_str()));
  c.def(
      "__iadd__",
      [from_iterable](nb::handle self_h,
                      Iterable values) -> nb::typed<nb::object, ArrayT> {
        ArrayT &self = nb::cast<ArrayT &>(self_h);
        std::vector<stored_type> items = from_iterable(values);
        self.insert(self.end(), std::make_move_iterator(items.begin()),
                    std::make_move_iterator(items.end()));
        return nb::borrow<nb::typed<nb::object, ArrayT>>(self_h);
      },
      nb::sig(sig("def __iadd__(self, values: collections.abc.Iterable[{T}], "
                  "/) -> typing_extensions.Self")
                  .c_str()));

  c.def(
      "pop",
      [](ArrayT &self,
         Py_ssize_t i) -> nb::typed<nb::object, typename Ops::return_type> {
        const size_t index = WrapIndexOrThrow(i, self.size());
        stored_type value = std::move(self[index]);
        self.erase(self.begin() + index);
        return nb::borrow<nb::typed<nb::object, typename Ops::return_type>>(
            Ops::Take(std::move(value)));
      },
      nb::arg() = -1,
      nb::sig(sig("def pop(self, index: int = -1, /) -> {T}").c_str()));
  c.def(
      "remove",
      [](nb::handle self_h, nb::handle value) {
        ArrayT &self = nb::cast<ArrayT &>(self_h);
        const Py_ssize_t index =
            FindIndex<ArrayT, Ops>(self, self_h, value, 0, self.size());
        if (index < 0) {
          throw nb::value_error(
              nb::str("{} is not in sequence").format(nb::repr(value)).c_str());
        }
        self.erase(self.begin() + index);
      },
      // Like `list.remove`, any value is accepted (and raises ValueError if it
      // is not found).
      nb::sig(sig("def remove(self, value: {T}, /) -> None").c_str()));
  c.def("clear", [](ArrayT &self) { self.clear(); });
  c.def("reverse",
        [](ArrayT &self) { std::reverse(self.begin(), self.end()); });
}

// Operations for std::vectors whose elements can be default-constructed in
// place.
template<typename ArrayT, typename Ops, typename PyClass>
inline void BindStdVectorAllocOperations(PyClass &c) {
  // In-place appending an element.
  c.def(
      "add",
      [](ArrayT &self) -> typename Ops::return_type {
        self.push_back(Ops::New());
        return Ops::Get(self, self.size() - 1);
      },
      Ops::policy);

  c.def(
      "reserve", [](ArrayT &self, size_t size) { self.reserve(size); },
      nb::sig("def reserve(self, size: int, /) -> None"));
  c.def(
      "resize", [](ArrayT &self, size_t size) { self.resize(size); },
      nb::sig("def resize(self, size: int, /) -> None"));
}

// Returns the data pointer of an array for the buffer protocol.
template<typename ArrayT> inline void *DataPointer(ArrayT &self) {
  // Some empty containers have a null data pointer, which Python rejects.
  static char kEmpty = 0;
  void *data = const_cast<void *>(static_cast<const void *>(self.data()));
  return data != nullptr ? data : &kEmpty;
}

// The PEP 3118 format character of a scalar type.
template<typename T> constexpr const char *FormatOf() {
  if constexpr (std::is_same<T, bool>::value) {
    return "?";
  } else if constexpr (std::is_floating_point<T>::value) {
    return sizeof(T) == 4 ? "f" : "d";
  } else if constexpr (std::is_signed<T>::value) {
    return sizeof(T) == 1   ? "b"
           : sizeof(T) == 2 ? "h"
           : sizeof(T) == 4 ? "i"
                            : "q";
  } else {
    return sizeof(T) == 1   ? "B"
           : sizeof(T) == 2 ? "H"
           : sizeof(T) == 4 ? "I"
                            : "Q";
  }
}

// The storage type for arithmetic arrays (enums are stored as their underlying
// type).
template<typename T, typename = void> struct StorageType { using type = T; };
template<typename T>
struct StorageType<T, std::enable_if_t<std::is_enum<T>::value>> {
  using type = std::underlying_type_t<T>;
};

template<typename ArrayT>
int GetBuffer(PyObject *exporter, Py_buffer *view, int flags) noexcept {
  using storage_type = typename StorageType<item_type_t<ArrayT>>::type;
  ArrayT *self = nb::inst_ptr<ArrayT>(exporter);
  auto *shape_and_strides = new Py_ssize_t[2]{
    static_cast<Py_ssize_t>(self->size()),
    static_cast<Py_ssize_t>(sizeof(storage_type)),
  };
  view->buf = DataPointer(*self);
  view->obj = exporter;
  Py_INCREF(exporter);
  view->len = shape_and_strides[0] * shape_and_strides[1];
  view->readonly = 0;
  view->itemsize = sizeof(storage_type);
  view->format = (flags & PyBUF_FORMAT)
                     ? const_cast<char *>(FormatOf<storage_type>())
                     : nullptr;
  view->ndim = 1;
  view->shape = (flags & PyBUF_ND) == PyBUF_ND ? shape_and_strides : nullptr;
  view->strides = (flags & PyBUF_STRIDES) == PyBUF_STRIDES
                      ? shape_and_strides + 1
                      : nullptr;
  view->suboffsets = nullptr;
  view->internal = shape_and_strides;
  return 0;
}

inline void ReleaseBuffer(PyObject *, Py_buffer *view) {
  delete[] static_cast<Py_ssize_t *>(view->internal);
}

template<typename ArrayT> PyType_Slot *BufferSlots() {
  static PyType_Slot slots[] = {
    { Py_bf_getbuffer, reinterpret_cast<void *>(&GetBuffer<ArrayT>) },
    { Py_bf_releasebuffer, reinterpret_cast<void *>(&ReleaseBuffer) },
    { 0, nullptr },
  };
  return slots;
}

template<typename ArrayT, typename PyClass>
inline void BindArithmeticOperations(PyClass &c) {
  using storage_type = typename StorageType<item_type_t<ArrayT>>::type;

  c.def("numpy", [](nb::handle self_h) {
    ArrayT &self = nb::cast<ArrayT &>(self_h);
    return nb::ndarray<nb::numpy, storage_type, nb::ndim<1>>(
        DataPointer(self), { static_cast<size_t>(self.size()) }, self_h);
  });

  // Declares the buffer protocol (PEP 688) for type checkers. The buffer is
  // exported via the type's buffer slots, which Python < 3.12 uses directly.
  c.def(
      "__buffer__",
      [](nb::handle self_h, int) {
        return nb::memoryview(self_h.attr("numpy")());
      },
      nb::arg("flags"),
      nb::sig("def __buffer__(self, flags: int, /) -> memoryview"));
}

template<typename ArrayT, typename... Extra>
inline nb::class_<ArrayT> MakeClass(nb::handle scope, const char *name,
                                    const char *base,
                                    const std::string &element_name,
                                    const Extra &...extra) {
  const std::string signature =
      SequenceClassSignature(name, base, element_name);
  nb::class_<ArrayT> c(scope, name, nb::sig(signature.c_str()), extra...);
  RegisterAbc(c, base);
  return c;
}

}  // namespace detail

// Returns the name of the type variable `name` defined in the module of the
// union `EnumT`, relative to `scope` (e.g. for use in signatures).
template<typename EnumT>
inline std::string TypeVarName(nb::handle scope, const char *name) {
  const std::string module =
      nb::cast<std::string>(nb::type<EnumT>().attr("__module__"));
  if (module == nb::cast<std::string>(scope.attr("__name__"))) { return name; }
  return module + "." + name;
}

// Binds a ::flatbuffers::Array or ::flatbuffers::Vector.
template<typename ArrayT>
inline void BindArrayReadonly(nb::handle scope, const char *name) {
  using Ops = detail::DefaultOps<ArrayT>;
  if (detail::ReuseBinding<ArrayT>(scope, name)) { return; }
  const std::string element_name =
      detail::ElementTypeName<typename Ops::return_type>(scope);
  auto c = detail::MakeClass<ArrayT>(scope, name, "Sequence", element_name);
  detail::BindReadOperations<ArrayT, Ops>(c, element_name);
}

// Binds a ::flatbuffers::Array or ::flatbuffers::Vector.
template<typename ArrayT>
inline void BindArrayReadwrite(nb::handle scope, const char *name) {
  using Ops = detail::DefaultOps<ArrayT>;
  if (detail::ReuseBinding<ArrayT>(scope, name)) { return; }
  const std::string element_name =
      detail::ElementTypeName<typename Ops::return_type>(scope);
  auto c = detail::MakeClass<ArrayT>(scope, name, "Sequence", element_name);
  detail::BindReadOperations<ArrayT, Ops>(c, element_name);
  detail::BindFbsWriteOperations<ArrayT>(c, element_name);
}

// Binds a ::flatbuffers::Array or ::flatbuffers::Vector whose data type is
// arithmetic (or an enum).
template<typename ArrayT>
inline void BindArrayArithmetic(nb::handle scope, const char *name) {
  using Ops = detail::DefaultOps<ArrayT>;
  if (detail::ReuseBinding<ArrayT>(scope, name)) { return; }
  const std::string element_name =
      detail::ElementTypeName<typename Ops::return_type>(scope);
  auto c =
      detail::MakeClass<ArrayT>(scope, name, "Sequence", element_name,
                                nb::type_slots(detail::BufferSlots<ArrayT>()));
  detail::BindReadOperations<ArrayT, Ops>(c, element_name);
  detail::BindFbsWriteOperations<ArrayT>(c, element_name);
  detail::BindArithmeticOperations<ArrayT>(c);
}

// Binds a std::vector (e.g. for object API vector fields).
template<typename ArrayT>
inline void BindStdVector(nb::handle scope, const char *name) {
  using Ops = detail::StdVectorOps<ArrayT>;
  if (detail::ReuseBinding<ArrayT>(scope, name)) { return; }
  const std::string element_name =
      detail::ElementTypeName<typename Ops::return_type>(scope);
  auto c =
      detail::MakeClass<ArrayT>(scope, name, "MutableSequence", element_name);
  detail::BindReadOperations<ArrayT, Ops>(c, element_name);
  detail::BindStdVectorWriteOperations<ArrayT, Ops>(c, element_name);
  detail::BindStdVectorAllocOperations<ArrayT, Ops>(c);
}

// Binds a std::vector (e.g. for object API vector fields) whose data type is
// arithmetic (or an enum).
template<typename ArrayT>
inline void BindStdVectorArithmetic(nb::handle scope, const char *name) {
  using Ops = detail::StdVectorOps<ArrayT>;
  if (detail::ReuseBinding<ArrayT>(scope, name)) { return; }
  constexpr bool kHasBuffer =
      !std::is_same<typename ArrayT::value_type, bool>::value;
  const std::string element_name =
      detail::ElementTypeName<typename Ops::return_type>(scope);
  auto c = [&] {
    // std::vector<bool> is bit-packed, so it cannot expose a buffer.
    if constexpr (kHasBuffer) {
      return detail::MakeClass<ArrayT>(
          scope, name, "MutableSequence", element_name,
          nb::type_slots(detail::BufferSlots<ArrayT>()));
    } else {
      return detail::MakeClass<ArrayT>(scope, name, "MutableSequence",
                                       element_name);
    }
  }();
  detail::BindReadOperations<ArrayT, Ops>(c, element_name);
  detail::BindStdVectorWriteOperations<ArrayT, Ops>(c, element_name);
  detail::BindStdVectorAllocOperations<ArrayT, Ops>(c);
  if constexpr (kHasBuffer) { detail::BindArithmeticOperations<ArrayT>(c); }
}

// Binds a std::vector of object API unions. `add(union_type)` constructs an
// element of the given union variant in place and returns a reference to it.
template<typename ArrayT, typename Traits>
inline void BindUnionStdVector(nb::handle scope, const char *name,
                               const std::string &type_var) {
  using Ops = detail::UnionStdVectorOps<Traits>;
  if (detail::ReuseBinding<ArrayT>(scope, name)) { return; }
  const std::string element_name =
      detail::VariantTypeNames<typename Traits::variant_type>::Get(scope);
  auto c =
      detail::MakeClass<ArrayT>(scope, name, "MutableSequence", element_name);
  detail::BindReadOperations<ArrayT, Ops>(c, element_name);
  detail::BindStdVectorWriteOperations<ArrayT, Ops>(c, element_name);

  const std::string add_signature =
      "def add(self, union_type: type[" + type_var + "], /) -> " + type_var;
  c.def(
      "add",
      [](ArrayT &self, nb::handle union_type) -> typename Ops::return_type {
        typename Traits::union_type value;
        Traits::Ensure(value, union_type);
        self.push_back(std::move(value));
        return Traits::Get(self.back());
      },
      nb::rv_policy::reference_internal, nb::sig(add_signature.c_str()));
  c.def(
      "reserve", [](ArrayT &self, size_t size) { self.reserve(size); },
      nb::sig("def reserve(self, size: int, /) -> None"));
}

// Binds the view of a packed union vector field.
template<typename Traits>
inline void BindUnionFbsVector(nb::handle scope, const char *name) {
  using ArrayT = detail::UnionFbsVectorView<Traits>;
  using Ops = detail::UnionFbsVectorOps<Traits>;
  if (detail::ReuseBinding<ArrayT>(scope, name)) { return; }
  const std::string element_name =
      detail::VariantTypeNames<typename Traits::variant_type>::Get(scope);
  auto c = detail::MakeClass<ArrayT>(scope, name, "Sequence", element_name);
  detail::BindReadOperations<ArrayT, Ops>(c, element_name);
}

}  // namespace nanobind
}  // namespace flatbuffers

#endif  // FLATBUFFERS_NANOBIND_BIND_ARRAY_H_
