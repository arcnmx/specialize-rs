# specialize-rs

[![travis-badge][]][travis] [![release-badge][]][cargo] [![docs-badge][]][docs] [![license-badge][]][license]

`specialize` is an experimental Rust library for working with type specialization.

## [Documentation][docs]

See the [documentation][docs] for all provided macros and types, and more
up-to-date syntax and information.

## Constrain

The `constrain!()` macro can be used to add additional type bounds to an existing
generic parameter. For example, imagine you want to print out the debug representation
of a type without adding it to your generic bounds:

```rust
#[macro_use]
extern crate specialize;

fn some_func<T: SomeTrait>(t: &T) {
    if let Some(debug) = constrain!(ref [t: Debug] = ()) {
        println!("some_func({:?})", debug);
    }
    t.something();
}
```

It can also be used as a replacement for `Any`, allowing a generic type to become
its matching concrete type again.

## Specialize

The `specialize! { }` macro allows for more advanced matching of types, but is
more cumbersome due to its syntax and implementation of an external function.
See the documentation for more details.

[travis-badge]: https://img.shields.io/travis/arcnmx/specialize-rs/master.svg?style=flat-square
[travis]: https://travis-ci.org/arcnmx/specialize-rs
[release-badge]: https://img.shields.io/crates/v/specialize.svg?style=flat-square
[cargo]: https://crates.io/crates/specialize
[docs-badge]: https://img.shields.io/badge/API-docs-blue.svg?style=flat-square
[docs]: http://arcnmx.github.io/specialize-rs/specialize/
[license-badge]: https://img.shields.io/badge/license-MIT-orange.svg?style=flat-square
[license]: https://github.com/arcnmx/specialize-rs/blob/master/COPYING
