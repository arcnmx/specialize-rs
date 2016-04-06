#![cfg_attr(test, feature(specialization))]

/*!
 * Experimental specialization macros.
 *
 * Syntax is highly volatile and subject to change. The use of these macros
 * requires an unstable rust compiler and the `#![feature(specialization)]`
 * crate attribute.
 *
 * ## constrain!()
 *
 * `constrain!()` attempts to add additional trait bounds to a generic type.
 *
 * ```
 * # #![feature(specialization)]
 * # #[macro_use]
 * # extern crate specialize;
 * # fn main() { }
 * use std::io::{Read, Seek, SeekFrom, Cursor, Repeat};
 *
 * fn generic_function<T: Read>(read: &mut T) {
 *     if let Some(read_seek) = constrain!(ref mut read as [IsReadSeek: Read + Seek] = Cursor<Repeat>) {
 *         read_seek.seek(SeekFrom::Start(0));
 *     }
 * }
 * ```
 * 
 * ### Caveats
 *
 * There are a few oddities in the above example...
 *
 * 1. You must specify a name for the trait to be used behind the scenes for
 *    specialization, as in `IsReadSeek`. It may be called whatever you like.
 * 2. A default fallback type that implements the desired trait bounds must also
 *    be provided. This type must be concrete but will never be used or
 *    instantiated, and is only used to appease the type checker.
 *
 * ## specialize! { }
 *
 * Provides a type-level `match` statement with specialization bounds.
 *
 * ```
 * # #![feature(specialization)]
 * # #[macro_use]
 * # extern crate specialize;
 * # fn main() { }
 * use std::io::{Read, BufRead, Write, copy, sink};
 *
 * fn generic_function<T: Read>(read: &mut T) {
 *     specialize! {
 *         trait fn Trait::nonsensical[W: Write](&mut self, w: &mut W) -> bool where [W: Write];
 *
 *         match impl['a, T: Read] for T where [T: Read] {
 *             where [T: BufRead] => self.fill_buf().and_then(|buf| w.write(buf)).is_ok(),
 *             impl[U: Into<u8> + Clone] where [T: BufRead + Iterator<Item=&'a U>, U: 'a] =>
 *                 w.write(&self.cloned().map(Into::into).collect::<Vec<u8>>()).is_ok(),
 *             _ => copy(self, w).is_ok(),
 *         }
 *     }
 *
 *     Trait::nonsensical(read, &mut sink());
 * }
 * ```
 *
 * ### Caveats
 *
 * 1. This is not an inline statement or expression, and is instead used to
 *    generate a trait and method pair. This means the prototype must be specified
 *    up-front, and no variables will be automatically captured from the outside
 *    scope.
 * 2. Generic bounds and where clauses must be surrounded by `[]` rather than
 *    `<>` or similar due to macro parsing limitations.
 * 3. The specialization "more specific" rules must be followed in order to
 *    prevent conflicting trait impls.
 * 4. The various `where [B: Bounds...]` clauses may be omitted, and are mostly
 *    included here for syntax demonstration.
 *
 */

#[macro_export]
#[doc(hidden)]
macro_rules! specialize {
    (
        trait fn $trait_id:ident :: $trait_fn_id:ident
        $($unparsed:tt)*
    ) => {
        specialize! {
            @parse_fn_bounds
            $trait_id $trait_fn_id
            $($unparsed)*
        }
    };
    (@parse_fn_bounds $trait_id:ident $trait_fn_id:ident (
            $($trait_fn_args:tt)*
        )
        $($unparsed:tt)*
    ) => {
        specialize! {
            @parse_fn_ty
            $trait_id $trait_fn_id () ($($trait_fn_args)*)
            $($unparsed)*
        }
    };
    (@parse_fn_bounds $trait_id:ident $trait_fn_id:ident [
            $($trait_fn_bounds:tt)+
        ] (
            $($trait_fn_args:tt)*
        )

        $($unparsed:tt)*
    ) => {
        specialize! {
            @parse_fn_ty
            $trait_id $trait_fn_id ($($trait_fn_bounds)+) ($($trait_fn_args)*)
            $($unparsed)*
        }
    };
    (@parse_fn_ty $trait_id:ident $trait_fn_id:ident $trait_fn_bounds:tt $trait_fn_args:tt
        ;
        $($unparsed:tt)*
    ) => {
        specialize! {
            @parse_impl
            $trait_id
            ($trait_fn_id (()) $trait_fn_bounds () $trait_fn_args)
            $($unparsed)*
        }
    };
    (@parse_fn_ty $trait_id:ident $trait_fn_id:ident $trait_fn_bounds:tt $trait_fn_args:tt
        -> $trait_fn_ty:ty;
        $($unparsed:tt)*
    ) => {
        specialize! {
            @parse_impl
            $trait_id
            ($trait_fn_id ($trait_fn_ty) $trait_fn_bounds () $trait_fn_args)
            $($unparsed)*
        }
    };
    (@parse_fn_ty $trait_id:ident $trait_fn_id:ident $trait_fn_bounds:tt $trait_fn_args:tt
        -> $trait_fn_ty:ty where [$($trait_fn_where:tt)+];
        $($unparsed:tt)*
    ) => {
        specialize! {
            @parse_impl
            $trait_id
            ($trait_fn_id ($trait_fn_ty) $trait_fn_bounds (where $($trait_fn_where)+) $trait_fn_args)
            $($unparsed)*
        }
    };
    (@parse_fn_ty $trait_id:ident $trait_fn_id:ident $trait_fn_bounds:tt $trait_fn_args:tt
        where [$($trait_fn_where:tt)+];
        $($unparsed:tt)*
    ) => {
        specialize! {
            @parse_impl
            $trait_id
            ($trait_fn_id (()) $trait_fn_bounds (where $($trait_fn_where)+) $trait_fn_args)
            $($unparsed)*
        }
    };

    (@parse_impl $trait_id:ident $trait_fn:tt
        match impl[
            $($trait_impl_bounds:tt)*
        ] for $trait_impl_id:ident where [
            $($trait_impl_where:tt)+
        ] {
            $($unparsed:tt)*
        }
    ) => {
        specialize! {
            @parse
            ($trait_id $trait_impl_id ($($trait_impl_bounds)*) (, $($trait_impl_where)+))
            $trait_fn
            ()
            ($($unparsed)*)
        }
    };
    (@parse_impl $trait_id:ident $trait_fn:tt
        match impl[
            $($trait_impl_bounds:tt)*
        ] for $trait_impl_id:ident {
            $($unparsed:tt)*
        }
    ) => {
        specialize! {
            @parse
            ($trait_id $trait_impl_id ($($trait_impl_bounds)*) ())
            $trait_fn
            ()
            ($($unparsed)*)
        }
    };
    // Parse match arms
    // TODO: support T: X => {} match syntax without trailing comma?
    (@parse
        $trait_impl:tt
        $trait_fn:tt
        ($($clauses:tt)*)
        (
            where [
                $($clause_where:tt)*
            ] => $clause_expr:expr,
            $($unparsed:tt)*
        )
    ) => {
        specialize! { @parse
            $trait_impl
            $trait_fn
            ($($clauses)*
                (() ($($clause_where)*) $clause_expr)
            )
            ($($unparsed)*)
        }
    };
    (@parse
        $trait_impl:tt
        $trait_fn:tt
        ($($clauses:tt)*)
        (
            impl [
                $($clause_bounds:tt)*
            ] where [
                $($clause_where:tt)*
            ] => $clause_expr:expr,
            $($unparsed:tt)*
        )
    ) => {
        specialize! { @parse
            $trait_impl
            $trait_fn
            ($($clauses)*
                (($($clause_bounds)*) ($($clause_where)*) $clause_expr)
            )
            ($($unparsed)*)
        }
    };
    // Match catchall
    (@parse
        $trait_impl:tt
        $trait_fn:tt
        ($($clauses:tt)*)
        (
            _ => $clause_expr:expr $(,)*
        )
    ) => {
        specialize! { @parse
            $trait_impl
            $trait_fn
            ($($clauses)*
                (() (u8: Copy) $clause_expr)
            )
            ()
        }
    };
    // Base case
    (@parse
        $trait_impl:tt
        $trait_fn:tt
        $clauses:tt
        (/*unparsed*/)
    ) => {
        specialize! { @itemize
            $trait_impl
            $trait_fn
            ()
            $clauses
        }
    };
    // Clause to trait impl
    (@itemize
        ($trait_id:ident $trait_impl_id:ident ($($trait_impl_bounds:tt)*) ($($trait_impl_where:tt)*))
        ($trait_fn_id:ident ($trait_fn_ty:ty) ($($trait_fn_bounds:tt)*) ($($trait_fn_where:tt)*) ($($trait_fn_args:tt)*))
        ($($items:tt)*)
        ((($($clause_bounds:tt)*) ($($clause_where:tt)*) $clause_expr:expr) $($clauses:tt)*)
    ) => {
        specialize! { @itemize
            ($trait_id $trait_impl_id ($($trait_impl_bounds)*) ($($trait_impl_where)*))
            ($trait_fn_id ($trait_fn_ty) ($($trait_fn_bounds)*) ($($trait_fn_where)*) ($($trait_fn_args)*))
            ($($items)*
                impl<$($trait_impl_bounds)*, $($clause_bounds)*> $trait_id for $trait_impl_id where $($clause_where)* $($trait_impl_where)* {
                    default fn $trait_fn_id<
                        $($trait_fn_bounds)*
                    >($($trait_fn_args)*) -> $trait_fn_ty
                    $($trait_fn_where)* {
                        $clause_expr
                    }
                }
            )
            ($($clauses)*)
        }
    };
    // Base case
    (@itemize
        $trait_impl:tt
        $trait_fn:tt
        ($($items:tt)*)
        (/*clauses*/)
    ) => {
        specialize! { @trait
            $trait_impl
            $trait_fn
        }

        specialize! { @items $($items)* }
    };
    (@trait
        ($trait_id:ident $trait_impl_id:ident ($($trait_impl_bounds:tt)*) ($($trait_impl_where:tt)*))
        ($trait_fn_id:ident ($trait_fn_ty:ty) ($($trait_fn_bounds:tt)*) ($($trait_fn_where:tt)*) ($($trait_fn_args:tt)*))
    ) => {
        specialize! { @items
            trait $trait_id {
                fn $trait_fn_id<
                    $($trait_fn_bounds)*
                >($($trait_fn_args)*) -> $trait_fn_ty
                $($trait_fn_where)*;
            }
        }
    };
    /*(@
        ($trait_id:ident $trait_impl_id:ident ($($trait_impl_bounds:tt)*) ($($trait_impl_where:tt)*))
        ($trait_fn_id:ident ($trait_fn_ty:ty) ($($trait_fn_bounds:tt)*) ($($trait_fn_where:tt)*) ($($trait_fn_args:tt)*))
        ($($items:tt)*)
        ($($clauses:tt)*)
        ($($unparsed:tt)*)
    ) => {
        specialize! { @
            ($trait_id $trait_impl_id ($($trait_impl_bounds)*) ($($trait_impl_where)*))
            ($trait_fn_id ($trait_fn_ty) ($($trait_fn_bounds)*) ($($trait_fn_where)*) ($($trait_fn_args)*))
            ($($items:tt)*)
            ($($clauses:tt)*)
            ($($unparsed:tt)*)
        }
        ...
    };*/
    /*(@
        $trait_impl:tt
        $trait_fn:tt
        $items:tt
        $clauses:tt
        $unparsed:tt
    ) => {
        specialize! { @
            $trait_impl
            $trait_fn
            $items
            $clauses
            $unparsed
        }
    };*/
    // Convert tts to items
    (@items $($i:item)*) => { $($i)* };
    // Drops tokens into the void
    (@drop $($tt:tt)*) => { };
}

#[macro_export]
#[doc(hidden)]
macro_rules! constrain {
    (ref mut $value_id:ident as [$trait_id:ident : $($bounds:tt)*] = $default_ty:ty) => {
        {
            constrain! { @trait $trait_id ($($bounds)*) ($default_ty) }

            $trait_id::as_mut($value_id)
        }
    };
    (ref $value_id:ident as [$trait_id:ident : $($bounds:tt)*] = $default_ty:ty) => {
        {
            constrain! { @trait $trait_id ($($bounds)*) ($default_ty) }

            $trait_id::as_ref($value_id)
        }
    };
    (@trait $trait_id:ident ($($bounds:tt)*) ($default_ty:ty)) => {
        constrain! { @items
            trait $trait_id {
                type Out: ?Sized + $($bounds)*;

                fn as_ref(&self) -> Option<&Self::Out>;
                fn as_mut(&mut self) -> Option<&mut Self::Out>;
            }

            impl<T: ?Sized + $($bounds)*> $trait_id for T {
                type Out = T;

                fn as_ref(&self) -> Option<&Self::Out> { Some(self) }
                fn as_mut(&mut self) -> Option<&mut Self::Out> { Some(self) }
            }
        }

        impl<T: ?Sized> $trait_id for T {
            default type Out = $default_ty;

            default fn as_ref(&self) -> Option<&Self::Out> { None }
            default fn as_mut(&mut self) -> Option<&mut Self::Out> { None }
        }
    };
    (@items $($i:item)*) => { $($i)* };
}

#[cfg(test)]
mod test {
    #[test]
    fn by_type() {
        fn is_default<T: ?Sized>() -> bool {
            specialize! {
                trait fn IsDefault::is_default() -> bool;

                match impl[T: ?Sized] for T {
                    // Silly extraneous bounds for testing
                    where [T: Default + Default, T: Default] => true,
                    where [T: Default + Copy] => true,
                    where [T: Default + Copy + ::std::ops::Deref, T::Target: Copy] => true,
                    _ => false,
                }
            }

            T::is_default()
        }

        assert_eq!(true, is_default::<()>());
        assert_eq!(false, is_default::<[()]>());
        assert_eq!(false, is_default::<&'static ()>());
    }

    #[test]
    fn by_value() {
        fn is_default<T: ?Sized>(t: &T) -> bool {
            specialize! {
                trait fn IsDefault::is_default(&self, true_: bool) -> bool;

                match impl[T: ?Sized] for T {
                    where [T: Default] => true && true_,
                    where [T: Default + PartialEq] => *self == <Self as Default>::default() && true_,
                    _ => false && true_,
                }
            }

            t.is_default(true)
        }

        assert_eq!(true, is_default(&()));
        assert_eq!(true, is_default(&0u8));
        assert_eq!(false, is_default(&1u8));
        assert_eq!(false, is_default(&[()][..]));
    }

    #[test]
    fn fn_bounds() {
        use std::io::{self, Write, sink};
        use std::fmt::Display;

        fn write_test<T: ?Sized>(v: &T) -> io::Result<()> {
            specialize! {
                trait fn WriteTest::write_test[W: Write](&self, mut w: W) -> io::Result<()>;

                match impl[T: ?Sized] for T {
                    where [T: Display] => writeln!(w, "{}", self),
                    _ => Err(io::Error::new(io::ErrorKind::Other, "unimplemented")),
                }
            }

            v.write_test(sink())
        }

        assert!(write_test(&0u8).is_ok());
        assert!(write_test(&()).is_err());
    }

    #[test]
    fn constrain_ref() {
        use std::fmt::Display;

        fn is_display<T: ?Sized>(t: &T) -> bool {
            if let Some(display) = constrain!(ref t as [IsDisplay: Display] = u8) {
                println!("{}", display);
                true
            } else {
                false
            }
        }

        assert_eq!(is_display(&()), false);
        assert_eq!(is_display(&0u32), true);
    }

    #[test]
    fn constrain_mut() {
        use std::io::{Write, Stdout, sink};

        fn try_write<T: ?Sized>(t: &mut T) -> bool {
            if let Some(write) = constrain!(ref mut t as [IsWrite: Write] = Stdout) {
                writeln!(write, "hello!").is_ok()
            } else {
                false
            }
        }

        assert_eq!(try_write(&mut ()), false);
        assert_eq!(try_write(&mut sink()), true);
    }
}
