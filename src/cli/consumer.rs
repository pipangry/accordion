/// `ValueConsumer` is a thing that used for flags that need to consume
/// other args, for example, we may need to consume one or two
/// arguments after `--flag` as its value
pub trait ValueConsumer<'a>: Sized {
    /// Returned usize means how many arguments after flag parser
    /// should consume. If 0, parser assign `ConsumedValue::SelfPoint`
    fn consume(&self) -> usize;

    /// This function should return array of consumed arguments with
    /// their names for `help` command, for example `--flag <name> <name>`
    fn help(&self) -> &'a [&'a str];
}

/// Value consumer returns. SelfPoint means that flag
/// is just true/false, and Arg means consumed arg
#[derive(Debug, PartialEq, Clone)]
pub enum ConsumedValue {
    Arg(String),
    SelfPoint,
}

impl ConsumedValue {
    /// Returns `true` if this `ConsumedValue` is `SelfPoint`
    pub fn is_self_point(&self) -> bool {
        if let ConsumedValue::SelfPoint = *self {
            return true;
        }
        false
    }

    /// Returns value of `Arg`. Panics
    /// if called on `SelfPoint`
    pub fn unwrap_arg(&self) -> &str {
        if let ConsumedValue::Arg(s) = self {
            return s;
        }
        panic!("ConsumedValue::unwrap_arg called on SelfPoint");
    }
}

/// Default `ValueConsumer` for flags.
/// To create `SelfPoint` flag, leave array empty
#[derive(Debug, PartialEq)]
pub struct GenericConsumer<'a>(pub &'a [&'a str]);

impl<'a> ValueConsumer<'a> for GenericConsumer<'a> {
    fn consume(&self) -> usize {
        self.0.len()
    }

    fn help(&self) -> &'a [&'a str] {
        self.0
    }
}

impl<'a, T: ValueConsumer<'a>> ValueConsumer<'a> for &T {
    fn consume(&self) -> usize {
        (*self).consume()
    }

    fn help(&self) -> &'a [&'a str] {
        (*self).help()
    }
}
