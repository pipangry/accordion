use crate::cli::consumer::{ConsumedValue, GenericConsumer, ValueConsumer};
use crate::error::Error;
use crate::utils::fast_vec_fill_with;
use crate::utils::ncmi_map::ncmi_map;
use std::fmt::Write;
use std::marker::PhantomData;
use std::str::from_utf8;

#[derive(PartialEq, Debug, Clone, Default)]
/// Represents consumed value of argument
pub enum Value {
    Some(Box<[ConsumedValue]>),
    #[default]
    None,
}

impl Value {
    /// Returns true if value is `None`
    pub fn is_none(&self) -> bool {
        if Value::None == *self {
            return true;
        }
        false
    }

    /// Returns .len() of `Some`
    pub fn len(&self) -> Option<usize> {
        match self {
            Value::Some(v) => Some(v.len()),
            Value::None => None,
        }
    }

    /// Checks if length of `Some` is 0.
    /// Returns `false` if value is `None`
    pub fn is_empty(&self) -> bool {
        if let Self::Some(v) = self {
            v.is_empty()
        } else {
            false
        }
    }

    /// Returns first value of `Some`
    pub fn first(&self) -> Option<&ConsumedValue> {
        if let Self::Some(v) = self {
            return v.first();
        }
        None
    }

    /// Convert to `Option`
    pub fn as_option(&self) -> Option<&[ConsumedValue]> {
        match self {
            Value::Some(v) => Some(v),
            Value::None => None,
        }
    }

    /// Convert `Some` to vector
    pub fn to_vec(&self) -> Option<Vec<ConsumedValue>> {
        if let Value::Some(array) = self {
            return Some(array.to_vec());
        }
        None
    }

    /// Panics if `Value` is `None`
    pub fn unwrap(&self) -> &[ConsumedValue] {
        if let Value::Some(array) = self {
            return array;
        }
        panic!("Value is not `Some`");
    }
}

#[derive(Default, Debug, PartialEq)]
/// Type of command argument
///
/// **Flag** – "--flag" argument with support of short variant
///
/// **OptionalStatic** – Optional argument depended on position, use it
/// only as last static argument to prevent breaking static positioning
///
/// **Static** – Depended on position argument, that is, it consumes value
/// in the same place as it located in args array
///
/// If you're using short variant of `Flag`, be sure that it doesn't
/// have conflicts with other arguments
pub enum ArgType<'a, C: ValueConsumer<'a> + Sized> {
    Flag {
        short: Option<&'a str>,
        values_consumer: C,
    },
    OptionalStatic,
    #[default]
    Static,
}

#[derive(Debug, Default, PartialEq)]
/// Command argument. Name is necessary field for `help` command
pub struct Arg<'a, C = GenericConsumer<'a>>
where
    C: ValueConsumer<'a> + Sized,
{
    pub name: &'a str,
    pub arg_type: ArgType<'a, C>,
}

impl<'a, C> Arg<'a, C>
where
    C: ValueConsumer<'a> + Sized,
{
    /// Create a new argument with `Static` type
    pub const fn default(name: &'a str) -> Self {
        Self {
            name,
            arg_type: ArgType::Static,
        }
    }

    /// Create a new argument with `OptionalStatic` type
    pub const fn optional(name: &'a str) -> Self {
        Self {
            name,
            arg_type: ArgType::OptionalStatic,
        }
    }

    /// Create a new argument with `Flag` type
    /// `ValuesConsumer` is a thing that used for flags that need to consume
    /// other args, by default, it set to `GenericConsumer`.
    ///
    /// `GenericConsumer` needs an array of flag's arguments represented as strings
    /// to provide additional information for `help` command
    ///
    /// # Example
    /// ```rust
    /// use accordion::cli::command::Arg;
    /// use accordion::cli::consumer::GenericConsumer;
    ///
    /// let my_flag_arg = Arg::flag(
    ///     "my_flag",
    ///     Some("mf"), // Short option, here it would be -mf
    ///     GenericConsumer(&["arg1"])
    /// ); // --my_flag
    /// ```
    /// # Example with Self-point
    /// If your flag don't need to consume other arguments, it can be
    /// true/false switch represented as `ConsumedValue::SelfPoint` (that is, true)
    /// `None` will represent false
    /// ```rust
    /// use accordion::cli::command::Arg;
    /// use accordion::cli::consumer::GenericConsumer;
    ///
    /// let my_self_point_flag = Arg::flag(
    ///     "my_self_point_flag",
    ///     None, // No short variant
    ///     GenericConsumer(&[]),
    ///     // By defining empty array, GenericConsumer will
    ///     // tell to parser: I don't need to consume anything, so
    ///     // assign me as SelfPoint
    /// );
    /// // --my_self_point_flag
    /// ```
    pub const fn flag(name: &'a str, short: Option<&'a str>, values_consumer: C) -> Self {
        Self {
            name,
            arg_type: ArgType::Flag {
                short,
                values_consumer,
            },
        }
    }
}

#[macro_export]
/// Temporary command for tests
/// 
/// Panics on callback
macro_rules! temp_command {
    () => {{
        Command::new("temp", |_: Vec<Value>| panic!("Temporary command was called"))
            .with_desc("The temp command used for tests. Panics when called")
    }};
}

#[derive(PartialEq, Debug, Default)]
/// The command struct. Contains information about:
///
/// **name** – necessary field used for `help` generation and commands
/// resolving in CLI.
///
/// **desc** – description of the command used for `help` command
///
/// **args** – list of command's arguments
///
/// **callback** – function that CLI will call after parsing
pub struct Command<'a, T, C = fn(T)>
where
    C: Fn(T),
{
    pub(crate) name: &'a str,
    pub(crate) desc: &'a str,
    pub(crate) args: &'a [Arg<'a>],
    pub(crate) callback: C,
    _phantom: PhantomData<T>,
}

/// Parsed flag: Vector of consumed values and usize
type ParsedFlag<'a> = (Value, usize);

impl<'a, C, T> Command<'a, T, C>
where
    C: Fn(T),
{
    /// Create a new command with given name and callback
    /// # Example
    /// ```rust
    /// use accordion::cli::command::{Command, Value};
    ///
    /// let command = Command::new("run", run);
    ///
    /// fn run(_: Vec<Value>) { /* */ }
    /// ```
    pub const fn new(name: &'a str, callback: C) -> Self {
        Self {
            name,
            desc: "",
            args: &[],
            callback,
            _phantom: PhantomData,
        }
    }

    /// Create a new command with all data needed at once
    pub const fn new_from(name: &'a str, desc: &'a str, args: &'a [Arg], callback: C) -> Self {
        Self {
            name,
            desc,
            args,
            callback,
            _phantom: PhantomData,
        }
    }

    /// Assign given array of args to this command
    /// ## Attention
    /// **Args collision can bring undefined behavior**, be sure that
    /// your args are valid and not malformed and doesn't have
    /// similar names or short flag variations
    pub const fn with_args(mut self, args: &'a [Arg<'a>]) -> Self {
        self.args = args;
        self
    }

    /// Assign given string as description of this command
    pub const fn with_desc(mut self, desc: &'a str) -> Self {
        self.desc = desc;
        self
    }

    /// Reassign callback with the new one
    pub fn reassign_callback(mut self, callback: C) -> Self {
        self.callback = callback;
        self
    }

    /// Call **callback** of this function with given arguments
    pub fn call(&self, value: T) {
        (self.callback)(value);
    }

    pub fn help(&self, color_scheme: Option<(&str, &str)>) -> String {
        // p - Primary color
        // r - Reset color
        let p = color_scheme.map_or("", |(p, _)| p);
        let r = color_scheme.map_or("", |(_, r)| r);
        
        let usage_parts: Vec<String> = self
            .args
            .iter()
            .filter_map(|a| match a.arg_type {
                ArgType::Flag { .. } => None,
                ArgType::Static => Some(format!("<{}>", a.name)),
                ArgType::OptionalStatic => Some(format!("[{}[", a.name)),
            })
            .collect();

        let usage = format!("{} {}", self.name, usage_parts.join(" "));

        let options = self.generate_options_column();
        format!(
            "{} – {}\n\n{p}USAGE:{r}\n{}\n\n{p}OPTIONS:{r}\n{}",
            self.name, self.desc, usage, options,
            p = p, r = r
        )
    }

    /// Helper function for `Command::help()`
    /// Generates aligned column of options (flags)
    fn generate_options_column(&self) -> String {
        let flags: Vec<_> = self
            .args
            .iter()
            .filter_map(|arg| {
                if let ArgType::Flag {
                    short,
                    values_consumer,
                    ..
                } = &arg.arg_type
                {
                    // Call .help() to get consumed arguments and assemble them to string
                    let help_text = values_consumer
                        .help()
                        .iter()
                        .map(|a| format!("<{}>", a))
                        .collect::<Vec<String>>()
                        .join(" ");
                    Some((*short, arg.name, help_text))
                } else {
                    None
                }
            })
            .collect();

        if flags.is_empty() {
            return String::new();
        }

        let max_short = flags
            .iter()
            .map(|(short, _, _)| short.map(|s| s.len() + 1).unwrap_or(0)) // +1 for "-"
            .max()
            .unwrap_or(0);

        let max_long = flags
            .iter()
            .map(|(_, long, _)| long.len() + 2) // +2 for "--"
            .max()
            .unwrap_or(0);

        let max_help = flags
            .iter()
            .map(|(_, _, help)| help.len())
            .max()
            .unwrap_or(0);

        // Assembling all strings to one string
        let mut buffer = String::new();
        for (short, long, help) in flags {
            let short_display = short.map_or(String::new(), |s| format!("-{}", s));
            let long_display = format!("--{}", long);
            writeln!(
                buffer,
                "  {:<short_width$}  {:<long_width$}  {:<help_width$}",
                short_display,
                long_display,
                help,
                short_width = max_short,
                long_width = max_long,
                help_width = max_help
            )
            .expect("Failed to write to buffer");
        }

        buffer
    }

    /// Parse given array of arguments
    ///
    /// Method will return `Result` where Ok a vector of
    /// `Value` where each `Value` has the same index as the
    /// argument it belongs to
    /// # Example
    /// ```rust
    /// use accordion::cli::command::{Arg, Command, Value};
    /// use accordion::cli::consumer::GenericConsumer;
    ///
    /// let args = &[
    ///     Arg::default("type"),
    ///     Arg::flag("merge", Some("m"), GenericConsumer(&["from", "to"])),
    ///     Arg::optional("optional_type"),
    /// ];
    /// // Assign arguments to our command
    /// let mut my_command = Command::new("git", git).with_args(args);
    /// my_command.parse(vec![
    ///     "--merge".to_owned(),
    ///     "my_branch".to_owned(),
    ///     "master".to_owned(),
    ///     "some_type".to_owned()
    /// ]).unwrap();
    /// // Ok:
    /// // [
    /// //     Some(Box<[Arg("some_type")]>),
    /// //     Some(Box<[Arg("my_branch"), Arg("master")]>),
    /// //     None,
    /// // ]
    /// fn git(_: Vec<Value>) { /**/ }
    /// ```
    /// To work with `Value` enum more efficiently check
    // I used too much .clone() / .to_owned() in this function because I can't
    // pass &'a [&'a str] as args due the errors with lifetime on user side
    pub fn parse(&'a self, args: Vec<String>) -> Result<Vec<Value>, Error<'a>> {
        // We assign None to all results right now so we
        // can just rewrite it later and perform checks
        // with known size
        let len = self.args.len();
        let mut parsed = Vec::with_capacity(len);

        // Initializing parsed with None
        unsafe {
            fast_vec_fill_with(&mut parsed, || Value::None, len);
        }

        let ncmi_map = ncmi_map(self.args, len, |s| {
            !matches!(s.arg_type, ArgType::Flag { .. })
        });

        // Index for statically positioned arguments
        let mut static_positioned_index = 0;
        let mut global_index = 0;
        // I choose using indexing instead of iter since ValueConsumer
        // implies jump indices
        while global_index < args.len() {
            // We can safely use index because self.parse_flag have checks
            let arg = args[global_index].clone();

            // Trying to match flag
            match arg.as_bytes() {
                // Long case
                [b'-', b'-', root @ ..] => {
                    self.handle_flag(
                        &mut parsed,
                        args[global_index..].to_owned(),
                        &mut global_index,
                        root,
                        |arg, root| {
                            if let ArgType::Flag {
                                values_consumer, ..
                            } = &arg.arg_type
                            {
                                if arg.name == root {
                                    return Some(values_consumer.consume());
                                }
                            }
                            None
                        }
                    )?;
                }
                // Short case
                [b'-', root @ ..] => {
                    self.handle_flag(
                        &mut parsed,
                        args[global_index..].to_owned(),
                        &mut global_index,
                        root,
                        |arg, root| {
                            if let ArgType::Flag {
                                short: Some(short),
                                values_consumer,
                            } = &arg.arg_type
                            {
                                if *short == root {
                                    return Some(values_consumer.consume());
                                }
                            }
                            None
                        }
                    )?;
                }
                // Statically positioned arg
                _ => {
                    // Using NCMI map to match the closest static arg
                    if let Some(arg_index) = ncmi_map[static_positioned_index] {
                        // To be honest, we shift all the hard work to the NCMI algorithm
                        parsed[arg_index] = Value::Some(Box::from([ConsumedValue::Arg(arg)]));
                        static_positioned_index += 1;
                    } else {
                        return Err(Error::UnknownArgument(arg));
                    }
                }
            }
            global_index += 1;
        }

        self.check_missing_static_arguments(&parsed)?;

        Ok(parsed)
    }

    /// Perform check for arguments with type `Static` that have `None` value
    /// in parsed vector. Returns `Error::MissingArguments` on error
    fn check_missing_static_arguments(&self, parsed: &[Value]) -> Result<(), Error> {
        let missing_arguments = parsed
            .iter()
            .enumerate()
            .filter_map(|(idx, value)| {
                if matches!(value, Value::None) {
                    let arg = &self.args[idx];
                    if arg.arg_type == ArgType::Static {
                        return Some(arg.name);
                    }
                }
                None
            })
            .collect::<Vec<_>>();

        if missing_arguments.is_empty() {
            Ok(())
        } else {
            Err(Error::MissingArguments(missing_arguments))
        }
    }

    #[inline]
    /// Helper function for `Command::parse()`
    fn handle_flag<F>(
        &self,
        parsed: &mut [Value],
        args: Vec<String>,
        global_index: &mut usize,
        arg: &[u8],
        predicate: F,
    ) -> Result<(), Error>
    where
        F: Fn(&Arg, &str) -> Option<usize>,
    {
        let parsed_root = convert_flag_byte_root(arg.to_vec())?;
        let result = self.parse_flag(parsed_root.to_owned(), args, predicate)?;
        
        apply_parsed_flag(parsed, &mut *global_index, result);
        Ok(())
    }
    
    /// Parse given flag
    /// **root** - root of the flag, that is, its name
    /// **predicate** - args filter function for short/long flag parsing
    fn parse_flag<F>(
        &self,
        root: String,
        args: Vec<String>,
        predicate: F,
    ) -> Result<ParsedFlag, Error>
    where
        F: Fn(&Arg, &str) -> Option<usize>,
    {
        // Trying to find flag with given predicate filter
        let (idx, required_args) = self
            .args
            .iter()
            .enumerate()
            .find_map(|(idx, arg)| predicate(arg, &root).map(|req_args| (idx, req_args)))
            .ok_or(Error::UnknownFlag(root.clone()))?;

        // If ValueConsumer returned 0, we assume that this flag is SelfPoint
        if required_args == 0 {
            return Ok((Value::Some(Box::new([ConsumedValue::SelfPoint])), idx));
        }

        let mut consumed_args = Vec::with_capacity(required_args);
        // Consuming arguments
        for i in 1..=required_args {
            if i >= args.len() {
                return Err(Error::FlagArgumentsOutOfBounds(root));
            }
            let next_arg = &args[i];
            consumed_args.push(ConsumedValue::Arg(next_arg.to_string()));
        }

        Ok((Value::Some(consumed_args.into_boxed_slice()), idx))
    }
}

// Helper functions for flag parsing to escape boilerplate

/// Converts bytes from flag root to str
/// Helper function for Command::parse() function
fn convert_flag_byte_root(byte_root: Vec<u8>) -> Result<String, Error<'static>> {
    let root_as_string = from_utf8(&byte_root).map_err(|_| Error::InvalidUTF8)?.to_owned();

    if byte_root.is_empty() {
        return Err(Error::EmptyFlag);
    }

    Ok(root_as_string)
}

/// Applies jump indices for global index and assign value to parsed array of args
/// Helper function for Command::parse() function
fn apply_parsed_flag(
    parsed: &mut [Value],
    global_index: &mut usize,
    result: ParsedFlag,
) {
    // We can safely use .unwrap here because parse_args
    // can't return Value::None
    let len = result.0.len().unwrap();
    // Increasing global_index to make jump throw consumed args
    *global_index += len;

    // Assign value from result to parsed arg
    parsed[result.1] = result.0;
}