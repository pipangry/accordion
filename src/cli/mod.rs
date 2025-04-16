use crate::cli::command::{Command, Value};
use crate::error::{CommandStreamError, Error, ErrorSource};
use crate::utils::split_args;

pub mod command;
pub mod consumer;

/// Prettifier used to format `Error` enums and add colors to
/// `CommandStream` output
pub trait Prettifier {
    /// Should return string that will be used as primary color
    fn primary_color(&self) -> &str;

    /// Should return string that will be used to reset terminal color
    fn reset_color(&self) -> &str;

    /// Prettify `accordion::error::Error`
    /// `ErrorSource` represents name of the command which caused this error, it
    /// can be helpful for additional information
    fn prettify_error(&self, error: CommandStreamError) -> String;
}

/// Predefined color schemes
///
/// **Default** – primary Blue, error Red
///
/// **Warn** – primary Yellow, error Red
///
/// **Neon** – primary Green, error Light Red
///
/// ## Warning
/// This color schemes intended for terminals with ANSI colors
/// support. Use `DefaultPrettifier::new_with_custom_scheme` for
/// your custom color schemes or create your own `Prettifier`
pub enum ColorScheme {
    Default,
    Warm,
    Neon,
}
#[derive(Debug)]
/// `DefaultPrettifier` – default implementation of `Prettifier` with
/// all things needed out of the box
pub struct DefaultPrettifier<'a> {
    color_scheme: (&'a str, &'a str, &'a str),
}

impl Default for DefaultPrettifier<'_> {
    fn default() -> Self {
        Self::new(ColorScheme::Default)
    }
}

impl<'a> DefaultPrettifier<'a> {
    /// Create a new prettifier with predefined color palette
    /// 
    /// `ColorScheme` defines which color palette you want to use
    /// from predefined ones 
    pub fn new(color_scheme: ColorScheme) -> Self {
        let scheme = match color_scheme {
            ColorScheme::Default => ("\x1b[34m", "\x1b[31m", "\x1b[0m"),
            ColorScheme::Warm => ("\x1b[33m", "\x1b[38;5;208m", "\x1b[0m"),
            ColorScheme::Neon => ("\x1b[96m", "\x1b[91m", "\x1b[0m"),
        };

        Self {
            color_scheme: scheme,
        }
    }

    /// Create a new prettifier with custom color palette
    /// 
    /// **Index 0** – used as primary color
    /// 
    /// **Index 1** – used as error color
    /// 
    /// **Index 2** – used as reset color
    pub fn new_with_custom_scheme(color_scheme: (&'a str, &'a str, &'a str)) -> Self {
        Self { color_scheme }
    }

    /// Helper function for `prettify_error()` method
    fn generate_error_body(&self, error: Error, source: ErrorSource) -> String {
        let source_fmt = format!("{} ...", source);
        let source_example = (
            source_fmt.as_ref(),
            source_fmt.len()
        );

        let primary_colors = (self.color_scheme.0, self.color_scheme.2);
        let error_colors = (self.color_scheme.1, self.color_scheme.2);

        match error {
            Error::MissingArguments(args) => {
                let formatted_args = args.iter()
                    .map(|arg| format!("<{}>", arg))
                    .collect::<Vec<String>>()
                    .join(" ");

                format_error_body(
                    source_example,
                    &formatted_args,
                    "Consider adding these arguments.",
                    primary_colors
                )
            },
            Error::EmptyFlag => {
                let message = String::from("-- (empty flag)");

                format_error_body(
                    source_example,
                    &message,
                    "Consider specifying the flag's name.",
                    error_colors
                )
            },
            Error::FlagArgumentsOutOfBounds(arg) => {
                let message = format!("<{}>", arg);
                let mut flag_example = source_fmt.clone();
                flag_example.push_str(" [FLAG]");

                format_error_body(
                    (&flag_example, flag_example.len()),
                    &message,
                    "Consider adding this argument to the flag.",
                    primary_colors
                )
            },
            Error::InvalidUTF8 => {
                let message = String::from("<Invalid UTF-8>");

                format_error_body(
                    ("", 0),
                    &message,
                    "Consider removing non-UTF8 characters.",
                    error_colors
                )
            },
            Error::UnknownFlag(flag) => {
                format_error_body(
                    source_example,
                    &flag,
                    "Consider removing this flag.",
                    error_colors
                )
            },
            Error::UnknownArgument(arg) => {
                format_error_body(
                    source_example,
                    &arg,
                    "Consider removing this argument.",
                    error_colors
                )
            }
        }
    }
}

impl Prettifier for DefaultPrettifier<'_> {
    fn primary_color(&self) -> &str {
        self.color_scheme.0
    }

    fn reset_color(&self) -> &str {
        self.color_scheme.2
    }

    fn prettify_error(&self, error: CommandStreamError) -> String {
        let source = match error {
            CommandStreamError::CommandParseError((_, s)) => s,
            _ => "CommandStream",
        };

        // Needed colors: error and reset
        let c = (self.color_scheme.1, self.color_scheme.2);

        let error_header = format!("{}error[{}]:{} {}", c.0, source, c.1, error);
        let header_context = format!("  --> source: {}", source);
        let body = match error {
            CommandStreamError::CommandParseError((e, s)) => {
                self.generate_error_body(e, s)
            },
            _ => " ".to_owned()
        };

        format!("{}\n{}\n{}",
                error_header,
                header_context,
                body
        )
    }
}

/// Helper function for `DefaultPrettifier::generate_error_body()` method
fn format_error_body(
    source_example: (&str, usize),
    message: &str,
    help: &str,
    colors: (&str, &str)
) -> String {

    format!(
        "    | {} {p}{}{r}\n    | {} {p}{}{r}\n  help: {}",
        source_example.0,
        message,
        " ".repeat(source_example.1),
        "^".repeat(message.len()),
        help,
        p = colors.0,
        r = colors.1,
    )
}

/// **CommandStream** – assembled parser for multiple commands with support of
/// `help` command and prettifier for beautiful error messages and colors
/// 
/// # Example
/// ```
/// use accordion::cli::command::{Arg, Command, Value};
/// use accordion::cli::{CommandStream, DefaultPrettifier};
/// use accordion::cli::consumer::GenericConsumer;
///
/// const CARGO: Command<Vec<Value>> = Command::new_from(
///         "cargo",
///         "Example description",
///         &[
///            Arg::default("subcommand"),
///            Arg::flag("version", Some("v"), GenericConsumer(&[]))
///         ],
///         cargo
///     );
///
/// let command_stream = CommandStream::new(vec![
///     CARGO
///     // Add more as needed
/// ]);
///
/// // Parsing input from user
/// match command_stream.parse("cargo -v") {
///     Ok(_) => {},
///     // For potential errors we use prettifier
///     Err(e) => println!("{}", command_stream.prettify(e))
/// }
///
/// fn cargo(_: Vec<Value>) {}
/// ```
/// ### How to set up custom prettifier
/// 
/// To make your errors look beautiful and provide additional coloring
/// with `ANSI` colors, `CommandStream` supports `Prettifier` trait, which should
/// implement methods for error prettifying and color set
/// 
/// First, create your `Prettifier` (see in `Prettifier` trait documentation), then,
/// specify generic for `CommandStream`:
/// ```
/// use accordion::cli::{CommandStream, DefaultPrettifier, Prettifier};
/// use accordion::temp_command;
///
/// // Replace `DefaultPrettifier` with your custom one
/// let command_stream: CommandStream<_, _, DefaultPrettifier> = CommandStream::new(vec![
///     temp_command!()
/// ]);
/// ```
/// `CommandStream::new()` method uses `::default()` to construct your prettifier, so be
/// sure that you prettifier implements it
pub struct CommandStream<'a, T, C, P = DefaultPrettifier<'a>>
where
    T: From<Vec<Value>>,
    P: Prettifier + Sized + Default,
    C: Fn(T)
{
    commands: Vec<Command<'a, T, C>>,
    help_callback: Box<dyn Fn(String)>,
    prettifier: P,
}

impl<'a, T, C> CommandStream<'a, T, C, DefaultPrettifier<'_>>
where
    T: From<Vec<Value>>,
    C: Fn(T)
{
    /// Create a new `CommandStream`
    pub fn new(commands: Vec<Command<'a, T, C>>) -> Self {
        Self {
            commands,
            help_callback: Box::new(|string| println!("{}", string)),
            prettifier: DefaultPrettifier::default(),
        }
    }

    /// Call method `prettify_error` for specified Prettifier
    /// 
    /// # Example of prettified error
    /// Using `DefaultPrettifier` and error `Error::MissingArguments`
    /// ```plaintext
    /// error[cargo]: 'Missing arguments: ["type", "args"]' – returned by command 'cargo'
    ///   --> source: cargo
    ///     | cargo ... <type> <args>
    ///     |           ^^^^^^^^^^^^^
    ///   help: Consider adding these arguments.
    /// ```
    pub fn prettify(&self, e: CommandStreamError) -> String {
        self.prettifier.prettify_error(e)
    }

    /// Parse user input as commands stream
    /// 
    /// This method takes a string and divides it into arguments using spaces
    /// **(except for spaces in quotes)**, then looks at the first argument and
    /// matches it with the name of any command in the CommandStream. If
    /// one is found, then call its method `parse`
    /// 
    /// This feature includes the `help` command right out of the box, but
    /// you can override it
    /// 
    /// # Example
    /// ```
    /// use accordion::cli::{CommandStream, DefaultPrettifier};
    /// use accordion::cli::command::{Command, Value};
    /// use accordion::temp_command;
    ///
    /// let command_stream = CommandStream::new(vec![
    ///     temp_command!(),
    /// ]);
    ///
    /// command_stream.parse("help").unwrap();
    /// // By default, output from `help` command goes to
    /// // println!(), but you can also specify that
    /// ```
    /// This is that we got in terminal output after calling command `help`
    /// ```plaintext
    /// USAGE: <command> [ARGS]
    /// 
    /// COMMANDS:
    ///   temp – The temp command used for tests. Panics when called
    /// 
    /// Use `help [command]` to get more information about specific command.
    /// ```
    pub fn parse(&self, str: &str) -> Result<(), CommandStreamError> {
        let mut args = split_args(str);
        if args.is_empty() {
            return Err(CommandStreamError::EmptyInput)
        }
        
        // First argument we use as target command name
        let cmd_name = &args[0];
        if self.commands.is_empty() {
            return Err(CommandStreamError::NoCommandsAvailable)
        }
        
        if let Some(cmd) = self.commands.iter()
            .find(|c| c.name == cmd_name) {

            args.remove(0);
            
            let output = cmd.parse(args)
                .map_err(|e| CommandStreamError::CommandParseError((e, cmd.name)))?;

            let converted_output = T::from(output);
            
            // Call callback of this function
            (cmd.callback)(converted_output);

            return Ok(());
        }

        // Help command resolving goes after to allow user to override it
        if cmd_name == "help" {
            // At index one (that is, second argument) we expect optional help context
            if let Some(context) = args.get(1) {
                self.find_and_request_help(context)?;
                return Ok(())
            }
            let output = self.help();
            (self.help_callback)(output);

            return Ok(());
        }

        Err(CommandStreamError::UnknownCommand(cmd_name.to_owned()))
    }

    /// Finds `Command` in CommandStream using given name and returns
    /// index of this `Command`
    fn find_cmd(&self, name: &str) -> Option<usize> {
        if let Some((index, _)) = self.commands.iter()
            .enumerate()
            .find(|(_, c)| c.name == name) {
            return Some(index);
        }
        None
    }

    /// Helper function for `CommandStream::parse_input()`
    /// Finds command in `CommandStream` by its name and calls `self.help_callback`
    /// with output from `.help()` call
    fn find_and_request_help(&self, context: &str) -> Result<(), CommandStreamError> {
        if let Some(index) = self.find_cmd(context) {
            let output = self.request_command_help(index);
            (self.help_callback)(output);
        } else {
            return Err(CommandStreamError::UnknownCommand(context.to_owned()));
        }
        Ok(())
    }

    /// Generates output of `help` message as list of all commands with their description
    pub fn help(&self) -> String {
        let p = self.prettifier.primary_color();
        let r = self.prettifier.reset_color();
        
        let formatted = self
            .commands
            .iter()
            .map(|c| format!("  {} – {}", c.name, c.desc))
            .collect::<Vec<String>>();
        
        format!(
            "{p}USAGE:{r} <command> [ARGS]\n\n{p}COMMANDS:{r}\n{}\n\nUse {p}`help [command]`{r} to get more information about specific command.",
            formatted.join("\n"),
            p = p,
            r = r
        )
    }

    /// Call `.help()` method on `Command` located at given idx
    pub fn request_command_help(&self, idx: usize) -> String {
        if idx >= self.commands.len() {
            return format!("No command at index {}", idx);
        }
        self.commands[idx].help(Some((
            self.prettifier.primary_color(),
            self.prettifier.reset_color(),
        )))
    }
}
