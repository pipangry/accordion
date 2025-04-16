use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum Error<'a> {
    FlagArgumentsOutOfBounds(String),
    MissingArguments(Vec<&'a str>),
    UnknownArgument(String),
    UnknownFlag(String),
    InvalidUTF8,
    EmptyFlag,
}

/// This type represents name of `Command` which threw this error
pub type ErrorSource<'a> = &'a str;

pub type Result<T> = std::result::Result<T, Error<'static>>;

impl Display for Error<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::MissingArguments(arguments) => write!(f, "Missing arguments: {:?}", arguments),
            Error::FlagArgumentsOutOfBounds(flag) => write!(
                f,
                "Cannot reach arguments for '{}' flag because they out of bounds",
                flag
            ),
            Error::InvalidUTF8 => write!(f, "Invalid UTF8"),
            Error::EmptyFlag => write!(f, "Empty flag"),
            Error::UnknownFlag(s) => write!(f, "Unknown flag: {}", s),
            Error::UnknownArgument(s) => write!(f, "Unknown argument: {}", s),
        }
    }
}

#[derive(Debug)]
pub enum CommandStreamError<'a> {
    CommandParseError((Error<'a>, ErrorSource<'a>)),
    UnknownCommand(String),
    NoCommandsAvailable,
    EmptyInput
}

impl Display for CommandStreamError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandStreamError::CommandParseError((error, source)) =>
                write!(f, "'{}' – returned by command '{}'", error, source),
            CommandStreamError::UnknownCommand(command) => write!(f, "Unknown command: {}", command),
            CommandStreamError::EmptyInput => write!(f, "Empty input"),
            CommandStreamError::NoCommandsAvailable => write!(f, "No commands available"),
        }
    }
}