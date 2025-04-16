use crate::cli::{CommandStream, DefaultPrettifier};
use crate::cli::command::{Arg, Command, Value};
use crate::cli::consumer::GenericConsumer;

const CMD: Command<Vec<Value>> = Command::new_from(
    "cargo",
    "", 
    &[
        Arg::default("type"),
        Arg::flag("name", Some("n"), GenericConsumer(&["from", "to"])),
        Arg::flag("name1", Some("n"), GenericConsumer(&["from", "to"])),
        Arg::flag("name2", None, GenericConsumer(&["from", "to"])),
        Arg::default("args")
    ],
    hi,
);

fn hi(_: Vec<Value>) {}

#[test]
fn test() {
    let command_stream = CommandStream::new(vec![
        CMD
    ]);
    
    match command_stream.parse("cargo") {
        Ok(_) => (),
        Err(e) => println!("{}", 
                           command_stream.prettify(e),
        )
    }
}
