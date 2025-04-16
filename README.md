# Example
Creating **CommandStream** – assembled parser for multiple commands
```
use accordion::cli::command::{Arg, Command, Value};
use accordion::cli::{CommandStream, DefaultPrettifier};
use accordion::cli::consumer::GenericConsumer;

const CARGO: Command<Vec<Value>> = Command::new_from(
        "cargo",
        "Example description",
        &[
           Arg::default("subcommand"),
           Arg::flag("version", Some("v"), GenericConsumer(&[]))
        ],
        cargo
    );

let command_stream = CommandStream::new(vec![
    CARGO
    // Add more as needed
]);

// Parsing input from user
match command_stream.parse("cargo -v") {
    Ok(_) => {},
    // For potential errors we use prettifier
    Err(e) => println!("{}", command_stream.prettify(e))
}

fn cargo(_: Vec<Value>) {}
```
# Features
- Colors customization support out of the box
- Automatic `help` command
- Easy parsing for one or multiple commands
- Error prettifying

### Example of prettified error message
```plaintext
error[cargo]: 'Missing arguments: ["type", "args"]' – returned by command 'cargo'
  --> source: cargo
    | cargo ... <type> <args>
    |           ^^^^^^^^^^^^^
  help: Consider adding these arguments.
```