pub mod ncmi_map;

/// Fill vector with values produced by a factory function
///
/// # Safety
///
/// - 'vec` must have a sufficient capacity (capacity) of at least 'len' elements
/// - The function sets the length of the vector to 'len`
/// - The closure 'factory' must return correctly initialized values
pub unsafe fn fast_vec_fill_with<T, F>(vec: &mut Vec<T>, factory: F, len: usize)
where
    F: Fn() -> T,
{
    debug_assert!(
        vec.capacity() >= len,
        "Vector capacity is less than required length"
    );

    let ptr: *mut T = vec.as_mut_ptr();

    unsafe {
        for i in 0..len {
            std::ptr::write(ptr.add(i), factory());
        }

        vec.set_len(len);
    }
}

#[derive(Debug, PartialEq)]
enum ParserState {
    Outside,
    Inside(char)
}

/// Splits user input string into a vector of command arguments using whitespace as a separator,
/// but also allows spaces to be used within quotes.
///
/// Example input: `command "white space arg" arg`
///
/// Result: `["command", "white space arg", "arg"]`
pub fn split_args(args_as_string: &str) -> Vec<String> {
    let mut buffer = Vec::new();
    let mut state = ParserState::Outside;
    let mut current = String::new();

    for char in args_as_string.chars() {
        match (&state, char) {
            (ParserState::Outside, '\'') | (ParserState::Outside, '"') => {
                state = ParserState::Inside(char);
            },
            (ParserState::Inside(quote), _) if *quote == char => {
                state = ParserState::Outside;
            },
            (ParserState::Outside, ' ') => {
                if !current.is_empty() {
                    buffer.push(std::mem::take(&mut current));
                }
            },
            _ => current.push(char),
        }
    }

    if !current.is_empty() {
        buffer.push(current);
    }
    
    buffer
}