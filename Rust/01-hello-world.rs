// This is a single-line comment, and is ignored by the compiler.

/* This is a block comment. In general, line comments are the recommended
comment style. But block comments are useful for temporarily disabling
chunks of code. /* Block comments can be /* nested, */ */ so it takes
only a few keystrokes to comment out what you want */

// This is the main function.
fn main() {
    // Statements here are executed when the compiled binary is called.

    // Prints text to the console.
    println!("Hello World!");    // Prints "Hello World!"

    // Block comments can be embedded in the middle of code.
    let x = 5 + /* 90 + */ 5;
    println!("{}", x);    // Prints "10"
}
