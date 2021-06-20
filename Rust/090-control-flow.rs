// Based on:
// https://doc.rust-lang.org/stable/rust-by-example/flow_control.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/if_else.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/loop.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/loop/nested.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/loop/return.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/while.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/for.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/match.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/match/destructuring.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/match/destructuring/destructure_tuple.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/match/destructuring/destructure_enum.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/match/destructuring/destructure_pointers.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/match/destructuring/destructure_structures.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/match/guard.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/match/binding.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/if_let.html
// https://doc.rust-lang.org/stable/rust-by-example/flow_control/while_let.html

fn main() {
  let n = 5;

  if n < 0 {
      print!("{} is negative", n);
  } else if n > 0 {
      print!("{} is positive", n);
  } else {
      print!("{} is zero", n);
  }

  let big_n =
      if n < 10 && n > -10 {
          println!(", and is a small number, increase ten-fold");

          // This expression returns an `i32`.
          10 * n
      } else {
          println!(", and is a big number, halve the number");

          // This expression must return an `i32` as well.
          n / 2
          // TODO ^ Try suppressing this expression with a semicolon.
      };
  //   ^ Don't forget to put a semicolon here! All `let` bindings need it.

  println!("{} -> {}", n, big_n);
}

// -----------------------------------------------------------------------------

fn main() {
  let mut count = 0u32;

  println!("Let's count until infinity!");

  // Infinite loop
  loop {
      count += 1;

      if count == 3 {
          println!("three");

          // Skip the rest of this iteration
          continue;
      }

      println!("{}", count);

      if count == 5 {
          println!("OK, that's enough");

          // Exit this loop
          break;
      }
  }
}

// -----------------------------------------------------------------------------

fn main() {
  'outer: loop {
      println!("Entered the outer loop");

      'inner: loop {
          println!("Entered the inner loop");

          // This would break only the inner loop
          //break;

          // This breaks the outer loop
          break 'outer;
      }

      println!("This point will never be reached");
  }

  println!("Exited the outer loop");
}

// -----------------------------------------------------------------------------

fn main() {
  let mut counter = 0;

  let result = loop {
      counter += 1;

      if counter == 10 {
          break counter * 2;
      }
  };

  assert_eq!(result, 20);
}

// -----------------------------------------------------------------------------

fn main() {
  // A counter variable
  let mut n = 1;

  // Loop while `n` is less than 101
  while n < 101 {
      if n % 15 == 0 {
          println!("fizzbuzz");
      } else if n % 3 == 0 {
          println!("fizz");
      } else if n % 5 == 0 {
          println!("buzz");
      } else {
          println!("{}", n);
      }

      // Increment counter
      n += 1;
  }
}

// -----------------------------------------------------------------------------

fn main() {
  // `n` will take the values: 1, 2, ..., 100 in each iteration
  for n in 1..101 {
      if n % 15 == 0 {
          println!("fizzbuzz");
      } else if n % 3 == 0 {
          println!("fizz");
      } else if n % 5 == 0 {
          println!("buzz");
      } else {
          println!("{}", n);
      }
  }
}

// -----------------------------------------------------------------------------

fn main() {
  // `n` will take the values: 1, 2, ..., 100 in each iteration
  for n in 1..=100 {
      if n % 15 == 0 {
          println!("fizzbuzz");
      } else if n % 3 == 0 {
          println!("fizz");
      } else if n % 5 == 0 {
          println!("buzz");
      } else {
          println!("{}", n);
      }
  }
}

// -----------------------------------------------------------------------------

fn main() {
  let number = 13;
  // TODO ^ Try different values for `number`

  println!("Tell me about {}", number);
  match number {
      // Match a single value
      1 => println!("One!"),
      // Match several values
      2 | 3 | 5 | 7 | 11 => println!("This is a prime"),
      // Match an inclusive range
      13..=19 => println!("A teen"),
      // Handle the rest of cases
      _ => println!("Ain't special"),
  }

  let boolean = true;
  // Match is an expression too
  let binary = match boolean {
      // The arms of a match must cover all the possible values
      false => 0,
      true => 1,
      // TODO ^ Try commenting out one of these arms
  };

  println!("{} -> {}", boolean, binary);
}

// -----------------------------------------------------------------------------

fn main() {
  let triple = (0, -2, 3);
  // TODO ^ Try different values for `triple`

  println!("Tell me about {:?}", triple);
  // Match can be used to destructure a tuple
  match triple {
      // Destructure the second and third elements
      (0, y, z) => println!("First is `0`, `y` is {:?}, and `z` is {:?}", y, z),
      (1, ..)  => println!("First is `1` and the rest doesn't matter"),
      // `..` can be the used ignore the rest of the tuple
      _      => println!("It doesn't matter what they are"),
      // `_` means don't bind the value to a variable
  }
}

// -----------------------------------------------------------------------------

// `allow` required to silence warnings because only
// one variant is used.
#[allow(dead_code)]
enum Color {
    // These 3 are specified solely by their name.
    Red,
    Blue,
    Green,
    // These likewise tie `u32` tuples to different names: color models.
    RGB(u32, u32, u32),
    HSV(u32, u32, u32),
    HSL(u32, u32, u32),
    CMY(u32, u32, u32),
    CMYK(u32, u32, u32, u32),
}

fn main() {
    let color = Color::RGB(122, 17, 40);
    // TODO ^ Try different variants for `color`

    println!("What color is it?");
    // An `enum` can be destructured using a `match`.
    match color {
        Color::Red   => println!("The color is Red!"),
        Color::Blue  => println!("The color is Blue!"),
        Color::Green => println!("The color is Green!"),
        Color::RGB(r, g, b) =>
            println!("Red: {}, green: {}, and blue: {}!", r, g, b),
        Color::HSV(h, s, v) =>
            println!("Hue: {}, saturation: {}, value: {}!", h, s, v),
        Color::HSL(h, s, l) =>
            println!("Hue: {}, saturation: {}, lightness: {}!", h, s, l),
        Color::CMY(c, m, y) =>
            println!("Cyan: {}, magenta: {}, yellow: {}!", c, m, y),
        Color::CMYK(c, m, y, k) =>
            println!("Cyan: {}, magenta: {}, yellow: {}, key (black): {}!",
                c, m, y, k),
        // Don't need another arm because all variants have been examined
    }
}

// -----------------------------------------------------------------------------

fn main() {
  // Assign a reference of type `i32`. The `&` signifies there
  // is a reference being assigned.
  let reference = &4;

  match reference {
      // If `reference` is pattern matched against `&val`, it results
      // in a comparison like:
      // `&i32`
      // `&val`
      // ^ We see that if the matching `&`s are dropped, then the `i32`
      // should be assigned to `val`.
      &val => println!("Got a value via destructuring: {:?}", val),
  }

  // To avoid the `&`, you dereference before matching.
  match *reference {
      val => println!("Got a value via dereferencing: {:?}", val),
  }

  // What if you don't start with a reference? `reference` was a `&`
  // because the right side was already a reference. This is not
  // a reference because the right side is not one.
  let _not_a_reference = 3;

  // Rust provides `ref` for exactly this purpose. It modifies the
  // assignment so that a reference is created for the element; this
  // reference is assigned.
  let ref _is_a_reference = 3;

  // Accordingly, by defining 2 values without references, references
  // can be retrieved via `ref` and `ref mut`.
  let value = 5;
  let mut mut_value = 6;

  // Use `ref` keyword to create a reference.
  match value {
      ref r => println!("Got a reference to a value: {:?}", r),
  }

  // Use `ref mut` similarly.
  match mut_value {
      ref mut m => {
          // Got a reference. Gotta dereference it before we can
          // add anything to it.
          *m += 10;
          println!("We added 10. `mut_value`: {:?}", m);
      },
  }
}

// -----------------------------------------------------------------------------

fn main() {
  struct Foo {
      x: (u32, u32),
      y: u32,
  }

  // Try changing the values in the struct to see what happens
  let foo = Foo { x: (1, 2), y: 3 };

  match foo {
      Foo { x: (1, b), y } => println!("First of x is 1, b = {},  y = {} ", b, y),

      // you can destructure structs and rename the variables,
      // the order is not important
      Foo { y: 2, x: i } => println!("y is 2, i = {:?}", i),

      // and you can also ignore some variables:
      Foo { y, .. } => println!("y = {}, we don't care about x", y),
      // this will give an error: pattern does not mention field `x`
      //Foo { y } => println!("y = {}", y),
  }
}

// -----------------------------------------------------------------------------

fn main() {
  let pair = (2, -2);
  // TODO ^ Try different values for `pair`

  println!("Tell me about {:?}", pair);
  match pair {
      (x, y) if x == y => println!("These are twins"),
      // The ^ `if condition` part is a guard
      (x, y) if x + y == 0 => println!("Antimatter, kaboom!"),
      (x, _) if x % 2 == 1 => println!("The first one is odd"),
      _ => println!("No correlation..."),
  }
}

// -----------------------------------------------------------------------------

// A function `age` which returns a `u32`.
fn age() -> u32 {
  15
}

fn main() {
  println!("Tell me what type of person you are");

  match age() {
      0             => println!("I haven't celebrated my first birthday yet"),
      // Could `match` 1 ..= 12 directly but then what age
      // would the child be? Instead, bind to `n` for the
      // sequence of 1 ..= 12. Now the age can be reported.
      n @ 1  ..= 12 => println!("I'm a child of age {:?}", n),
      n @ 13 ..= 19 => println!("I'm a teen of age {:?}", n),
      // Nothing bound. Return the result.
      n             => println!("I'm an old person of age {:?}", n),
  }
}

// -----------------------------------------------------------------------------

fn some_number() -> Option<u32> {
  Some(42)
}

fn main() {
  match some_number() {
      // Got `Some` variant, match if its value, bound to `n`,
      // is equal to 42.
      Some(n @ 42) => println!("The Answer: {}!", n),
      // Match any other number.
      Some(n)      => println!("Not interesting... {}", n),
      // Match anything else (`None` variant).
      _            => (),
  }
}

// -----------------------------------------------------------------------------

// Make `optional` of type `Option<i32>`
let optional = Some(7);

match optional {
    Some(i) => {
        println!("This is a really long string and `{:?}`", i);
        // ^ Needed 2 indentations just so we could destructure
        // `i` from the option.
    },
    _ => {},
    // ^ Required because `match` is exhaustive. Doesn't it seem
    // like wasted space?
};

// -----------------------------------------------------------------------------

fn main() {
  // All have type `Option<i32>`
  let number = Some(7);
  let letter: Option<i32> = None;
  let emoticon: Option<i32> = None;

  // The `if let` construct reads: "if `let` destructures `number` into
  // `Some(i)`, evaluate the block (`{}`).
  if let Some(i) = number {
      println!("Matched {:?}!", i);
  }

  // If you need to specify a failure, use an else:
  if let Some(i) = letter {
      println!("Matched {:?}!", i);
  } else {
      // Destructure failed. Change to the failure case.
      println!("Didn't match a number. Let's go with a letter!");
  }

  // Provide an altered failing condition.
  let i_like_letters = false;

  if let Some(i) = emoticon {
      println!("Matched {:?}!", i);
  // Destructure failed. Evaluate an `else if` condition to see if the
  // alternate failure branch should be taken:
  } else if i_like_letters {
      println!("Didn't match a number. Let's go with a letter!");
  } else {
      // The condition evaluated false. This branch is the default:
      println!("I don't like letters. Let's go with an emoticon :)!");
  }
}

// -----------------------------------------------------------------------------

// Our example enum
enum Foo {
  Bar,
  Baz,
  Qux(u32)
}

fn main() {
  // Create example variables
  let a = Foo::Bar;
  let b = Foo::Baz;
  let c = Foo::Qux(100);

  // Variable a matches Foo::Bar
  if let Foo::Bar = a {
      println!("a is foobar");
  }

  // Variable b does not match Foo::Bar
  // So this will print nothing
  if let Foo::Bar = b {
      println!("b is foobar");
  }

  // Variable c matches Foo::Qux which has a value
  // Similar to Some() in the previous example
  if let Foo::Qux(value) = c {
      println!("c is {}", value);
  }

  // Binding also works with `if let`
  if let Foo::Qux(value @ 100) = c {
      println!("c is one hundred");
  }
}

// -----------------------------------------------------------------------------

// This enum purposely neither implements nor derives PartialEq.
// That is why comparing Foo::Bar == a fails below.
enum Foo {Bar}

fn main() {
    let a = Foo::Bar;

    // Variable a matches Foo::Bar
    if Foo::Bar == a {
    // ^-- this causes a compile-time error. Use `if let` instead.
        println!("a is foobar");
    }
}

// -----------------------------------------------------------------------------

// Make `optional` of type `Option<i32>`
let mut optional = Some(0);

// Repeatedly try this test.
loop {
    match optional {
        // If `optional` destructures, evaluate the block.
        Some(i) => {
            if i > 9 {
                println!("Greater than 9, quit!");
                optional = None;
            } else {
                println!("`i` is `{:?}`. Try again.", i);
                optional = Some(i + 1);
            }
            // ^ Requires 3 indentations!
        },
        // Quit the loop when the destructure fails:
        _ => { break; }
        // ^ Why should this be required? There must be a better way!
    }
}

// -----------------------------------------------------------------------------

fn main() {
  // Make `optional` of type `Option<i32>`
  let mut optional = Some(0);

  // This reads: "while `let` destructures `optional` into
  // `Some(i)`, evaluate the block (`{}`). Else `break`.
  while let Some(i) = optional {
      if i > 9 {
          println!("Greater than 9, quit!");
          optional = None;
      } else {
          println!("`i` is `{:?}`. Try again.", i);
          optional = Some(i + 1);
      }
      // ^ Less rightward drift and doesn't require
      // explicitly handling the failing case.
  }
  // ^ `if let` had additional optional `else`/`else if`
  // clauses. `while let` does not have these.
}
