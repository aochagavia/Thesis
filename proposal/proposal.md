# Introducing WebAssembly

WebAssembly is, according to its official website, "a new portable, size- and load-time-efficient format suitable for compilation to the web". That is, an assembly language that is designed to run in the browser, though it can also run embedded in other programs. A formal definition of the language has already been published by Haas et al. [@2017wasm].

Since november 2017, WebAssembly is supported by all major web browsers (Safari, Firefox, Chrome and Edge). Besides browser support, many compilers are already able to target WebAssembly. Particularly, languages with a lightweight runtime such as C, C++ and Rust have currently an advantage on this space.

This is a significative moment for the web, because it opens the door to writing frontend applications in any conceivable programming language. Besides, WebAssembly provides excellent performance, given its low level nature.

# The challenge

By design, WebAssembly is mostly a computation platform. It is possible to allocate memory and call functions, but there is no access to the DOM or other browser APIs. Therefore, the only way to interact with the browser is by:

1. Exposing functions that can be called from Javascript
1. Importing functions that are provided by Javascript
1. Allocating memory and letting Javascript read and write to it

Furthermore, developing an application that involves WebAssembly requires dealing with three languages:

1. Javascript, to drive execution of the module
1. A programming language able to target WebAssembly, such as Rust or C++
1. WebAssembly itself, which is very different from other common compilation targets

Given these facts, it is expected that developing a WebAssembly application may result in unwanted accidental complexity. This is an undesirable situation, as it threatens to counteract WebAssembly's benefits.

# A proof of concept

In order to explore the advantages and limitations of WebAssembly on the browser, we developed a [game](https://github.com/aochagavia/rocket_wasm) in Rust and compiled it to WebAssembly. Interestingly, the game is an adaptation of a preexisting desktop game, which shows the code reuse opportunities enabled by this technology. Besides the code reuse benefits, performance of the game is excellent. However, the interaction between Rust and the browser was far from seamless, as described in the following sections.

## Restricted types on function signatures

Given its low-level nature, WebAssembly supports only integers and floating point numbers (both 32 and 64-bit wide). Therefore, when defining a Rust function to be called by Javascript, we can only use such data types in the function signature.

This creates additional difficulties when defining an API, as there is no straightforward way to pass more complex data structures. For instance, passing strings requires storing the characters in a memory buffer that is shared by Javascript and Rust and then passing a pointer to the data. Not only is this a cumbersome way to share data, it also introduces room for errors related to memory management.

As a consequence of this limitations, we decided to keep the state as a global variable inside the Rust module, instead of passing pointers to it between Rust and Javascript. This means that the update functions operate on global state, instead of being pure, as in the original implementation of the game. While not very elegant, this pragmatic approach liberated us from fiddling with pointers.

The problem of keeping and updating state is a manifestation of a much deeper issue. Sending objects from Rust to Javascript, or the other way around, is cumbersome and error-prone. It decreases the maintainability of an application and introduces undesirable accidental complexity.

## No access to the DOM

Since the game is rendered on a HTML5 canvas element, we need functions to draw the different entities present in the world. However, there is no access to the DOM from WebAssembly. Workarounds exist, but they are again a hindrance to maintainability.

Ideally, when drawing bullets we would like to use the function below:

```js
// Draw a bullet on the canvas, centered at position x, y
ctx.drawImage(res.bullet, x - 3, y - 3);
```

> Note: `ctx` is the drawing context associated to a canvas and `res.bullet` is the image of a bullet. We subtract 3 from `x` and `y` in order to center the image, which is 6 pixels wide and high.

We would like to call `ctx.drawImage(...)` directly from Rust, but there is no way to pass `ctx`, `res.bullet` or any Javascript object to Rust. Fortunately, we know that it is possible to pass whole Javascript functions to a WebAssembly module. Therefore, we can implicitly pass `ctx` and `res.bullet` by wrapping them in a function, as shown below:

```js
// Draw a bullet on the canvas, centered at position x, y
function draw_bullet(x, y) {
    ctx.drawImage(res.bullet, x - 3, y - 3);
}
```

After doing this, we can add the following code to Rust and call it from there:

```rust
// Draw a bullet on the canvas, centered at position x, y
extern "C" fn draw_bullet(x: c_double, y: c_double);
```

While passing the whole function works, it is basically glue code to make up for the lack of DOM access in WebAssembly. In our use case, the resulting code is still relatively simple. However, we expect things to get much worse in bigger projects, as more and more functions need to be wrapped. Again, this has a negative effect on the complexity and maintainability of the application.

## No access to a source of entropy

The game we chose for the proof of concept requires random number generation. Interestingly, we were able to reuse a Rust library for this purpose, showing again that code reuse in WebAssembly delivers what it promises.

However, generating random numbers requires an initial seed. Usually, the seed is obtained through special platform-dependent functions. For instance, Windows exposes the `CryptGenRandom` function for this purpose.

Since WebAssembly is platform-agnostic, there is no special function available to obtain a random seed. In our case, we resorted to using a constant seed for the sake of simplicity. However, this situation is far from ideal.

Note that it would have been possible to declare an extern function in Rust that links to a Javascript function, in the same spirit of `draw_bullet`. However, in this case we were not willing to pay the complexity toll.

# Conclusion

In light of the aforementioned proof of concept, it is clear that the integration between the browser and Rust applications is an open challenge. Limitations result in high amounts of glue code and in workarounds that are a maintainability hazard.

These conclusions apply not only to Rust, but to all languages that compile to WebAssembly. In this regard, it is highly probable that the solution for one language is applicable to other languages as well.

# Existing research

Emscripten [@2011emscripten]

# Proposal

The advent of WebAssembly opens a challenge in the area of programming language integration. From this perspective, our research question is: how can we easen the interaction between the browser and WebAssembly applications?

Given the complexity of the question posed above, we can split it into the following components:

1. How can we easen the integration between Javascript and other programming languages running on the browser?
1. How can we deal with platform-specific APIs that are not available on WebAssembly (e.g. file system, networking primitives, etc)?
1. How can we deal with browser-specific APIs that are not available on WebAssembly (e.g. the DOM API, the Web Workers API)?

We propose answering these questions through the following steps:

1. Identify and evaluate the approaches that the programming community has taken in an attempt to tackle the problem of WebAssembly-browser integration.
1. Develop and evaluate alternative approaches to the problem.
1. Develop a proof-of-concept showing how one of these approaches works in practice.
