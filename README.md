# glsl-parser

**glsl-parser** is an offline GLSL parser which can be used to do many things with GLSL source code.

The straight-forward API allows you to parse GLSL into an abstact-syntax-tree in only a couple lines, for example

    glsl::parser parse(sourceCode);
    glsl::astTU *translationUnit = parse.parse(astTU::kFragment);
    if (translationUnit) {
        // Do something with the AST here
    } else {
        // A parse error occured
        fprintf(stderr, "%s\n", parse.error());
    }

A test-suite and GLSL source-generator is included to get you started.

Check out the superior diagnostics [here](EXAMPLE_ERRORS.md)

### Possible uses
  * Verify a shader without running it
  * Find hard-to-find syntax or semantic errors with the superior diagnostics
  * Extend GLSL
  * Transcompile GLSL to other languages
  * Quickly check or verify something
  * Optimize shaders
  * Generate introspection information

### Portable and embeddable
  * Written in portable C++03.
    * Only uses *std::vector* from the standard library
  * Exception free
  * Doesn't use virtual functions
  * Small (~90 KB)
  * Permissive (MIT)
