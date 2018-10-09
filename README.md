### Compositional Deep Learning

This is my attempt to [continue](https://github.com/bgavran/autodiff) formalizing and understanding neural networks in the language of category theory.
It's a compositional approach to organizing and layering different abstractions found in neural networks:
* Differentiation of composition of functions (forward mode, reverse mode)
* Notion of composition of *parametrized* functions
* Notion of a cost function (fixed or adversarial)
* Notion of an update rule
* Notion of meta learning

Most of the work in this repository is inspired by two recent papers:

* [The simple essence of automatic differentiation](http://conal.net/papers/essence-of-ad/)
* [Backprop as Functor](https://arxiv.org/abs/1711.10455)

They're talking about similar things in the language of category theory, albeit in a slightly different way. My goal is to reconcile their work and expand on it.

----

Progress so far:
* Main parts of SimpleAD paper are fully implemented
  * Generalized, mode-independent automatic differentiation with rudimentary functions
  * Backpropagation (which is *just a specialization of GAD to the dual category of additive functions*)
* Implemented category **Para** from BackpropFunctor
* **Novel**: Implemented **Para** in terms of **GAD**, making _request_ function from BackpropFunctor obsolete


**TODO**:
* Find a suitable tensor library with the following features:
  * Static tensor shapes, known and type-checked at compile time
  * Some variant of Einstein summation notation for handling of tensors of arbitrary rank
* Provide working examples of training simple neural networks


This is a heavy work in progress, but feel free to drop me an email with any questions you might have!
