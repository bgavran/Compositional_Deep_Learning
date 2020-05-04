### Compositional Deep Learning

**Update May 2020:** It's important to note that while eventual implementation of neural networks from first principles is my main goal, I've currently shifted to purely category-theoretic perspective, which is at the moment not in software and mostly all on paper. This is why for a while there hasn't been any developments in this repo, although I plan on getting back to it.

This is a long-term project of understanding and reimplementing neural networks from first principles using the language of category theory.
It's a compositional approach to organizing and layering different abstractions found in neural networks:
* Differentiation of composition of functions (forward mode, reverse mode)
* Notion of composition of *parametrized* functions
* Notion of a cost function (fixed or adversarial)
* Notion of an update rule
* Notion of meta learning
* Notion of multi-agent neural networks 

As more and more components of our deep learning systems stop being fixed throughout training, there is an increasingly larger need for more precise formal specification of the things that _do_ stay fixed.
Standard methods don't seem to be as effective: the invariants across all these networks seem to be rather abstract and hard to describe. This repository explores the speculation that the language of category theory could be well suited to describe and quantify these structures.

Focus is currently not on reimplementation of popular new network architectures, but rather on principled, structured design of both neural networks and the way they are trained.

This is research level stuff and at the moment not really usable for anything other than playing around.

Related work:

* [The simple essence of automatic differentiation (SimpleAD)](http://conal.net/papers/essence-of-ad/)
* [Backprop as Functor](https://arxiv.org/abs/1711.10455)
* [Lenses and Learners](https://arxiv.org/abs/1903.03671)

----

A tiny example of simple linear regression can be found in Examples.hs

----

Progress so far:
* Main parts of SimpleAD paper are fully implemented
  * Generalized, mode-independent automatic differentiation with rudimentary functions
  * Backpropagation (which is *just a specialization of GAD to the dual category of additive functions*)
* Implemented category **Para** from BackpropFunctor (in a slightly different way than in the paper, such that the _request function_ in the specified form isn't really needed)


**TODO**:
* Find a suitable tensor library with the following features:
  * Static tensor shapes, known and type-checked at compile time
  * Some variant of Einstein summation notation for handling of tensors of arbitrary rank
I'm working on one such library in Idris, which implements [Dependently Typed Einstein Summation](https://github.com/bgavran/Dependently_Typed_Einsum).
* Provide working examples of training simple neural networks
* Find a way to graphically show composition of **GAD**, **Para** and **Learners**
* Explore using effects for data loading


This is a heavy work in progress, but feel free to drop me a message with any questions!
