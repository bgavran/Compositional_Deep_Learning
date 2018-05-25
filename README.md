### Functional Deep Learning

This is my attempt to [continue](https://github.com/bgavran/autodiff) formalizing and understanding neural networks from the first principles.

It started as a Haskell implementation of the [Backprop as Functor](https://arxiv.org/abs/1711.10455) paper which is an intriguing approach to neural networks from the perspective of Category Theory.

In the meantime there have been a few papers that started talking about similar things!

[a) The simple essence of automatic differentiation](http://conal.net/papers/essence-of-ad/)

[b) Demystifying Differentiable Programming: Shift/Reset the Penultimate Backpropagator](https://arxiv.org/abs/1803.10228)

Both a) and b) talk about implementation of generalized reverse-mode AD in a purely functional context.
My [previous](https://github.com/bgavran/autodiff) attempt in implementing such a system relied heavily on the concept of a computational graph, which seems to have been rendered obsolete by these amazing approaches.

The `src` directory currently consists of various files with implementations of `Backprop as Functor` and `The simple essence of automatic differentiation` papers, together with my experiments.
