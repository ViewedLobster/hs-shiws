# Independent Haskell Web Framework #

Beginning of implementation of a simple web server framework in Haskell. Has a
working socket input/output system built from scratch.

Partly to learn the ins and outs of the Haskell FFI, we use low level FFI calls
to implement socket handling. We use the epoll system in order to achieve
lighweigth blocking semantics. In the end it turned out very similar to
the socket handling in the Network.Socket module, albeit a bit more explicit.
