# Parametric lenses: change notification for bidirectional lenses

Haskell code for the paper: [http://dl.acm.org/citation.cfm?id=2746333]

Most complex applications inevitably need to maintain dependencies between
the different subsystems based on some shared data. The dependent parts must be
informed that the shared information is changed. As every actual notification
has some communication cost, and every triggered task has associated
computation cost, it is crucial for the overall performance of the application
to reduce the number of notifications as much as possible. To achieve this,
one must be able to define, with arbitrary precision, which party is
depending on which data. In this paper we offer a general solution to this
general problem. The solution is based on an extension to bidirectional lenses,
called parametric lenses. With the help of parametric lenses one can define
compositional parametric views in a declarative way to access some shared
data. Parametric views, besides providing read/write access to the shared data,
also enable to observe changes of some parts, given by an explicit parameter,
the focus domain. The focus domain can be specified as a type-based query
language defined over one or more resources using predefined combinators of
parametric views.
