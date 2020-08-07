

Aliases

```
UIO[A] — This is a type alias for ZIO[Any, Nothing, A], which represents an effect that has no requirements, and cannot fail, but can succeed with an A.
URIO[R, A] — This is a type alias for ZIO[R, Nothing, A], which represents an effect that requires an R, and cannot fail, but can succeed with an A.
Task[A] — This is a type alias for ZIO[Any, Throwable, A], which represents an effect that has no requirements, and may fail with a Throwable value, or succeed with an A.
RIO[R, A] — This is a type alias for ZIO[R, Throwable, A], which represents an effect that requires an R, and may fail with a Throwable value, or succeed with an A.
IO[E, A] — This is a type alias for ZIO[Any, E, A], which represents an effect that has no requirements, and may fail with an E, or succeed with an A.
```


Default features

* Asynchronous Programming — Write asynchronous code as easily as synchronous code, handling all errors and never leaking resources.
* Concurrent Programming — Write concurrent code that scales easily, without locks or deadlocks, with maximal laziness and resource safety.
* Parallelism — Trivially partition work among many parallel fibers to make short work of CPU-intensive processing.
* Queuing — Build work processing flows and ration scarce resources with powerful asynchronous queues.
* Retrying — Create and test robust retry strategies that make your application resilient to transient failures.
* Scheduling — Schedule repeating work, like report generation or email notifications, using flexible, composable schedules.
* Streaming — Handle huge or infinite amounts of data in constant heap space with efficient, lazy, concurrent streams.
* Testing - Easily test effectual programs with powerful combinators, built-in property based testing, and seamless mocking capabilities.


Retrying

```
val retriedOpenFile: ZIO[Clock, IOException, Array[Byte]] =
  openFile("primary.data").retry(Schedule.recurs(5))
```
