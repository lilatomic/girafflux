# Girafflux

Girafflux is a language which helps you be more productive with Flux.
It converts common operations into dedicated operators and removes some boilerplate.
Girafflux's main usecase is for monitoring and dashboards, as part of a stack using InfluxDB, like the TIG stack.

## Examples

The following Flux query:

```flux
from(bucket: "telegraf")
	|> range(start: -1h)
	|> filter(fn: (r) => (r["_measurement"] == "cpu"))
	|> filter(fn: (r) => (r["_field"] == "usage_user"))
	|> filter(fn: (r) => (r.cpu == "cpu-total"))
	|> aggregateWindow(every: 5m, fn: mean)
```

can be transpiled from this Girafflux:

```
from telegraf
|@ start -1h
|$ "cpu"
|% "usage_user"
|? cmp.eq(_.cpu, "cpu-total")
| aggregateWindow(every: 5m, fn: mean)
```

## Basics

Girafflux is basically syntactic frosting on top of flux.
The most obvious part of frosting is the use of concise punctiation-based operators

| Girafflux | Description         |
|-----------|---------------------|
| &#124;    | Operation           |
| &#124;@   | Range               |
| &#124;$   | Filter _measurement |
| &#124;%   | Filter _field       |
| &#124;?   | Filter              |
| &#124;.   | Assign              |
| &#124;._  | Assign to _value    |

Girafflux doesn't implement arithmetic and comparison as builtin infix operators, which is a little annoying. It makes
the parser a lot easier to implement, and I didn't use them that much. Math is implemented in the `math` package:

| Flux | Girafflux  |
|------|------------|
| `+`  | `math.add` |
| `-`  | `math.sub` |
| `*`  | `math.mul` |
| `/`  | `math.div` |
| `^`  | `math.exp` |
| `%`  | `math.mod` |

Comparison is implemented in the `cmp` package:

| Flux | Girafflux      |
|------|----------------|
| `==` | `cmp.eq`       |
| `!=` | `cmp.ne`       |
| `<`  | `cmp.lt`       |
| `>`  | `cmp.gt`       |
| `<=` | `cmp.le`       |
| `>=` | `cmp.ge`       |
| `=~` | `cmp.regex_eq` |
| `!~` | `cmp.regex_ne` |

Modifying the value is easy. For the most common case of overwriting `_value`, use the `|._` stage. For example, `|._ math.exp(r._value, 2)` will become `|> map(fn: (r) => {r with _value: (r._value ^ 2)})`
Modifying another attribute of the value is done with `|.`. For example, the following computes the FQDN `|.fqdn math.add(math.add(r.host, "."), r.domain)` which expands to `	|> map(fn: (r) => {r with fqdn: ((r.host + ".") + r.domain)})`
Modifying multiple attributes at the same time is also possible. Simply create a record instead of a single value. For example, `|.{ _measurement: "notifications", _status_timestamp: int(v: _._time), _time: now() }` becomes `|> map(fn: (r) => return {r with _measurement: "notifications", _status_timestamp: int(v: r._time), _time: now()})`
