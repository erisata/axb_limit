

# Module axb_limit_rate #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Limits the caller to the specified rate.

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="description"></a>

## Description ##
Example:

```
      `
   axb_limit_rate:start_link({local, my_limiter}, 3). % We want 3 tps max.
   axb_limit_rate:ask(my_limiter, my_caller). % Can block for some time, if limit is reached.
```

'

In a typical case, these two function calls will be wrapper in
an application specific module. The start_link/2 function will
be called from some supervisor and the ask/2 function from a
process, for which the limit should be applied.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ask-2">ask/2</a></td><td>
Ask for execution.</td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td>
Code upgrades.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td>
Synchronous calls.</td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td>
Asynchronous events.</td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td>
Other messages.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>
Initialization.</td></tr><tr><td valign="top"><a href="#set_rate-2">set_rate/2</a></td><td>
Set new rate.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>
Start the server and register it as <code>Ref</code>.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td>
Process termination.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ask-2"></a>

### ask/2 ###

`ask(Ref, Who) -> any()`

Ask for execution.

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

Code upgrades.

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Unknown, From, State) -> any()`

Synchronous calls.

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Unknown, State) -> any()`

Asynchronous events.

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Unknown, State) -> any()`

Other messages.

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

Initialization.

<a name="set_rate-2"></a>

### set_rate/2 ###

`set_rate(Ref, Rate) -> any()`

Set new rate.

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Ref::term(), Rate::number()) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

Start the server and register it as `Ref`.
The Rate is specified as a number of events per second.
One can use Rate smaller than 1 to have events with delays more thant second.

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

Process termination.
