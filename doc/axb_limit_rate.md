

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
   axb_limit_rate:await(my_limiter, my_caller). % Can block for some time, if limit is reached.
```

'

In a typical case, these two function calls will be wrapper in
an application specific module. The start_link/2 function will
be called from some supervisor and the ask/2 function from a
process, for which the limit should be applied.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ask-2">ask/2</a></td><td>
Ask for execution for permission to proceed.</td></tr><tr><td valign="top"><a href="#await-2">await/2</a></td><td>
Await for permission to proceed.</td></tr><tr><td valign="top"><a href="#set_rate-2">set_rate/2</a></td><td>
Set new rate for the limiter.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Start the process without registering it.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>
Start the server and register it as <code>Ref</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ask-2"></a>

### ask/2 ###

<pre><code>
ask(Ref::term(), Who::term()) -&gt; ok | {delay, DelayMS::integer()}
</code></pre>
<br />

Ask for execution for permission to proceed.
This function does not block, if needed
it tells the caller to wait.

Here Ref is a reference of the limiter process
and Who is a name of thing to limit.

<a name="await-2"></a>

### await/2 ###

<pre><code>
await(Ref::term(), Who::term()) -&gt; ok
</code></pre>
<br />

Await for permission to proceed.
This function will block, if the rate limit is reached.

<a name="set_rate-2"></a>

### set_rate/2 ###

<pre><code>
set_rate(Ref::term(), Rate::number()) -&gt; ok
</code></pre>
<br />

Set new rate for the limiter.
The rate is set asynchronously.

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Rate::number()) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

Start the process without registering it.

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Ref::term(), Rate::number()) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

Start the server and register it as `Ref`.
The Rate is specified as a number of events per second.
One can use Rate smaller than 1 to have events with delays more thant second.

