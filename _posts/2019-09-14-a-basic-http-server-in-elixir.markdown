---
layout: post
title:  "Writing the basis of an HTML Server with Elixir"
date:   2019-09-14 16:55:26 +0100
categories: programming elixir
---
Building a basic http server in Elixir.


I've been writing an http server in Erlang, which is still far from finished but coming together slowly, as a way of learning more in depth the http protocol and how to deal with it from scratch.

To write down my findings and share them I decided I should write a post on how to do that and in the process show some very neat more lowlevel aspects of erlang. There's a good range of http server libs available in Erlang, if you use Phoenix you'll be using Cowboy underneath, each one with its own design decisions, all of them pretty fine.

We won't be tackling in this post websockets nor any proper http connection upgrades, http2 and such, that will be hopefully for future posts, but instead we'll see how to create a simple socket acceptor pool, use gen_statems to create a way to run a "pipeline", design a basic router, parse the incoming requests, etc.

We'll have at least 4 components:

* the socket acceptor pool - creates a number of ready to work socket listeners waiting for requests on a given port
* the gen_statem that will use a modular and exchangeable pipeline definition to parse the request and route it appropriately
* the router module that allows a custom DSL that generates a proper router for our gen_statem to call with the appropriate routes and data
* a response creator, to send the responses to our clients when they visit our website

We'll also need several parsing functions. 

To follow along you need to have installed erlang 22 and elixir 1.9 (compiled with erlang 22). After having those installed, lets start by creating an umbrella app, from the command line:

`mix new satellitex --umbrella`

Lets go into our newly created directory and further into the `apps` folder and create our socket acceptor app (it will be a simple lib and not an application with a supervision tree):


```
cd satellitex/apps
mix new launchpad 
```

Now we'll be using gen_statem and so we should add some key properties to our logger configuration so that we can get system reports if the gen_statem crashes, otherwise we won't see the actual errors caused by it since `Logger` by default doesn't output system logs unless told to do so and gen_statem outputs its errors as system reports. Lets open our `config.exs` file for the umbrella (`satellitex/config/config.exs`) and make it look like the following:

{% highlight elixir %}
use Mix.Config

import_config "../apps/*/config/config.exs"

config :logger,
  handle_otp_reports: true,
  handle_sasl_reports: true
{% endhighlight %}


Now save the file. This is not to say that our code will have errors, that's quite outrageous to even think, but just in case we mistyped something.

Let's start writing our acceptor. This will be our "server" per se. It won't deal with requests by itself, instead, what it will do is open a socket in a given `port` and then create any number of ready to fire acceptors, listening on the socket. Each acceptor will be itself an independent process, to which we give "control" of the request as soon as a tcp connection is open, so that they can read from the socket and parse the request into something structured and usable for our application. We'll dive into that shortly after, but the idea is that it will be able to accept a "pipeline" definition and process the request according to that pipeline so that it's easily extendable.

First, if you never used the `gen_statem` OTP behaviour it's a fairly useful abstraction. It's similar to `gen_servers`, but it's newer and requires a bit more ceremony. `gen_server`s, due to having been there for so long and having a very simple layout are used much more often but lately I've been finding that gen_statems are really nice to use in a lot of different situations, even if you're not writing anything that is a complex state machine, whenever you may have state transitions it fits very well (I already used continue handles a lot in `gen_server`s and now I think they're the poor man's version of a gen_statem).

They're a behaviour, so we need to declare that, and they can operate in two different modes of callback, `state_function`s or `handle_event_function`s. We'll use handle_events mode, but to just cover the differences, in handle event mode you write every callback as `def handle_event(........) end` and the state can be as complex as you want. In `state_function` mode you write functions where the name of the function is the `state` to which it refers and the state must always be an atom. So if you had a `state` of `starting`, you would have a `def starting(.....) end` function(s) to handle all events when the state machine is in that state and other similarly named functions for each state you have. I personally like better the handle mode.

Our ideal server will accept a bunch of settings to decide how to start itself, it will have a start_link function that receives these options, then call a init function with these options that opens up the socket and starts the acceptors.

So let's go to our launchpad application, and open `/apps/launchpad/lib/launchpad.ex` and replace it with the following:

{% highlight elixir %}
defmodule Launchpad do
    @behaviour :gen_statem

    defstruct [:socket, :config, max_pool: 5, pool_count: 0, acceptors: %{}]
   
    def start_link(opts) do
        name = Map.get(opts, :name, {:local, __MODULE__})
    
        :gen_statem.start_link(name, __MODULE__, opts, [])
    end

    @impl true
    def callback_mode(), do: :handle_event_function

    @impl true
    def init(opts) do
        Process.flag(:trap_exit, true)
    
        port = Map.get(opts, :port, 4000)
    
        {:ok, socket} = :gen_tcp.listen(port, [:binary, {:packet, :raw}, {:active, true}, {:reuseaddr, true}])

    data = %__MODULE__{socket: socket, config: opts}
    
        {:ok, :starting, data, [{:next_event, :internal, :create_listener}]}
    end

    @impl true
    def handle_event(:internal, :create_listener, _state,
        %__MODULE__{
            socket: socket,
            config: config,
            max_pool: max,
            pool_count: pc,
            acceptors: acceptors
        } = data
    ) when pc < max do
    
        {:ok, pid} = Satellite.start_link(socket, Map.put(config, :number, pc))
        n_acceptors = Map.put(acceptors, pid, true)

    {:keep_state, %{data | pool_count: pc + 1, acceptors: n_acceptors}, [{:next_event, :internal, :create_listener}]}
    end

    def handle_event(:internal, :create_listener, :starting, data), do: {:next_state, :running, data, []}

    def handle_event(:internal, :create_listener, _state, _data), do: {:keep_state_and_data, []}

    def handle_event(:info, {:EXIT, pid, _reason}, _, %{pool_count: pc, acceptors: acceptors} = data) when :erlang.is_map_key(pid, acceptors) do
        {_, n_acceptors} = Map.pop(acceptors, pid)
        {:keep_state, %{data | pool_count: pc - 1, acceptors: n_acceptors}, [{:next_event, :internal, :create_listener}]}
    end

    def handle_event(:info, {:EXIT, pid, reason}, _, _data) do
        IO.puts("Received exit from unknown process #{inspect pid} with reason #{reason}")	
        {:keep_state_and_data, []}
    end

end
{% endhighlight %}

There's quite a lot going on there so let's break it up:

* `@behaviour :gen_statem` - we're simply stating that this module will implement the `:gen_statem` OTP behaviour
* `defstruct ....` - a structure we created to hold the relevant information about our server, it has a few keys, `:socket`, which will hold the socket ref once we open it, `:config`, which will store the configuration passed to it, `:max_pool`, which is the maximum number of acceptors we want to have running at each time, `:pool_count`, just a counter that holds how many acceptors we have running to decide if we need to start more or not, and `:acceptors` were we keep a map of our acceptors pids
* `def callback_mode()` - it's a required function for this behaviour and it informs what type of callbacks our `:gen_statem` will be running, in this case we set it to return `:handle_event_function`

The others are regular things, a `start_link/1` function that takes an argument (which will be a map/struct with configuration) a `init/1` function that the behaviour requires to start, in our case the argument will be the same as passed down to start_link, and then two `handle_event` functions, these are functions defining the behaviour of our :gen_statem and how it should act when there's either a state change or an event trigger.

In the `start_link/1`, for now, we will just try to get a key `:name` from it in order to register our process. In case no `name` is provided we default to `{:local, __MODULE__}`, which will register our process locally, on start, as `Launchpad` (that's what `__MODULE__` gets translated to).

So for `:gen_statem.start_link(name, __MODULE__, opts, [])`, the first argument is the name to register, the second is the module implementing the statem (in this case it's this module itself), the 3rd is the arguments passed to `init/1` and the 4th is a list of keyword options for the :gen_statem initialization itself, that we're leaving empty. We could also make `gen_statem` anonymous by excluding the 1st argument.

On the `init/1` function we're setting the process to trap exits by using `Process.flag(:trap_exit, true)`, the reason for this is because we'll be starting each acceptor as a link, so that if our "launchpad" process dies they are killed as well, but in turn, because links are bi-directional, this makes it so that if any of the acceptors dies our "launchpad" would too. Since we set it to trap exits, instead, if an acceptor dies, what will happen is we will receive a message telling us about it, this will allow us to "know" when one of them goes down in case we want to start another one to replace it, instead of it simply exiting automatically.


Then we read the port from the config or default to `4000` and next we start the actual socket process listening on that port, with a bunch of options through `:gen_tcp.listen`.

The options we pass are `:binary`, telling we want to read the socket as binary segments, we set `{:active, false}`, this means that we don't want to read data immediately whenever it arrives, we want to control how and when the reading is going to happen. :reuseaddr is for the socket to allow reusing the same address and `{:packet, :raw}`, tells to pass the data as read. If the `:gen_tcp.listen` succeeds it means we were able to bind to that socket port, otherwise this will raise an error, crash, and if the user of our server started it from a supervisor hopefully crash their app, so that they notice something is wrong.

Then we just create a struct for our data, with the previously opened socket and the config map passed to it.

The `init/1` callback has to return when successful an `{:ok, ...statem_definitions}` tuple, in our case:

`{:ok, :starting, data, [{:next_event, :internal, :create_listener}]}`

Means, set the internal initial state to `:starting` and the statem data as the contents of `data`. The last element is a list of actions specific to the :gen_statem behaviour that dictate any event it should trigger upon finishing this callback. In this case we say, we want to trigger a new event, a specific internal one that we can be sure is only triggered from our gen_statem, with the name `:create_listener`.


Hence the next `handle_event` functions we wrote. Each handle_event function receives as its arguments:

`type_of_event`, `event`, `currrent_state`, `current_data`

The type of event can be `:internal`, `:info`, `:call`, `:cast`, `:timeout`, etc... You can find all the documentation for gen_statem in [erlang documentation](http://erlang.org/doc/man/gen_statem.html) The event is the contents of that event, and the others are self explanatory. An `:internal` event type can only be issued by your own code inside the gen_statem where they happen, you can be sure these were generated by you.

On the first `handle_event` we pattern match on our struct specifically so we can access the value of `:max_pool` and `:pool_count`. This way we can know if the maximum number of acceptors are already running or not, and we do that by setting a guard clause, `when pc < max`. 

So if the pool_count is lower than the max we have set, then open a new one, otherwise the next function head that matches will be the one called - we have two, one if we're in the `:starting` state, which will just switch to the state `:running` while keeping everything else, and another one that matches whenever the state is not starting and the max acceptors have already been created, it does nothing by returning `{:keep_state_and_data, []}`, which is a shorthand for `{:next_state, :same_state, same_data, []}`.

When we create a new listener, we increment the pool count, we add that pid as a key to our `acceptors` map and we trigger the same event again.
So it repeats the listener creation until we have the max number of acceptors created and then sits waiting, doing a siesta.


Lastly we have a function to receive the `EXIT` message in case any of our linked processes dies, because we're trapping exits. When we receive one we decrease the pool count by one and  trigger a new event to create another listener. We know this exit is from an "acceptor" process because we use the guard `:erlang.is_map_key(acceptors, pid)`, basically saying only match this function when the pid on the `EXIT` message is a key on the map `acceptors`, meaning one of the pids from an acceptor we started (that we know because we placed them in the acceptors map as keys). If it's not we do a naive log of it because we should theoretically not have received that exit signal.

So our app is actually a library, this is because we don't want to control how it's started from itself, instead we expose the `start_link/1` function that then someone can start on their own supervisor trees. We'll see that later but, for now, lets see if it does something already.  Go to the root of the umbrella and start an iex shell, with:

`iex -S mix run`

You should see some warnings, and:

```
Erlang/OTP 22 [erts-10.4.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]

==> launchpad
Compiling 1 file (.ex)
warning: function Satellite.start_link/2 is undefined (module Satellite is not available)
  lib/launchpad.ex:39


19:02:57.776 [info]  Application logger started at :nonode@nohost
 
19:02:57.776 [info]  Application launchpad started at :nonode@nohost
```

We can see already a problem, we don't have a `Satellite.start_link/2` function neither a `Satellite` module, and if we try to start our `Launchpad` we see that error actually happening:

```
iex(1)> Launchpad.start_link(%{})
{:ok, #PID<0.151.0>}
iex(2)> 
19:05:14.065 [error] Process Launchpad (#PID<0.151.0>) terminating
** (UndefinedFunctionError) function Satellite.start_link/2 is undefined (module Satellite is not available)
    Satellite.start_link(#Port<0.8>, %{number: 0})
    .....
```
But the `Launchpad` did start and effectively tried to start one acceptor. Lets write the first completely barebones version of the acceptor.

Lets navigate again to `apps/` dir, and do:

`mix new satellite`

Now let's open `apps/satellite/lib/satellite.ex`

{% highlight elixir %}
defmodule Satellite do
  @behaviour :gen_statem
  
  defstruct [:socket, :config, :conn, pipeline: [], request: %{}]
   
  def start_link(socket, opts) do
    :gen_statem.start_link(__MODULE__, {socket, opts}, [])
  end

  @impl true
  def callback_mode(), do: :handle_event_function
  
  @impl true
  def init({socket, config}) do
    {:ok, :waiting,  %__MODULE__{socket: socket, config: config}, [{:next_event, :internal, :wait}]}
  end

  @impl true
  def handle_event(:internal, :wait, :waiting, %{socket: socket} = data) do
    {:ok, conn} = :gen_tcp.accept(socket)
    
    :gen_tcp.controlling_process(conn, self())

    {:next_state, :parsing, %{data | conn: conn}, [{:next_event, :internal, :read}]}
  end

  def handle_event(:internal, :read, :parsing, %{conn: conn} = data) do
    case :gen_tcp.recv(conn, 0, 1000) do
      {:ok, packet} ->
        IO.inspect(packet, label: "received")
        n_data = %{data | request: packet}
        {:next_state, :response, n_data, [{:next_event, :internal, :send_response}]}

      {:error, reason} ->
        IO.inspect(reason, label: "error on recv")
        {:keep_state, data, [{:next_event, :internal, :close}]}
    end
  end

  def handle_event(:internal, :send_response, :response, %{conn: conn, request: request} = data) do
     b = :erlang.iolist_to_binary(request)
     response = :io_lib.fwrite(
       "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
       [:erlang.size(b), b]
     )

     :gen_tcp.send(conn, response)

     {:keep_state, data, [{:next_event, :internal, :close}]}
  end

  def handle_event(:internal, :close, _, %{conn: conn} = data) do
    :gen_tcp.close(conn)
    {:next_state, :waiting, %{data | conn: nil, request: %{}}, [{:next_event, :internal, :wait}]}
  end

end
{% endhighlight %}

This is nowhere close to what we want, but it will allow us to already receive http requests. Lets see that and then go through the code. Go to the root of your umbrella on the terminal and again run `iex -S mix run`

Inside the shell now do:

`Launchpad.start_link(%{})`

You should see `{:ok, #PID....}`

Now open a browser and visit `http://localhost:4000`.

Voilá, we have a "server" listening on port 4000. It doesn't do really anything, it just echoes whatever was the request as a `text/html` response. 

To confirm that indeed we have 5 acceptors currently running, let's change this line:

{% highlight elixir %}
def handle_event(:internal, :read, :parsing, %{conn: conn} = data) do
    case :gen_tcp.recv(conn, 0, 1000) do
      {:ok, packet} ->
        IO.inspect(packet, label: "received")
        n_data = %{data | request: packet}
        {:next_state, :response, n_data, [{:next_event, :internal, :send_response}]}

      {:error, reason} ->
        IO.inspect(reason, label: "error on recv")
        {:keep_state, data, [{:next_event, :internal, :close}]}
    end
end
{% endhighlight %}

To: 

{% highlight elixir %}
def handle_event(:internal, :read, :parsing, %{conn: conn, config: %{number: number}} = data) do
    case :gen_tcp.recv(conn, 0, 1000) do
      {:ok, packet} ->
        IO.inspect(packet, label: "received")
        n_data = %{data | request: packet <> "\n\nAcceptor number: #{number}"}
        {:next_state, :response, n_data, [{:next_event, :internal, :send_response}]}

      {:error, reason} ->
        IO.inspect(reason, label: "error on recv")
        {:keep_state, data, [{:next_event, :internal, :close}]}
    end
end
{% endhighlight %}

Save the file and on the iex shell run `recompile()`.

Now visit the browser and refresh the page some times. You should see the acceptor changing as you do. Which means that each request you're sending from the browser is being answered by a different process altogether.

If you look in the shell you should also be seeing the output from the `IO.inspect` functions we placed.

So this string you're seeing will vary depending on the browser, but it will have a structure that is the same. Using Safari I see this:

```
GET / HTTP/1.1\r\nHost: localhost:4000\r\nConnection: keep-alive\r\nCache-Control: max-age=0\r\nUpgrade-Insecure-Requests: 1\r\nUser-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.132 Safari/537.36\r\nSec-Fetch-Mode: navigate\r\nSec-Fetch-User: ?1\r\nDNT: 1\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3\r\nSec-Fetch-Site: none\r\nAccept-Encoding: gzip, deflate, br\r\nAccept-Language: pt-PT,pt;q=0.9,en-US;q=0.8,en;q=0.7\r\n\r\n"
received: "GET /favicon.ico HTTP/1.1\r\nHost: localhost:4000\r\nConnection: keep-alive\r\nPragma: no-cache\r\nCache-Control: no-cache\r\nSec-Fetch-Mode: no-cors\r\nDNT: 1\r\nUser-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.132 Safari/537.36\r\nAccept: image/webp,image/apng,image/*,*/*;q=0.8\r\nSec-Fetch-Site: same-origin\r\nReferer: http://localhost:4000/\r\nAccept-Encoding: gzip, deflate, br\r\nAccept-Language: pt-PT,pt;q=0.9,en-US;q=0.8,en;q=0.7\r\n\r\n
```

There's a whole bunch of documents detailing exactly how an HTTP request (on the various versions of the protocol) must be constructed, what it can contain, how those things are encoded and the same for the HTTP responses. We'll not read the RFC for the HTTP Protocol but basically implement the logic for parsing these requests and deliver a response on a more lightway approach.

We can see that the request is basically:

VERB `whitespace` PATH `whitespace` PROTOCOL/VERSION\r\nHEADERS\r\n\r\n

where `\r\n` is the carriage return escape sequence.

After the headers there would be a body but in this case since it's a get request there's none. In case of having a body the request should include a Content-Length header stating the size of the body although it's not totally required that it has. 

So if we approach this from the point of view of creating a pipeline, basically we would need to:

* Parse the Verb
* Parse the Path
* Parse the Protocol
* Parse the headers
* Maybe Parse the Body if it has one
* Dispatch to the appropriate route

On each step we can then store the information we parsed so that we can later decide how to respond to it, which we'll do by dispatching that request into an appropriate handler.

We should read the RFC to make sure our server is completely correct, but like we said before, this is a lightweight version so we'll just read the basics from that request to decide what we need.

Verb, Path and Protocol are each one separated by a white space. Headers are separated from the previous section and between them by `\r\n`, meaning carriage return, a set of two characters, `\r` return and `\n` newline, each header key ends with `:` followed by a white space, after which comes the respective value. 

The end of the headers is separated by double carriage return, `\r\n\r\n`. After this is the body and this can be formatted in different ways depending on the type of request.

There's many ways we can implement the parsing part, but we want it to be modular, so that anybody can hook their own things into a pipeline if they want, or change it completely, the idea is the basic pipeline will parse everything needed and then dispatch the request. We will want to implement some control on maximum length of the requests (or allow that even if by default it accepts anything). We want the pipeline to be halt-able, meaning that we can trigger an halt between any actions. We don't want to have explicit hardcoded behaviour, we want each step to be a call to a function that can be customised.

We will also want to have different kinds of actions, we'll want to have parsing/reading actions, that get part of the data from the socket and do something with it (parse it and if needed store it in the context of that specific request), we want to have actions that simply change the form of the data according to something in a certain place when they occur in the pipeline and we want actions that check the current state of what we have parsed/stored to decide if to continue parsing, halt, error, or dispatch. 

One problem we will always run into is that the HTTP protocol doesn't require a request to say how many bytes it is in total, so we can't know how many bytes to read from the socket until we figure the end of the request. To add to that, we have the fact that we might read incomplete requests due to the buffering on the socket at the os level (and subsequent transmission to the Erlang VM). Most of the times the socket when read will contain all the data, but on big length requests we might need to read more than once from it.

When we set the socket to `{:active, false}` we explicitly told that we will somehow read the socket by ourselves. The reason to do this is because if we say `{:active, true}` we forego any possibility of applying backpressure to the data being sent and read from the socket, and someone could potentially flood our server with gigantic requests, possibly crashing it down in the process.

By saying `{:active, false}` the BEAM will not automatically send that data, it will be in the socket buffer, waiting to be read at which point we need to manually request a reading on it. One normal way of doing it is after we set the acceptor and pass the controlling process to the one that will receive the request, to set the socket to `{:active, :once}`, which means it will process once the data currently in the buffer (emptying it so at the os level the client tcp connection can push more data into it), send it as a message to the controlling process, and then switch automatically to `{:active, false}` again.

This has the benefit of allowing us to apply backpressure, it's like opening a faucet once for a little while and immediately closing it again. If we need to read more we repeat the same.

Other option is by using `:gen_tcp.recv`. In this way you can control how many bytes to read along with a timeout for it and instead of receiving that data as a message, you get the result of the read as the return of the function with `{:ok, data}`, or an `{:error, reason}` if there was some error (the socket was closed on the client for instance), or `:timeout` if the amount of bytes specified couldn't be read in that given time frame. `:gen_tcp.recv/3` takes the accepted socket from where to read as its first argument, the number of bytes to read as its second argument and the timeout as its third argument. 

One thing to notice is that, if we specify a number that is not zero, then this function will only return after having read EXACTLY that number of bytes. In this case, this is not very useful, because we don't know beforehand how many bytes there will be in total, so unless we read a single byte at a time we would always end up timing out. Reading one byte at a time is not efficient in any way either. But if we specify the number as zero (0) then `recv` will act like `{:active, :once}`, in that it reads the whole available buffer in one go, and then returns that data. If we need to read more we call again `recv` and again we get whatever is in the buffer up to that point and so on. This again allows us to manually control the backpressure and the flow of data (we can even when creating the socket say what's the size we want for our buffer and other things but we'll not cover that). 

(sidenote: you might ask then why is there an option to specify the number of bytes and not only 0, and the reason is, if you control the client (it's an application you wrote) or you have a defined protocol where you can specify the length, then you can optimise the readout to your specific system, network and even the client application flow. Of course when dealing with HTTP requests, that's not the case)

We can then implement a way of limiting the maximum number of bytes a request might have before we close the connection. We'll see that in more detail when we get to it. Now lets start implementing our basic request parser.


First we'll need to define a structure that will hold the parsed information in an organised way, so that we can check it. We also need to define a structure to hold the `configuration` information.

For the request lets create another module in the satellite app, `apps/satellite/lib/satellite_request.ex` . Open the file and add to it:


{% highlight elixir %}
defmodule Satellite.Request do

  defstruct [
    :verb,
    :protocol,
    :version,
    :accept,
    :body,
    path: [],
    host: [],
    query: %{},
    headers: %{},
    finished_headers: false,
    halt: false,
    params: %{},    
 ]
  
end
{% endhighlight %}

The `:query`, `:headers` and `:params` field are maps, because they're key-value elements. `:path`  and `:host` are defined as lists although the url requested and the host will be strings if we want to dispatch on certain paths, match parts of the path, etc, and the same for the host/domain/port, we will need to decompose it into individual elements in order to do so. The remaining keys will either be booleans or strings/atoms that we will fill as we move through the pipeline.

Save that file.

Now let's create one for the configuration. The previous request structure is only used by the acceptors gen_statem and possibly any lib that depends on our whole application. On the other hand the config is used by the `launchpad` and `satellite`. So we will create a new app/lib to store these shared elements. We haven't added any dependencies to our apps until now but we will afterwards.

Go to the `apps` folder on the terminal again, and execute `mix new satellite_shared`.

Now create and open a file at `apps/satellite_shared/lib/satellite_configuration.ex`

Place inside it the following:

{% highlight elixir %}
defmodule Satellite.Configuration do

  defstruct [
    :pipeline,
    :name,
    :router,
    port: 4000,
    acceptors: 5,
    max_size: 50_000,
    keep_full_request: false,
   
 ]

end
{% endhighlight %}

Now open `apps/launchpad/mix.exs` and replace the `deps` function by:

{% highlight elixir %}
defp deps do
    [
      {:satellite, in_umbrella: true},
      {:satellite_shared, in_umbrella: true}
    ]
end
{% endhighlight %}

Open the `apps/satellite/mix.exs` and replace the `deps` function by:

{% highlight elixir %}
defp deps do
    [
      {:satellite_shared, in_umbrella: true}
    ]
end
{% endhighlight %}

The reason we were able to run previously without specifying these dependencies is because `mix` will compile and load all applications and modules in the umbrella, but if we were to assemble a release, then the compilation would fail, because the dependencies weren't specified.

Basically we're now saying `launchpad` depends on both the app `satellite` and `satellite_shared` and the app `satellite` depends on `satellite_shared`. 

Ok, so now let's think about how we want to actually have this working. (I've done the thinking for you but I'll pretend it's fresh).

I'll throw in that one neat way of having it defined is as a list of things, so that we can move through it as we go. Each element of this list should have the type of action it is, the name for that particular step, which function to call for doing the step and also, because we might get things in several receives, an accumulator specific for that step. So say we want to encode the step for parsing the verb out of the request, given the 4 elements we mentioned previously, it could look like this:

`{:read, :verb, &Satellite.Verb.parse/3, <<>>}`

So the action would be of the type `:read`, the name of the step would be `:verb`, the function that would execute this step would be `Satellite.Verb.parse/3` and the accumulator would be an empty binary. 

Let's start writing it, first create another file in the satellite lib, `apps/satellite/lib/satellite_defaults.ex` and write in it:

{% highlight elixir %}
defmodule Satellite.Defaults do
  
  def default_pipeline() do
    [
      {:read, :verb, &Satellite.Verb.parse/3, <<>>}
    ]
  end
end
{% endhighlight %}

Save it. We'll add the others as we go. Now we need to create a module `Satellite.Verb`. So lets create another file `apps/satellite/lib/satellite_verb.ex` and put in it:

{% highlight elixir %}
defmodule Satellite.Verb do

  def parse(ctx, rem, <<"GET">>), do: {:done, %{ctx | verb: :get}, rem}

  def parse(ctx, rem, <<"POST">>), do: {:done, %{ctx | verb: :post}, rem}

  def parse(ctx, <<>>, acc), do: {:cont, ctx, acc}

  def parse(ctx, <<?\s, rem::bits>>, acc), do: parse(ctx, rem, acc)

  def parse(ctx, <<h, rem::bits>>, acc), do: parse(ctx, rem, <<acc::bits, h>>)

  def parse(ctx, _, _), do: {:error, "Parsing Verb"}
  
end
{% endhighlight %}

Here the idea is very simple, we create a recursive function. First this function will be called the first time with `(ctx, the_request_binary_from_the_socket, <<>>)` because the initial accumulator is `<<>>` as set in our pipeline definition. So it won't match the first 4 functions, but will match the 5th, were we pluck 1 single character from it, add call again parse with the remaining of the packet, while adding that single character we removed to the accumulator. 

It repeats this until the accumulator is `<<"GET">>` or `<<"POST">>`, at which point we consider it done because we have a matching verb (right now it will fail for any other verb). If there was a white-space somewhere, it would ignore it and move on to the next character, but because it will assemble GET or POST before that it's never called, anyway, I'm leaving it there just because.

Most parsing will be similar to this, but it will have its own peculiarities. This should work even if for instance we were reading 1byte at a time from the socket, because in that case, we would read the first, rendering an accumulator of `<<"G">>`, and an remaining of `<<>>`, which would match the 3rd function and in that case return `{:cont, ctx, acc}`, which would trigger a new read from the socket and consequent parsing.

Now how do we make our satellite actually work with this? So one thing we can do is treat our state of the gen_statem as the current step in the pipeline. With pattern matching we can describe this very elegantly, because if the pipeline is a list, we can simply pluck the first element from it and store it as the current state, and every time it moves we pluck another one, until we have no more steps to do, at which point we know we've finished the pipeline and parsed the full request. The complete story is a bit different because of the specifics of http requests, but that's the overall idea, we'll adjust as we need while going through it. So now we need to change how the `launchpad` is started, so that it uses our recent `Satellite.Configuration` struct.

Add this to `apps/launchpad/lib/launchpad.ex`

{% highlight elixir %}
  alias Satellite.Configuration, as: Config


  defp set_defaults(config) do
    maybe_set_name(config)
    |> maybe_set_port()
    |> maybe_set_pipeline()
  end
  
  defp maybe_set_name(%Config{name: name} = config), do: maybe_set(:name, name, {:local, __MODULE__}, config)

  defp maybe_set_port(%Config{port: port} = config), do: maybe_set(:port, port, 4000, config)

  defp maybe_set_pipeline(%Config{pipeline: pipeline} = config) do
	maybe_set(:pipeline, pipeline, Satellite.Defaults.default_pipeline(), config)
  end

  defp maybe_set(key, nil, default, config), do: Map.put(config, key, default)
  defp maybe_set(_, _, _, config), do: config
{% endhighlight %}

And change `start_link` and `init` to the following:

{% highlight elixir %}
def start_link(%Config{} = config) do
    %{name: name} = ok_config = set_defaults(config)
    
    :gen_statem.start_link(name, __MODULE__, ok_config, [])
end

def init(%{port: port} = config) do
    Process.flag(:trap_exit, true)
    
    {:ok, socket} = :gen_tcp.listen(port, [:binary, {:packet, :raw}, {:active, false}, {:reuseaddr, true}])

    data = %__MODULE__{socket: socket, config: config}
    
    {:ok, :starting, data, [{:next_event, :internal, :create_listener}]}
end
{% endhighlight %}

We created some helper functions to set defaults in case it isn't passed into and also changed the `start_link` and `init` to use that. If there's no `:name`, `:port` or `:pipeline` set in the config it will set them to some default. Because we're now matching explicitly on the `%Config{}` struct in `start_link/1`, if you pass anything else it will fail with an unmatched function call.

Now we need to change the `satellite.ex` file, to use the pipeline and create the `handle_event` functions that will coordinate that.

I'm going to past the full file as it should look and we go through the changes:

{% highlight elixir %}
defmodule Satellite do
  @behaviour :gen_statem

  alias Satellite.Request

  defstruct [:socket, :config, :conn, pipeline: [], request: %Request{}]
  
  @impl true
  def callback_mode(), do: :handle_event_function
  
  def start_link(socket, opts) do
    :gen_statem.start_link(__MODULE__, {socket, opts}, [])
  end
  
  @impl true
  def init({socket, config}) do
    {:ok, :waiting,  %__MODULE__{socket: socket, config: config}, [{:next_event, :internal, :wait}]}
  end

  @impl true
  def handle_event(:internal, :wait, :waiting, %{socket: socket, config: %{pipeline: pipeline}} = data) do
    {:ok, conn} = :gen_tcp.accept(socket)
    
    :gen_tcp.controlling_process(conn, self())

    n_data = %{data | conn: conn, pipeline: pipeline, request: %Request{}}

    set_next_step(n_data, <<>>)
 end

  def handle_event(:internal, :read, _, %{conn: conn} = data) do
    case :gen_tcp.recv(conn, 0, 1000) do
      {:ok, packet} ->
        {:keep_state_and_data, [{:next_event, :internal, {:parse, packet}}]}

      {:error, reason} ->
        {:next_state, :response, %{data | request: "Error: #{inspect reason}"}, [{:next_event, :internal, :send_response}]}
    end
  end

  def handle_event(:internal, {:parse, packet}, {type, name, fun, acc}, %{request: request} = data) do

    case fun.(request, packet, acc) do
      {:cont, n_request, n_acc} ->
        {:next_state, {type, name, fun, n_acc}, %{data | request: n_request}, [{:next_event, :internal, :read}]}

      {:done, n_request, remaining} ->
        set_next_step(%{data | request: n_request}, remaining)

      {:error, reason} ->
        {:next_state, :response, %{data | request: "#{inspect reason}"}, [{:next_event, :internal, :send_response}]}
    end
    
  end

  def handle_event(:internal, :send_response, :response, %{conn: conn, request: request} = data) do
     
     :gen_tcp.send(conn, make_response(request))

     {:keep_state, data, [{:next_event, :internal, :close}]}
  end

  def handle_event(:internal, :close, _, %{conn: conn} = data) do
    :gen_tcp.close(conn)
    {:next_state, :waiting, %{data | conn: nil}, [{:next_event, :internal, :wait}]}
  end


  defp set_next_step(%{pipeline: [h | t]} = data, remaining) do

    event = case remaining do
              <<>> -> :read
              _ -> {:parse, remaining}
            end

    {:next_state, h, %{data | pipeline: t}, [{:next_event, :internal, event}]}
  end

  defp set_next_step(%{pipeline: [], request: request} = data, _remaining) do

    n_request = "#{inspect request}"
    
    {:next_state, :response, %{data | request: n_request}, [{:next_event, :internal, :send_response}]}
  end

  defp make_response(request) do
    b = :erlang.iolist_to_binary(request)
    :io_lib.fwrite(
      "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
       [:erlang.size(b), b]
    )
  end
end
{% endhighlight %}

So first, we changed the `defstruct` to hold a `%Satellite.Request{}` (we aliased the module so we can write `%Request{}` instead) by default instead of an empty map.

Then the `init` is the same as before, but the `:wait` event while `:waiting` is different. Now, because we know we have the pipeline in the `%Config{}` structure, we take out that through pattern matching and then after having made a connection we create a new structure with the `:conn`, the `:pipeline` value and an empty `%Satellite.Request{}` as the `:request`, we then, instead of returning the statem tuple, call the `set_next_step` function, which takes a `Satellite` struct, and the bytes read from the socket as the 2nd argument. In this case, since we're just starting the pipeline we don't have any bytes, so we pass an empty binary `<<>>`. 

This function then pattern matches on the `:pipeline` key and sees if we have at least one element there, and in case we do, it means we haven't finished the pipeline yet. In that matching function we also check the already read data we have available to parse and depending wether it's empty (`<<>>`) or not, we set the next event for the state machine. This event will either be `{:parse, data}` or `:read` in case we don't have any data to parse. First time this is called it will obviously be `:read` because we passed `<<>>`.

In case we have no other elements in the pipeline, instead, we know we have reached the end of the pipeline and as such should send a response back. This function will change of course, for now it just does an inspect on the current `request` struct so that it becomes a stringified version, and then sets the next event to `:send_response`.

On the first iteration though we have a pipeline element, we will have a single one, so the function match will look like this:

`set_next_step(%{pipeline: [h | t]} = data, remaining)`

Where `h` will be `{:read, :verb, &Satellite.Verb.parse/3, <<>>}` and `t` will be `[]`.

Because remaining is `<<>>`, event will be `:read`.
So the final return from the `set_next_step` will be:

`{:next_state, {:read, :verb, &Satellite.Verb.parse/3, <<>>}, %{data | pipeline: []}, [{:next_event, :internal, :read}]}`

This shows how cool is the `:handle_event_function` type of callbacks, due to that we can have complex states as this one, which allows us to describe in very clean steps what should happen when such states have to process new events.

This means that now, it will trigger the event `:read` that doesn't care about what state it's in, and just does what we were doing before by reading from the socket. When it reads then it sets the next event to be `{:parse, data_that_was_read}`, and now this will trigger the next event were we parse and - in this case - we do care about what state we're in because it contains the function we need to call to handle this step.

We had set the state to be `{:read, :verb, &Satellite.Verb.parse/3, <<>>}`, which means the accumulator starts empty. We also used the `&` notation to capture that function, because what we want is a reference to a given function so that we can then apply it. We do that by calling
`fun.(request, packet, acc)`. The first argument is the current `%Request` struct as it is, the second is what was read from the socket up till now, and the third is the accumulator, which starts empty. 

This function should return one of 3 possible outcomes (we would write a proper behaviour for this but now we'll skip it):

* `{:cont, new_request_struct, new_acc}`

this return means that that given function was unable to finish with the current data retrieved from the socket, so we should ask to read more from the socket, it replaces the state of the `gen_statem` by almost the same as it is currently, with exception of the `acc`, which gets replaced by the `new_acc` returned from the function. It also replaces the `:request` key in the gen_statem's data, in case the `new_request_struct` has changed. When we get this event, the next event will be `:read` and afterwards it will trigger this event again, with new data read from the socket. The `new_request_struct` might or may not have changed, but to simplify logic we just replace it.

* `{:done, new_request_struct, remaining}`

this return means, the function was able to according to its logic finish all it had to do, so this step can be considered complete, lets move to the next one. It includes the `new_request_struct` and any bytes that weren't parsed. This calls `set_next_step` which will follow the logic we already discussed, placing the next step in the pipeline as the current. In this case we might or may not have any remaining bytes available, and `set_next_step` will decide accordingly what is the next event to trigger

* `{:error, reason}`
this is self explanatory - right now it just translated whatever is the error reason into a string and jumps right to the `:send_response` event. We will further down change this and the `:send_response` steps as well

We will also need to add proper error catching but everything at its time.

If we look at what we have right now, we should be able to see some response already containing our `%Satellite.Request{}` struct and the proper verb parsed and included in it. Lets recompile / restart the iex shell, and do now:

` Launchpad.start_link(%Satellite.Configuration{})`

You should see again the familiar `{:ok, pid...}`
Now lets open up a browser and visit again http://localhost:4000 - you should be greeted by:

```
%Satellite.Request{accept: nil, finished_headers: false, halt: false, headers: %{}, host: nil, params: %{}, path: [], protocol: nil, query: %{}, verb: :get, version: nil}
```

And as you can see, the `:verb` key is set to `:get`. Very nice very good, great success!

Let's go adding one by one the remaining parsing steps, open `apps/satellite/lib/satellite_defaults.ex` and replace it by:

{% highlight elixir %}
defmodule Satellite.Defaults do
  
  def default_pipeline() do
    [
      {:read, :verb, &Satellite.Verb.parse/3, <<>>},
      {:read, :path, &Satellite.Path.parse/3, {false, [], <<>>}}
    ]
  end
  
end
{% endhighlight %}

Now let's create the `Satellite.Path` module, in `apps/satellite/lib/satellite_path.ex`. This one is going to be more complex because we want to separate the path into it's individual constituents, so if someone would visit `http://localhost:4000/basic/path` our final path list would look like `["basic", "path"]` and because a path might include also query strings, in the form of `?key=value&key2=value2` we need to do more work, and in case there is query string we need to place it on the `:query` key of the `%Request{}` struct.

The reason we want to do this will become clearer when we design the dispatching mechanism, but it will allow us to do some cool stuff with it.

{% highlight elixir %}
defmodule Satellite.Path do

  def parse(ctx, <<>>, acc), do: {:cont, ctx, acc}
  def parse(ctx, <<?\s, rem::bits>>, {false, _, _} = acc), do: parse(ctx, rem, acc)

  def parse(ctx, <<?\s, rem::bits>>, {true, acc, prior}) do
    n_acc = maybe_add_prior(acc, prior)
    
    n_ctx = %{
      ctx |
      path: Enum.reverse(
        List.flatten(n_acc)
      )
    }

    {:done, n_ctx, rem}
  end

  def parse(%{query: q} = ctx, <<?\s, rem::bits>>, {:query, :value, key, value}) do
    {:done, %{ctx | query: Map.put(q, key, value)}, rem}
  end

  def parse(ctx, <<?\s, rem::bits>>, _), do: {:done, ctx, rem}

  def parse(ctx, <<?=, rem::bits>>, {:query, :key, key}) do
    parse(ctx, rem, {:query, :value, key, <<>>})
  end

  def parse(ctx, <<h, rem::bits>>, {:query, :key, key}) do
    parse(ctx, rem, {:query, :key, <<key::binary, h>>})
  end

  def parse(%{query: q} = ctx, <<?&, rem::bits>>, {:query, :value, key, value}) do
    parse(%{ctx | query: Map.put(q, key, value)}, rem, {:query, :key, <<>>})
  end

  def parse(ctx, <<h, rem::bits>>, {:query, :value, key, value}) do
    parse(ctx, rem, {:query, :value, key, <<value::binary, h>>})
  end

  def parse(ctx, <<?/, rem::bits>>, {_, acc, prior}) do
    n_acc = maybe_add_prior(acc, prior)
    parse(ctx, rem, {true, n_acc, <<>>})
  end

  def parse(ctx, <<??, rem::bits>>, {:true, acc, prior}) do
    n_acc = maybe_add_prior(acc, prior)
    n_ctx = %{
      ctx |
      path: Enum.reverse(
        List.flatten(n_acc)
      )
    }

    parse(n_ctx, rem, {:query, :key, <<>>})
  end

  def parse(ctx, <<h, rem::bits>>, {_, acc, prior}) do
    parse(ctx, rem, {true, acc, <<prior::binary, h>>})
  end

  def parse(_ctx, _, _), do: {:error, "Parsing path"}

  defp maybe_add_prior(acc, <<>>), do: acc
  defp maybe_add_prior(acc, prior), do: [prior | acc]
  
end
{% endhighlight %}

We won't go through all the logic in this one, but suffice to say that, it will parse bytes until it reads a `"/"`, at that point it will look to see if it has accumulated anything, in case it has it puts that as one element of the paths list. It does that until it either finds a whitespace or a `?` character. If it finds a `?` character it means it's expecting query parameters, so it changes its own internal acc state to now match on first extracting the value of the key for that query parameter, and once finding an `=` sign, switching to finding the value for that. Once it does it places those under the `:query` key on the context map (that is the `%Satellite.Request{}` struct), and it does this until again, it finds a white space. 

When it finds a white space it considers this step complete. Now you know why URI's can't contain white-spaces and instead use either `+` or `%20`, imagine having to deal with that ...

Now our implementation doesn't deal with everything, it's definitively not RFC compliant, anyway, it will work mostly fine for what we want, and you can extend it by yourself, how the remaining works is left as an exercise.

Let's quit the shell and restart it, then run again:

`Launchpad.start_link(%Satellite.Configuration{})`

And this time visit `http://localhost:4000/some/path?query1=val1`

You should see now:

```
%Satellite.Request{accept: nil, finished_headers: false, halt: false, headers: %{}, host: nil, params: %{}, path: ["some", "path"], protocol: nil, query: %{"query1" => "val1"}, verb: :get, version: nil}
```

So even more success! It correctly parsed the path and separated it into each individual segment and also the query string!

Next is the `Satellite.Conn`, create the file at `apps/satellite/lib/satellite_conn.ex` and place inside it:

{% highlight elixir %}
defmodule Satellite.Conn do

  def parse(ctx, rem, <<"HTTP/1.1", ?\r, ?\n>>), do: {:done, %{ctx | version: {1, 1}}, rem}
  def parse(ctx, rem, <<"HTTP/1.0", ?\r, ?\n>>), do: {:done, %{ctx | version: {1, 0}}, rem}

  def parse(ctx, <<>>, acc), do: {:cont, ctx, acc}

  def parse(ctx, <<?\s, rem::bits>>, acc), do: parse(ctx, rem, acc)
  def parse(ctx, <<h, rem::bits>>, acc), do: parse(ctx, rem, <<acc::binary, h>>)

  def parse(ctx, _, _), do: {:error, "Parsing Conn"}
end
{% endhighlight %}
Again we're not dealing with everything possible here, but for now it will work.

Set the `satellite_defaults.ex` to:

{% highlight elixir %}
defmodule Satellite.Defaults do
  
  def default_pipeline() do
    [
      {:read, :verb, &Satellite.Verb.parse/3, <<>>},
      {:read, :path, &Satellite.Path.parse/3, {false, [], <<>>}},
      {:read, :conn, &Satellite.Conn.parse/3, <<>>}
    ]
  end
end
{% endhighlight %}

You can restart the shell, start again the `launchpad` and visiting localhost should now show you the version as well in the struct.

Now we'll move to the headers part, add the following line to the `default_pipeline()` (don't forget to add a comma on the previous last line)

`{:read, :headers, &Satellite.Headers.parse/3, {:header, <<>>}}`

And let's create the `apps/satellite/lib/satellite_headers.ex` file, and place this on it:
	
{% highlight elixir %}
defmodule Satellite.Headers do

  import Satellite.Shared, only: [downcase: 1]

  def parse(ctx, <<>>, acc), do: {:cont, ctx, acc}
  def parse(ctx, <<?:, rem::bits>>, {:header, key}), do: parse(ctx, rem, {:value, key, <<>>})
  def parse(ctx, <<?\s, rem::bits>>, {:header, key}), do: parse(ctx, rem, {:header, key})
  def parse(ctx, <<?\s, rem::bits>>, {:value, key, acc}), do: parse(ctx, rem, {:value, key, acc})

  def parse(ctx, <<h, rem::bits>>, {:header, key}) do
    n_h = downcase(h)
    parse(ctx, rem, {:header, <<key::binary, n_h>>})
  end

  def parse(ctx, <<?\r, rem::bits>>, {:header, _}) do
    parse(ctx, rem, {:in_termination, <<?\r>>})
  end

  def parse(ctx, <<?\n, rem::bits>>, {:in_termination, <<?\r>>}) do
    parse(ctx, rem, {:in_termination, <<?\r,?\n>>})
  end

  def parse(ctx, <<?\r, rem::bits>>, {:in_termination, <<?\r, ?\n>>}) do
    parse(ctx, rem, {:in_termination, <<?\r,?\n,?\r>>})
  end

  def parse(ctx, <<?\n, rem::bits>>, {:in_termination, <<?\r, ?\n, ?\r>>}) do
    {:done, %{ctx | finished_headers: true}, rem}
  end

  def parse(ctx, <<h, rem::bits>>, {:in_termination, <<?\r, ?\n>>}) do
    n_h = downcase(h)
    parse(ctx, rem, {:header, <<n_h>>})
  end

  def parse(%{headers: headers} = ctx, <<?\r, rem::bits>>, {:value, key, acc}) do
    value = translate_header_content(key, acc)
    n_ctx = %{ctx | headers: Map.put(headers, key, value)}
    parse(n_ctx, rem, {:in_termination, <<?\r>>})
  end


  def parse(ctx, <<h, rem::bits>>, {:value, key, acc}) do

    n_h = case downcased_header?(key) do
            true -> downcase(h)
            _ -> h
          end
    
    parse(ctx, rem, {:value, key, <<acc::binary, n_h>>})
  end

  def parse(_ctx, _, _), do: {:error, "Parsing headers"}

  def translate_header_content(<<"content-length">>, val) when is_binary(val) do
    :erlang.binary_to_integer(val)  
    catch _ -> 0
  end

  def translate_header_content(_, val), do: val


  def downcased_header?(<<"content-type">>), do: true
  def downcased_header?(<<"accept">>), do: true
  def downcased_header?(_), do: false
end
{% endhighlight %}

Before doing some explanations, lets create/rewrite the `apps/satellite_shared/lib/satellite_shared.ex` (it should be there unless you deleted it), and place this inside of it:

{% highlight elixir %}
defmodule Satellite.Shared do

  use Bitwise, only_operators: true
  
  Enum.each(?A..?Z, fn(value) ->
    downcased = value ^^^ 32
    def downcase(unquote(value)), do: unquote(downcased)
  end)

  def downcase(val), do: val
end
{% endhighlight %}

So this module uses the `Bitwise` macros, we specify we only want the `operators` type, which are in the form of `^^^` (which is the same as using the `Bitwise.bxor(a, b)`). 

We use some simple macro magic to basically, at compile time, create a list of functions for all letters that can be uppercase that just return their downcased counterpart. We have then a catch all that just returns the arg passed into it as it is, because if it's being called the reason is the arg isn't an uppercased letter.

In erlang strings are lists of integers that are represented by their ascii code, so for instance `'A'` is actually a list with one element, the integer `65`, as in `[65]`. When erlang sees a list that is composed only of printable ascii characters it actually prints them in the shell as characters! So if you write on the shell `[65, 66, 67]`, you'll see `'ABC'`.

The elixir operator prefix `?` before a character actually gives us the numeric code for that character. If you do `?A` you'll see `65`. So if you use a range such as `?A..?Z` what will happen is that the range will be `65..90`, which encompasses all the regular uppercase letters in the alphabet.

The ascii table design also has a very neat property when it comes to the alphabet letters, they're placed in such a way, that if you flip one single bit in them they become the other case, and that's what `value ^^^ 32` does, so if `?A` is 65, when you do that bitwise operation of `bxor 32` on it it becomes 97, which is the ascii code for `a` so effectively you downcased the character.

Knowing this we can see that basically we define all these functions with those 5 lines of code.

```
def downcase(65), do: 97
def downcase(66), do: 98
... all other integers in the range
def downcase(90), do: 122
def downcase(something_else), do: something_else
```

And there's it! A downcase function very succintly written that doesn't do any string work, assembled at compile time. Now, this only works with ascii letters, but that's fine, because that's what we need when parsing a request at this level. We would need to add other properties for when possibly parsing url encoded characters that aren't ascii, but that is left as an exercise.

You can now also see why we used `<<?some_character, ....>>` in the pattern matching, basically it gives us the numeric code of the character and because binaries are at their inner structure integer sequences it works. 

The headers `parse` function on the other hand is again following the same logic as the previous ones, it starts with an accumulator of `{:header, <<>>}`, and adds characters to the `<<>>` until it runs into a `:`, which signifies that the key for the header is finished, and then switches the accumulator to `{:value, previously_parsed_header, <<>>}` which now accumulates characters until it runs into `\r` which will be the ending sequence, so we change the acc to be `:in_termination` mode, when followed by `\n` indicates the header and its value has finished, if it's followed by another `\r\n` sequence then it means all headers have been parsed, if it's not we know we're in another header key and we change to start accumulating the new header key and then value and then entering again the termination sequence until we hit `\r\n\r\n`, and we're now done with the request unless it has a body too, which it won't in a `get` request.

The headers keys should be case insensitive, so we always downcase them. We also add some logic for certain headers that we want their values to also be always downcased, namely `content-type` and `accept`. We could add additional ones or remove these if we wanted to. We also add a special case for the `content-length` header in case it's present, before we set it in the request struct, we try to translate it into an integer value because that will be more useful if we wanted then to use the value to count how many bytes we need to parse or whatever.

If you restart the iex console and the launchpad, and then visit localhost you should see that now we also have the `headers` key filled with all headers neatly mapped.

The same way we downcase the value for certain headers, we could also create, if wanted, a special white-list of header keys to be placed as :atoms in our headers map, but that would be confusing by mixing both atom and string keys so we won't worry with that now.


Let's add this new pipeline element to `apps/satellite/lib/satellite_defaults.ex`

`{:noread, :host, &Satellite.Host.set/2, nil}`

And create the respective module in `apps/satellite/lib/satellite_host.ex` with the following content:

{% highlight elixir %}
defmodule Satellite.Host do

  import Satellite.Shared, only: [downcase: 1]

  def set(%{headers: headers} = ctx, _) do
    host = Map.get(headers, "host", "")
    {:ok, %{ctx | host: split_host(host)}}
  end

  def split_host(val), do: split_host(val, <<>>, [])
  def split_host(<<>>, segment, acc), do: Enum.reverse([segment | acc])
  def split_host(<<?., rem::bits>>, segment, acc), do: split_host(rem, <<>>, [segment | acc])
  def split_host(<<?:, _::bits>>, segment, acc), do: Enum.reverse([segment | acc])
  def split_host(<<h, rem::bits>>, segment, acc) do
    n_h = downcase(h)
    split_host(rem, <<segment::binary, n_h>>, acc)
  end

  def split_host(_, _, _), do: []
end
{% endhighlight %}

This step in the pipeline is new, so we also have to implement our handle_event for this particular case. 

Place this `handle_event` function between the existing `...:internal, :wait` and `...:internal, :read`, the order in which these appear is important, because otherwise, if this was after the `:internal, :read` that would be the one being matched and it would hang and timeout waiting for new data on the socket, since it doesn't care about which state we're in. By placing this one before, we actually make it so that when the state is of type `{:noread, ....}` it will be called instead of anything else.

{% highlight elixir %}
def handle_event(:internal, event, {:noread, _name, fun, acc}, %{request: request} = data) do
    case fun.(request, acc) do
      {:ok, n_request} ->
        set_next_step(%{data | request: n_request}, {:event, event})

      {:error, reason} ->
        {:next_state, :response, %{data | request: "Error: #{inspect reason}"}, [{:next_event, :internal, :send_response}]}
    end
end
{% endhighlight %}

Now, the `event` that is matched as the second argument already contains either `{:parse, data}` or `:read` as its event, so we will pass the remaining to `set_next_step` as a new type of tuple, `{:event, event}` and change our `set_next_step` function to account for this new type by changing it to:

{% highlight elixir %}
defp set_next_step(%{pipeline: [h | t]} = data, remaining) do

    event = case remaining do
              <<>> -> :read
              {:event, event} -> event
              _ -> {:parse, remaining}
            end

    {:next_state, h, %{data | pipeline: t}, [{:next_event, :internal, event}]}
end
{% endhighlight %}

This way, it sets the already existing defined event as the next one.
Let's restart iex and launchpad and make sure its still working, we should now see the host key populated as well. Again, we're setting it as a list, instead of a plain string, because, imagine we would want to distinguish between requests in our router that went to a certain subdomain, by having it as a list split by the periods, we will be able to easily encode that logic in our dispatcher/router, which is not evident when you only see a list with a single element `["localhost"]`, but if it was `["mysubdomain", "domain", "com"]` things change, the same if we wanted to have localised logic pertaining to the top domain, then it becomes more evident why it's a good idea. 

Next on the pipeline will be this:

`{:noread, :accept, &Satellite.Accept.set/2, nil}`

Since we already wrote the handle for the `:noread` case we just need to write a module at `apps/satellite/lib/satellite_accept.ex` with:

{% highlight elixir %}
defmodule Satellite.Accept do

  def extract(%{headers: headers} = ctx, _) do
    {:ok, %{ctx | accept: extract_accept(headers)}}
  end

  def extract_accept(%{"accept" => <<"application/json", _::bits>>}), do: :json
  def extract_accept(%{"accept" => <<"text/html", _::bits>>}), do: :html
  def extract_accept(_), do: :any
    
end
{% endhighlight %}

Now this is very basic, perhaps we would like to do as we did with paths and hosts to split all accepted content-types and create a list of that that we could further match down the road but for now this will do, if the first characters of the header are `application/json` or `text/html` we set it to `:json` or `:html` respectively, anything else we set to `:any` (which is obviously incorrect, but we would also need to set up mimes and... we'll leave it for a future occasion).

Restart iex and the launchpad, you should now see the `accept` key also populated.

We're getting closer to finishing our first version of our server. Add this new step to the pipeline:

`{:check, :check_request_type, &Satellite.Check.check/2, nil}`

And create `apps/satellite/lib/satellite_check.ex` with:

{% highlight elixir %}
defmodule Satellite.Check do

  def check(%{verb: :get} = ctx, _), do: {:dispatch, ctx}
  def check(%{verb: :post} = ctx, _), do: {:next, ctx}
  def check(ctx, _), do: {:dispatch, ctx}
end
{% endhighlight %}

This of course is not enough to cover everything an http request might have, but is enough for our sample, if it's a `get` request we'll immediately run the not yet written `dispatch` event and if it's a `post` we will actually want to continue parsing. Now we need to write the corresponding handling for this event with those possible return values on our `satellite.ex` gen_statem:

{% highlight elixir %}
def handle_event(:internal, event, {:check, _name, fun, acc}, %{request: request} = data) do
    case fun.(request, acc) do
      {:next, n_request} ->
        set_next_step(%{data | request: n_request}, {:event, event})

      {:dispatch, n_request} ->
        {:next_state, :dispatching, %{data | request: n_request}, [{:next_event, :internal, :dispatch}]}

      {:response, resp} ->
        {:next_state, :response, %{data | request: resp}, [{:next_event, :internal, :send_response}]}

      {:error, reason} ->
        {:next_state, :response, %{data | request: "Error, #{inspect reason}"}, [{:next_event, :internal, :send_response}]}
    end
end
{% endhighlight %}

Again this needs to be placed before the `handle_event(:internal, :read.....)` function. There's some things we haven't implemented yet, like the `:dispatch` event and in the `{:error, ...}` or `{:response, ...}` we're just doing a very silly thing, so to correct that we will now work on the router and afterwards implement the request dispatcher and lastly the body parsing step. If we restart the shell and `launchpad` we'll now hit an error when visiting the page because the next event `:dispatch` isn't implemented yet.

For the router we'll use some of elixir's macro magic. Macros have some complexity but also allow us to write some really useful code. We'll try to not abuse their usage and basically write code that is still readable, with the main difference being it's written to be run at compile time. I'm no expert in elixir macros, so I won't explain everything (there's much I don't understand also), but basically they allow you to define AST code to be used in code generation at compile time.

What we want to achieve is the ability to create a module and then be able to write:

`route "get", "/some/:path", AnotherModule, :a_function`

(just like Phoenix does for us, but our version will be very plain and lacking many other features)

And with this create a route that accepts a `get` request, on a path composed by `/some/*anything*` and calls a `:a_function` on `AnotherModule` passing it the parsed request, and in the process setting a variable in the params with key `:path` set to the whatever was the second segment of that path. 

So we want to accept literal paths, path segments to be bound to a named key (prefixed by `:`), or any segment (`*`). We also want for each route to be able to accept optionally a host/domain to be matched, defaulting to `*` (any), and again offering the possibility of binding those parts of the domain to named keys in the params map that will be populated in the request. Create the file `apps/satellite/lib/satellite_routing.ex` and place in it:

{% highlight elixir %}
defmodule Satellite.Routing do


  defmacro __using__(_opts) do
    quote do
      import Satellite.Routing
      @before_compile Satellite.Routing
    end
  end

  defmacro route(verb, path, module, function, domain \\ "*") do

    verb_atom = String.to_atom(String.downcase(verb))
    {path_splitted, vars} = split_path(path)
    {domain_splitted, domain_vars} = split_domain(domain)
    all_vars = vars ++ domain_vars
    
    quote do
      def route(unquote(verb_atom), unquote(path_splitted), unquote(domain_splitted), %{params: params} = request) do
        ctx = %{
          request |
          params: Enum.reduce(unquote(all_vars), params, fn({key, var}, acc) ->
            Map.put(acc, key, Macro.escape(var))
          end)
        }
        
        apply(unquote(module), unquote(function), [ctx])
      end
    end
  end

  defp split_path(path) do
    case String.split(path, "/", trim: true) do
      ["*"] -> {(quote do: _), []}
      
      split ->
          Enum.reduce(split, {[], []}, fn
            ("*", {acc1, acc2}) ->
              {[(quote do: _) | acc1], acc2}
            
            (<<?:, rem::binary>>, {acc1, acc2}) ->
              {[Macro.var(String.to_atom(rem), nil) | acc1], [{String.to_atom(rem), Macro.var(String.to_atom(rem), nil)} | acc2]}
            
            (other, {acc1, acc2}) ->
              {[other | acc1], acc2}
            
          end)
          |> case do
               {paths, vars} -> {Enum.reverse(paths), Enum.reverse(vars)}
             end
    end
 end

  defp split_domain(domain) do
    case String.split(domain, ".", trim: true) do
      ["*"] -> {(quote do: _), []}
      
      [<<?:, rem::binary>>] ->
          host_var = String.to_atom("host_#{rem}")
          {Macro.var(host_var, nil), [{host_var, Macro.var(host_var, nil)}]}

        split ->
        Enum.reduce(split, {[], []}, fn
          ("*", {acc1, acc2}) ->
            {[(quote do: _) | acc1], acc2}

          (<<?\\, rem::binary>>, {acc1, acc2}) -> {[rem | acc1], acc2}
          
          (<<?:, rem::binary>>, {acc1, acc2}) ->
            host_var = String.to_atom("host_#{rem}")
            {[Macro.var(host_var, nil) | acc1], [{host_var, Macro.var(host_var, nil)} | acc2]}
          
          (other, {acc1, acc2}) ->
            {[other | acc1], acc2}
          
        end)
        |> case do
             {paths, vars} -> {Enum.reverse(paths), Enum.reverse(vars)}
           end
    end
  end


  defmacro __before_compile__(_env) do
    quote do
      def route(_, _, _, _ctx) do
        Satellite.Response.not_found()
      end
    end
  end
end
{% endhighlight %}

We start by defining a `__using__` macro. This is a special macro definition that then allows us to write `use Name_of_the_module_where_the_using_is_defined` and it automatically makes the contents of block available in that module. 

This `__using__` does only two things, it imports the module to where it is invoked and sets a hook to be run before that module that calls `use` is compiled.

Then we define our actual `route` macro. This macro accepts 4 or 5 arguments, defaulting the last one to `"*"` (the domain). It takes the verb to match, the path, the module to call, and the function in that module to call and, optionally a domain string to match.

With this info it makes some transformations, the verb to an atom (to match what our parser creates), splits the `path` into its own individual segments - it does this by a regular function, and with help of `Macro.var` creates representations for `variables` -

this is what allows us to actually transform a segment such as `:something`, into a variable in the function definition, and then use that and the parsed named to set a key in the params map with the value that that variable will be bound to when ran at runtime. Since this is creating a AST representation of code it doesn't have any values right now, it just has a "representation" of what that code will be when run -

and does the same for the domain. On the splitting functions, you can see that we treat the `"*"` match in the domain and path in a special way when it's the only segment. Because if we treated it as other segments, the generated code would look like `[ _ ]`. If then someone did a request and our host was `host.domain.com`, this would be split into `["host", "domain", "com"]`, and that wouldn't match a list with a single value like `[ _ ]`, so we need to treat it specially so that the whole thing is simply `_`, this way it matches anything like the wildcard actually means, either `["localhost"]`, or `["some", "domain", "com"]`.

Then, because we do `quote do` and inside it define a function, when the macro is called from some other module, what will actually happen is that this function definition we're creating is written in that module with the arguments we prepared. 

So let's say again we have this module:


{% highlight elixir %}
defmodule Test.Router do
    use Satellite.Routing

    route "get", "/some/:path", AnotherModule, :a_function
end
{% endhighlight %}

(the route macro could be written as `route("get", "/some/:path", AnotherModule, :a_function)`, but we can omit parenthesis and in this case it makes it look more declarative, like the Phoenix Router).

When elixir compiles this particular module with that sample code, what actually ends up being defined is


{% highlight elixir %}
defmodule Test.Router do
    def route(:get, ["some", path], _, %{params: params} = request) do
        ctx = %{
            request | 
            params:  
                Enum.reduce([{:path, ast_representation_of_the_var_variable}], params, fn({key, var}, acc) ->
                    Map.put(acc, key, var)
                end)
            }

        apply(AnotherModule, :a_function, [ctx])
	end

    def route(_, _, _, _ctx), do: Satellite.Response.not_found()
end
{% endhighlight %}

The last `route` definition comes from the `@before_compile Satellite.Routing` declaration in the `__using__` macro, which basically allows us to run that macro function that defines that route, after the other macros have been run, but before compilation, so they end up having the correct order.

We can see how we can use this in our `satellite` gen_statem to dispatch the requests since we have the verb, the path and domain neatly decomposed into segments  from the parsing we do in the pipeline.

So now we need to change our `satellite.ex` file. The changes are mostly self-evident but to prevent errors I'm just going to past the full contents of the file as it should be:

{% highlight elixir %}
defmodule Satellite do
  @behaviour :gen_statem

  alias Satellite.{Request, Response}
  
  defstruct [:socket, :config, :conn, :response, :router, pipeline: [], request: %Request{}]
  
  @impl true
  def callback_mode(), do: :handle_event_function
  
  def start_link(socket, opts) do
    :gen_statem.start_link(__MODULE__, {socket, opts}, [])
  end
  
  @impl true
  def init({socket, config}) do
    {:ok, :waiting,  %__MODULE__{socket: socket, config: config}, [{:next_event, :internal, :wait}]}
  end

  @impl true
  def handle_event(:internal, :wait, :waiting, %{socket: socket, config: %{router: router, pipeline: pipeline}} = data) do
    {:ok, conn} = :gen_tcp.accept(socket)
    
    :gen_tcp.controlling_process(conn, self())

    n_data = %{data | conn: conn, router: router, pipeline: pipeline, request: %Request{}}

    set_next_step(n_data, <<>>)
  end

  def handle_event(:internal, event, {:noread, _name, fun, acc}, %{request: request} = data) do
    case fun.(request, acc) do
      {:ok, n_request} ->
        set_next_step(%{data | request: n_request}, {:event, event})

      {:error, reason} ->
        response = Response.error_resp("#{inspect reason}")
        {:next_state, :response, %{data | response: response}, [{:next_event, :internal, :send_response}]}
    end
  end

  def handle_event(:internal, event, {:check, _name, fun, acc}, %{request: request} = data) do
    case fun.(request, acc) do
      {:next, n_request} ->
        set_next_step(%{data | request: n_request}, {:event, event})

      {:dispatch, n_request} ->
        {:next_state, :dispatching, %{data | request: n_request}, [{:next_event, :internal, :dispatch}]}

      {:response, response} ->
        {:next_state, :response, %{data | response: response}, [{:next_event, :internal, :send_response}]}

      {:error, reason} ->
        response = Response.error_resp("#{inspect reason}")
        {:next_state, :response, %{data | response: response}, [{:next_event, :internal, :send_response}]}
    end
  end

  def handle_event(:internal, :read, _, %{conn: conn} = data) do
    case :gen_tcp.recv(conn, 0, 1000) do
      {:ok, packet} ->
        IO.inspect(packet, label: "packet")
        {:keep_state_and_data, [{:next_event, :internal, {:parse, packet}}]}

      {:error, reason} ->
        response = Response.error_resp("#{inspect reason}")
        {:next_state, :response, %{data | response: response}, [{:next_event, :internal, :send_response}]}
    end
  end

  def handle_event(:internal, {:parse, packet}, {type, name, fun, acc}, %{request: request} = data) do

    case fun.(request, packet, acc) do
      {:cont, n_request, n_acc} ->
        IO.inspect({:cont, n_request, n_acc})
        {:next_state, {type, name, fun, n_acc}, %{data | request: n_request}, [{:next_event, :internal, :read}]}

      {:done, n_request, remaining} ->
        IO.inspect({:done, n_request, remaining})
        set_next_step(%{data | request: n_request}, remaining)

      {:error, reason} ->
        response = Response.error_resp("#{inspect reason}")
        {:next_state, :response, %{data | response: response}, [{:next_event, :internal, :send_response}]}
    end
    
  end

  def handle_event(:internal, :dispatch, :dispatching, %{request: request, conn: conn, router: router} = data) do
    try_dispatch(conn, router, request)
    {:next_state, :waiting, %{data | conn: nil}, [{:next_event, :internal, :wait}]} 
  end

  def handle_event(:internal, :send_response, :response, %{conn: conn, response: response} = data) do
    try_send_response(conn, response)
    {:next_state, :waiting, %{data | conn: nil}, [{:next_event, :internal, :wait}]}
  end

  def handle_event(:internal, :close, _, %{conn: conn} = data) do
    :gen_tcp.close(conn)
    {:next_state, :waiting, %{data | conn: nil}, [{:next_event, :internal, :wait}]}
  end


  defp set_next_step(%{pipeline: [h | t]} = data, remaining) do

    event = case remaining do
              <<>> -> :read
              {:event, event} -> event
              _ -> {:parse, remaining}
            end

    {:next_state, h, %{data | pipeline: t}, [{:next_event, :internal, event}]}
  end

  defp set_next_step(%{pipeline: []} = data, _remaining) do
    {:next_state, :dispatching, data, [{:next_event, :internal, :dispatch}]}
  end

  defp try_dispatch(conn, router, %{verb: verb, path: path, host: host} = request) do
    case apply(router, :route, [verb, path, host, request]) do
      response -> try_send_response(conn, response)
    end
  rescue
    e -> try_send_response(conn, Response.error_resp())
  end

  defp try_send_response(conn, %Response{} = response) do
    n_response = Response.make_resp(response)
    send_response(conn, n_response)
  after
    :gen_tcp.close(conn)
  end

  defp try_send_response(conn, response) when is_binary(response) do
    send_response(conn, response)
  after
    :gen_tcp.close(conn)
  end

  defp send_response(conn, response) do
    :gen_tcp.send(conn, response)
  end
end
{% endhighlight %}

And we need to define also, a module to handle responses, create a file `apps/satellite/lib/satellite_response.ex` with:

{% highlight elixir %}
defmodule Satellite.Response do

  defstruct [code: 200, headers: [{<<"content-type">>, <<"text/html">>}], body: <<>>]

  @codes [{200, "OK"}, {404, "Not Found"}, {500, "Internal Server Error"}]
  
  def make_resp(
    %__MODULE__{
      code: code,
      headers: headers,
      body: body
    }
  ) do

    code_prep = make_code(code)
    headers_prep = map_headers(headers)
    {n_body, content_length_prep} = create_length(body)
    
    <<"HTTP/1.0 ", code_prep::binary, "\n", headers_prep::binary, content_length_prep::binary, "\n", n_body::binary>>
  end

  defp map_headers(headers), do: map_headers(headers, <<>>)
  defp map_headers([{header, value} | t], acc), do: map_headers(t, <<acc::binary, header::binary, ": ", value::binary, "\n">>)
  defp map_headers([], acc), do: acc
  
  defp create_length(nil), do: {<<>>, <<"content-length: 0">>}
  defp create_length(<<>>), do: {<<>>, <<"content-length: 0">>}
  defp create_length(body) do
    size = :erlang.integer_to_binary(:erlang.size(body))
    {body, <<"content-length: ", size::binary, "\n">>}
  end


  Enum.each(@codes, fn({code, val}) ->

    string_v = Integer.to_string(code)
    atom_v = String.to_atom(string_v)

    defp make_code(unquote(code)), do: <<unquote(string_v)::binary, " ", unquote(val)::binary>>
    defp make_code(unquote(string_v)), do: <<unquote(string_v)::binary, " ", unquote(val)::binary>>
    defp make_code(unquote(atom_v)), do: <<unquote(string_v)::binary, " ", unquote(val)::binary>>

  end)

  def error_resp(body \\ "Internal Server Error") do
    %__MODULE__{code: 500, headers: [{<<"content-type">>, <<"text/html">>}], body: body}
  end

  def not_found(body \\ "Not Found") do
    %__MODULE__{code: 404, headers: [{<<"content-type">>, <<"text/html">>}], body: body}
  end
  
end
{% endhighlight %}

And lastly, just to test that everything goes according to plan, lets create a test router and a controller for it. `apps/satellite/lib/test_router.ex`:


{% highlight elixir %}
defmodule Test.Router do
  use Satellite.Routing

  route "get", "/", Test.Controller, :test
  route "get", "/:any/oi/:some", Test.Controller, :test2, "*"
  route "get", "*", Test.Controller, :test3
  route "post", "/data", Test.Controller, :test4
end

defmodule Test.Controller do

  def test(request) do
    %Satellite.Response{body: "this is the root path, nothing to see here"}
    |> Satellite.Response.make_resp()
  end

  def test2(request) do
    %Satellite.Response{body: "#{inspect request}"}
    |> Satellite.Response.make_resp()
  end

  def test3(request) do
     %Satellite.Response{body: """
	<html><body><h1>Wildcard match!</h1><br><br><div style="color: red;">#{inspect request}</div></body></html>
      """}
      |> Satellite.Response.make_resp()
   end
  
  def test4(%{body: parsed_body} = request) do
     response_body =  "Parsed: " <> Jason.encode!(parsed_body) <> "\n"
     %Satellite.Response{body: response_body, headers: [{<<"content-type">>, <<"application/json">>}]}
     |> Satellite.Response.make_resp()
   end

end
{% endhighlight %}

Since we'll also want to test `post` requests and we'll use `Jason` to encode JSON, we might as well go ahead and add the last bit to our pipeline, the body parser. Right now we'll only do it for JSON. 

On `apps/satellite/lib/satellite_defaults.ex` add the following `{:read, :body, &Satellite.Body.parse/3, {0, <<>>}}`

The full pipeline should now look like:

{% highlight elixir %}
defmodule Satellite.Defaults do
  
  def default_pipeline() do
    [
      {:read, :verb, &Satellite.Verb.parse/3, <<>>},
      {:read, :path, &Satellite.Path.parse/3, {false, [], <<>>}},
      {:read, :conn, &Satellite.Conn.parse/3, <<>>},
      {:read, :headers, &Satellite.Headers.parse/3, {:header, <<>>}},
      {:noread, :host, &Satellite.Host.set/2, nil},
      {:noread, :accept, &Satellite.Accept.set/2, nil},
      {:check, :check_request_type, &Satellite.Check.check/2, nil},
      {:read, :body, &Satellite.Body.parse/3, {0, <<>>}}
    ]
  end
  
end
{% endhighlight %}

(note: we kept it as tuples, because tuples are the fastest way to access something, if in erlang we would use `Records` which are still tuples underneath and in Elixir we would use proper structs to encode this, something like `%Satellite.Step{mode: :read, name: :verb, fun: &Satellite.Verb.parse/3, acc: <<>>}`, this would be much more structured, specially with `@enforce_keys` we could give some guarantees on when placing a step in the pipeline but that's all out of scope for this tutorial) 

And create the file `apps/satellite/lib/satellite_body.ex`:

{% highlight elixir %}
defmodule Satellite.Body do

  def parse(%{verb: :post, headers: headers} = request, rem, {count, acc}) do
    n_size = count + :erlang.size(rem)
    case headers do
      %{<<"content-length">> => 0} -> {:done, request, <<>>}
      %{<<"content-length">> => ^n_size} ->
        case parse_content(headers, <<acc::binary, rem::binary>>) do
          {:ok, decoded} -> {:done, %{request | body: decoded}, <<>>}
          {:error, error} -> {:error, error}
        end
      _ ->
        {:cont, request, {n_size, <<acc::bits, rem::bits>>}}
    end
  end

  def parse(request, _, _), do: {:done, request, <<>>}

  def parse_content(%{<<"content-type">> => <<"application/json">>}, rem) do
    Jason.decode(rem)
    catch e -> {:error, e}
  end
  
end
{% endhighlight %}

Lastly, we need to add Jason as a dependency so we can use it, on `apps/satellite/mix.exs` change your `deps` function to this:

{% highlight elixir %}
defp deps do
    [
      {:satellite_shared, in_umbrella: true},
      {:jason, "~> 1.0"}
    ]
end
{% endhighlight %}

After that, save the file, quit the shell and do:

`mix deps.get` 

This will install the dependencies.


With this in place, let's exit the shell, restart it with `iex -S mix run` and then do `Launchpad.start_link(%Satellite.Configuration{router: Test.Router})`

Now visit `http://localhost:4000`, `http://localhost:4000/something/oi/else` and `http://localhost:4000/anything_goes`

You should see the correct ouput in the browser.

Now to test `post`s we'll use `curl` with the `-v` flag (verbose) option to see if it works. Open a new terminal window (but don't close the one running the server ofc), and execute:

`curl -v -d '{"key1":"value1", "key2":"value2"}' -H "Content-Type: application/json" -X POST http://localhost:4000/data`

You should see an output of the request details, finished by:

```
* upload completely sent off: 34 out of 34 bytes
* HTTP 1.0, assume close after body
< HTTP/1.0 200 OK
< content-type: application/json
< content-length: 42
< 
Parsed: {"key1":"value1","key2":"value2"}
Closing connection 0
```

If you try:

`curl -v -d 'oiioi' -H "Content-Type: application/json" -X POST http://localhost:4000/data`

You should see instead:

`%Jason.DecodeError{data: "oiioi", position: 0, token: nil}`  as the response, which is the error from trying to parse invalid json `"oiioi"`. Of course this is also not how it should in reality be implemented, it should not leak the details of what is wrong internally, but we wrote it in such a way to illustrate that although the request raised an error, it was still answered.

Now one last thing is to see if we can use this on another app. So let's remove the file `apps/satellite/lib/test_router.ex`, go outside the folder of the umbrella and create a new app, with `mix new test_satellitex --sup`

Then add to `test_satellitex/lib` two files, `router.ex` and `controller.ex`, on `router.ex` add:

{% highlight elixir %}
defmodule Router do
  use Satellite.Routing

  route "get", "/", Controller, :test
  route "get", "/:any/oi/:some", Controller, :test2, "*"
  route "get", "*", Controller, :test3
  route "post", "/data", Controller, :test4
end
{% endhighlight %}

And on `controller.ex` add:

{% highlight elixir %}
defmodule Controller do

  def test(request) do
    %Satellite.Response{body: "#{inspect request}"}
    |> Satellite.Response.make_resp()
  end

  def test2(request) do
    %Satellite.Response{body: "#{inspect request}"}
    |> Satellite.Response.make_resp()
  end

  def test3(request) do
    %Satellite.Response{body: """
    <html><body><h1>Wildcard match!</h1><br><br><div style="color: red;">#{inspect request}</div></body></html>
    """}
    |> Satellite.Response.make_resp()
  end

  def test4(%{body: parsed_body}) do
    response_body = "Parsed: " <> Jason.encode!(parsed_body) <> "\n"
    %Satellite.Response{body: response_body, headers: [{<<"content-type">>, <<"application/json">>}]}
    |> Satellite.Response.make_resp()
  end

end
{% endhighlight %}

On `test_satellitex/mix.exs` change the deps to:

{% highlight elixir %}
defp deps do
    [
      {:satellitex, path: "../satellitex"}
    ]
end
{% endhighlight %}

(assuming you created the `test_satellitex` app on the same folder level as `satellitex`, if not change the relative path accordingly).
We're using a local path just to test, in a real situation this would be either in git or hex and you would use either a git path or a normal dep definition.

Save the file, and edit `test_satellitex/lib/test_satellitex/application.ex` to:

{% highlight elixir %}
defmodule TestSatellitex.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      Supervisor.child_spec(%{id: Server1, start: {Launchpad, :start_link, [%Satellite.Configuration{router: Router}]}}, type: :worker)
    ]

    opts = [strategy: :one_for_one, name: TestSatellitex.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
{% endhighlight %}
Then from the root dir of this app do `mix deps.get` to install the dependencies.
And then start it with:

`iex -S mix run` 

And you should have a running server on another application.

And that's it! This post is already gigantic, so we'll stop here. It's very, very far from being a compliant http server, there's many things missing but the general structure is laid out, about what is missing:

* parsing cookies!
* serving static assets
* appropriate response handlers for different formats other than html/json
* mime support
* html full range of response codes
* http protocol version negotiation
* ssl/tls 
* websockets
* another type of callback in our gen_statem besides `:read`, `:noread` and `:check`, probably named `:connection` where the called function receives the request and the socket itself, so if wanted it could retrieve information from the socket such as the peer_address (ip address of whoever is making the request, amongst other things) - this would allow to easily create a rate-limiter as the first step of the pipeline, if wanted without actually reading anything from the socket
* preparing templates & "views" to send as responses
* defining the pipeline elements as behaviours so that it would be easier to understand what you need to do to have a "compliant" pipeline step created

And many of those are really grunt work, extensive research (reading the RFC's and making the implementation be spec compliant) and/or tricky to implement correctly, so hopefully, besides having learned some things about gen_statems, erlang and elixir it also increased your appreciation for the existing libraries that do all of this, correctly and efficiently and available for free!


