---
author: micael nussbaumer
date: 2026-05-31 00:00:00 +0700
layout: Micaelnussbaumer.PostLayout
title: "Creating a Card Draft Bot in Elixir with State Machines"
slug: creating-card-draft-bot-elixir
categories:
- programming
- elixir
- bot
- ai
- autonomous
- how-to
- tutorial
permalink: /posts/:slug
---

Today I'm going to guide you through implementing an automatic bot card drafter, very close to what I do in https://aethersummon.com for the playground Chaos Brawl Drafts.
To do that we'll learn about `gen_statem`, one of the most powerful process abstractions in Erlang OTP that is sadly very underused when compared to the `gen_server`. I'll be referring to these process abstractions as if they came from Elixir to make it less confusing, since that's what we'll be using but, obviously, they come from Erlang and its OTP abstractions.

If you don't know processes and at the very least `GenServers` it would be better to first understand those.

If you want to jump directly into the `bot_draft.exs` file that you can run as long as you have elixir installed:

<a href="#before-bot-draft">go to beginning of bot_draft.exs</a>

Or if you want to rather read through the explanation of the code, follow:

<a href="#after-bot-draft">explanation of bot_draft.exs</a>

`gen_server`, `GenServer` in Elixir, is one of the most used and known process abstractions when writing systems in Elixir. It has been there for so long it's completely battle-tested and provides the basic structure of a "generic server" process that is great to solve a whole range of problems. It provides a basic interface/behaviour with which to create structured processes that follow a very well defined interface.

With this basic interface you can pretty much solve for all problems you would need such a process although not perfectly (some solutions create inate race-conditions, but so unlikely they're just ignored or worked around when needed) and because it's so simple it's just the default people reach for - even when they shouldn't, even in major ecosystem libraries where clearly they aren't the right tool for the job.

`gen_statem`s provide the same OTP interface as a `gen_server` but charges them with a whole bunch of amazing features that just take it way beyond the `gen_server`, things like:

- Timers - a whole range of internal timers with separation between "process interaction" timers (the same `gen_server`s already offers) and internally defined timers that don't depend on interaction;
- Separation between `state` and `data`, that allow arbitrarly complex state definitions;
- Internal events of any shape and form along with continuations (`gen_server`s offer the `:continue` & `handle_continue` that while useful is very limited compared to what we have in `gen_statem`s)
- "proper" state machine interface, providing state transitions, event postponing, retrying, etc.

I think one of the reasons people forgo even trying `gen_statem`s is that they associate them strictly with state machines, reasonably perhaps due to the name and affordances, and a perceived increase in cognitive complexity. Nonetheless they don't need to be used only when you have a state machine modelling problem, in fact, I used them 99% of the time instead of `gen_server`s because they're just better all-around for the same problems (and slightly faster even, which you might not think is so since they appear heavier in terms of implementation).

When I do so I almost don't use the state machiney affordances and that renders a very similar looking "skeleton" to what would be the `gen_server` implementation of the same process if we use the `callback_mode` of `handle_event_functions`.

Let's start by just explaining a bit of the major upgrades you get in the process interface, that I regularly make use of.

### Timers
`gen_server`s have an in-built time-out functionality but you can only specify an "integer" for the duration of the timeout and this timeout is cancelled whenever a message arrives to the process. This means it can be used to "detect" process idleness in most general cases, for instance to auto-shutdown a process that is no longer being interacted with. Sadly, this timeout is also cancelled due to internal messages that you don't have full control of and might be triggered by other actions/events originating in the process itself.

An example would be an handle in a `gen_server`:

```elixir
def handle_call(:info, from, state) do
  {:reply, state, state, 1000 * 60 * 15}
end

def handle_info(:timeout, state) do
  {:stop, :normal, state}
end
```

This basically handles a `call` for `:info`, replies with the current state, keeps the current state as the state, and then sets a timeout of 15 min (1000ms * 60s * 15min, in milliseconds).

If no other message arrives to this `gen_server` in those 15min, the timeout will trigger and `handle_info(:timeout, state)` is going to be called. In case this happens we will simply stop the `gen_server` at that point. You can see how this can be used to control the lifecycle of a process. Say you have some data fetching that requires starting a process, and this data can be accessed by multiple "clients" but ultimately is temporal - it will be accessed for a while and then eventually stop being accessed.

This very simple definition we made allows us to shutdown the server if it stops being interacted with for more than 15min. We don't need to define anything else, it just works. But there's some limitations too. First, you can't really disambiguate between timers, you have 1 single integer timer that is cancelled by messaging alone. Second, an inherent design flaw is that certain system/internal messages emited by the BEAM can cancel the timers as if they were "client" level messages.

What this means is that once you need more complex timer firing/segregation you need to use `:erlang.start_timer`, `send_after`, etc. The problem with these is that now you introduce possible race conditions that are much more difficult to account for since the timer is independent from the process it will affect. It's still possible to write flawless processes but at that point you need to take into consideration a whole range of other edge-cases.

`gen_statem`s on the other hand offer 3 types of timeouts. They offer the same as `gen_server`s, called `event timeouts` that behave the same way - cancelled by any interaction with the process (by my understanding they don't suffer from the internal/sys messages issue of `gen_server`s). `generic timeouts` that require you to specify a `name` and payload (even if `nil`) and `state timeouts` (that we won't talk here but are also very useful when you're actually writing proper state machines as they are tied with the state lifecycle).

An example would be:

```elixir
@reload_timeout 1000 * 60 * 15
@statem_timeout @reload_timeout - 1000

def init(data) do
  {:ok, :running, data, [{{:timeout, :special}, @reload_timeout, data.something}, @statem_timeout]}
end 

def handle_event({:call, from}, :info, state, data) do
  {
    :keep_state_and_data,
    [
      {:reply, from, data},
      @statem_timeout
    ]
  }
end

def handle_event({:timeout, :special}, something, current_state, data) do
  case something == data.something do
    true -> {:keep_state_and_data, [{{:timeout, :special}, @reload_timeout, new_data.something}]}
    false -> {:keep_state_and_data, [{:next_event, :internal, :reload}]}
  end  
end

def handle_event(:timeout, _, _, _) do
  {:stop, :normal}
end

def handle_event(:internal, :reload, _, data) do
    new_data = Contexts.Data.load(data.id)
    {
      :keep_state,
      new_data,
      [{{:timeout, :special}, @reload_timeout, new_data.something}]
    }
end
```

In this case you can see we have slightly more complex logic (this is by no means even suggested to be a good solution for anything, it's just to illustrate the mechanics of the `gen_statem` - a proper logical implementation is provided as a fully runnable single file mix script, further down).

Here we have a process event timeout (`@statem_timeout`, that is exactly 1sec shorter than our reload timeout) and is specified simply by a non-negative integer in the list of next actions of any callback handle, and a different one specified by a name (that can be any term) and includes a payload, and is specified in the form:

`{{:timeout, name_term}, time_in_ms, payload_term}`

In this case we wrote it so that when this process is started (`init`), we set both timeouts, the "event" timeout by being 1sec earlier than our `special` timeout, will always fire before in case no further interaction is made with the process after setting the timers.

This means if 14min 59sec pass without interaction it shuts down just the same as our `gen_server` example.

But if something interacts with the process, like when requesting `:info`, this `event timeout` will be set again to 14min 59sec in the future from "then", removing any previous event timeout. This means that as long as some request comes in each 15min the process will be kept alive.

But something else happens as well, since we set a general timeout `{{:timeout, :special}, ...}` if it reaches its time, it will automatically do some data reloading and set itself to fire again in another 15min.

So you can see that by simply defining these 2 timeouts and proper handles, we get a process that basically is alive for at least 14min and 59secs, if nothing interacts with it in those almost 15min it shuts down itself. If this happens it means the `:special` timeout doesn't get a chance to fire, and as such no reloading happens. But if something interacts in the meanwhile of those 14min 59secs, then that timer is reset to start again, and eventually the 15min timer will trigger and due to our logic trigger the reload of data.

An hypothetical flow could look something like:


`Time 00:00:00` - Client asks for `:info`, the process is started somehow, and a reply with the info sent, on `init` two timers are set, one to fire at `00:14:59`, and another at `00:15:00`

`Time 00:07:23` - Another client asks for `:info`, the process is running, reply with the info sent, the `event timer` supposedly firing at `00:14:59` is replaced by a new `event timer`, this time setting it to fire at `00:21:22` (7min:23s + 14min:59s).

`Time 00:15:00` - `{:timeout, :special}` fires, triggering a reload of the data.

`Time 00:19:00` - Another client asks for `:info`, it gives the updated info now since we had reloaded the data we held, and the `event timeout` is again reset and set to fire at `00:33:59` (19min + 14min:59s)

`Time 00:30:00` - `{:timeout, :special}` fires again, triggering a reload of the data

`Time 00:33:59` - `:timeout` finally triggers because there was no further interaction since the last `:info` request and the `gen_statem` is shutdown.

We can pretty much program running processes that have complex, yet simple, timing conditions for their own lifecycles, freeing us from having to do all that book keeping and dealing with race conditions - since the order of events inside the statem is guaranteed - the timers aren't firing outside and then having to go through the message queue to be processed, they are kept inside the process and they are placed in queue exactly where they should be with no chance (other than by misprogramming or logical error) of race-conditions.

It also shows the `:internal` events that `gen_statem`s can produce. These are similar to `{:continue, continuation}` events in `gen_server`s, but again, fairly more powerful (and we won't even touch in state transitions, event postponing, event replaying, that have defined APIs to deal with too).

By including an action of the form `{:next_event, :internal, term}` we place immediately that event in the queue to trigger a  `handle_event(:internal, ...)` callback. These are the basic blocks we will use to model our drafting bot.

I've come to use a few macros to help me write cleaner handles in `gen_statem`. Basically so that I don't need to write the full `{:keep_state_and_data, ....}`, `{:keep_state, ...}`

```elixir
defmodule Statems.Helpers do
  @general_timeout if(Mix.env() == :dev, do: 1000 * 60 * 60, else: 1000 * 60 * 15)

  defmacro keep_all(actions \\ []) do
    quote do
      {:keep_state_and_data, unquote(actions)}
    end
  end

  defmacro keep_state(data, actions \\ []) do
    quote do
      {:keep_state, unquote(data), unquote(actions)}
    end
  end

  defmacro internal(payload) do
    quote do
      {:next_event, :internal, unquote(payload)}
    end
  end

  defmacro reply(to, payload) do
    quote do
      {:reply, unquote(to), unquote(payload)}
    end
  end

  defmacro timeout(data) do
    quote do
      Map.get(unquote(data), :timeout, unquote(@general_timeout))
    end
  end

  defmacro next_state(next_state, data, events) do
    quote do
      {:next_state, unquote(next_state), unquote(data), unquote(events)}
    end
  end
end
```

This allows me to then write `internal(:reload)` instead of `{:next_event, :internal, :reload}`, `keep_all([internal(:reload), @timeout])`, instead of `{:keep_state_and_data, [{:next_event, :internal, :reload}, @timeout]}`. It's only for making it more readable as complex handles that return a bunch of actions become overly lengthy, so on the following code snippets you'll see them being used in places.

Before diving into the bot logic and flow I'll explain what and how a draft happens - note that the original was written only for live players, the "bot" draft was added later and one of the amazing things of this way of writing processes (when properly thought of) is that it was a matter of adding the right handles for that version of the draft.

In the game players can build collections and play drafts (called Chaos Brawls) with packs they buy. Nonetheless there's a type of gameplay, called Playground, where you don't collect scrolls, you have access to all existing scrolls, and the drafts in that mode of play, because those packs don't provide scrolls for your collections, are entirely for practicing - you are provided packs of this type on signup for free and up to 9 are refilled for free every week, allowing 3 free drafts per week - for this type of draft I wanted them to always be available to do, meaning not be dependent on having enough players (8) on-line at the same time to join a draft.

To do that I had to program something that simulated a bot, in order to fill the remaining 7 spots needed for a draft. Obviously, since the draft for this game requires making conscious choices that ideally lead to a balanced pile of scrolls across some in-game dimensions, just taking scrolls at random wouldn't be the ideal. Why? Because it would increase the probability of either the human player having easier access to combinations of scrolls that usually won't be happening if playing against other human players due to their contextual value (somehow in the current meta - meta meaning the context of all scrolls and prevalent strategies being played at a given time - they're perceived as higher value and highly prized so they can steer the overall picks into their direction even when other dimensions would say to pick something else).

To solve that I had to define in more or less logical terms what a "balanced" codex is (final list of scrolls the user arrives at after going through the 60 draft picks), so that then I could pick some dimensions (the cost to play the scroll, its type, etc) to model a more "realistic" draft sequence. The most important part is that the bot funnels towards a maximum of 3 domains (the "colors" of the energy the scrolls require) because this leads to a balanced distribution across 8 players drafting and the available scrolls from 3 tomes (packs) per player.  

So the `domain` is one dimension, we'll see how we codified this a bit further down. Other dimension is the type of scroll. There's many types of scrolls, fluxes, creatures, hexes, nimble spells and conjuration spells. Fluxes don't appear in tomes (packs) so that one we can ignore. Creatures are the most important usually, since they work as guarders and strikers, meaning they protect your life total and threaten the opponent's life total, being life total the most usual way of winning, by being the one driving the opponent's life total to 0.

Obviously there's a place in a final list for each type of spell. You need nimbles to allow you to interact with your opponent's spells. Some hexes and conjurations due to their usually unique effects are also welcomed. Nonetheless, ultimately we will want to skew our choices towards a much higher number of creatures spreading the remaining slots across the other types.

So through this rough sketch of the structure of a functional final pile of scrolls we can derive some more rules, that we'll analyse after introducing the state machine definition.

Here I got rid and/or commented out some bits of the code, simplified others, so that it would fit a single elixir script, with 3 dependencies related to schema/types. I skipped the persistance part but in production it's basically using postgres and stores each pick as they come. This means the initialization of the state machine is slightly different too, it just takes a pod ID, that can be part of the url the players are in for the draft - and then loads from the database the draft plus the saved actions and just replays them if there are any.

In this way a node holding a live draft can go down, and back up, losing at most the last round of picks - but because it's a static deterministic set of picks the players can just re-play their last choice and be back at the same point as before the node going down, or the server crashing, or whatever, meaning it doesn't really loose anything in a practical sense. The players are viewing a page with the pod id on the url - server goes down, websocket is cut, when server comes back up, or on reconnect if any node is still up, it asks for that draft, derived from the url and so on... 

It's also easy to write automated unit, or integration tests. For unit you can test a single instance of the `statem`, for integration you can start the supervisor under a test supervised process tree, or as part of the normal app supervision tree for e2e tests.

<div id="before-bot-draft">
  <pre class="lumis code">bot_draft.exs</pre>
</div>

You can download the <a href="/downloads/bot_draft.exs.txt" target="_blank" class="">bot_draft.exs</a>
or read it here, or <a href="#after-bot-draft">skip it and follow along</a> with the file on your IDE or text editor to follow along.


```elixir
Mix.install([
  {:typed_enum, "~> 0.1"},
  {:typed_ecto_schema, "~> 0.4.1", runtime: false},
  {:typed_struct, "~> 0.3.0"}
])

defmodule Scrollrack do
  @doc """
  This is placeholder module that just returns  a severely cut version of the 
  scrolls, with just enough data to make the draft picks according to our 
  weights. An original scroll would look like:

  %Schemas.Scroll{
    id: 8,
    class: nil,
    cost: "111",
    domain: [],
    flavour: "Even if invisible, it's there, a thread weaving this and all other worlds, those that follow and those that precede it, in a manner so seamless that it indeed looks like it's a single piece of cosmic cloth.",
    name: "Cosmic Link",
    quantity: 1,
    slug: "cosmic_link",
    text: ["Return 1 target scroll that is forsaken to its owner's recall tome"],
    type: :conjuration,
    gid: nil,
    version: 1,
    artist: ["Ana Fernandes", "PT"],
    rules: %Schemas.Scroll.Rules{
      portal: nil,
      maintenance: nil,
      create: nil,
      global: nil,
      creature: nil,
      mimic: nil,
      trigger_on: [],
      targets: [
        %Schemas.Scroll.Rules.Target{
          type: :scroll,
          owner: :any,
          to: :recall,
          from: :forsaken,
          repeatable: nil,
          amount: nil,
          action: :return,
          times: 1,
          cost: nil,
          defined: nil,
          add_cost: [],
          constraints: nil,
          effect: nil,
          gid: nil,
          gift_from: nil,
          replace: nil
        }
      ],
      etge: [],
      gifts: []
    },
    add_cost: []
  }

  And it's missing many other definitions because they're nil in this case.
  """

  @all_scrolls [
    %{
      id: 8,
      name: "Cosmic Link",
      type: :conjuration,
      domain: [],
      slug: "cosmic_link",
      cost: "111"
    },
    %{
      id: 9,
      name: "Shadow Born",
      type: :creature,
      domain: [],
      slug: "shadow_born",
      cost: "1"
    },
    %{
      id: 10,
      name: "Sleep Paralysis",
      type: :nimble,
      domain: [],
      slug: "sleep_paralysis",
      cost: "111"
    },
    %{
      id: 11,
      name: "Voodoo Doll",
      type: :creature,
      domain: [],
      slug: "voodoo_doll",
      cost: "11"
    },
    %{
      id: 12,
      name: "Air Skirmisher",
      type: :creature,
      domain: [:air],
      slug: "air_skirmisher",
      cost: "A"
    },
    %{
      id: 13,
      name: "Cloud-String",
      type: :creature,
      domain: [:water, :air],
      slug: "cloud_string",
      cost: "AW"
    },
    %{
      id: 14,
      name: "Cloudstringer",
      type: :creature,
      domain: [:water, :air],
      slug: "cloudstringer",
      cost: "11AAW"
    },
    %{
      id: 15,
      name: "Favourable Winds",
      type: :hex,
      domain: [:air],
      slug: "favourable_winds",
      cost: "1AA"
    },
    %{
      id: 16,
      name: "Firestorm Elemental",
      type: :creature,
      domain: [:fire, :air],
      slug: "firestorm_elemental",
      cost: "AAFFF"
    },
    %{
      id: 17,
      name: "Gamma Phoenix",
      type: :creature,
      domain: [:light, :fire, :air],
      slug: "gamma_phoenix",
      cost: "1ALFF"
    },
    %{
      id: 18,
      name: "Meditation Rune",
      type: :hex,
      domain: [:water, :light, :air],
      slug: "meditation_rune",
      cost: "ALW"
    },
    %{
      id: 19,
      name: "Mountain Drake",
      type: :creature,
      domain: [:fire, :air],
      slug: "mountain_drake",
      cost: "AAF"
    },
    %{
      id: 20,
      name: "Oxygen Deprivation",
      type: :nimble,
      domain: [:air],
      slug: "oxygen_deprivation",
      cost: "11A"
    },
    %{
      id: 21,
      name: "Relapse",
      type: :nimble,
      domain: [:void, :air],
      slug: "relapse",
      cost: "AV"
    },
    %{
      id: 22,
      name: "Royal Gryphus",
      type: :creature,
      domain: [:air],
      slug: "royal_gryphus",
      cost: "1AA"
    },
    %{
      id: 23,
      name: "Sweeping Breeze",
      type: :nimble,
      domain: [:air],
      slug: "sweeping_breeze",
      cost: "AA"
    },
    %{
      id: 24,
      name: "Time Stretch",
      type: :nimble,
      domain: [:air],
      slug: "time_stretch",
      cost: "1A"
    },
    %{
      id: 25,
      name: "Wind Blower",
      type: :creature,
      domain: [:air],
      slug: "wind_blower",
      cost: "A"
    },
    %{
      id: 26,
      name: "Wind Shards",
      type: :nimble,
      domain: [:water, :air],
      slug: "wind_shards",
      cost: "WAA"
    },
    %{
      id: 27,
      name: "Æther Eater",
      type: :creature,
      domain: [:void, :air],
      slug: "æther_eater",
      cost: "AVV"
    },
    %{
      id: 28,
      name: "Atrocity",
      type: :conjuration,
      domain: [:void, :earth],
      slug: "atrocity",
      cost: "EV"
    },
    %{
      id: 29,
      name: "Druidic Growth",
      type: :nimble,
      domain: [:earth],
      slug: "druidic_growth",
      cost: "1E"
    },
    %{
      id: 30,
      name: "Flux Channeler",
      type: :creature,
      domain: [:earth],
      slug: "flux_channeler",
      cost: "E"
    },
    %{
      id: 31,
      name: "Flux Tampering",
      type: :conjuration,
      domain: [:fire, :earth],
      slug: "flux_tampering",
      cost: "EEF"
    },
    %{
      id: 32,
      name: "Fungi Coven",
      type: :hex,
      domain: [:earth],
      slug: "fungi_coven",
      cost: "E"
    },
    %{
      id: 33,
      name: "Fungi Spread",
      type: :nimble,
      domain: [:earth, :water],
      slug: "fungi_spread",
      cost: "WE"
    },
    %{
      id: 34,
      name: "Fungus",
      type: :creature,
      domain: [:earth],
      slug: "fungus",
      cost: "E"
    },
    %{
      id: 35,
      name: "Giant Ragnak",
      type: :creature,
      domain: [:void, :earth],
      slug: "giant_ragnak",
      cost: "1EEVV"
    },
    %{
      id: 36,
      name: "Light Decomposition",
      type: :conjuration,
      domain: [:light, :earth],
      slug: "light_decomposition",
      cost: "EELLL"
    },
    %{
      id: 37,
      name: "One-Eyed Tuva",
      type: :creature,
      domain: [:void, :light, :earth],
      slug: "one_eyed_tuva",
      cost: "ELLV"
    },
    %{
      id: 38,
      name: "Raging Wootolle",
      type: :creature,
      domain: [:earth],
      slug: "raging_wootolle",
      cost: "EE"
    },
    %{
      id: 39,
      name: "Replenish",
      type: :nimble,
      domain: [:earth],
      slug: "replenish",
      cost: "EE"
    },
    %{
      id: 40,
      name: "Rot",
      type: :nimble,
      domain: [:earth],
      slug: "rot",
      cost: "1EE"
    },
    %{
      id: 41,
      name: "Scavenging Beetles",
      type: :creature,
      domain: [:void, :earth],
      slug: "scavenging_beetles",
      cost: "EV"
    },
    %{
      id: 42,
      name: "Blaze Seer",
      type: :creature,
      domain: [:fire],
      slug: "blaze_seer",
      cost: "F"
    },
    %{
      id: 43,
      name: "Burning Wave",
      type: :conjuration,
      domain: [:fire],
      slug: "burning_wave",
      cost: "11FF"
    },
    %{
      id: 44,
      name: "Fire Geist",
      type: :creature,
      domain: [:fire],
      slug: "fire_geist",
      cost: "1F"
    },
    %{
      id: 45,
      name: "Fire Pit",
      type: :hex,
      domain: [:fire],
      slug: "fire_pit",
      cost: "F"
    },
    %{
      id: 46,
      name: "Fire Purge",
      type: :conjuration,
      domain: [:fire],
      slug: "fire_purge",
      cost: "11FFF"
    },
    %{
      id: 47,
      name: "Fire Spike",
      type: :nimble,
      domain: [:fire],
      slug: "fire_spike",
      cost: "1F"
    },
    %{
      id: 48,
      name: "Fire Weaver",
      type: :creature,
      domain: [:fire],
      slug: "fire_weaver",
      cost: "1F"
    },
    %{
      id: 49,
      name: "Magma Firefly",
      type: :creature,
      domain: [:fire],
      slug: "magma_firefly",
      cost: "FF"
    },
    %{
      id: 50,
      name: "Rift Worm",
      type: :creature,
      domain: [:fire],
      slug: "rift_worm",
      cost: "111FF"
    },
    %{
      id: 51,
      name: "Unholy Fire",
      type: :conjuration,
      domain: [:void, :fire],
      slug: "unholy_fire",
      cost: "FFV"
    },
    %{
      id: 52,
      name: "Blinding Remission",
      type: :conjuration,
      domain: [:void, :light],
      slug: "blinding_remission",
      cost: "LV"
    },
    %{
      id: 53,
      name: "Call for Reinforcements",
      type: :nimble,
      domain: [:light],
      slug: "call_for_reinforcements",
      cost: "LL"
    },
    %{
      id: 54,
      name: "Dimensional Artificer",
      type: :creature,
      domain: [:light],
      slug: "dimensional_artificer",
      cost: "L"
    },
    %{
      id: 55,
      name: "Dimensional Lock",
      type: :conjuration,
      domain: [:water, :light],
      slug: "dimensional_lock",
      cost: "LW"
    },
    %{
      id: 56,
      name: "Enchantation Collapse",
      type: :conjuration,
      domain: [:light],
      slug: "enchantation_collapse",
      cost: "LL"
    },
    %{
      id: 57,
      name: "Equanimous Judgement",
      type: :conjuration,
      domain: [:light],
      slug: "equanimous_judgement",
      cost: "BLL"
    },
    %{
      id: 58,
      name: "Justice Mantle",
      type: :hex,
      domain: [:light],
      slug: "justice_mantle",
      cost: "11LL"
    },
    %{
      id: 59,
      name: "Light Bearer",
      type: :creature,
      domain: [:light],
      slug: "light_bearer",
      cost: "LL"
    },
    %{
      id: 60,
      name: "Light Shaper",
      type: :creature,
      domain: [:light],
      slug: "light_shaper",
      cost: "LL"
    },
    %{
      id: 61,
      name: "Militia Air Patrol",
      type: :creature,
      domain: [:light],
      slug: "militia_air_patrol",
      cost: "L"
    },
    %{
      id: 62,
      name: "Militia Skirmisher",
      type: :creature,
      domain: [:light],
      slug: "militia_skirmisher",
      cost: "L"
    },
    %{
      id: 63,
      name: "Militia Swordsman",
      type: :creature,
      domain: [:light],
      slug: "militia_swordsman",
      cost: "L"
    },
    %{
      id: 64,
      name: "Mind Bender",
      type: :creature,
      domain: [:void, :light],
      slug: "mind_bender",
      cost: "LV"
    },
    %{
      id: 65,
      name: "Omen Octopus",
      type: :creature,
      domain: [:water, :light],
      slug: "omen_octopus",
      cost: "11LWW"
    },
    %{
      id: 66,
      name: "Deadly Gloom",
      type: :hex,
      domain: [:void],
      slug: "deadly_gloom",
      cost: "VVV"
    },
    %{
      id: 67,
      name: "Devoid",
      type: :nimble,
      domain: [:void],
      slug: "devoid",
      cost: "1VV"
    },
    %{
      id: 68,
      name: "Mind Collapse",
      type: :conjuration,
      domain: [:void],
      slug: "mind_collapse",
      cost: "VVV"
    },
    %{
      id: 69,
      name: "Mystical Binding",
      type: :hex,
      domain: [:void],
      slug: "mystical_binding",
      cost: "1VV"
    },
    %{
      id: 70,
      name: "Portal to Nothingness",
      type: :conjuration,
      domain: [:void],
      slug: "portal_to_nothingness",
      cost: "VV"
    },
    %{
      id: 71,
      name: "Void Caller",
      type: :creature,
      domain: [:void],
      slug: "void_caller",
      cost: "V"
    },
    %{
      id: 72,
      name: "Void Stinger",
      type: :creature,
      domain: [:void],
      slug: "void_stinger",
      cost: "1VVV"
    },
    %{
      id: 73,
      name: "Æther Sink",
      type: :conjuration,
      domain: [:water],
      slug: "aether_sink",
      cost: "1W"
    },
    %{
      id: 74,
      name: "Dimensional Walk",
      type: :nimble,
      domain: [:water],
      slug: "dimensional_walk",
      cost: "WW"
    },
    %{
      id: 75,
      name: "Leviathan",
      type: :creature,
      domain: [:water],
      slug: "leviathan",
      cost: "WWW"
    },
    %{
      id: 76,
      name: "Mind Control",
      type: :conjuration,
      domain: [:water],
      slug: "mind_control",
      cost: "1WWW"
    },
    %{
      id: 77,
      name: "Monsoon Wave",
      type: :nimble,
      domain: [:water],
      slug: "monsoon_wave",
      cost: "11WW"
    },
    %{
      id: 78,
      name: "Rune Arbiter",
      type: :creature,
      domain: [:water],
      slug: "rune_arbiter",
      cost: "W"
    },
    %{
      id: 79,
      name: "Winged Sea Serpent",
      type: :creature,
      domain: [:water],
      slug: "winged_sea_serpent",
      cost: "11W"
    },
    %{
      id: 80,
      name: "Oak-Bark Druid",
      type: :creature,
      domain: [:earth],
      slug: "oak_bark_druid",
      cost: "E"
    },
    %{
      id: 81,
      name: "Dryad",
      type: :creature,
      domain: [:earth],
      slug: "dryad",
      cost: "1E"
    },
    %{
      id: 82,
      name: "Arcane Mastery",
      type: :nimble,
      domain: [:light],
      slug: "arcane_mastery",
      cost: "BBLL"
    },
    %{
      id: 83,
      name: "Emptiness Pull",
      type: :nimble,
      domain: [:void],
      slug: "emptiness_pull",
      cost: "XVV"
    }
  ]

  def all_scrolls(), do: @all_scrolls

  def persist_term_scrolls() do
    Enum.each(all_scrolls(), fn %{id: scroll_id, slug: slug} = scroll ->
      :persistent_term.put({:scroll, scroll_id}, scroll)
      :persistent_term.put({:scroll, slug}, scroll)
    end)
  end
end

defmodule Statems.Helpers do
  @general_timeout if(Mix.env() == :dev, do: 1000 * 60 * 60, else: 1000 * 60 * 15)

  defmacro keep_all(actions \\ []) do
    quote do
      {:keep_state_and_data, unquote(actions)}
    end
  end

  defmacro keep_state(data, actions \\ []) do
    quote do
      {:keep_state, unquote(data), unquote(actions)}
    end
  end

  defmacro internal(payload) do
    quote do
      {:next_event, :internal, unquote(payload)}
    end
  end

  defmacro reply(to, payload) do
    quote do
      {:reply, unquote(to), unquote(payload)}
    end
  end

  defmacro timeout(data) do
    quote do
      Map.get(unquote(data), :timeout, unquote(@general_timeout))
    end
  end

  defmacro next_state(next_state, data, events) do
    quote do
      {:next_state, unquote(next_state), unquote(data), unquote(events)}
    end
  end
end

defmodule Schemas.Draft.Pod.Pick do
  use TypedEctoSchema

  @primary_key {:order, :integer, autogenerate: false}
  typed_embedded_schema do
    field(:player_id, :binary_id)
    field(:pick_index, :integer)
    field(:tome_index, :integer)
    field(:scroll, :integer)
  end

  def new(user_id, order, scroll_index, tome_index, scroll),
    do: %__MODULE__{
      player_id: user_id,
      order: order,
      pick_index: scroll_index,
      tome_index: tome_index,
      scroll: scroll
    }
end

defmodule Schemas.Draft.Pod.Bag do
  use TypedEctoSchema
  @primary_key false
  typed_embedded_schema do
    field(:id, :binary_id)
    field(:index, :integer)
    field(:tomes, {:array, :integer})
    field(:scrolls, {:array, :integer})
    field(:preselected, :integer)
    field(:timer, :integer)
  end
end

defmodule Types.Draft do
  @moduledoc """
  The possible types of draft that can be played.
  """
  use TypedEnum,
    values: [
      :playground,
      :only_owner,
      :competitive
    ]
end

defmodule Schemas.Draft.Pod do
  use TypedEctoSchema

  @type id :: Ecto.UUID.t()

  @primary_key {:id, :binary_id, autogenerate: true}
  typed_schema("draft_pod") do
    field(:players, {:array, :binary_id})
    field(:type, Types.Draft, default: :playground)
    field(:original_tomes, {:array, {:array, {:array, :integer}}}, default: [])
    field(:started_at, :utc_datetime_usec)
    field(:finished_at, :utc_datetime_usec)
    embeds_many(:picks, __MODULE__.Pick, on_replace: :delete)

    timestamps(type: :utc_datetime_usec)
  end
end

defmodule Types.Duel.Game.Aether do
  use TypedEnum,
    values: [
      :air,
      :earth,
      :fire,
      :light,
      :void,
      :water,
      :all
    ]

  @only_aether Enum.reject(@valid_atoms, &(&1 == :all))

  @aether_conversion Enum.reduce(@only_aether, %{}, fn aether, acc ->
                       string =
                         aether
                         |> Atom.to_string()
                         |> String.upcase()
                         |> String.first()

                       acc
                       |> Map.put(string, aether)
                       |> Map.put(aether, string)
                     end)

  @spec only_aether() :: list(__MODULE__.t())
  def only_aether(),
    do: @only_aether

  @spec aether_conversion_map() :: map()
  def aether_conversion_map(),
    do: @aether_conversion

  @spec convert_aether_char_to_atom(String.t()) :: __MODULE__.t()
  def convert_aether_char_to_atom(char),
    do: Map.get(aether_conversion_map(), char)
end

defmodule Contexts.Draft do
  @tome_size 20
  @all_scrolls Scrollrack.all_scrolls() |> Enum.map(& &1.id)

  @spec create_bot_pod(Ecto.UUID.t()) :: Schemas.Draft.Pod.t()
  def create_bot_pod(player_id) do
    time_now = DateTime.utc_now()

    %Schemas.Draft.Pod{
      id: Ecto.UUID.generate(),
      players: [player_id],
      type: :playground,
      original_tomes: create_tomes(),
      started_at: time_now,
      picks: [],
      inserted_at: time_now,
      updated_at: time_now
    }
  end

  @spec create_tomes() :: list(list(non_neg_integer()))
  def create_tomes() do
    Enum.reduce(1..8, [], fn _, acc ->
      [
        Enum.reduce(1..3, [], fn _, acc_inner ->
          [generate_tome_scrolls(@tome_size) | acc_inner]
        end)
        | acc
      ]
    end)
  end

  @spec generate_tome_scrolls(total :: non_neg_integer()) :: list(non_neg_integer())
  def generate_tome_scrolls(tome_size \\ @tome_size) do
    Enum.reduce(1..tome_size, [], fn _, acc ->
      [Enum.random(@all_scrolls) | acc]
    end)
  end
end

defmodule Statems.Draft.Pod.Supervisor do
  use DynamicSupervisor

  require Logger

  alias Schemas.Draft.Pod

  def child_spec(_),
    do: %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, []},
      type: :supervisor
    }

  def start_link() do
    Logger.info("::: ::: Starting #{__MODULE__} ::: :::")
    DynamicSupervisor.start_link(__MODULE__, nil, name: __MODULE__)
  end

  @spec start_child(Pod.t()) :: {:ok, pid()} | {:error, any()}
  def start_child(%Pod{} = pod) do
    DynamicSupervisor.start_child(
      __MODULE__,
      %{
        id: Statems.Draft.Pod,
        start: {Statems.Draft.Pod, :start_link, [pod]},
        shutdown: 25_000,
        restart: :temporary,
        type: :worker
      }
    )
  end

  def init(_), do: DynamicSupervisor.init(strategy: :one_for_one)
end

defmodule Statems.Draft.Pod do
  @behaviour :gen_statem
  import Statems.Helpers

  alias __MODULE__, as: Data
  alias Schemas.Draft.Pod
  alias Schemas.Draft.Pod.Bag
  alias Types.Duel.Game.Aether

  @initial_timer 12_000_000
  @time_to_cut_x_round 6_000
  @tome_size 20

  @bots Enum.map(1..7, fn n -> "bot_#{n}" end)

  @enforce_keys [:id]
  defstruct [
    :id,
    :pod,
    :indexes,
    :bags,
    :tomes,
    :picks,
    initial_timer: @initial_timer
  ]

  @type t() :: %__MODULE__{}
  @type draft_pod_statem_name :: {:draft_pod, Pod.id()}
  @type player_id :: Ecto.UUID.t() | String.t()

  @spec statem_name(Pod.id()) :: draft_pod_statem_name()
  def statem_name(id), do: {:draft_pod, id}

  @spec statem_global_id(Pod.id()) :: {:global, draft_pod_statem_name}
  def statem_global_id(id), do: {:global, statem_name(id)}

  @spec whereis(Pod.id()) :: :undefined | pid
  def whereis(id), do: :global.whereis_name(statem_name(id))

  @spec interact(Pod.id(), term()) :: {:ok, term()} | {:error, term()}
  def interact(id, command) do
    case whereis(id) do
      :undefined ->
        {:error, :not_running}

      pid ->
        :gen_statem.call(pid, command)
    end
  end

  def start_link(%Pod{id: pod_id} = pod) do
    name = statem_global_id(pod_id)

    case :gen_statem.start_link(name, __MODULE__, pod, []) do
      {:ok, pid} ->
        {:ok, pid}

      {:error, {:already_started, pid}} ->
        Process.link(pid)
        {:ok, pid}

      error ->
        error
    end
  end

  #################################
  ########### Statem Implementation / Internal 
  #################################

  @impl :gen_statem
  def callback_mode(), do: :handle_event_function

  @impl :gen_statem
  def init(%Pod{id: id, players: [player_id], original_tomes: [_ | _]} = pod) do
    {bags, indexes, _} =
      [player_id | @bots]
      |> Enum.reduce({%{}, %{}, 0}, fn player, {bags, indexes, count} ->
        bag = %Bag{
          index: count,
          id: player,
          tomes: [count],
          scrolls: [],
          preselected: nil,
          timer: false
        }

        {
          Map.put(bags, player, bag),
          indexes |> Map.put(count, player) |> Map.put(player, count),
          count + 1
        }
      end)

    data = %Data{id: id, pod: pod, indexes: indexes, bags: bags}

    {:ok, :loading, data, [internal(:set_tomes)]}
  end

  @impl :gen_statem
  def handle_event(:internal, :broadcast, _, %Data{pod: %{id: id}} = _data) do
    # Phoenix.PubSub.broadcast(
    #  Server.PubSub,
    #  "draft_pod:#{id}",
    #  %Broadcast{
    #    topic: "draft_pod:#{id}",
    #    type: :POD_CHANGE,
    #    msg: data
    #  }
    # )
    IO.puts("BROADCASTING pod #{id}")
    keep_all([])
  end

  def handle_event(
        :internal,
        :set_tomes,
        state,
        %Data{bags: bags, pod: %Pod{players: [player_id], original_tomes: all_tomes}} = data
      ) do
    current_tome_index =
      bags
      |> Map.get(player_id)
      |> get_round()

    player_tomes = hd(all_tomes)
    current_tome = Enum.at(player_tomes, current_tome_index)

    case current_tome do
      nil ->
        keep_all([internal(:finish)])

      _ ->
        current_tomes =
          Enum.reduce(all_tomes, [], fn tomes, acc ->
            [Enum.at(tomes, current_tome_index) | acc]
          end)
          |> Enum.reverse()

        actions =
          case state do
            :loading ->
              [internal(:apply_picks)]

            _ ->
              [internal(:new_round)]
          end

        keep_state(%{data | tomes: current_tomes}, actions)
    end
  end

  def handle_event(
        :internal,
        :apply_picks,
        :loading,
        %Data{pod: %Pod{picks: picks}, picks: nil} = data
      ) do
    keep_state(%Data{data | picks: picks}, [internal(:apply_picks)])
  end

  def handle_event(
        :internal,
        :apply_picks,
        :loading,
        %Data{picks: [_ | _] = picks} = data
      ) do
    select_actions =
      Enum.map(picks, &internal({:select, &1.player_id, &1.pick_index}))

    actions = select_actions ++ [internal(:apply_picks)]
    keep_state(%Data{data | picks: []}, actions)
  end

  def handle_event(
        :internal,
        :apply_picks,
        :loading,
        %Data{picks: []}
      ),
      do: keep_all([internal(:set_on)])

  def handle_event(:internal, :apply_picks, :on, %Data{picks: []}),
    do: keep_all([])

  def handle_event(:internal, :new_round, _, %Data{bags: bags} = data) do
    new_bags =
      case Enum.all?(bags, fn {_, %Bag{tomes: tomes}} -> tomes == [] end) do
        true ->
          Enum.reduce(bags, bags, fn {player_id, %Bag{} = bag}, acc ->
            Map.put(acc, player_id, %Bag{bag | tomes: [bag.index]})
          end)

        false ->
          bags
      end

    keep_state(%Data{data | bags: new_bags}, [internal(:check_if_bot_draft)])
  end

  def handle_event(:internal, :set_on, :loading, data),
    do: next_state(:on, data, [internal({:set_timers, :all}), internal(:broadcast)])

  def handle_event(:internal, :finish, _, %Data{pod: _pod, bags: _bags} = _data) do
    # this obvioulsy in real life does a bunch more of actions, including saving
    # the pod and players bags and then moving to finished state
    # Here we simply close it
    {:stop, :finished}
  end

  def handle_event(
        :internal,
        {:select, user_id, scroll_index},
        _state,
        data
      ) do
    case select(user_id, scroll_index, data) do
      {:ok, next_player, tome_index, new_data, scroll} ->
        keep_state(
          new_data,
          [
            internal({:add_pick, user_id, scroll_index, tome_index, scroll}),
            internal(:check_round),
            internal({:set_timers, user_id}),
            internal({:maybe_next_timer, next_player, tome_index}),
            internal(:broadcast)
            | []
          ]
        )

      _missed ->
        keep_all([internal(:check_round)])
    end
  end

  def handle_event(
        :internal,
        {:maybe_next_timer, next_id, tome_index},
        :on,
        %Data{bags: bags}
      ) do
    case Map.get(bags, next_id) do
      %Bag{tomes: [^tome_index | _]} ->
        keep_all([internal({:set_timers, next_id})])

      _ ->
        keep_all([])
    end
  end

  def handle_event(:internal, {:maybe_next_timer, _, _}, :loading, _),
    do: keep_all([])

  def handle_event(
        :internal,
        {:set_timers, :all},
        :on,
        %Data{bags: bags, initial_timer: it} = data
      ) do
    time_now = DateTime.utc_now()

    {new_bags, actions} =
      Enum.reduce(bags, {bags, []}, fn {player_id, %Bag{} = bag}, {bags_acc, actions_acc} ->
        with round <- get_round(bag),
             timeout when is_integer(timeout) <- get_timeout(bag, it),
             new_bag <- set_timeout(bag, time_now, timeout),
             new_bags <- Map.put(bags_acc, player_id, new_bag),
             action <- {{:timeout, {:pick, player_id}}, timeout, round} do
          {new_bags, [action | actions_acc]}
        else
          _miss ->
            {Map.put(bags_acc, player_id, %Bag{bag | timer: false}), actions_acc}
        end
      end)

    keep_state(
      %Data{data | bags: new_bags},
      actions
    )
  end

  def handle_event(
        :internal,
        {:set_timers, player_id},
        :on,
        %Data{bags: bags, initial_timer: it} = data
      )
      when is_binary(player_id) do
    with %Bag{} = bag <- Map.get(bags, player_id),
         round <- get_round(bag),
         timeout when is_integer(timeout) <- get_timeout(bag, it),
         time_now <- DateTime.utc_now(),
         %Bag{} = new_bag <- set_timeout(bag, time_now, timeout),
         new_bags <- Map.put(bags, player_id, new_bag) do
      keep_state(
        %Data{data | bags: new_bags},
        [{{:timeout, {:pick, player_id}}, timeout, round}]
      )
    else
      _miss ->
        keep_all([])
    end
  end

  def handle_event(:internal, {:set_timers, _}, _, _),
    do: keep_all([])

  def handle_event(:internal, :check_round, _, %Data{bags: bags}) do
    case Enum.all?(bags, fn {_, %Bag{tomes: tomes}} -> tomes == [] end) do
      true -> keep_all([internal(:set_tomes)])
      _ -> keep_all([internal(:check_if_bot_draft)])
    end
  end

  def handle_event(
        :internal,
        {:add_pick, _user_id, _scroll_index, _tome_index, _scroll},
        :on,
        %Data{pod: _pod} = _data
      ) do
    # usually we would save the pick into the database record holding this draft
    # for in case the statem goes down (node crash, deployment, etc) when this
    # statem re-starts it can load the picks that have been made already and
    # simply resume the draft but that involves other functionality so in this
    # case we don't do anything.
    # Notice also that we have a second clause that does the same and this is
    # because, again, in the real implementation the loading flow on statem init
    # goes through the same selection flow as if the draft was happening, when it
    # loads an existing started draft, so in that case, when the select is done
    # we don't want to add a new "pick", because we're simply "replaying" the
    # existing ones, and in that case, we can differentiate what is what by the
    # "state" the state machine is on, if it's :on, sure, add the pick as a
    # record but if :loading, ignore the "save pick" part.

    # {:ok, new_pod} =
    #  Contexts.ChaosBrawls.add_pick_to_pod(pod, user_id, scroll_index, tome_index, scroll)
    # keep_state(%{data | pod: new_pod}, [])
    keep_all([])
  end

  def handle_event(:internal, {:add_pick, _, _, _, _}, :loading, _),
    do: keep_all([])

  # this handle relies on our knowledge that when there's a single `player_id`
  # in the `%Pod{players: players}` ids it means it's a bot draft. And in case
  # it is it goes on to execute the bot picks
  def handle_event(
        :internal,
        :check_if_bot_draft,
        :on,
        %Data{pod: %Pod{players: players}}
      ) do
    case players do
      [_] -> keep_all([internal(:bot_draft_picks)])
      _ -> keep_all([internal(:broadcast)])
    end
  end

  def handle_event(:internal, :check_if_bot_draft, _, _),
    do: keep_all([])

  def handle_event(
        :internal,
        :bot_draft_picks,
        :on,
        %Data{bags: bags, tomes: tomes}
      ) do
    select_actions =
      bags
      |> Enum.sort_by(
        fn {id, _} ->
          case id do
            "bot_" <> _ -> {1, id}
            _ -> {0, id}
          end
        end,
        :desc
      )
      |> Enum.reduce_while([], fn
        {"bot_" <> _ = bot_id, bag}, acc ->
          case get_current_tome(bag.tomes, tomes) do
            nil -> {:cont, acc}
            [] -> {:cont, acc}
            tome -> {:halt, gen_select_action(bag, tome, bot_id)}
          end

        _, acc ->
          {:cont, acc}
      end)

    keep_all([internal(:broadcast) | select_actions])
  end

  def handle_event(
        :internal,
        {:bot_draft_picks, "bot_" <> _ = bot_id},
        :on,
        %Data{bags: bags, tomes: tomes, indexes: indexes}
      ) do
    case bags do
      %{^bot_id => bot_bag} ->
        actions =
          case get_current_tome(bot_bag.tomes, tomes) do
            nil -> [internal({:bot_draft_picks, get_next_player(bot_id, indexes)})]
            [] -> [internal({:bot_draft_picks, get_next_player(bot_id, indexes)})]
            tome -> gen_select_action(bot_bag, tome, bot_id)
          end

        keep_all([internal(:broadcast) | actions])

      _bags ->
        keep_all([])
    end
  end

  def handle_event(:internal, {:bot_draft_picks, _}, _, _),
    do: keep_all([])

  def handle_event({:timeout, {:pick, player_id}}, round, :on, %Data{bags: bags}) do
    with bag <- Map.get(bags, player_id),
         ^round <- get_round(bag),
         index <- bag.preselected || 0 do
      keep_all([internal({:select, player_id, index})])
    else
      _miss ->
        keep_all([])
    end
  end

  def handle_event({:timeout, _}, _, _, _),
    do: keep_all([])

  def handle_event({:call, from}, _, :finished, _),
    do: keep_all([reply(from, {:error, :finished})])

  def handle_event(
        {:call, from},
        {:preselect, user_id, scroll_index},
        :on,
        %Data{bags: bags, tomes: tomes} = data
      ) do
    with %{tomes: [h | _]} = bag <- Map.get(bags, user_id),
         [_ | _] = tome <- Enum.at(tomes, h),
         {scroll, _} when is_integer(scroll) <- List.pop_at(tome, scroll_index),
         new_bag <- %{bag | preselected: scroll_index},
         new_bags <- Map.put(bags, user_id, new_bag) do
      keep_state(%{data | bags: new_bags}, [reply(from, :ok)])
    else
      _ ->
        keep_all([reply(from, {:error, :invalid})])
    end
  end

  def handle_event(
        {:call, from},
        {:select, user_id, scroll_index} = selection,
        :on,
        %Data{bags: bags}
      ) do
    case Map.get(bags, user_id) do
      %{preselected: ^scroll_index} ->
        keep_all([reply(from, :ok), internal(selection)])

      _ ->
        keep_all([reply(from, {:error, :invalid})])
    end
  end

  def handle_event({:call, from}, :details, :on, %Data{} = data),
    do: keep_all([reply(from, {:ok, data})])

  def handle_event({:call, from}, :start, :on, _),
    do: keep_all([reply(from, :ok), internal(:new_round)])

  @spec add_tome_to_next_player(
          list(non_neg_integer()),
          player_id(),
          tome_index :: non_neg_integer(),
          map_of_bags :: map()
        ) :: map()
  def add_tome_to_next_player([], _, _, bags), do: bags

  def add_tome_to_next_player(_, next_player, tome_index, bags) do
    next_player_bag =
      bags
      |> Map.get(next_player)
      |> Map.update!(:tomes, fn tomes -> tomes ++ [tome_index] end)

    Map.put(bags, next_player, next_player_bag)
  end

  @spec get_next_player(player_id(), list(non_neg_integer())) :: player_id()
  def get_next_player(player_id, indexes) do
    index = Map.get(indexes, player_id)
    Map.get(indexes, index + 1, Map.get(indexes, 0))
  end

  @spec get_timeout(Bag.t(), timer :: non_neg_integer()) :: non_neg_integer() | false
  def get_timeout(%{tomes: [_ | _], scrolls: scrolls}, initial_timer),
    do:
      get_timeout_for_round(
        initial_timer,
        length(scrolls),
        @tome_size,
        @time_to_cut_x_round
      )

  def get_timeout(_, _), do: false

  @spec get_timeout_for_round(
          timer :: non_neg_integer(),
          round :: non_neg_integer(),
          rounds_per_tome :: non_neg_integer(),
          timer_decrease_p_round :: non_neg_integer()
        ) :: non_neg_integer()
  def get_timeout_for_round(
        initial_timer,
        current_round,
        rounds_per_tome \\ @tome_size,
        time_to_cut_x_round \\ @time_to_cut_x_round
      )

  def get_timeout_for_round(initial_timer, current_round, rounds_per_tome, time_to_cut_x_round) do
    initial_timer - Integer.mod(current_round, rounds_per_tome) * time_to_cut_x_round
  end

  @spec set_timeout(Bag.t(), DateTime.t(), timeout :: non_neg_integer()) :: Bag.t()
  def set_timeout(%Bag{} = bag, time_now, timeout),
    do: %Bag{bag | timer: DateTime.add(time_now, timeout, :millisecond)}

  @spec select(Ecto.UUID.t(), non_neg_integer(), __MODULE__.t()) ::
          {:ok, Ecto.UUID.t(), tome :: non_neg_integer(), __MODULE__.t()} | :error
  def select(
        user_id,
        scroll_index,
        %Data{bags: bags, tomes: tomes, indexes: indexes} = data
      ) do
    with %Bag{scrolls: scrolls, tomes: [h | rem]} = bag <- Map.get(bags, user_id),
         [_ | _] = tome <- Enum.at(tomes, h),
         {scroll, new_tome} when is_integer(scroll) <- List.pop_at(tome, scroll_index),
         new_tomes <- List.replace_at(tomes, h, new_tome),
         new_bag <- %Bag{bag | preselected: nil, scrolls: [scroll | scrolls], tomes: rem},
         bags <- Map.put(bags, user_id, new_bag),
         next_player <- get_next_player(user_id, indexes),
         bags_final <- add_tome_to_next_player(new_tome, next_player, h, bags) do
      {:ok, next_player, h, %{data | bags: bags_final, tomes: new_tomes}, scroll}
    else
      _error ->
        :error
    end
  end

  @spec get_round(Bag.t() | list(non_neg_integer())) :: non_neg_integer()
  def get_round(%Bag{scrolls: scrolls}) do
    get_round(scrolls)
  end

  def get_round(scrolls) when is_list(scrolls) do
    length(scrolls)
    |> Integer.floor_div(@tome_size)
    |> Kernel.+(1)
  end

  @spec get_current_tome(list(non_neg_integer()), list(non_neg_integer())) ::
          list(non_neg_integer()) | nil
  def get_current_tome([current | _], tomes),
    do: Enum.at(tomes, current)

  def get_current_tome([], _), do: nil

  #
  @spec gen_select_action(Bag.t(), tome :: list(Scroll.id()), bot_id :: String.t()) :: list({:next_event, :internal, {:select, String.t(), non_neg_integer()}})
  def gen_select_action(%{scrolls: scrolls}, tome, bot_id) do
    selection_ctx =
      gen_selection_ctx(scrolls)

    case find_bot_selection(selection_ctx, tome) do
      nil ->
        []

      index ->
        [internal({:select, bot_id, index})]
    end
  end

  @types [:creature, :nimble, :conjuration, :hex]
  def find_bot_selection(selection_ctx, possible_scrolls) do
    possible_scrolls
    |> Enum.with_index()
    |> weight_scrolls_for_selection(selection_ctx)
    |> List.first()
    |> case do
      {_, selection, _, _} -> selection
      _ -> nil
    end
  end

  def weight_scrolls_for_selection(scrolls_w_index, ctx) do
    Enum.map(scrolls_w_index, fn {scroll_id, index} ->
      %{type: type} = scroll = :persistent_term.get({:scroll, scroll_id})

      domains_quantity_weight = get_domains_quantity_weight(scroll)
      domains_weight = get_domains_weight(scroll, ctx)
      cost_weight = get_cost_weight(scroll, ctx)
      type_weight = Map.get(ctx, :"#{type}_preference", 1)

      {scroll_id, index, domains_quantity_weight + domains_weight + cost_weight + type_weight,
       scroll.name}
    end)
    |> Enum.sort_by(fn {_, _, weight, name} -> {weight, name} end, :desc)
  end

  def get_domains_quantity_weight(%{domain: domains}) do
    case length(domains) do
      0 -> 2.5
      1 -> 2
      2 -> 1.5
      _ -> 1
    end
  end

  def get_domains_weight(%{domain: []}, _ctx),
    do: 1

  def get_domains_weight(%{domain: domains}, ctx) do
    total_domains = length(domains)

    percentuals_summed =
      Enum.reduce(domains, 0, fn domain, acc ->
        Map.get(ctx, :"#{domain}_percentual", 0) + acc
      end)

    percentuals_summed / total_domains + 1
  end

  def get_cost_weight(%{cost: cost}, ctx) do
    cost_total = String.length(String.replace(cost, ["B", "0"], "", global: true))
    cost_base_weight = abs(2 - cost_total + 1) * -1

    cost_domain_map =
      cost
      |> String.split("")
      |> Enum.reduce(%{}, fn char, acc ->
        case char do
          "B" ->
            acc

          "1" ->
            acc

          "X" ->
            acc

          _ ->
            Map.update(acc, char, 1, &(&1 + 1))
        end
      end)

    cost_domain_weight =
      Enum.reduce(cost_domain_map, 0, fn {k, v}, acc ->
        percentual = Map.get(ctx, :"#{Aether.convert_aether_char_to_atom(k)}_percentual", -1)
        percentual_inverse = 1 - percentual
        final = percentual_inverse * -v

        acc + final
      end)

    cost_base_weight + cost_domain_weight
  end

  @spec gen_selection_ctx(list(Scroll.id())) :: map()
  def gen_selection_ctx(scrolls) do
    Enum.reduce(scrolls, %{domains: [], types: [], scrolls_total: length(scrolls)}, fn scroll_id,
                                                                                       acc ->
      %{domain: domains, type: type} =
        :persistent_term.get({:scroll, scroll_id})

      Enum.reduce(domains, acc, fn domain, acc ->
        acc
        |> Map.update(domain, 1, &(&1 + 1))
        |> Map.update!(:domains, &[domain | &1])
        |> Map.update({domain, type}, 1, &(&1 + 1))
      end)
      |> Map.update!(:domains, &Enum.uniq(&1))
      |> Map.update!(:types, &Enum.uniq([type | &1]))
      |> Map.update(type, 1, &(&1 + 1))
    end)
    |> add_percentuals_to_selection_ctx()
  end

  def add_percentuals_to_selection_ctx(%{domains: _, types: _, scrolls_total: total} = ctx)
      when total > 0 do
    ctx
    |> add_domain_percentuals_to_selection_ctx()
    |> add_type_percentuals_to_selection_ctx()
  end

  def add_percentuals_to_selection_ctx(ctx),
    do: ctx

  def add_domain_percentuals_to_selection_ctx(%{domains: domains, scrolls_total: total} = ctx)
      when total > 0 do
    Enum.reduce(domains, ctx, fn domain, acc ->
      domain_total = Map.get(acc, domain)
      percentual = Float.round(domain_total / total, 2)

      acc
      |> Map.put(:"#{domain}_percentual", percentual)
    end)
  end

  def add_type_percentuals_to_selection_ctx(%{types: types, scrolls_total: total} = ctx)
      when total > 0 do
    Enum.reduce(types, ctx, fn type, acc ->
      type_total = Map.get(acc, type)
      percentual = Float.round(type_total / total, 2)

      acc
      |> Map.put(:"#{type}_percentual", percentual)
    end)
    |> add_contextual_percents_to_selection_ctx()
  end

  def add_contextual_percents_to_selection_ctx(%{scrolls_total: total} = ctx) when total > 0 do
    Enum.reduce(@types, ctx, fn type, acc ->
      current = Map.get(ctx, type, 0)
      current_percentual = Map.get(ctx, :"#{type}_percentual", 0)

      type_preference =
<..        get_contextual_type_percentual_preference(type, current, current_percentual, total)

      acc
      |> Map.put(:"#{type}_preference", type_preference)
    end)
  end

  def get_contextual_type_percentual_preference(:creature, _, percentual, _) do
    case percentual do
      p when p < 0.50 -> 2
      p when p < 0.65 -> 1.5
      _ -> 1
    end
  end

  def get_contextual_type_percentual_preference(:nimble, _, percentual, total) do
    case percentual do
      p when p < 0.25 and total > 12 -> 2
      p when p < 0.25 -> 1.5
      _ -> 1
    end
  end

  def get_contextual_type_percentual_preference(:conjuration, _, percentual, total) do
    case percentual do
      p when p < 0.15 and total > 12 -> 2
      p when p < 0.15 -> 1.4
      _ -> 1
    end
  end

  def get_contextual_type_percentual_preference(:hex, _, percentual, total) do
    case percentual do
      p when p < 0.10 and total > 12 -> 2
      p when p < 0.10 -> 1.2
      _ -> 1
    end
  end
end

defmodule TestDraft do
  def start() do
    Scrollrack.persist_term_scrolls()
    player_id = Ecto.UUID.generate()
    %Schemas.Draft.Pod{id: pod_id} = pod = Contexts.Draft.create_bot_pod(player_id)

    {:ok, sup_pid} = Statems.Draft.Pod.Supervisor.start_link()
    {:ok, pod_pid} = Statems.Draft.Pod.Supervisor.start_child(pod)
    {:ok, draft} = Statems.Draft.Pod.interact(pod_id, :details)

    %{
      player_id: player_id,
      pod_id: pod_id,
      draft: draft,
      supervisor_pid: sup_pid,
      pod_pid: pod_pid
    }
  end
end
```
<div id="after-bot-draft">
  <a href="#before-bot-draft">go to beginning of bot_draft.exs</a>
</div>

As said you can also download the file containing the code and then as long as you have elixir you can run it as a script, with iex, where we can try out the draft engine interactively.

<a href="/downloads/bot_draft.exs.txt" target="_blank" class="">bot_draft.exs</a>

<div class="article-index">
  <ol>
    <li class="main-link"><a href="#index-dependencies">Dependencies</a></li>
    <li class="main-link"><a href="#index-scrollrack">Scrollrack</a></li>
    <li class="main-link"><a href="#index-statems-helpers">Statems Helpers</a></li>
    <li class="main-link"><a href="#index-schemas">Schemas & Types</a></li>
    <li class="main-link"><a href="#index-pod-supervisor">Pod Supervisor</a></li>
    <li class="main-link"><a href="#index-statems-pod">Statems.Draft.Pod</a></li>
    <li class="main-link"><a href="#index-statems-pod-api">Statem API</a></li>
    <li class="main-link"><a href="#index-statems-pod-init">Pod Init</a></li>
    <li class="main-link"><a href="#index-statems-pod-broadcast">Pod Updates Broadcasts</a></li>
    <li class="main-link"><a href="#index-statems-pod-flow">Statem Engine Flow</a></li>
    <li class="main-link"><a href="#index-statems-pod-bots">BOT Logic and flow</a></li>
    <li class="main-link"><a href="#index-statems-pod-example">Running an example in IEX</a></li>
  </ol>
</div>

<div class="index-anchor" id="index-dependencies">
  <h4>Dependencies</h4>
</div>

Going through it we install some type helpers for our schemas. I've trimmed some parts but I still think it makes sense to show the code using those.

```elixir
Mix.install([
  {:typed_enum, "~> 0.1"},
  {:typed_ecto_schema, "~> 0.4.1", runtime: false},
  {:typed_struct, "~> 0.3.0"}
])
```

<div class="index-anchor" id="index-scrollrack">
  <h4>Scrollrack</h4>
</div>

Then we have a placeholder `Scrollrack` module. In the real application this module is responsible for reading the `scroll` definitions from in disk files, loading them into memory at application start through `persistent_term` and assembling a final json file containing the whole definition (as the number of items grows they can be further segregated, first into domains - instead of all_scrolls.json be air.json, earth, etc... - and once those grow significantly again, further into domains plus types - air_creatures.json, air_nimbles, etc, without changing significantly the architecture of the existing system to load/download/client setup. 

The scrolls here contain just the basic for allowing our bots to draft pick choices that at least are closer to an average human drafted pick. I've kept an example as a `@doc` but even that doesn't show many of the possible variations and effects a scroll can have since they're `nil` in the example.

We then have a `persist_term_scrolls()` that just uses those hardcoded scroll definitions and places them in `persistent_term`. In the real app this happens automatically on application start and since they don't change throughout a node deployment `persistent_term` is the ideal place to store them and allow our application to refer to any scroll from anywhere when running by its `id` or `slug`.

<div class="index-anchor" id="index-statems-helpers">
  <h4>Statems.Helpers</h4>
</div>

Following that we have the `Statems.Helpers` that we already talked.

<div class="index-anchor" id="index-schemas">
  <h4>Schemas & Types</h4>
</div>

Then we have the `Schemas` that are used for the draft. These are for the most part fairly the same since I also included the custom `Types`, `Schemas.Draft.Pod.Pick`, `Schemas.Draft.Pod.Bag`, `Types.Draft`, `Schemas.Draft.Pod` and `Types.Duel.Game.Aether`. We have a very slimmed down context for the draft, `Contexts.Draft`. In the real version it's also responsible for the `glue` connecting the `draft picks` and the `storage layer` but here just has some helpers for creating the schemas and populating them for a draft start.

<div class="index-anchor" id="index-pod-supervisor">
  <h4>Statems.Draft.Pod.Supervisor</h4>
</div>

I've included a supervisor, `Statems.Draft.Pod.Supervisor`, that allows starting globally registered drafts dynamically under a supervision tree. There's two major differences from a running version, first the supervisor is running under the application supervising tree and second, it only takes a `Pod` id, instead of the full `Pod`. It could be made either way and be solid, here we do it so it's all self contained without any database interaction, while in production we favour having an `"API"` that just takes the `id`, to mimic what the client view (a `liveview` in this case) has access by default - the `pod id` in the `url` - which allows it to always ask for the right `pod` (registered globally) even across node deployments and reconnections - since then the logic to access them has a `self starting` logic.

<div class="index-anchor" id="index-statems-pod">
  <h4>Statems.Draft.Pod</h4>
</div>

Finally the main subject of this writeup the `statem` definition itself, `Statems.Draft.Pod`. I've kept the general structure, removed some handles and simplified some parts of this since it's already lengthy if it has to work in full. There's a part of the structure that is repeated across many `statem`s I write but isn't something I automated because it always has some little detail that differs.

We declare this as a `@behaviour :gen_statem` and `import Statems.Helpers`, alias a bunch of modules, set a bunch of module attributes to use throughout the module and a module struct. Because this is a `gen_statem`, as such an independent process, I organise it in this way, the `struct` the module defines is the `data` to be used in any running instance of this `process`. To reinforce this idea I alias the module as `Data` and then refer throughout the handles as `%Data{}`.

<div class="index-anchor" id="index-statems-pod-api">
  <h4>Statems.Draft.Pod API</h4>
</div>

We also have a bunch of specs and helper functions to figure out what is the `:global` id of a given "pod" from its `pod id` alone. Meaning can be used to create a deterministic `global id` that always and only refers to a single possible `draft pod`.

I always include an `interact` function that just abstracts finding the `gen_statem` instance it needs from the base identifying property or starting one if it's not running (becoming a call/start and call idempotent procedure). Depending on the needs of the `statem` I might include a `cast` or `info` to abstract finding the instance but then `casting` or sending simply a `message`. Here we don't need any other (they're good to refresh running processes out-of-band, where worse case scenario is for a while being out of correct data, and that's preferable to possibly locking a process across boundary calls).

<div class="index-anchor" id="index-statems-pod-init">
  <h4>Statems.Draft.Pod Init</h4>
</div>

I kept the logic to create a `valid` `draft pod` and loading it outside of the concerns of this article, in the app it actually takes the id and then loads it from the database, plus it stores every player pick along with the draft as it goes, this guarantees it can be always restored to at least the last pick choice before the process crashes (or the node, or the database, or the connection, etc). Here we would pass the `%Pod{}` in the state we want to start from. This also means it's easy to test it. We can build a `Pod.t()` schema and start a `Statems.Draft.Pod` exactly as needed to test and then interact with it, assert on the results, assert some things can't happen, etc.

The init handle is specific to the `Pod` being a `bot draft`, because it expects a single `player_id` under `players`. We just reduce through a list of `player_id` plus another 7 fake ids, and create the basic initial `bag` for each player. The `bag` is where we accumulate the `players picks` and current choices, timers, etc.

Because before finishing the `draft` the bags are simply a reflection of `picks` we can simply rebuild the correct state by replaying the `player picks` that we would be storing. Once the draft is finished we can just ditch the actions and store the final choices in the individual bags that are added to the player's account.

<div class="index-anchor" id="index-statems-pod-broadcast">
  <h4>Statems.Draft.Pod Updates Broadcast</h4>
</div>

We also have just a placeholder for the broadcast handle. Basically when running live we emit broadcasts for anyone listening on the pod topic, this allows it to work exactly the same under a single player bot draft or a real 8 player draft pod. No changes needed because no-one subscribes as the "bot" players and the players that are on the draft are subscribed through their active tabs - every time a change occurs in the state the draft state is published and filtered per user before going out to the client - either as payloads or liveview updates. In case this ever becomes a bottleneck we can do the filtering before emiting the events.

<div class="index-anchor" id="index-statems-pod-flow">
  <h4>Statems.Draft.Pod engine flow</h4>
</div>

`set_tomes` is what kickstarts the actual engine, it's called during initial load and then after each pick, and in case it's in the `:loading` state it then goes on to `apply_picks` and there (if any `picks` exists) brings the just loaded draft into the last correct state by replaying every pick in order, otherwise it tests the round.

Since it starts in `:loading` state and we end up running `:apply_picks` (which we will always on the first time the `statem` is interacted with) and this ends up doing `:set_on`. On `:set_on` we first set the timers for all players in the draft.

If we're on an already loaded draft, then it simply checks first if the list of current tomes for draft for every player is empty - if it is it means we are on the end of a pack and so we should either finish the draft if it's the last pack or open a new round of packs.

Either way we set the bags as they should be accordingly and then do `check_if_bot_draft`.

Before the `bot` picks lets check the `:set_timers` for `:all` players. There's a similar one that is by `player` alone and the logic is very similar. We iterate through each bag, get the round, get the current timeout, set the timeout again - because the timeouts are segregated and identified by `player id`, `{:pick, player_id}`, and the way `gen_statem` works, they're simply "overwritten" everytime we set the same named timer - can think of it as idempotency - and because they're dealt internally by the `statem` itself we know they are set in order as we return from the handles.

Remember a process in Elixir has a mailbox and in case of `gen_statem`s also an internal list of actions that the writer can program through the `callbacks`, by returning the wanted actions definitions in that return value. So what happens is you have a box with data and state that you can interact with messages (perhaps, similar in API to an object), by knowing how to find or address the process - while these messages and events can be asynchronous the happen inside the `"process"` as a sequential list of actions.

So if you return from a `gen_statem` handle running a process:

```elixir
{:keep_state_and_data, [{:reply, from, {:ok, :done}}, {:next_event, :internal, :persist}, {:next_event, :internal, :broadcast}]}
```

You kow it will first `reply` to the address (`from`) and then set an `:internal` event `:persist`, followed by another `:internal` event `broadcast`. The next events can carry any term too when we're using the `:handle_event_function` for the `callback_mode()` along with arbitrary state definitions.

The only exception (if I'm not mistaken to this ordering) are `reply` actions themselves - inside a single returned list - I think those are given priority. If you want to actually only `"reply"` after some additional handle is ran, like after `:persist`, then instead of `{:reply, from, {:ok, :done}}` on this initial call handle, you would do something like `{:next_event, :internal, {:persist, reply_to: from}}`, and on that handle do all you need and then on the action list include the `{:reply, from, whatever}`.

What this means is that you can make assumptions about how the process moves through its states - and you get all the nice tools to model proper state machines if or when your process requires it.

Once we loaded any initial picks, turned the state into `:on`, set the timers and checked for a `new_round` we check if the draft is with bots. Again, a simple handle that when the pod only has a single `player_id` assumes it's a bot draft. When it is so we trigger the `bot_draft_picks` internal event.

Here we go through all the `"bags"` that we have (meaning if we change the number of players for some reason - special draft, or new form of game-play - in the pod creation logic, etc, it will still work correctly herre), that are `keyed` by the player id in the pod data. Since real player IDs will be valid UUIDs and `bots` start with `bot_` we can simply filter out non-bot ids. We order them in descending order and give a higher order for the `bot_ids`.

With the `bot_ids` we reduce through them getting the tome (pack) that they should be picking from if any. If the bot has a tome to pick from we generate a pick selection action for it and add that to the action list we're accumulating - if no current tome to pick from we just continue iterating without changing the action list.

In the end we simply return those actions in the list we accumulated, which will be a list of `{:select, bot_id, selection_index}`. How we arrive at that `selection_index` for the `bot_id` in a running draft is the purpose of our "bot logic" and we will dive into it right after finishing the overall logic.

It's important to notice also one detail in case you haven't yet, you can see that there's two different `{:select, player_id, scroll_index_in_the_tome_to_pick}` handles/actions. One is the `:internal` version, that does the pick without any other check than that if it's a valid pick. Because only we can issue these `:internal` events from the inside of the running `gen_statem` we can use them to do things like forcing `timeout picks` (the player has let the clock run out), forcing bot picks, etc. On the other hand, if the pick comes through the UI, then it's also checked that it is matching the `preselected` scroll. This means a user can only pick after he pre-selects - which makes sense in the UI flow - a user clicks a scroll, it automatically becomes pre-selected, then he clicks it again and it selects it. This also allows for the timeout pick to reflect a pre-selected scroll, so they can pre-select and then think a bit more before commiting.

We'll see on the iex shell the interaction with the process but in the prod version, except for some tests, this is all interacted with by client side UIs from a liveview, but guaranteed to be correct from our end. A user can only select a valid scroll index.

Before diving into the bot logic we'll just touch on the remaining functionality that makes this work. So when a single "selection" is made effective, the engine takes the new pack after the pick is removed from it and simply moves it from this player's current tome list into the next player in the tables current tome list, as the last item in it.

If the other player had no current tomes then he will be updated through the broadcast (his own liveview instance is subscribed to the topic in question on mount) and we can show the new pack for him to pick. On the back-end new timers were set for his pick. The tome is always added to the end of the list, if he still had packs to pick he needs to pick from them first, new tomes go to the end of list as well so it always reflects the right packs and choices to each player accordingly.

Lastly, when no more picks nor more tomes exist we simply close down the `statem`.

<div class="index-anchor" id="index-statems-pod-bots">
  <h4>Statems.Draft.Pod Bots Logic</h4>
</div>

So now on to the bot draft logic. We kick it off by checking that we are on a bot enabled draft, then if so we create the list of selections for each bot. For each player we take the corresponding bag which contains the scrolls they already have and create a "context" for the current pick. What this means is we take every scroll we have and then we calculate the domains we already have, the types and how many scrolls we have - with this we can calculate some percentages - from our picks what percentage were creatures, what percentage were for domain X, how many domains.

Because the bag really just stores mostly lists of integers, with the integers being the scroll ids it's a very economic format to store. We can then, using `persistent_term`, retrieve all scrolls one by one while iteracting through the picks the player already made and calculate those stats.

Then we go through the actual choices the bot can make. To decide which scroll should be picked we then calculate the value of each possible scroll according to the "weights" we decided to evaluate. So we have:

- Domains Quantity Weight
- Domains Weight
- Cost Weight
- Type Weight

The `domains quantity weight` evaluates how many domains the scroll in question has. We give higher weight to scrolls that have the least domains. Why is this important? Bevcause spells with 0 domains can be usually played by any deck optimally so they should be picked up at a higher rate because they will always (on average) fit whatever list you end up with.

The weight decreases 0.5 units per domain increase up to 2 domains, beyond that it's always 1 weight. So anything with 3 or 5 domains is 1 weight and as such from that part always weighted lower to be chosen.

Then we have `domains weight` itself, that takes the domains once again but calculates instead a percentage of those scrolls are part of the current choices already made. The reason is that if we have 8 scrolls, and 4 are `earth`, 2 `air` and 2 `fire`, we are closer theoretically to be playing `earth` at that point, since we have double the scrolls of that domain, so given scrolls of these or other domains we will value more for this pick with this particular weight those that match more percentage of what we already have in that domain. Again because in the end this is the pick that more likely makes sense at this time.

Additionally we have the `cost weight` calculation. This is calculated using two simple units. One the `cost_base_weight` where we calculate the lenght of the cost string and simply calculate it's difference from our "idealized" average cost, which is 2. So the more below or above 2 the cost gets more penalized it is in the weight result - for less than 2 because usually they're less powerful so you don't want too many of those, and for more because they although more powerful by definition you also don't want to have more than the necessary to round your "play curve".

The other unit is the domain weight in the cost - simply takes the cost string (e.g.: `BFFX`), strips it from aether costs that don't matter, like `B` which is blood, and paid with life points so can always be paid, and `X` that can be paid with any `aether` (ending with `FF`, `[:fire, :fire]`) and uses this to calculate a domain weight cost for this particular weight. Then sums both of these values to generate an independent weight value for this scroll.

Lastly we just retrieve the type "preference" from the `context` we assembled earlier. With all this we sum the values for each scroll and lastly we just sort the list of picks by their weight, in a `descending` order, giving us the scroll that across the weights we set has the highest value. Notice that we can keep adding dimensions, improving them to reflect better, use machine learning once we have enough data between "draft picks" and "game-play success", to either do automatic full drafts or reflect some other dimensions we find in the datasets.

Nonetheless, this by itself guarantees that we have a baseline, each bot will attempt and in normal situations succeed in creating a final 60 scroll list with an average of +-60% creatures, +-20% nimbles, +-10% conjurations, +-10% hexes, divided across 3 domains manily (more than 90%) with perhaps a few mispicks when no other from the desired domains is available. On top of that it will also select with more preference for 2 aether total cost, a bit less for 1 and 3, a bit less for 0 and 4, and then everything else at the same point.

All this is easy to change, add to, or remove but provides the human player a draft that won't be simply based on randomness, but actual picks by the bots in the "general" correct direction. Obviously since there can be, in practical terms, close to infinite combinations of abilities and effects that a scroll can have, and because these themselves would in a real world draft by humans assume different values accordingly to their synergies, combinations and functions in a draft at different points of the draft it's not perfect but should provide enough variance in a logical way.

Maybe a scroll that costs 6 aether and 3 of them from a single domain, even if it is really powerful, if you open or get passed that scroll in the last pack, where you already have settled in another 3 domains strongly, won't have the same value as if you open it on the first pack, because there you still have almost the entirety of the draft to pick choices that will maximize and benefit that first pick. While if you're on the 3rd pack, if you haven't moved into that direction yet, adding a powerful scroll that requires 3 aether of a single domain you aren't using at all by then is probably not a very good pick - the power of the scroll hasn't changed, but playing it means you need to re-architect your list and perhaps you're just better off with a fairly weaker scroll that fits like a glove in your current picks.

So the logic accounts a bit for that but without a proper machine learning algo can't make the best option naturally just by average coincidence, encoding all possible details that have a weight in a real situation is overly complex if by hand.

<div class="index-anchor" id="index-statems-pod-example">
  <h4>Statems.Draft.Pod running example in IEX</h4>
</div>

Now lets actually run this so you can see it in action. To do that copy the file contents over to a `bot_draft.exs` and then just execute it with:

```bash
elixir bot_draft.exs
```

This first installs through `mix` the dependencies specified in the `Mix.install` command and then opens an interactive elixir shell that is loaded with the contents (modules) of that file. You could also start this as an identifiable node and even interact through distribution if you connect both shells afterwards, since the `gen_statem` is registered globally and as such would be shared between any nodes meshed together while being publicly visible to interaction.

To test the draft engine from the `iex shell` we opened we run `TestDraft.start()`. This just wraps the needed actions to start a working draft. It's the last module in the `bot_draft.exs`. In `start()` we first run the `persistent_term` routine, then we generate a fake UUID to represent our single-player and then we create the bot pod, using the `Contexts.Draft` functionality. This creation just generates a properly populated `Schemas.Draft.Pod.t()` for the player with the ID we provide. This gives a `Pod` with an ID too. In the real running version we would store it in the database as part of that `Contexts.Draft` call. We then start the `Pod` supervisor, we start the draft by its `start_child` function and ultimately ask for the details of the draft. 

```elixir
%{draft: draft, player_id: player_id, pod_id: pod_id} = TestDraft.start()
```

You should see output:

```elixir
16:11:23.954 [info] ::: ::: Starting Elixir.Statems.Draft.Pod.Supervisor ::: :::
BROADCASTING pod 3e3d644f-7ea8-4b70-96da-a230ede88b5e
```

If we want to see the current bag for the player we can do:

```elixir
bag = Map.get(draft.bags, hd(draft.pod.players))
```

```elixir
%Schemas.Draft.Pod.Bag{
  id: "39e664b6-488c-4768-bf26-4edb3adc0b39",
  index: 0,
  tomes: [0],
  scrolls: [],
  preselected: nil,
  timer: ~U[2026-05-30 15:48:08.194658Z]
}
```

And to see the contents of choosable scrolls for the current tome:

```elixir
tome = Enum.at(draft.tomes, hd(bag.tomes))
Enum.each(tome, fn id -> IO.inspect(:persistent_term.get({:scroll, id})) end)
```

And we can simulate an interaction with:

```elixir
:ok = Statems.Draft.Pod.interact(pod_id, {:preselect, player_id, 0})
:ok = Statems.Draft.Pod.interact(pod_id, {:select, player_id, 0})
```

You should now see a whole bunch of `BROADCASTING...` lines, this is because there's 8 players, 7 bots. So the first player in the "line", selects one, passes its tome to the next, now this player selects 1 from the original tome, plus 1 from the newly passed tome, the following player will pick 3 in total, then 4, etc. So what you're seeing is the broadcasts for each one of these picks that each "bot" executes automatically because it keeps receing new tomes.

And if now you ask for the details, once again, take the bag and see the current selected ones then you should see whatever was the scroll at index 0 of your current draft tome.

```elixir
{:ok, draft} = Statems.Draft.Pod.interact(draft.id, :details)
%Schemas.Draft.Pod.Bag{scrolls: scrolls} = bag = Map.get(draft.bags, hd(draft.pod.players))
```

```elixir
%Schemas.Draft.Pod.Bag{
  id: "39e664b6-488c-4768-bf26-4edb3adc0b39",
  index: 0,
  tomes: [7, 6, 5, 4, 3, 2, 1, 0],
  scrolls: ~c"P",
  preselected: nil,
  timer: ~U[2026-05-30 15:49:08.650870Z]
}
```

You can now see the `scrolls` column/field has now 1 item, and if you check the numeric value of it (sometimes it's displayed as a valid character, since it's in a list, and you can check with `?` followed by character, so if `scrolls: ~c"P"` you can check the integer value of "P" by doing `?P`. This should be the id of the scroll in the index 0 we pre-selected and then selected.

You can keep doing this until you finish the draft, you can check what the bots have chosen too. For instance now if we check the last bot in line (7), it should have a bunch of selections already done, while `bot_1` has only 2:

```elixir
%Schemas.Draft.Pod.Bag{scrolls: scrolls_1} = bag = Map.get(draft.bags, "bot_1")
%Schemas.Draft.Pod.Bag{scrolls: scrolls_2} = bag = Map.get(draft.bags, "bot_7")
```

```elixir
Enum.each(scrolls_1, fn id -> IO.inspect(:persistent_term.get({:scroll, id})) end)
Enum.each(scrolls_2, fn id -> IO.inspect(:persistent_term.get({:scroll, id})) end)
```
In my case `bot_1` was:

```elixir
%{
  id: 44,
  name: "Fire Geist",
  type: :creature,
  domain: [:fire],
  slug: "fire_geist",
  cost: "1F"
}
%{
  id: 47,
  name: "Fire Spike",
  type: :nimble,
  domain: [:fire],
  slug: "fire_spike",
  cost: "1F"
}
```

Which means it snatched two spells from fire, with 2 cost, single domain, one creature and a nimble. Seems very plausible. In this case as you can see if you know the spells in question, these 2 scrolls are pretty good by themselves and is a totally valid draft start.

For `bot_7` we have:

```elixir
%{
  id: 75,
  name: "Leviathan",
  type: :creature,
  domain: [:water],
  slug: "leviathan",
  cost: "WWW"
}
%{
  id: 40,
  name: "Rot",
  type: :nimble,
  domain: [:earth],
  slug: "rot",
  cost: "1EE"
}
%{
  id: 73,
  name: "Æther Sink",
  type: :conjuration,
  domain: [:water],
  slug: "aether_sink",
  cost: "1W"
}
%{
  id: 75,
  name: "Leviathan",
  type: :creature,
  domain: [:water],
  slug: "leviathan",
  cost: "WWW"
}
%{
  id: 40,
  name: "Rot",
  type: :nimble,
  domain: [:earth],
  slug: "rot",
  cost: "1EE"
}
%{
  id: 79,
  name: "Winged Sea Serpent",
  type: :creature,
  domain: [:water],
  slug: "winged_sea_serpent",
  cost: "11W"
}
%{
  id: 81,
  name: "Dryad",
  type: :creature,
  domain: [:earth],
  slug: "dryad",
  cost: "1E"
}
%{
  id: 10,
  name: "Sleep Paralysis",
  type: :nimble,
  domain: [],
  slug: "sleep_paralysis",
  cost: "111"
}
```

If you analyse the domains we have water and earth domains plus a domainless scroll. Again a good distribution between types, domains and costs.

You can now continue doing picks and the bots will keep selecting until you and them finish the draft in full.

The last thing we will touch on is the testability of this design. as you can see through the examples we just did in the `iex shell`, testing this is nothing else than doing the same but with somehow deterministic contents in a test file.

We can test that selecting a non-existing index for the current tome doesn't work:

```elixir
{:error, :invalid} = Statems.Draft.Pod.interact(pod_id, {:preselect, player_id, 19})
```

This is because now every tome has at least 1 less scroll, so the highest index possible will be 18 (indexes are 0 based, we start with 20 items, so initially 0 through 19 are the valid indexes to pick, after the first pick 0..18, next, 0..17, and so on.

If you create (for test purposes) deterministic packs you can test that their position in the pack matches the draft index when picking them, either by unit or e2e tests. You also can, if needed, create some more abstract tests where you simply test that you start with X scrolls on pack, where X is the pack size, that each pick you do matches the order of the indexes given, that out-of-bonds indexes can't be selected, etc, and that in the end, when you create an ordered list with all the scrolls picked you get exactly the same order list of scrolls that were included in the initial tomes.

This rounds up our article on `gen_statem`s. If you're interested in knowing more about erlang/elixir processes and architectures it allows for real-time systems, or hire me, get in touch.

In case you're programming in elixir/erlang and have not yet used `gen_statem`, even in exploratory form, perhaps give it another look under this lens, of a structured, synchronous in itself, but with an external asynch/can turn synchronous interface, process/program. It's an encapsulated program/unit of work, with its own context (usually data and state), that has an almost declarative structure by way of the `handle_event/4` and the options to pattern match in those callback definitions.

`GenServer` does indeed have many, and perhaps more importantly, most of the essential properties of an abstraction of a "server" (in the sense of a blackbox to the outer world with a defined interface/API) and so there's plenty of cases that for all practical terms it's totally fine to use `gen_server` and no real practical reason to perhaps refactor or rewrite those, or justify introducing a new process type that can have subtle differences in its behaviour, or there's code already to account for those "quirks" in the existing code, that perhaps would break - perhaps the default behaviour of the underlying abstraction would be different and now the "quirk" corrections ended leading to overcorrected data, or other reasons.

But if you're writing something new, or a library, perhaps use it instead. As I said, in plain benchmarks (truth be said, those were ran some time ago so would have to be redone to be sure...) but it performed better than `gen_server` in plain number of requests/s. It also has a better interface for when you need `{:continue, ...}` in the form of `internal events`, a much richer `timer` story, and when you need big guns you have an actual `state machine` API to code your process underneath without changing a single thing.

Also when you provide macros that work on the underlying process as an abstraction to the library functionality, when you have these options underneath, you can also surface to the user of the library additional ways of controlling the lifecycle of the process outside of your defined abstraction - continuations can now be surfaced to the user of the library without needing to fall back to message sending patterns, by using `internal events`, `timers` that can control lifecycles of forms, components, redirects, can also be surfaced to the user of the library without even changing the interface/contract of your library (obviously these handles have to be merged into the process definition, but that already happens currently in some form).

The code organisation becomes almost "visual" if we make use of pattern matching - and using pattern matching usually increases the type, shape, and form information of the data that is being used, so it also aids type specification that can be used internally to prove at least type correctness. Today possible with dialyzer/xir and increasingly so with elixir's type system. Ultimately it boils down to learning a few patterns and keywords and in exchange getting access to the same familiar process structure but with a much more well-rounded API and in-built capabilities.
