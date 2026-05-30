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

  defmacro keep_state(state, actions \\ []) do
    quote do
      {:keep_state, unquote(state), unquote(actions)}
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

    keep_state(%Data{data | bags: new_bags}, [internal(:bots_picks)])
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

  def handle_event({:timeout, _}, _, :finished, _),
    do: keep_all([])

  def handle_event({:timeout, {:pick, player_id}}, round, _, %Data{bags: bags}) do
    with bag <- Map.get(bags, player_id),
         ^round <- get_round(bag),
         index <- bag.preselected || 0 do
      keep_all([internal({:select, player_id, index})])
    else
      _miss ->
        keep_all([])
    end
  end

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
  @spec gen_select_action(bag :: map(), tome :: list(Scroll.id()), bot_id :: String.t()) :: list()
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
    cost_total = String.length(cost)
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
        get_contextual_type_percentual_preference(type, current, current_percentual, total)

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
