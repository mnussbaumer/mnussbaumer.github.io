---
layout: post
title: "Creating an Ecto Enum Type"
canonical_url: "https://dev.to/mnussbaumer/creating-an-ecto-enum-type-43dh"
date: 2022-01-06 16:30:00 +0700
categories: elixir ecto enum database type
---

Today I'm going to write about a little Ecto type I use regularly in most Elixir projects I work in. Basically, it's an enum wrapper that allows you to use a limited set of `:atoms` or `"strings"` values in your `Ecto` schemas definitions.

You might be wondering why a writing an enum type when `Ecto` ships with an `Ecto.Enum` type since `v3.5`. The main reason is the typing of the "type" itself and sharing this type between multiple schemas.

`Ecto.Enum` is great and very handy for most regular cases where you simply want to be able to deal with `atom` versions of strings in your schemas and Elixir logic, while being able to store them as strings automatically at the database level and be able to cast from `strings` to `atoms` - which is a pretty normal usecase when dealing with external parameters or inputs.

To show you what I mean lets say you have a schema like:

```elixir
defmodule User.Action do
  use Ecto.Schema

  schema("users_actions") do
    belongs_to(:user, User)

    field(:action, :string)
  end
end
```

Now, this is a totally valid schema. But if you have a limited set of values that `:action` might assume, right now you have no way of ensuring that when dealing with casting / inserting records of this type of schema the strings that are passed on are valid.

You might create this constraint at the database level, and if it's an essential part of your business logic you probably should keep alongside such constraint (although it makes it more onerous to update and change), but even if you don't, it would be nice to at least in your business logic be able to treat this `:action` field as having only a value from a set of known, well defined values, without having to explicitly read the field/param and manually checking it.

This is where `Ecto.Enum` can be used. Applying it to our schema means:

```elixir
defmodule User.Action do
  use Ecto.Schema

  schema("users_actions") do
    belongs_to(:user, User)

    field(:action, Ecto.Enum, values: [:bid, :request, :upload, :pay])
  end
end
```

In this case, we basically defined as valid values only that specific set of atoms, and by extension, when casting the schema. Any string counterpart that matches those atoms if converted would also be a valid value.

What this gives us is:

- the ability to cast params and make sure the `action` field is only valid if it conforms to those values, ex:

```elixir

params = %{"user_id" => 1, "action" => "bid"}

Ecto.Changeset.cast(%User.Action{}, params, [:user_id, :action])
```

- The ability to deal strictly with the `atom` versions in our programs logic, by being sure that after casting or retrieving a record from the database it will have to conform, ex:

```elixir

def log_action?(%User.Action{action: action}),
    do: action in [:bid, :pay]
```

And in many cases this is enough. But if we, for instance, want to use that same "action type" in another schema, then we have to duplicate its definition - and it's never really the same "type", we need to keep both (or more versions if used in more places) in synch. It also doesn't expose directly the normal `cast/load/dump` functions that can also be useful, in some contexts. For instance, you might want to cast a single string somewhere and depending on the success of such take a path on your code or another.

It also doesn't allow you to use `integer` versions for the underlying enum definition at the database level, nor do some useful tricks with casting & evaluating the value when dealing with "translations" - e.g. some external datasource uses some strings values that you don't want to store as they are, nor assume as valid for the field, but instead convert them to your app's version, or you simply assert their "equivalence", even if they have different representations - say one external source uses `bidding` but all others including yours use `bid`. You don't want to allow `bidding` to be stored or deal with it internally, but you want to be able to cast `bidding` as `bid`. 

There's also the fact that it doesn't expose a `@typespec` that you can use, which can be useful as well, when using dialyzer (which you, most of the times, should be) or simply when writing `@spec`s in your code's documentation (which you should always strive to do).

None of these are showstoppers though and if you don't need any of the additional functionality then just using `Ecto.Enum` is a significant improvement over bare strings and might cover all your requirements while being part of `Ecto` itself (meaning, no additional dependencies or code to maintain).

If we were to be writing a *single* `Ecto.Type`, following the examples before,  we could do something such as:

```elixir
defmodule User.Action.Type do
  @behaviour Ecto.Type

  def type, do: :string

  @valid_types [:bid, :request, :upload, :pay]
  @valid_strings Enum.reduce(@valid_types, [], fn t, acc -> [Atom.to_string(t) | acc] end)
  @valid_map Enum.reduce(@valid_types, %{}, fn t, acc -> Map.put(acc, Atom.to_string(t), t) end)

  @valid_inverse_map Enum.reduce(@valid_types, %{}, fn t, acc -> Map.put(acc, t, Atom.to_string(t)) end)

  @type t() :: unquote(Enum.reduce(Enum.reverse(@valid_types), &{:|, [], [&1, &2]}))

  def load(data), do: cast(data)

  def cast(data) when is_atom(data) and data in @valid_types, do: {:ok, data}
  def cast(data) when is_binary(data) and data in @valid_strings, do: {:ok, @valid_map[data]}
  def cast(_), do: :error

  def dump(data) when is_atom(data) and data in @valid_types, do: {:ok, @valid_inverse_map[data]}
  def dump(data) when is_binary(data) and data in @valid_strings, do: {:ok, data}
  def dump(_), do: :error

  def embed_as(_), do: :dump
  def equal?(data, data), do: true

  def equal?(data_1, data_2) do
    case {cast(data_1), cast(data_2)} do
      {{ '{{' }}:ok, same}, {:ok, same}} -> true
      _ -> false
    end
  end
end
```

Breaking it down:

- declare a behaviour of Ecto.Type for our module (this ensures we'll get warnings if we don't implement some functions that are expected to be defined for such behaviour)
- Declare a function `type/0` that returns the underlying type of the `Ecto.Type` (what is used to persist values of this type in the database), in this case `:string` (which ecto uses to abstract over all char, varchar, and text column types)
- Declare a typespec, that returns the valid atoms this type represents
- From the list of possible atoms, we create a list of their counterparts in string form, and two module attributes to store the mappings between atom<->string versions and vice-versa
- Declare all required behaviour functions for the Ecto.Type, since we have lists of both `atoms` and `strings` representing values we consider valid, we can use these in guards, to cast them appropriately to their atom form when in string, and the same for dumping them from atoms to strings.

As you can see it's easy to create an `Ecto.Type` that provides us with a more constrained set of values and helps us being more assertive on our code about what we expect.

It also makes it so that now we can actually cast those values independently of a schema. We can just do `User.Action.Type.cast("bid")` to validate if it's a valid action according to the type.

If you don't mind the copy pasting and changing the `@valid_types` list whenever you want to add a new type it's fine to use it like this. You can just use this as a blueprint then change the module name, and the list of values whenever you need to define another enum.

This already has some advantages, besides being able to cast the values independently of schemas, the type itself is now independent of their declaration in a schema, and as such can be used in multiple schemas as well. 

We could also easily make it use integers for its underlying representation, as in a conventional enum. But we can also write a macro that effectively does all of this for us with a single line of code. Our objective is to have a macro that enables us to write the previous `User.Action.Type` module as:

```elixir
defmodule User.Action.Type do
  use TypedEnum, values: [:bid, :request, :upload, :pay]
end
```

Followed by using it on the schema:

```elixir
defmodule User.Action do
  use Ecto.Schema

  schema("users_actions") do
    belongs_to(:user, User)

    field(:action, User.Action.Type)
  end
end
```

And if we want to deal with atoms in our inner logic, but store them as integers in the database, while still allowing the casting of stringed versions:


```elixir
defmodule User.Action.Type do
  use TypedEnum, values: [bid: 0, request: 1, upload: 2, pay: 3]
end
```

Let's breakdown what our module will need to do:

- Implement the `__using__` macro, so that it can be "used" in the way we wrote in the previous examples
- Be able to distinguish between a plain list of `atoms` or a `Keyword` list being passed as `values` to allow both integer and string based enums at the database layer
- Allow the caller to write any additional cases for the `cast`\`dump` logic of the `Ecto.Type` behaviour
- Implement automatically a typespec for it, reflecting all the valid atoms it can assume
- Raise on invalid `values` being passed to provide compilation time blowups.

This is our final module and we'll break it down after:

```elixir

defmodule TypedEnum do
  defmacro __before_compile__(_env) do
    # these are inserted in the before_compile hook to give opportunity to the
    # implementing module to define additional variations
    quote do
      def cast(_), do: :error
      def dump(_), do: :error
      defp get_term(data), do: data
    end
  end

  defmacro __using__(opts) do
    values = Keyword.fetch!(opts, :values)
    mod = __CALLER__.module

    is_int_enum? = Keyword.keyword?(values)

    case is_int_enum? do
      true ->
        :ok = validate_int_enum(values)
        bind_as_integer_version(values, mod)

      false ->
        :ok = validate_string_enum(values)
        bind_as_stringed_version(values, mod)
    end
  end

  defp validate_int_enum(values) do
    with {_, true} <- {:length, length(values) > 0},
         {_, true} <- {:format, Enum.all?(values, &valid_int_enum?/1)} do
      :ok
    else
        error -> raise_error(error)
    end
  end

  def validate_string_enum(values) do
    with {_, true} <- {:length, length(values) > 0},
         {_, true} <- {:format, Enum.all?(values, &is_atom/1)} do
      :ok
    else
        error -> raise_error(error)
    end
  end

  def valid_int_enum?({k, v}),
    do: is_atom(k) and is_integer(v)

  def raise_error({:length, _}),
    do: raise "TypedEnum expects `:values` to be a list or keyword list with at least 1 element"

  def raise_error({:format, _}),
    do: raise "TypedEnum expects the format of `:values` to be a keywordlist with the atom version as the key and an integer as the value (e.g.: [atom_key: 1, another_possible: 2, ...]), or a list of atoms for the string enum version (e.g.: [:atom_key, :another_possible, ...])"

  def bind_as_integer_version(values, mod) do
    quote bind_quoted: [atoms_ints: values, mod: mod] do
      @before_compile TypedEnum

      atom_integer_map =
        Enum.reduce(atoms_ints, %{}, fn {atom_val, int}, acc ->
          Map.put(acc, atom_val, int)
        end)

      string_integer_map =
        Enum.reduce(atom_integer_map, %{}, fn {atom_val, int}, acc ->
          Map.put(acc, Atom.to_string(atom_val), int)
        end)

      string_atom_map =
        Enum.reduce(atoms_ints, %{}, fn {atom_val, _}, acc ->
          Map.put(acc, Atom.to_string(atom_val), atom_val)
        end)

      integer_atom_map =
        Enum.reduce(atoms_ints, %{}, fn {atom_val, int}, acc ->
          Map.put(acc, int, atom_val)
        end)

      strings = Enum.map(atoms_ints, fn {atom_val, _} -> Atom.to_string(atom_val) end)
      atoms = Enum.map(atoms_ints, fn {atom_val, _} -> atom_val end)
      ints = Enum.map(atoms_ints, fn {_, int} -> int end)

      @behaviour Ecto.Type
      @impl Ecto.Type
      def type, do: :integer

      Module.put_attribute(mod, :valid_atoms, atoms)
      Module.put_attribute(mod, :valid_strings, strings)
      Module.put_attribute(mod, :valid_ints, ints)
      Module.put_attribute(mod, :validation_mappings, string_atom_map)
      Module.put_attribute(mod, :validation_mappings_atoms, atom_integer_map)
      Module.put_attribute(mod, :validation_mappings_strings, string_integer_map)
      Module.put_attribute(mod, :validation_mappings_ints, integer_atom_map)

      @type t() :: unquote(Enum.reduce(Enum.reverse(atoms), &{:|, [], [&1, &2]}))

      @spec values(:atoms | :strings | :ints) :: list(t()) | list(String.t()) | list(integer())
      @doc "Given a desired format returns the matching values for that format"
      def values(type \\ :atoms)
      def values(:ints), do: unquote(ints)
      def values(:atoms), do: unquote(atoms)
      def values(:strings), do: unquote(strings)

      @impl Ecto.Type
      def load(data), do: cast(data)

      @impl Ecto.Type
      @doc false
      def cast(data) when is_atom(data) and data in unquote(atoms),
        do: {:ok, data}

      def cast(data) when is_binary(data) and data in unquote(strings),
        do: {:ok, @validation_mappings[data]}

      def cast(data) when is_integer(data) and data in unquote(ints),
        do: {:ok, @validation_mappings_ints[data]}

      @impl Ecto.Type
      @doc false
      def dump(data) when is_atom(data) and data in unquote(atoms),
        do: {:ok, @validation_mappings_atoms[data]}

      def dump(data) when is_binary(data) and data in unquote(strings),
        do: {:ok, @validation_mappings_strings[data]}

      def dump(data) when is_integer(data) and data in unquote(ints), do: {:ok, data}

      @doc "Dumps but raises in case of non-valid data"
      def dump!(data) do
        case dump(data) do
          {:ok, value} ->
            value

          _ ->
            raise Ecto.CastError,
              message: "Unable to dump:: #{inspect(data)} ::into:: #{inspect(unquote(mod))}",
              type: unquote(mod),
              value: data
        end
      end

      @impl Ecto.Type
      @doc false
      def embed_as(_), do: :dump

      @impl Ecto.Type
      @doc false
      def equal?(term_1, term_1), do: true
      def equal?(term_1, term_2), do: get_term(term_1) == get_term(term_2)

      defp get_term(data) when is_atom(data) and data in unquote(atoms),
        do: @validation_mappings_atoms[data]

      defp get_term(data) when is_binary(data) and data in unquote(strings),
        do: @validation_mappings_strings[data]

      defp get_term(data) when is_integer(data) and data in unquote(ints),
        do: data
    end
  end

  def bind_as_stringed_version(values, mod) do
    quote bind_quoted: [atoms: values, mod: mod] do
      @before_compile TypedEnum

      strings = Enum.map(atoms, fn entry -> Atom.to_string(entry) end)
      mapped = Enum.zip(strings, atoms) |> Enum.into(%{})

      @behaviour Ecto.Type
      @impl Ecto.Type
      def type, do: :string

      Module.put_attribute(mod, :valid_atoms, atoms)
      Module.put_attribute(mod, :valid_strings, strings)
      Module.put_attribute(mod, :validation_mappings, mapped)

      @type t() :: unquote(Enum.reduce(Enum.reverse(atoms), &{:|, [], [&1, &2]}))

      @spec values(:atoms | :strings) :: list(t()) | list(String.t())
      def values(type \\ :atoms)
      def values(:atoms), do: unquote(atoms)
      def values(:strings), do: unquote(strings)

      @impl Ecto.Type
      def load(data), do: cast(data)

      @impl Ecto.Type
      @doc false
      def cast(data) when is_atom(data) and data in unquote(atoms), do: {:ok, data}

      def cast(data) when is_binary(data) and data in unquote(strings),
        do: {:ok, String.to_atom(data)}

      @impl Ecto.Type
      @doc false
      def dump(data) when is_atom(data) and data in unquote(atoms),
        do: {:ok, Atom.to_string(data)}

      def dump(data) when is_binary(data) and data in unquote(strings),
        do: {:ok, data}

      @doc "Dumps but raises in case of non-valid data"
      def dump!(data) do
        case dump(data) do
          {:ok, value} ->
            value

          _ ->
            raise Ecto.CastError,
              message: "Unable to dump:: #{inspect(data)} ::into:: #{inspect(unquote(mod))}",
              type: unquote(mod),
              value: data
        end
      end

      @impl Ecto.Type
      @doc false
      def embed_as(_), do: :dump

      @impl Ecto.Type
      @doc false
      def equal?(term_1, term_1), do: true
      def equal?(term_1, term_2), do: get_term(term_1) == get_term(term_2)

      defp get_term(data) when is_atom(data) and data in unquote(atoms),
        do: data

      defp get_term(data) when is_binary(data) and data in unquote(strings),
        do: @validation_mappings[data]
    end
  end
end

```


We declare the `__before_compile__` macro that allows us to place some code in the module after everything else is in place, in this case we want to include some catch-all clauses. The reason we do it in the `__before_compile__` hook is that this then enables a user of the module to implement their own versions of those functions, which can be useful for instance to include clauses that cast correctly values that come from third party systems that we can't change, but we do not want to allow on our db, instead translate them to our versions.

Because the `load/1` function will already catch all possible terms and simply call `cast/1` we do not include it there.


Then the actual `__using__` macro.

```elixir
  defmacro __using__(opts) do
    values = Keyword.fetch!(opts, :values)
    mod = __CALLER__.module

    is_int_enum? = Keyword.keyword?(values)

    case is_int_enum? do
      true ->
        :ok = validate_int_enum(values)
        bind_as_integer_version(values, mod)

      false ->
        :ok = validate_string_enum(values)
        bind_as_stringed_version(values, mod)
    end
  end
```

This is a macro, so it will be called at compile time. We `fetch!` the `:values` key from the options - since it's required we use the `!` version in order to raise when it's missing. We then decide if the underlying enum will be integer or string based through a very simple heuristic. Are the `values` a keyword list or a plain list? If it's  keyword list then it must be an integer based enum, otherwise a string based enum.

Depending on that we do a check on the form of the values to make sure they comply and then generate the actual code to fill the caller module. Remember that this is a macro, so these checks/raises will occur at compile time allowing us to signal to the user of our module any problem.

We'll only go through the integer version since it's the most complex, but you should be able to then understand easily the stringed version too. The validation helpers are plain functions, but since they're called in the context of a macro they will raise at compile time. Lastly, both `bind_as_` functions do `quote` their contents so that these are placed in the calling module as code.


```elixir
  def bind_as_integer_version(values, mod) do
    quote bind_quoted: [atoms_ints: values, mod: mod] do
      @before_compile TypedEnum

      atom_integer_map =
        Enum.reduce(atoms_ints, %{}, fn {atom_val, int}, acc ->
          Map.put(acc, atom_val, int)
        end)

      string_integer_map =
        Enum.reduce(atom_integer_map, %{}, fn {atom_val, int}, acc ->
          Map.put(acc, Atom.to_string(atom_val), int)
        end)

      string_atom_map =
        Enum.reduce(atoms_ints, %{}, fn {atom_val, _}, acc ->
          Map.put(acc, Atom.to_string(atom_val), atom_val)
        end)

      integer_atom_map =
        Enum.reduce(atoms_ints, %{}, fn {atom_val, int}, acc ->
          Map.put(acc, int, atom_val)
        end)

      strings = Enum.map(atoms_ints, fn {atom_val, _} -> Atom.to_string(atom_val) end)
      atoms = Enum.map(atoms_ints, fn {atom_val, _} -> atom_val end)
      ints = Enum.map(atoms_ints, fn {_, int} -> int end)

      @behaviour Ecto.Type
      @impl Ecto.Type
      def type, do: :integer

      Module.put_attribute(mod, :valid_atoms, atoms)
      Module.put_attribute(mod, :valid_strings, strings)
      Module.put_attribute(mod, :valid_ints, ints)
      Module.put_attribute(mod, :validation_mappings, string_atom_map)
      Module.put_attribute(mod, :validation_mappings_atoms, atom_integer_map)
      Module.put_attribute(mod, :validation_mappings_strings, string_integer_map)
      Module.put_attribute(mod, :validation_mappings_ints, integer_atom_map)

      @type t() :: unquote(Enum.reduce(Enum.reverse(atoms), &{:|, [], [&1, &2]}))

      @spec values(:atoms | :strings | :ints) :: list(t()) | list(String.t()) | list(integer())
      @doc "Given a desired format returns the matching values for that format"
      def values(type \\ :atoms)
      def values(:ints), do: unquote(ints)
      def values(:atoms), do: unquote(atoms)
      def values(:strings), do: unquote(strings)

      @impl Ecto.Type
      def load(data), do: cast(data)

      @impl Ecto.Type
      @doc false
      def cast(data) when is_atom(data) and data in unquote(atoms),
        do: {:ok, data}

      def cast(data) when is_binary(data) and data in unquote(strings),
        do: {:ok, @validation_mappings[data]}

      def cast(data) when is_integer(data) and data in unquote(ints),
        do: {:ok, @validation_mappings_ints[data]}

      @impl Ecto.Type
      @doc false
      def dump(data) when is_atom(data) and data in unquote(atoms),
        do: {:ok, @validation_mappings_atoms[data]}

      def dump(data) when is_binary(data) and data in unquote(strings),
        do: {:ok, @validation_mappings_strings[data]}

      def dump(data) when is_integer(data) and data in unquote(ints), do: {:ok, data}

      @doc "Dumps but raises in case of non-valid data"
      def dump!(data) do
        case dump(data) do
          {:ok, value} ->
.            value

          _ ->
            raise Ecto.CastError,
              message: "Unable to dump:: #{inspect(data)} ::into:: #{inspect(unquote(mod))}",
              type: unquote(mod),
              value: data
        end
      end

      @impl Ecto.Type
      @doc false
      def embed_as(_), do: :dump

      @impl Ecto.Type
      @doc false
      def equal?(term_1, term_1), do: true
      def equal?(term_1, term_2), do: get_term(term_1) == get_term(term_2)

      defp get_term(data) when is_atom(data) and data in unquote(atoms),
        do: @validation_mappings_atoms[data]

      defp get_term(data) when is_binary(data) and data in unquote(strings),
        do: @validation_mappings_strings[data]

      defp get_term(data) when is_integer(data) and data in unquote(ints),
        do: data
    end
  end
```

We do `bind_quoted` passing the `values` as the variable `atoms_ints` and caller module as `mod`.

The generated code will include the `@before_compile` hook referencing this module where we defined it, `TypedEnum`. Then we use the keyword list of `atoms_ints` to generate a few different maps and lists that we'll use further down.

Because keyword lists of literal terms are literal proplists inside macros it makes them very ergonomic to use inside macros. They also provide a nice easy syntax to pass options as in this case.

So from the `values` keyword list (e.g.: `[val_1: 1, val_2: 2]`) we generate 4 maps:

- A simple map version of the proplist, the prop key as map key and the integer as the value, e.g.: `%{val_1: 1, ...}`
- Another in the same form but now the keys as strings, e.g: `%{"val_1" => 1, ...}`
- Another string to atoms mapping, e.g: `%{"val_1" => :val_1, ...}`
- And lastly a integer to atom mapping, e.g: `%{1 => :val_1, ...}`

These 4 maps will be used to fetch the right form of the enum depending on if we're loading or dumping it, and if the original data being passed in is in atom, string or integer form.

Then we generate 3 lists to aid us in adding guard clauses to our functions:

- for strings, e.g: `["val_1", ...]`
- for atoms, e.g: `[:val_1, ...]`
- for ints, e.g: `[1, ...]`

We put all these things as module attributes (so the caller module can access them if they want, with the `@...` syntax) and we use them to provide the helper function `values(type)`, that allows the user to retrieve the valid enum representations in any of its valid forms.

The `cast` functions of the `Ecto.Type` (and in our case the `load`) are used to transform values from their representation in the database, or from external requests, into our internal representation, which are atoms. Following this then means that if we trie to load or cast an atom that is a valid representation we just return that same atom since it's already in its correct form. If on the other hand it's a string (comes from an external request perhaps), or an integer (loaded from the database layer), we, with the aid of the mappings we assembled before, convert those into their atom version.

For the `dump` it's the same logic, but in this case the final form we want is the integer form, so we convert atoms and strings into their integer version, and if it's already an integer and a valid one we use that.

Otherwise, because of the guards we set up on each function, if the value they're called with is invalid, the catch all clauses that we set on the `@before_hook` will kick in and error at the ecto layer.

This allows still a user of the module to implement intermediate versions. Say one client that interacts with this is using a legacy version of the enums, where instead of `"val_1"` they were using `"some_prop"` as its identifier. We could set a specific clause on our caller module  handling that specific case, for example:


```elixir
defmodule ExampleCallerModule do
  use TypedEnum, values: [val_1: 1, val_2: 2]

  def cast("some_prop"), do: {:ok, :val_1}
  def dump("some_prop"), do: {:ok, 1}
  defp get_term("some_prop"), do: :val_1
end
```

And this would allow us to cast it, and even test its equality automatically by defining as well the `get_term` version for it.

I also like to add a specific `dump!` version that just calls `dump` underneath and raises in case `dump` returns anything else than an `:ok tuple`. The `get_term` are just helpers to simplify the equality checks.

And this folks, is all there is to it. The `stringed` version is very very similar, but simpler because it only has to deal with atoms and strings.

I opted to keep them as two separate functions because although there's some overlap and duplication going on, I think that having a whole bunch of conditionals on the macro for generating functions depending on if it's the integer or stringed version, and different module attributes, etc would just make it less straightforward to read and change if the need arises.

Now that you've seen how it works, if you don't want to write it yourself you can just include the package I've you can find it here in [hexdocs](https://hexdocs.pm/typed_enum/TypedEnum.html) (or through github in [mnussbaumer/typed_enum](https://github.com/mnussbaumer/typed_enum) and use it straightaway. Otherwise hope you could take away something useful.