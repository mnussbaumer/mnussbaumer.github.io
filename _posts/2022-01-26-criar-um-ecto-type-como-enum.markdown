---
layout: post
title: "Criar um Ecto.Type como Enum"
canonical_url: "https://dev.to/mnussbaumer/criar-um-enum-ectotype-2646"
date: 2022-01-06 16:30:00 +0700
categories: elixir ecto enum database type
---

Hoje vou mostrar um enumerador implementado como um `Ecto.Type`, que uso em quase todos os projectos de Elixir em que contribuo. É um tipo semelhante ao `Ecto.Enum` que permite utilizar `átomos` na lógica interna da aplicação, validar/transformar valores equivalentes na sua forma de caracteres, e ao mesmo tempo limitar os possíveis valores que determinado campo/coluna pode assumir. Como não há tradução actual para o tipo de "string", no sentido em que é utilizado em programação, irei utilizar o termo `listagramma`, no sentido que uma "string" é uma lista de caracteres, e `gramma` em latim é símbolo/letra/desenho (hexagrama, etc), ou seja, uma lista de "grammas".

Apesar do `Ecto` incluir um `Ecto.Enum` desde a versão 3.5, este sofre de alguns problemas:
- é declarado em conjunto com o esquema no qual é utilizado, o que não permite que seja partilhado facilmente entre esquemas,
- não gera informações de tipo para usar em especificações,
- não permite lógica adicional no tratamento de parâmetros adicionais,
- não permite utilizar números inteiros para a representação do mesmo ao nível da base de dados (apenas grupos de caracteres)


Mesmo assim é por si só melhor do que usar simplesmente `string` como tipo de coluna nos casos em que sabemos que a mesma apenas pode assumir uma lista limitada de valores.
Para ilustrar isto, vejamos o seguinte módulo:

```elixir
defmodule User.Action do
  use Ecto.Schema

  schema("users_actions") do
    belongs_to(:user, User)

    field(:action, :string)
  end
end
```

Isto é um esquema totalmente válido, mas tanto quanto sabemos ao lê-lo é que o campo `:action` é um grupo de caracteres arbitrário. Neste caso, `:action` apenas pode assumir um número limitado de valores mas o esquema em si não nos transmite essa informação de forma alguma. 

Claro que poderíamos criar restrições ao nível da base de dados, e em certos casos independemente de tudo o resto devemos certamente fazê-lo, mas isso continuaria a não providenciar informação alguma a nível do código da nossa aplicação sobre as possíveis restrições, nem ajudaria a transformar ou traduzir valores entre sistemas externos e a nossa representação interna para esses mesmos valores. Para obter isso necessitamos de emergir ao nível dos nossos esquemas quais sãos os valores possíveis para tais colunas.

Nesse caso, o `Ecto.Enum` ajuda-nos a fazer exactamente isso, por exemplo, utilizando o mesmo esquema anterior, mas agora atribuíndo um `Ecto.Enum` como tipo ao invés da simples `:string`

```elixir
defmodule User.Action do
  use Ecto.Schema

  schema("users_actions") do
    belongs_to(:user, User)

    field(:action, Ecto.Enum, values: [:bid, :request, :upload, :pay])
  end
end
```

Agora é visível que valores este campo pode assumir. Simplesmente por ler a definição do esquema sabemos que são `:bid`, `:request`, `:upload` e `:pay`.

Este tipo também nos permite então traduzir valores no formato de simples caracteres para o nosso formato interno de átomos, o que é bastante útil uma vez que se tivermos de tratar de validar submissões de dados externas ao nosso sistema, essas submissões serão certamente no formato de caracteres e não em tipos nativos a Elixir/Erlang, como é o caso de átomos. No entanto, após traduzidos, podemos ter a certeza que são não só válidos (há uma equivalência explicíta entre ambas as representações) mas que podemos escrever a nossa lógica interna com recurso apenas a `átomos`, o que para além de ser mais eficaz é também mais robusto.

Resumindo:

- dá-nos a habilidade de transformar e validar parâmetros:

```elixir

params = %{"user_id" => 1, "action" => "bid"}

Ecto.Changeset.cast(%User.Action{}, params, [:user_id, :action])
```

- permite-nos, como mencionado anteriormente, usar estritamente a versão de `átomo` na nossa lógica interna, pois sabemos que após ser validado terá de conformar-se a essa representação:

```elixir

def log_action?(%User.Action{action: action}),
    do: action in [:bid, :pay]
```

Como `átomos` em Elixir e Erlang são internamente representados como números inteiros, é bastante mais performante utilizá-los, tanto para serializar ou de-serializar. Grupos de caracteres necessitam de muito mais trabalho para serem comparados e avaliados.

O que o `Ecto.Enum` adiciona é por si só em grande parte dos casos suficiente. É sempre melhor do que usar definições de tipos que permitem grupos arbitrários quando sabemos que apenas determinados valores são válidos.

Mas se por exemplo, agora quisessemos utilizar este mesmo tipo que definimos no `User.Action` noutro esquema, teríamos de duplicar esta definição, e não haveria ligação efectiva entre ambos, a não ser que fossemos buscar os valores a algum módulo/função partilhada e, mesmo nesse caso, a ligação não é formal, o tipo não é o mesmo nem é possível representá-lo como tal.

Para além disso, não temos também acesso a funções de validação independentes do esquema. O tipo sendo declarado no esquema em si, é parte do mesmo e as suas validações ocorrem no contexto de um esquema de `Ecto`. Adicionar claúsulas específicas para tratar de casos especiais é também impossível ou bastante mais complicado do que necessita de ser.

Adicionalmente, caso queiramos guardar tais valores como números inteiros ao nível da base de dados, também não o podemos fazer uma vez que o `Ecto.Enum` assume apenas listagrammas (`strings`) como a coluna ao qual está associado.

Por último, não gera um `typespec` para o tipo. Caso usemos `dialyzer` na nossa aplicação não temos forma de transpor as restrições definidas no esquema para especificações de tipos para documentação (e consistência de documentação das interfaces).

No entanto é, como referido, bastante melhor que utilizar apenas `:string` como tipo, e por vezes também não necessitamos de mais nenhuma funcionalidade.

Caso necessitemos, vejamos primeiro como poderíamos definir um `Ecto.Type` independente para este caso em particular :

```elixir
defmodule User.Action.Type do
  @behaviour Ecto.Type

  def type, do: :string

  @valid_atoms [:bid, :request, :upload, :pay]
  @valid_strings Enum.reduce(@valid_atoms, [], fn t, acc -> [Atom.to_string(t) | acc] end)
  @valid_map Enum.reduce(@valid_atoms, %{}, fn t, acc -> Map.put(acc, Atom.to_string(t), t) end)

  @valid_inverse_map Enum.reduce(@valid_atoms, %{}, fn t, acc -> Map.put(acc, t, Atom.to_string(t)) end)

  @type t() :: unquote(Enum.reduce(Enum.reverse(@valid_atoms), &{:|, [], [&1, &2]}))

  def load(data), do: cast(data)

  def cast(data) when is_atom(data) and data in @valid_atoms, do: {:ok, data}
  def cast(data) when is_binary(data) and data in @valid_strings, do: {:ok, @valid_map[data]}
  def cast(_), do: :error

  def dump(data) when is_atom(data) and data in @valid_atoms, do: {:ok, @valid_inverse_map[data]}
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

Analisando o módulo:

- declaramos que o módulo implementa o comportamento (`@behaviour`) de um `Ecto.Type` (o que nos garante avisos e erros aquando da compilação no caso de nos esquecer-mos de implementar alguma função)
- declaramos uma função `type/0` que resolve no tipo interno que o módulo representa (o que será utilizado aquando da serialização para a base de dados). Neste caso resolve em `:string` (ou seja, a coluna na base de dados será uma coluna de texto - varchar, char, text, etc)
- declaramos um `typespec`, que resolve numa lista contendo todos os átomos válidos para este tipo
- utilizando essa lista de átomos válidos (`@valid_atoms`) geramos uma lista de listagramas que correspondem directamente às respectivas versões dos átomos, assim como dois atributos do módulo para guardar o mapeamento entre os mesmos, átomos->listagrammas e listagrammas->átomos
- declaramos todas as funções requeridas pelo comportamento `Ecto.Type` - uma vez que temos uma lista compostas por todos os átomos e outra por todos os listagrammas válidos, podemos utilizar essas listas como cláusulas de restrição nas funções requeridas e dessa forma facilmente definir como se devem comportar.


Como podemos ver, é simples criar um `Ecto.Type` que represente um grupo restrito de valores, ajudando-nos a tornar o nosso código mais assertivo e explícito (e por conseguinte mais compreensível também).

Permite-nos também transformar e validar termos independentemente de um qualquer esquema. Podemos por exemplo executar `User.Action.Type.cast("bid")` para validar o termo `"bid"`, que neste caso seria válido visto ser a representação em letragramma do átomo `:bid`.

Caso copiar ou replicar este módulo com as devidas alterações sempre que necessitemos de um tipo novo para um outro grupo de valores não for um problema, podemos utilizar esta versão e fazer exactamente isso, utilizando isto como a "planta" para todos os outros.

Esta versão já demonstra algumas vantagens relativamente ao `Ecto.Enum`, nomeadamente a capacidade de fazer transformações & validações de termos independentemente de esquemas, assim como ser utilizado em vários esquemas, sem requerer duplicação de código ou código adicional para os manter actualizados - futuras alterações aos valores do tipo ocorrem apenas no módulo que o define.

Uma vez que controlamos também o funcionamento interno do mesmo poderíamos facilmente modificá-lo para que serializasse os valores como números inteiros. No entanto, o melhor seria criar uma macro que faça tudo isso por nós, permitindo-nos aceder essa funcionalidade com uma simples linha de código. Tendo isso em mente, o nosso objectivo é criar uma macro que nos permite escrever o módulo `User.Action.Type` anteriormente exposto, como:

```elixir
defmodule User.Action.Type do
  use TypedEnum, values: [:bid, :request, :upload, :pay]
end
```

De forma a utilizá-lo num esquema de `Ecto` da seguinte forma:

```elixir
defmodule User.Action do
  use Ecto.Schema

  schema("users_actions") do
    belongs_to(:user, User)

    field(:action, User.Action.Type)
  end
end
```

E caso queiramos lidar com átomos ao nível da aplicação mas serializá-los enquanto números inteiros, que possamos definir o mesmo tipo como:


```elixir
defmodule User.Action.Type do
  use TypedEnum, values: [bid: 0, request: 1, upload: 2, pay: 3]
end
```

Vejamos então uma lista do que o nosso módulo necessita suportar:

- a macro `__using__`, de forma que os módulos que queiram fazer uso dele, possam utilizar a palavra-chave `use`, como demonstrado nos exemplos do que queremos
- que seja capaz de distinguir entre listas de átomos como valores válidos e listas de palavras-chaves, de forma a que possa intuitivamente implementar ora versões com base de número inteiro, ou de listagrammas
- permitir a quem o deseje implementar a possibilidade de escrever funções adicionais para `cast`/`dump` do comportamento  `Ecto.Type`
- introduzir automaticamente um `typespec` (especificação de tipo) que reflita todos os valores válidos na sua forma de átomo
- que emerja erros aquando da compilação quando os valores utilizados na definição do tipo sejam inválidos

Dito isso, o nosso módulo final terá a seguinte forma:

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

Para que quem queira utilizar esta macro tenha a possibilidade de incluir também cláusulas específicas para `cast/dump/get_term`, definimos uma macro especial, `__before_compile__` que nos permite inserir código após o módulo ter sido compilado, mas antes da compilação ser dada como finalizada. Neste caso, através disso incluímos versões das funções `load/dump/get_term` que funcionam para quaisquer valores não válidos e resolve no átomo `:error` (que é o esperado pelo `Ecto` quando um valor não é válido).

Uma vez que a função `load/1` simplesmente executa a função `cast/1` não há necessidade de definir uma versão de erro para ela.

A, também especial, macro `__using__`: 

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

Uma vez que é uma macro, irá ser avaliada aquando da compilação do código. Nela, utilizamos `Keyword.fetch!` para o termo `:values` das opções que forem passadas ao `use`, por exemplo `use TypedEnum, values: [:bid, :request, :upload, :pay]` significa que a variável `opts` será uma lista `Keyword` com uma chave `:values`, cujo valor será uma lista de átomos (`:bid`, `:request`, etc). Uma vez que a chave `:values` é essencial ao funcionamento da macro, caso não esteja presente nas `opts` queremos que surja um erro, e daí utilizarmos `fetch!`.

De seguida decidimos se devemos definir o enumerador com valores de números inteiros ou listagrammas, e fazemo-lo avaliando a forma do valor da chave `:values`. Caso seja uma lista `Keyword` assumimos que seja números inteiros, pois será composta por pares de `versão_átomo -> número_inteiro_correspondente`. Caso seja uma simples lista de átomos, assumimos que o desejado é que o enumerador use listagrammas como formato.

Dependendo disso fazemos algumas verificações para nos certificarmos que os valores têm o formato correcto. Tudo isto acontece aquando da compilação, logo qualquer erro que haja emergirá nessa altura, o que permite ao utilizador saber imediatamente se há algum problema.

Agora iremos fazer uma leitura do que acontece na versão com base em números inteiros. Visto ser mais complexa que a com base em listagrammas acredito que compreendendo esta será fácil ler a outra. Sendo uma macro que utiliza `quote`, os conteúdos dos blocos de `quote do end` irão ser introduzidos nos módulos que utilizarem esta macro e é o que se passa nesses blocos que é o mais importante.



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

Neste bloco de `quote` usamos o atributo `bind_quoted`, que nos permite passar variáveis propriamente expandidas para serem utilizadas no interior do bloco. Neste caso, `atoms_ints` com os valores contidos em `values` (o que havia sido passado como chave `:values` à macro), e também `mod`, contendo o nome do módulo que executou a macro.

Primeiro incluímos imediatamente o atributo `@before_compile`, que faz com que após a compilação, mas antes da sua finalização, a macro `__before_compile__` do módulo em questão seja executada, nesta caso o `__before_compile__/1` do módulo `TypedEnum`. Depois utilizamos a lista de `Keyword`s (`atoms_ints`) para gerar um conjunto de mapas e listas que nos auxiliarão a decidir que validações e transformações são necessárias quando as funções de comportamento `Ecto.Type` sejam executadas.

Dessa lista de `values`, geramos 4 mapas - se utilizarmos o exemplo anterior onde a chave `:values` assumia o valor de uma lista com a seguinte forma `[val_1: 1, val_2: 2]`), o resultado de `atom_integer_map`, `string_integer_map`, `string_atom_map`, `integer_atom_map` será respectivamente:

- mapa com as mesmas chaves e valores que a lista `Keyword`, neste caso: `%{val_1: 1, ...}`
- outro mapa idêntico mas agora com as chaves em formato de letragramma ao invés de átomos: `%{"val_1" => 1, ...}`
- um mapa que mapeia as correspondências entre letragrammas e versões em átomos: `%{"val_1" => :val_1, ...}`
- por último, as correspondências entre números inteiros e as suas equivalências em formato de átomo, e.g: `%{1 => :val_1, ...}`

Com estes quatro mapas auxiliares será mais fácil resolvermos os valores do tipo dependendo se estamos a fazer uma validação/transformação (`cast/1`) ou uma serialização (`dump/1`), e tendo em conta o formato do termo original (n. inteiro, letragramma ou átomo).

De seguida geramos 3 listas adicionais, que nos facilitarão restringir as cláusulas das nossas funções:

- para letragrammas: `["val_1", ...]`
- para átomos: `[:val_1, ...]`
- para números inteiros: `[1, ...]`


Colocamos todas estas variáveis auxiliares em atributos do módulo (o que significa que quem esteja a utilizar o mesmo, os pode aceder com a síntaxe usual, `@nome_do_atributo`), e utilizamos a lista de valores válidos nos seus diferentes formatos para providenciar uma função, `values/1`, que resolve em todos os valores válidos para o formato desejado.

As funções `cast` (e no nosso caso `load` também), são usadas para transformar termos do seu formato interno na base de dados, ou externos à nossa aplicação, no formato utilizado pela nossa aplicação, neste caso em átomos. Por conseguinte, se parte do código por algum motivo executar `load/cast` com valores já no formato correcto, a resolução dessas funções resultará no mesmo termo. Se no entanto utilizarmos termos no formato de listagrammas ou números inteiros, se tiverem uma correspondência válida, serão convertidos no seu formato de átomo. Caso sejam inválidos emergirão um erro.

No caso da função `dump`, a lógica é em tudo semelhante, com a diferença que o formato final que procuramos é o de número inteiro (para serialização na base de dados), neste caso, listagrammas e átomos, se tiverem uma correspondência válida, serão convertidos no seu formato de número inteiro, caso o termo a avaliar seja já um número inteiro e válido, a função resolverá com esse mesmo termo.

Uma vez que cada função `load/1` e `dump/1` tem a acompanhar um grupo de restrições, caso os valores passados não sejam válidos, as cláusulas que inserimos através do `@before_hook` serão as executadas (uma vez que aceitam qualquer termo e não têm guardas), e resultarão num erro.

Caso o utilizador do módulo, tenha adicionado cláusulas extra, devido à ordem final de todas elas no módulo compilado, essas serão tentativamente executadas antes das dos erros. Por exemplo, imaginemos que estamos a programar uma aplicação que necessita interagir com uma versão antiga da base de dados, em que ao invés de utilizarem o termo `"val_1"`, estavam a utilizar o termo `"valor_1"`. Actualmente `"valor_1"` já não é uma representação válida, por isso não queremos permitir que seja persistida pelo nosso sistema, mas ao mesmo tempo, tendo uma correspondência directa ao termo `"val_1"` que por sua vez é válido, queremos convertê-la caso ela surja como parâmetro em alguma situação.

Nesse caso poderiamos definir o nosso tipo da seguinte forma:


```elixir
defmodule ExampleCallerModule do
  use TypedEnum, values: [val_1: 1, val_2: 2]

  def cast("valor_1"), do: {:ok, :val_1}
  def dump("valor_1"), do: {:ok, 1}
  defp get_term("valor_1"), do: :val_1
end
```

E isto permitir-nos-ia validar, traduzir e verificar a sua equivalência, sem no entanto necessitar de introduzir mais um item no enumerador, ou complicar significativamente o nosso programa de forma a suportar tal caso.


Adicionalmente às funções requeridas pelo comportamento `Ecto.Type` eu pessoalmente gosto de definir também uma `dump!/1`, que simplesmente executa a `dump/1` normal, e caso a resolução seja qualquer outra coisa que uma tupla `{:ok, _}`, que emerja um erro.

A função `get_term` é simplesmente para facilitar a verificação de equivalência entre termos.

E isto é basicamente tudo o que é necessario. A versão em listagrammas é em todo símile a esta, mas mais simples uma vez que apenas necessita de funcionar com átomos e listagrammas.

Como última nota relativa ao módulo que escrevemos, optei por manter duas versões separadas para a versão listagramma e outra para a versão de números inteiros - isto porque apesar de haver partes duplicadas entre ambas, escrever apenas uma versão requereria uso de várias condicionais, e penso que o código resultante seria bem mais complicado e não linear de entender. Assim temos um pouco de duplicação mas cada parte trata apenas do que tem a tratar.

E agora que vemos como poderíamos implementar tal módulo para nos auxiliar, caso queiram podem utilizar um pacote já feito, disponível em [hexdocs](https://hexdocs.pm/typed_enum/TypedEnum.html) (ou github [mnussbaumer/typed_enum](https://github.com/mnussbaumer/typed_enum). De qualquer das formas espero que tenha sido útil.