Pattern Matching in Elixir - Does it fit?
========

Pattern matching is a very powerful feature in any programming language that implements it, I think that essentially because pattern matching is inherent in computer programming logic, from a conceptual to a practical level. It's a feature that is present in many functional languages for the programmer to use. Elixir (inheriting from Erlang) gives us access to it and idiomatic code is most often peppered with pattern matching. 

It fits well in many places and allows us to describe very succinctly the code paths our running programs may take, while retaining, and most of the time increasing, the clarity of the code. While some features that allow succinctness and conciseness of code end up in a symbolical soup more close to mathematics than a "legible" declaration (for those of us not completely familiar with mathematical notation), pattern matching does not suffer from that, because what it allows us to express is the "form" of the data inside our running programs, which in turn allows us to express what pathways of code should be run given that form.

Erlang itself has on top of that a very accessible and intuitive way of treating binary data, that coupled with pattern matching gives it the ability to chop, split, and work with binaries in a very pleasant way.

Now I could go on talking about how the rainbow is perfect during this sunset, or how unicorns are pooping diamonds and so on, but that won't give you any insight into Pattern Matching. So let's move to do some exploration of it.


First we'll start by analysing the `=` operator in Elixir, which is in fact a match operator and not an assignment operator (if you're coming from Javascript, Ruby, etc). It has one side effect that makes it look like an assignment operator though, which is, when the expression on the left side of it is a `variable`, it binds whatever is on the right side to that variable.

We can do some simple tests of it on iex:

{% highlight elixir %}
1 = 1
# 1
1 = 2
# ** (MatchError) no match of right hand side value: 2

a = 1
# 1

a = 2
# 2

a + 1
# 3

a
# 2

3 = a
#** (MatchError) no match of right hand side value: 2

a = 3
# 3

a = a + 1
# 4

a
# 4
{% endhighlight %}

Ok, so what can we see from these examples? First we see that when a match evaluates correctly, we get the value of the expression that was matched. `1 = 1` returns 1, and not true. If we wanted to check for equality we would use the equality operator `==` and not the match operator `=`.

{% highlight elixir %}
> 1 == 1
# true

> 1 == 2
# false
{% endhighlight %}

On the second case we get a `MatchError`, because we're trying to convince our program that `1` matches `2`, and it simply states, dude, perhaps in your universe, but in mine, 1 can't match 2. 

Then we do a bunch of "assignments", and it works mostly as one would expect in other languages, the variable `a` gets bound to whatever  value. 

When we try to do `3 = a`, when `a` was bound to 2, we get again an error, but when we switch it to `a = 3` we no longer get an error, instead the value 3 is bound to the variable `a`.

In Erlang you can't use the same variable name in different assignments, but Elixir chose to allow it. Underneath, because the data is still immutable, what happens is that elixir creates various versions of `a`  and points `a` to the last version you entered - from a practical point of view though, it looks like you're "re-assigning" the variable.

Now you might be thinking, all of that for what?
Yes, it's not apparent yet why it helps in anything else other than silly examples. So let's move on.


Let's create a map and play around with it.

{% highlight elixir %}
a = %{key_one: "this is key number one", key_two: "this is key number two"}
# %{key_one: "this is key number one", key_two: "this is key number two"}

%{key_one: key_one_var} = a
# %{key_one: "this is key number one", key_two: "this is key number two"}

key_one_var
# "this is key number one"
{% endhighlight %}

So here things start to look more interesting. You can see that we placed a pattern of `%{key_one: key_one_var}` and matched it against the previously map bound to `a`. The match succeeded, but we also "bound" the variable `key_one_var` to whatever was the value of the key `:key_one` used in the pattern match.
If you used Javascript E6 destructuring you might notice it looks familiar. The Erlang (&Elixir) version though is way more powerful than javascript's version, because of its properties and how it can be used, but lets move on.

{% highlight elixir %}
%{key_three: key_three_var} = a
# ** (MatchError) no match of right hand side value: %{key_one: "this is key number one", key_two: "this is key number two"}
{% endhighlight %}

Ok, we tried to match the map bound to `a` to a map of the form `%{key_three: some_variable}` and this didn't work, because `a` doesn't have any key named `:key_three`, it's expected to fail.
But notice that we didn't need to specify all the keys when matching `:key_one` previously, although the `a` map had an additional `:key_two`. 

So how can we look at it conceptually? I think that the best way to describe it is, given `a = b`, could the form described in `a` be extracted from the contents held in `b`? If it can the match succeeds, any bindings are made effective, and the full expression is returned. If it can't, an error is thrown.

And in this case indeed it can, in the previous example, the form `map, with a key named :key_one, to be bound to a variable named key_one_var` can be extracted from `a`, because that map has a key named `:key_one` and we aren't specifying any value that that key must have.

So if we write:

{% highlight elixir %}
%{key_one: "this is key number one"} = a
{% endhighlight %}

It also matches. But if we write:

{% highlight elixir %}
%{key_one: "this is key number two"} = a
** (MatchError) no match of right hand side value: %{key_one: "this is key number one", key_two: "this is key number two"}
{% endhighlight %}

It fails, because we're saying match a map that has a key named `:key_one` with the value `"this is key number two"`, but since the value of that key in `a` is actually `"this is key number one"` it fails.

{% highlight elixir %}
%{key_one: "this is key number " <> keynumber} = a
# %{key_one: "this is key number one", key_two: "this is key number two"}

keynumber
# "one"
{% endhighlight %}

Here it matches, and not only that, we have extracted the last portion of the binary string as a variable. We could extract both the last portion and the whole binary string if we wanted with for instance:

{% highlight elixir %}
%{key_one: "this is key number " <> keynumber = whole_binary} = a
# %{key_one: "this is key number one", key_two: "this is key number two"}

keynumber
# "one"

whole_binary
# "this is key number one"
{% endhighlight %}

It doesn't end here though, because the patterns you use can be much more complex and involve nested levels of maps, binary specifications, and lists, and so on, you can "describe" in as much detail as you want their forms and also extract very easily what you need from them.

`%{} = a`

Matches alright again, because we're simply saying, is the value bound to `a` a map? And since it is, all is good. You might have thought that it would fail, because `%{}` could be interpreted as an `empty` map but maps behave in this regard a bit differently. To assert that a map is indeed empty, you need to compare it, `%{} == a` would return `false` for instance.

Now lists:

{% highlight elixir %}
b = ["a", "list", :of, :stuff]
# ["a", "list", :of, :stuff]

[head | tail] = b
# ["a", "list", :of, :stuff]

head
# "a"

tail
# ["list", :of, :stuff]
{% endhighlight %}

Here we bound `b` to a list of 4 elements. Then we pattern-matched `b` against the pattern `[head | tail]`. Lists in Erlang (and consequently in Elixir) are like LISP lists, they're composed of cons cells, that can be thought of as a group of cells, where each cell holds a value and also a "pointer" to the next cell in the list. 

In this case we can think of it as a structure where the first "cell" is `"a"`, which points to the cell with value `"list"`, which in turn points to the cell `:of`, which points to cell `:stuff` which in turn points to the end of the list (an empty list, `[]`).

So a list is a collection of cells where each element holds its value and points to the next element, that's why they're called `cons cells` and usually described as `(x . points_to_y)(y . points_to_z)(z . points_to_empty_list)()`, or more correctly `(x . (y . (z . ())))`. In Elixir and Erlang it looks like `[x | [y | [z | []]]]`.

So when we match `b` to the pattern `[head | tail]` what we're actually saying is, does a pattern of a non empty list (meaning where there is at least one cell, the `head`) match `b`? If it does,  bind the value of the first cell of that list to the variable `head` and the remaining of the list to the variable `tail`.

If we try:

{% highlight elixir %}
c = []
# []

[head | tail] = c
# ** (MatchError) no match of right hand side value: []
{% endhighlight %}

We get a match error, in this case we're trying to match a non-empty list (the pattern we wrote specifies it should have one element at least, `head`, pointing to a tail), against an empty list (`c`) so we get a match error, because an empty list has no cells, it's itself the end of the list. This contrasts with the previous seen case of the map, where the empty map `%{}` still matched ok, but in practical (and theoretical) terms it makes sense, once you start using it you'll see that the difference between what meaning an empty list and an empty map usually assume and the ability to match on simply being a `map` warrant this (seemingly) small contradiction. Also, if a match with `%{}` equated to saying an empty map, then when you were matching non-empty maps you would need to spell out all the keys in the map and that effectively and that completely defeats the pragmatic purpose of matches - or you would need to specify different behaviour for when using the pattern `%{}`. 

{% highlight elixir %}
d = ["non_empty"]
# ["non_empty"]

[head | tail] = d
# ["non_empty"]

head
# "non_empty"

tail
# []
{% endhighlight %}

In this case though, we bound a list with one cell, `"non_empty"` to `d` and when we pattern matched, it worked. `"non_empty"` got bound to `head` and the end of the list (an empty list) got bound to `tail`. 

Again, it's not super impressive (yet) but we'll get there soon.
Let's see one more example before though:

{% highlight elixir %}
deep = %{list_key: [:a_list, %{super_deep: [:a]}], date: "2019-05-01"}
# %{date: "2019-05-01", list_key: [:a_list, %{super_deep: [:a]}]}

%{date: <<year::binary-size(4), "-", month::binary-size(2), "-", day::binary-size(2)>>, list_key: [_, %{super_deep: [first_element_of_super_deep | t]}]}
# %{date: "2019-05-01", list_key: [:a_list, %{super_deep: [:a]}]}

> year
# "2019"
> month
# "05"
> day
# "01"
> first_element_of_super_deep
# :a
{% endhighlight %}

Wow. So with a simple pattern matching, we were able to extract a lot of information as you can see. We didn't need to split a string to get all the pieces of the date, we didn't need to iterate on the list to get it's nested elements and we plucked an element from inside a list, that was inside a map, that was inside another list itself inside another map. We can also write it in a way that is more readable

{% highlight elixir %}
%{
    date: <<
        year::binary-size(4),
        "-",
        month::binary-size(2),
        "-",
        day::binary-size(2)
    >>,
    list_key: [
        _,
        %{
            this_will_fail: [
                first_element_of_super_deep | t
            ]
        }
    ]
} = deep

# ** (MatchError) no match of right hand side value: %{date: "2019-05-01", list_key: [:a_list, %{super_deep: [:a]}]}
{% endhighlight %}

Here, in the pattern we described, we changed the key name inside the map inside the nested list to `this_will_fail` and it no longer matched, although everything else was the same as before.

So, this is a bit more impressive, although, still doesn't look very useful if we can only use this on match operations. Where it becomes really, really, useful is when we use it in conjunction with Elixir's (Erlang) ability to have multiple function definitions and/or inside some constructs the language provides, such as `case` statements (Well actually, most places, to be sincere, allow their usage).

You might also have noticed that we used `_` in these last pattern matches. `_` (or any variable name starting with `_`) tells the compiler we're not interested in that value, so it won't bind that variable, although it still requires something to be there.

So let's look at patterns with `case` statements, since they're very common too.

{% highlight elixir %}
a = [:a, :list]
# [:a, :list]

case a do
    [:a, :list] -> "a list with :a and :list"
    _ -> "something else"
end
# "a list with :a and :list"

a = [:b, :list]
# [:b, :list]

case a do
    [:a, :list] -> "a list with :a and :list"
    _ -> "something else"
end
# "something else"

case a do
    [_, :list] -> "a two element list where the second element is :list"
    _ -> "something else"
end
# "a two element list where the second element is :list"

case a do
    [_, :list | _t] -> "a list with at least two elements where the second element is :list"
    _ -> "something else"
end
# "a list with at least two elements where the second element is :list"

case a do
    [_] -> "a list with a single element"
    [_, _] -> "a list with 2 elements"
    _ -> "something else"
end
# "a list with 2 elements"

case a do
    [_] -> "a list with a single element"
    [:b, :something_else] -> "a list with 2 elements, :b and :something else"
    [:b, :list | tail] -> "a list with 2 elements and a tail"
end
# "a list with 2 elements and a tail"

tail
# []
{% endhighlight %}

Since lists are made of cons cells, this matched on the last branch, because although we only had 2 elements in the list it technically is 3, since a proper list always ends in an empty list itself. So by having a list with 2 elements what we actually have is:

`(element_1 . points_to_element_2)(element_2 . points_to_end_of_list)() <- this last element is itself an empty list.`

If we did this, on the other hand:

{% highlight elixir %}
case a do
    [_] -> "a list with a single element"
    [:b, :something_else] -> "a list with 2 elements"
    [:b, :list, _last_element] -> "a list with 3 elements"
end
{% endhighlight %}

We get instead:
`** (CaseClauseError) no case clause matching: [:b, :list]`

So although the last element is present, given that it is the list termination (empty list) it doesn't count as an "actual" element, but just as the termination of the list. Notice that instead of separating the last element with `|` like we did previously, we separated it with a `,` (comma), effectively indicating our pattern required 3 actual elements, and not 2 elements and a tail.
Because the empty list that signals the end of a list is not itself an "element" it didn't match, but when we use `|` it does match, because the tail might be a cons cell or the end of the list itself - the `|` separator means the "next pointer" of the list, while `,` means an actual element.

We also see that because no branch of the case could match, that we got a `CaseClauseError`, which just means, given the expression passed on to `case`, I couldn't find any "conforming" branch in those you've defined.

As you see the logic is the same as with the match operator we've seen before, but instead of getting `MatchError`s when it doesn't match we get `CaseClauseError`s.

What happens when we use `case` is that the expression in `case EXPRESSION do` gets matched against each branch of the case statement, so it's translated into something similar to (for illustration purposes):

{% highlight elixir %}
# when `a` is [:b, :list]
> case a do
>>   [_] = [:b, :list] -> ....
>>   [:b, :something_else] = [:b, :list] -> ...
>>   [:b, :list | tail] = [:b, :list] -> ...
> end
{% endhighlight %}

So the first branch `[_]` (list with a single element) doesn't match, neither does the second branch `[:b, :something_else]`, but the third does, because `a` is effectively in the form `[:b | [:list | []]]`. Or `[:b, :list | []]`

We can see some more examples, now with binary matching.

{% highlight elixir %}
date = "01-02-2019"
# "01-02-2019"

case date do
    "01-02-" <> year -> "it's year #{year}"
    _ -> "not sure"
end
# "it's year 2019"

case date do
    <<day::binary-size(3), month::binary-size(3), year::binary-size(4)>> -> "it's day #{day} in month #{month} and year #{year}"
    _ -> "don't know"
end
# "it's day 01- in month 02- and year 2019"

case date do
    <<day::binary-size(2), "-", month::binary-size(2), "-", year::binary-size(4)>> -> "it's day #{day} in month #{month} and year #{year}"
    _ -> "don't know"
end
# "it's day 01 in month 02 and year 2019"

date = "01/02/2019"
# "01/02/2019"

case date do
    <<day::binary-size(2), "-", month::binary-size(2), "-", year::binary-size(4)>> -> "it's day #{day} in month #{month} and year #{year}"
    _ -> "don't know"
end
# "don't know"
{% endhighlight %}

So this is way more interesting, because now we can start seeing ways to drive the "flow" of our programs by defining the form the data should have.

When we look at functions, all the same concepts apply, but instead of being inside the body of a function, they're used to define what "branch" of the function should be "called" when passed arguments of a certain form.

Let's create a file somewhere. The following examples will all need to be placed inside that module but we'll omit it in the examples, and then the whole module must be copied to your iex shell before running the examples.

{% highlight elixir %}
# some_file.ex

defmodule PatternMatching do

    def test_1(:a), do: "Function matched on :a"
    def test_1([]), do: "Function matched on empty list"
    def test_1([element]) do
        IO.puts("Function matched on list with a single element: #{inspect element}")
        test_1(element)
    end
    def test_1([head| t]) do
        IO.puts("Function matched on non_empty list, with head: #{inspect head} and tail #{inspect tail}")
        test_1(t)
    end

    def test_1(element), do: "Function with one non-list argument: #{inspect element}"

end
{% endhighlight %}

Copy that into the iex shell and you should see something like:

{% highlight elixir %}
# {:module, PatternMatching,
 <<70, 79, 82, 49, 0, 0, 7, 128, 66, 69, 65, 77, 65, 116, 85, 56, 0, 0, 0, 189,
   0, 0, 0, 20, 22, 69, 108, 105, 120, 105, 114, 46, 80, 97, 116, 116, 101, 114,
   110, 77, 97, 116, 99, 104, 105, 110, 103, ...>>, {:test_1, 1}}
{% endhighlight %}

Now let's try calling some functions:

{% highlight elixir %}
PatternMatching.test_1(:a)
# "Function matched on :a"

PatternMatching.test_1(:b)
# "Function with one non-list argument: :b"

PatternMatching.test_1([:b])
# Function matched on list with a single element: :b
# "Function with one non-list argument: :b"

PatternMatching.test_1([:a])
# Function matched on list with a single element: :a
# "Function matched on :a"

PatternMatching.test_1([:b, :b])
# Function matched on non_empty list, with head: :b and tail [:b]
# Function matched on list with a single element: :b
# "Function with one non-list argument: :b"
{% endhighlight %}

Now if we move `def test_1(element), do: "Function with one non-list argument: #{inspect element}"`
to be the first function defined (and then copy the module again into iex) and run the same function calls we did before:

{% highlight elixir %}
PatternMatching.test_1(:a)                                                                            
# "Function with one non-list argument: :a"

PatternMatching.test_1([:b, :a])
# "Function with one non-list argument: [:b, :a]"
{% endhighlight %}

Now everything matches the first one, because it has no defined pattern it accepts everything so none of the other functions we defined get the chance to be tested and consequently run.

If we add this one function to the end:
{% highlight elixir %}
def test_1(argument_1, argument_2), do: "Matched with 2 arguments: 1: #{inspect argument_1} #### 2: #{inspect argument_2}"
{% endhighlight %}`

Then copy the module again to iex and run:

{% highlight elixir %}
PatternMatching.test_1("arg1", ["arg", "2"])
# "Matched with 2 arguments: 1: \"arg1\" #### 2: [\"arg\", \"2\"]"
{% endhighlight %}

Now because we're passing two actual arguments, none of the others will match (since they have arity of 1, meaning the number of arguments they accept is 1) and since only this last one accepts 2 arguments, only this one will match.

If we do:

{% highlight elixir %}
PatternMatching.test_1("arg1", ["arg", "2"], "arg3")
# ** (UndefinedFunctionError) function PatternMatching.test_1/3 is undefined or private. Did you mean one of:

#      * test_1/1
#      * test_1/2

#    PatternMatching.test_1("arg1", ["arg", "2"], "arg3")
{% endhighlight %}

This basically means, "I'm sorry, I couldn't find a function test_1 with arity 3 in the module PatternMatching". It also hints, if it can find suitable hypotheses, other functions that are available in that module. In this case it can see that we have a function named "test_1" with both arity 1 and 2, so perhaps we might have tried to call that but got the arity wrong.

So by now we have seen some use cases for pattern matching and we've learned a bit about them, they will match the first case that has a conforming pattern, be it a branch of a `case` statement or a function definition.

This means, that the order in which we define the branches or functions has a meaning as well. And also that they will raise specific errors when it can't find an actionable code path.
If we define the branch or function as simply a bound (or unbound, `_`, variable) then it will match everything. 

So our patterns must go from the most explicit to the least explicit in order to be unambiguous and actually describe the flow of our program.
In `case` statements, the whole expression is matched as a single element, while in functions there's also arity to take into account.

One last piece of functionality that we can use in pattern matching is guard clauses. These allow us to extend further our pattern matching capabilities. These guard clauses allow the usage of a subset of kernel functions that are "special", in the sense that they are pure functions and guaranteed to be "fast". So for instance we might want to discern if the value is a list or a map, if it's a map we also want to discern between an empty map and a non-empty map, but in the case of it being a list we don't care if it's a list with 0 or more elements, just that it's a list.
We could write it as such:

{% highlight elixir %}
a_list = []

case a_list do
    expression when is_list(expression) -> "it's a list"
    %{} = expression when expression == %{} -> "it's an empty map"
    %{} = expression -> "it's a non-empty map"
end

# "it's a list"


a_list = [:two, :elements]

case a_list do
    expression when is_list(expression) -> "it's a list"
    %{} = expression when expression == %{} -> "it's an empty map"
    %{} = expression -> "it's a non-empty map"
end

# "it's a list"


a_list = %{}
case a_list do
    expression when is_list(expression) -> "it's a list"
    %{} = expression when expression == %{} -> "it's an empty map"
    %{} = expression -> "it's a non-empty map"
end

# "it's an empty map"

a_list = %{some_key: 1}
case a_list do
    expression when is_list(expression) -> "it's a list"
    %{} = expression when expression == %{} -> "it's an empty map"
    %{} = expression -> "it's a non-empty map"
end

# "it's a non-empty map"

If we switch the last two branches and set `a_list` variable to an empty map we get:

a_list = %{}
case a_list do
    expression when is_list(expression) -> "it's a list"
    %{} = expression -> "it's a non-empty map #{inspect expression}"
    %{} = expression when expression == %{} -> "it's an empty map"
end

# "it's a non-empty map %{}"
{% endhighlight %}

Although the map is empty, because the 2nd branch matches any map, empty or not, it's that one that gets evaluated when the expression is a map, and the 3rd branch now has no chance to be tested.


So now let's see some real cases where we can use this.

Let's define a module and structure that holds users and the type of users they are along with their age. We'll also define some functions to work with lists of users.

{% highlight elixir %}
defmodule User do
	defstruct [:name, :age, type: :regular]

    def count_older_than(list, age) do
        count_older_than(list, age, 0)
    end

    def count_older_than([], _, count), do: count

    def count_older_than(
        [%User{age: user_age} = user | t],
        age,
        count
    ) when user_age > age do
        count_older_than(t, age, count + 1)
    end

    def count_older_than([_ | t], age, acc), do: count_older_than(t, age, acc)
end
{% endhighlight %}

And now let's create some users and a list of them:

{% highlight elixir %}
user_1 = %User{name: "John", age: 25}
user_2 = %User{name: "Doris", age: 30, type: :administrator}
user_3 = %User{name: "Jane", age: 28}
user_4 = %User{name: "Joe", age: 60, type: :administrator}
user_5 = %User{name: "Jelly", age: 15}

list_of_users = [user_1, user_2, user_3, user_4, user_5]
# [
#   %User{age: 25, name: "John", type: :regular},
#   %User{age: 30, name: "Doris", type: :administrator},
#   %User{age: 28, name: "Jane", type: :regular},
#   %User{age: 60, name: "Joe", type: :administrator},
#    %User{name: "Jelly", age: 15, type: :regular}
# ]

count = User.count_older_than(list_of_users, 28)
# 2

count
# 2

count = User.count_older_than(list_of_users, 25)
# 3

count = User.count_older_than(list_of_users, 20)
# 4
{% endhighlight %}

Now lets add these functions to the previous module and copy it again to iex:

{% highlight elixir %}
def extract_administrators(list) when is_list(list) do
    extract_administrators(list, [])
end

def extract_administrators([], acc), do: acc

def extract_administrators([%User{type: :administrator} = user | t], acc) do
    extract_administrators(t, [user | acc])
end
def extract_administrators([_| t], acc), do: extract_administrators(t, acc)
{% endhighlight %}

And then run

{% highlight elixir %}
admins = User.extract_administrators(list_of_users)
# [
#   %User{age: 60, name: "Joe", type: :administrator},
#   %User{age: 30, name: "Doris", type: :administrator}
# ]
{% endhighlight %}

So you can see that we called first the `extract_administrators` with only 1 argument, a list, so it matched the first function definition. This function all it did was call the same function, but now with two arguments, the second being an empty list.

This is a fairly regular thing to do, that 2nd argument (in this case) is what is usually called the "accumulator" and it's a simple way of recursively calling functions and "accumulate" the results of each call. In this case it's used to build a new list with all the administrators we find in the original list.

So this call ends up as (given the list we were working with):

`extract_administrators([%User{name: "John", age: 25, type: :regular} | t], [])`

Given that the first argument is not an empty list, it can't match the second function declaration. Given that the first cell in the first argument list is not a `%User{}` struct with the type `:administrator` it can't match the 3rd function, so it matches the 4th function:

`def extract_administrators([_| t], acc), do: extract_administrators(t, acc)`

In this function, what happens is, we ignore that first value, and we're only interested in the remaining list.

{% highlight elixir %}
[%User{age: 25, name: "John", type: :regular} | #head
    [%User{age: 30, name: "Doris", type: :administrator} | 
        [%User{age: 28, name: "Jane", type: :regular} |
            [%User{age: 60, name: "Joe", type: :administrator} |
                [%User{name: "Jelly", age: 15, type: :regular} | [] ]
            ]
        ]
    ]
]
{% endhighlight %}

So in this case we ignore `_` the head, and we call again the function `extract_administrators` with the remaining list and whatever is in the `acc` variable, so, which ends up being this call:

{% highlight elixir %}
extract_administrators(
    [%User{name: "Doris"...} | 
        [%User{name: "Jane"...} |
            [%User{name: "Joe"...} |
                [%User{name: "Jelly"...} | []]
            ]
        ]
    ], [])
{% endhighlight %}

Now when this function is called with these new parameters it will actually match the 3rd function clause

{% highlight elixir %}
def extract_administrators([%User{type: :administrator} = user | t], acc) do
    extract_administrators(t, [user | acc])
end
{% endhighlight %}

So here what we do is bind `user` to the `element`, and then call the function again with its tail, while also adding the user into the accumulator in the last argument.

You can see that it's a bit of symmetric operation when the head matches our constraints/pattern, we take the `head` from one list, and we place that as the `head` of the accumulator, then we pass the `tail` of the list, from which we took the `head` from, as the new list to the function.

The first time, acc was empty,  `[]`
If we declare `[user | acc]` , what we're declaring is:

{% highlight elixir %}
[%User{....} | []]
{% endhighlight %}

If we do it again with another user

{% highlight elixir %}
[%User{} | [%User{} | []]]
{% endhighlight %}
And so on.

So now it will be called as: 

{% highlight elixir %}
extract_administrators(
    [%User{name: "Jane"...} |
        [%User{name: "Joe"...} | []]
    ], [%{name: "Doris"...}])
{% endhighlight %}


Which again will match only the 4th function clause.

So then it's called as:

{% highlight elixir %}
extract_administrators(
    [%User{name: "Joe"...} | 
        [%User{name: "Jelly"...} | []
    ]], [%{name: "Doris"...} | []])
{% endhighlight %}

Which matches on the 3rd, so it will now call as:

{% highlight elixir %}
extract_administrators(
    [%User{name: "Jelly"...}] | []],
    [%User{name: "Joe"...} | [%{name: "Doris"...} | []]])
{% endhighlight %}


Which again matches only the forth it's called again this time with an empty list as the first argument.
And now because the first argument is empty, it matches on the 2nd function clause, where we end the recursion and just return the `acc` argument, leading to:

{% highlight elixir %}
[%User{name: "Joe"...}, %{name: "Doris"...}]
{% endhighlight %}

These are also composable, for instance you can use:

{% highlight elixir %}
list_of_users
|> User.extract_administrators()
|> User.count_older_than(28)
# 2
{% endhighlight %}

The last part of this write up is about using function clauses with different arities and pattern-matches in anonymous functions. There are a lot of modules in the standard lib that take functions as one of their parameters, specially those that deal with collections, such as the `Enum` module.

The `Enum` module has one function called `reduce/3` that basically is an abstraction over what we did we these functions. It takes an enumerable, an accumulator and a function, and we can use it to reduce the elements of the enumerable into whatever accumulator we want. We could write the `extract_administrators` function as:

{% highlight elixir %}
def extract_administrators(list) when is_list(list) do
    Enum.reduce(list, [], fn
        (%User{type: :administrator} = user, acc) -> [user | acc]
        (_, acc) -> acc
    end)
end
{% endhighlight %}

You can see two clauses on the `fn` declaration. `Enum.reduce` passes one element at a time from the `enumerable` provided as the first argument, to the anonymous function, along with the 2nd argument as the accumulator. The first time the anonymous function is called, the accumulator is the original accumulator in the 2nd argument (an empty list), and on the following ones it's whatever the anonymous function returned.

Since we receive each element one by one (outside of their original list) we just pattern match on the element. When all elements from the enumerable have been `enumerated` it returns whatever is the value of the accumulator.

There are plenty of use cases for recursive traversal of collections, in functional languages that's usually how you work on collections of items. There are also other useful functions in the `Enum` module. I use `reduce` and `map` a lot but there are more.

There's also other tricks you can use, such as using the same name for variables, underneath this makes the pattern only succeed if all instances of the binding resolve to the same value.

So for instance let's say you wanted to take pairs of users out of that list, that shared the same type of user.

{% highlight elixir %}
def pluck_pairs(list, type) when is_list(list) do
    pluck_pair(list, type, {nil, []})
end

def pluck_pairs([], type, full_acc), do: full_acc

def pluck_pairs([%{type: type} = user | t], type, {nil, acc}) do
    pluck_pairs(t, type, {user, acc})
end

def pluck_pairs([%{type: type} = user | t], type, {previous, acc}) do
    pluck_pairs(t, type, {nil, [{previous, user} | acc]})
end


def pluck_pairs([_ | t], type, acc), do: pluck_pairs(t, type, acc)
{% endhighlight %}

Or using the `Enum.reduce` form

{% highlight elixir %}
def pluck_pairs(list_of_users, type) do
    Enum.reduce(list_of_users, {nil, []}, fn
        (%User{type: ^type} = user, {nil, acc}) -> {user, acc}
        (%User{type: ^type} = user, {previous_match, acc}) -> {nil, [{previous_match, user} | acc]}
        (_, full_acc) -> full_acc
    end)
end
{% endhighlight %}

(we need to use the `^` pin operator to pin down the value of `type`, otherwise it would be re-bound during the `Enum.reduce` and match anything)


Another thing to keep in mind is, if the order of the accumulation matters,  that after reducing a collection the accumulator will have the elements in the inverse order, so if that's relevant you need to reverse the list, like with `Enum.reverse/1` or the erlang `:lists.reverse/1` (if it's a lit obviously) function.

And now you could use this inside a case function

{% highlight elixir %}
{% raw %}
case User.pluck_pairs(list_of_users, :regular) do
    {nil, [_|_] = acc} -> 
        IO.puts("No unmatched user and #{length(acc)} matched pairs")
        Notifications.send_pair_emails(acc)
    {%{name: name} = no_pair, [_|_] = acc} ->
        IO.puts("Unmatched user #{name} and #{length(acc)} matched pairs")
        Notifications.send_no_pair_email(user)
        Notifications.send_pair_emails(acc)
    {%{name: name} = user, acc} -> 
        IO.puts("Unmatched user #{name} and no pairs")
        Notifications.send_no_pair_email(user)
        acc
    {nil, acc} ->
        IO.puts("No pairs and no unmatched users")
        acc
end
{% endraw %}
{% endhighlight %}

And so on. Given the natural support for concurrent and parallel processes in Erlang & Elixir, the usage of message passing and so on, pattern matching becomes even more useful, as it's fairly straightforward to describe state-machine'y behaviours using a combination of processes, receive blocks and pattern matching. Of course, most of that is already quite abstracted into higher OTP constructs such as gen_server, gen_statem and other friends.
Hope this post helped you understand better pattern matching and illustrated some use cases, although there's plenty more that can't simply be covered in detail here - nonetheless try out and experiment!
