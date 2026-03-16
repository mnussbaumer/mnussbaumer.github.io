defmodule Mix.Tasks.Micaelnussbaumer.Gen.Post do
  use Mix.Task

  @shortdoc "Generate a new post"
  @moduledoc @shortdoc

  @doc false
  def run(argv) do
    if argv == [] do
      raise "Missing argument: Filename"
    end

    post_title = Enum.join(argv, " ")
    post_date = Date.utc_today()
    post_time = "00:00:00 +07:00"

    file_name =
      post_title
      |> String.replace(" ", "-")
      |> String.replace("_", "-")
      |> String.replace(~r/[^[:alnum:]\/\-.]/, "")
      |> String.downcase()

    file_path =
      "./_posts/#{post_date}-#{file_name}.md"

    if File.exists?(file_path) do
      raise "File already exists"
    end

    front_matter = """
    ---
    author: micael nussbaumer
    layout: Micaelnussbaumer.PostLayout
    title: \"#{post_title}\"
    slug: #{String.downcase(post_title) |> String.replace(~r/[^a-z0-9\/\_]/, "-")}
    date: #{post_date} #{post_time}
    permalink: /posts/:slug
    categories:
    ---
    """

    File.write!(file_path, front_matter)

    Mix.shell().info("Succesfully created #{file_path}!")
  end
end
