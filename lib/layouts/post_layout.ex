defmodule Micaelnussbaumer.PostLayout do
  use Tableau.Layout, layout: Micaelnussbaumer.RootLayout
  use Phoenix.Component

  def strftime(%DateTime{} = datetime),
    do: Calendar.strftime(datetime, "%d, %b %Y")

  def strftime(%Date{} = date),
    do: Calendar.strftime(date, "%d, %b %Y")

  def strftime(
        <<year::binary-size(4), "-", month::binary-size(2), "-", day::binary-size(2), _::binary>>
      ),
      do: Calendar.strftime(Date.from_iso8601!("#{year}-#{month}-#{day}"), "%d, %b, %Y")

  def template(assigns) do
    ~H"""
    <div class="blog-post-header">
      <h1 class="post-title">{@page.title}</h1>
      <p class="post-date">
        {strftime(@page.date)} / <span class="post-author">{@page.author}</span>
      </p>
    </div>
    <div class="blog-post-content">
      {{:safe, render(@inner_content)}}
    </div>
    """
  end
end
