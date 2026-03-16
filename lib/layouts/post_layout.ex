defmodule Micaelnussbaumer.PostLayout do
  use Tableau.Layout, layout: Micaelnussbaumer.RootLayout
  use Phoenix.Component

  def template(assigns) do
    ~H"""
    <div class="blog-post-header">
      <h1 class="post-title">{@page.title}</h1>
      <p class="post-date">
        {Calendar.strftime(@page.date, "%d, %b %Y")} / <span class="post-author">{@page.author}</span>
      </p>
    </div>
    <div class="blog-post-content">
      {{:safe, render(@inner_content)}}
    </div>
    """
  end
end
