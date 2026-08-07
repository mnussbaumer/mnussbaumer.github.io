defmodule Micaelnussbaumer.BlogPage do
  use Tableau.Page,
    layout: Micaelnussbaumer.RootLayout,
    permalink: "/pages/blog"

  use Phoenix.Component

  alias Micaelnussbaumer.PostLayout

  def template(assigns) do
    ~H"""
    <p class="pre-intro">
      The place where I post long-form articles, either tutorials/explanations or rants. 
    </p>
    <br />
    <ol class="blog-posts-list">
      <%= for post <- @posts do %>
        <a href={post.permalink} alt={post.title} class="blog-posts-entry">
          <li class="blog-posts-entry-content">
            <span class="date">{PostLayout.strftime(post.date)}</span>
            {post.title}
          </li>
        </a>
      <% end %>
    </ol>
    """
  end
end
