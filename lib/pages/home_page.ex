defmodule Micaelnussbaumer.HomePage do
  use Tableau.Page,
    layout: Micaelnussbaumer.RootLayout,
    permalink: "/"

  use Phoenix.Component

  def template(assigns) do
    ~H"""
    <p class="pre-intro">
      My personal website and blog, where you can find some information about me and where I post articles in the form of tutorials/concept explanations and explore different ideas related mostly with computer programming.
    </p>
    <br />
    <p>
      It's built with <a href="https://github.com/elixir-tools/tableau" target="_blank">Tableau</a>. All articles previous to 2026 are either from the previous blog hosted at this same address, or from medium.com, dev.to, but I'm using the opportunity to bring the articles from my other profiles into the same place.
    </p>
    <br />
    <hr class="hr-separator-b" />
    <ol class="blog-posts-list">
      <%= for post <- @posts do %>
        <li>
          <span class="date">{Calendar.strftime(post.date, "%Y-%m-%d")}</span>
          <a href={post.permalink}>{post.title}</a>
        </li>
      <% end %>
    </ol>
    """
  end
end
