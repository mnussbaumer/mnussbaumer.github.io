defmodule Micaelnussbaumer.HomePage do
  use Tableau.Page,
    layout: Micaelnussbaumer.RootLayout,
    permalink: "/"

  use Phoenix.Component

  def template(assigns) do
    ~H"""
    <p class="pre-intro">
      I can help you build software, specially web based applications. Here you can find some my personal blog and a round up of my experience under the `About` section. A link to github gives you some insight into some of my public projects - not only personal but also some commercial open-source work done for different clients.
    </p>
    <p style="margin: 20px 5px 0;">Besides github you can find me in some freelancing platforms online.</p>
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
