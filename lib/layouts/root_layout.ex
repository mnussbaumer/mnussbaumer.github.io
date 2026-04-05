defmodule Micaelnussbaumer.RootLayout do
  use Tableau.Layout
  use Phoenix.Component

  def template(assigns) do
    ~H"""
    <!DOCTYPE html>
    <html lang="en" theme="dark-theme">
      <head>
        <meta charset="utf-8" />
        <meta http_equiv="X-UA-Compatible" content="IE=edge" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />

        <title>
          {[@page[:title], "micaelnussbaumer"]
          |> Enum.filter(& &1)
          |> Enum.intersperse("|")
          |> Enum.join(" ")}
        </title>
        <link rel="stylesheet" href="/css/site.css" />
        <link rel="alternate" title="RSS Feed" type="application/rss+xml" href="/feed.xml">
        <script src="/js/site.js" />
        <link rel="icon" type="image/png" href="/images/graphics/favicon-96x96.png" sizes="96x96" />
        <link rel="icon" type="image/svg+xml" href="/images/graphics/favicon.svg" />
        <link rel="shortcut icon" href="/images/graphics/favicon.ico" />
        <link rel="apple-touch-icon" sizes="180x180" href="/images/graphics/apple-touch-icon.png" />
        <meta name="apple-mobile-web-app-title" content="mnussbaumer" />
        <link rel="manifest" href="/images/graphics/site.webmanifest" />
      </head>

      <body class={assigns[:view_type]}>
        <header>
          <ul>
            <li><a href="/pages/blog">blog</a></li>
            <li><a href="/pages/about">about</a></li>
            <li><a href="https://github.com/mnussbaumer" target="_blank">github</a></li>
            <li>
              <button type="button" id="theme-toggle" data-current="dark-theme">light</button>
            </li>
            <li class="odot">&odot;</li>
            <li><a href="/">micaelnussbaumer</a></li>
          </ul>
        </header>
        <main>
          <div id="content">
            {render(@inner_content)}
          </div>
          <div id="sidebar">
            <%= if Map.get(@page, :sidebar) do %>
              {@page.sidebar}
            <% else %>
              <a class="since" href="/pages/blog">blog</a>
              <ol class="blog-posts-list">
                <%= for post <- Enum.take(@posts, 3) do %>
                  <a href={post.permalink} alt={post.title}>
                    <li>
                      <span class="date">{Calendar.strftime(post.date, "%Y-%m-%d")}</span>
                      {post.title}
                    </li>
                  </a>
                <% end %>
              </ol>
            <% end %>
            <img class="sidebar-logo" src="/images/graphics/apple-touch-icon.png" />
          </div>
        </main>
        <footer>
          <a href="/feed.xml" target="_blank">RSS</a>
        </footer>
      </body>

      <%= if Mix.env() == :dev do %>
        {Phoenix.HTML.raw(Tableau.live_reload(assigns))}
      <% end %>
    </html>
    """
    |> Phoenix.HTML.Safe.to_iodata()
  end
end
